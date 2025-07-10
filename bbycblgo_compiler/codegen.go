package main

import (
	"bbycblgo_compiler/parser"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

// CodeGenerator holds the state for LLVM IR generation.
type CodeGenerator struct {
	*parser.BasebbyCBLVisitor
	globalBuilder  strings.Builder
	mainBuilder    strings.Builder
	symbolTable    *SymbolTable
	errors         []SemanticError
	strCounter     int
	tempRegCounter int
	verbose        bool
	sourceFilename string
}

// NewCodeGenerator creates a new code generator.
func NewCodeGenerator(symbolTable *SymbolTable, verbose bool, sourceFilename string) *CodeGenerator {
	return &CodeGenerator{
		BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
		symbolTable:       symbolTable,
		errors:            []SemanticError{},
		strCounter:        0,
		tempRegCounter:    0,
		verbose:           verbose,
		sourceFilename:    sourceFilename,
	}
}

// Generate generates LLVM IR from the parse tree.
func Generate(tree antlr.ParseTree, symbolTable *SymbolTable, verbose bool, sourceFilename string) (string, []SemanticError) {
	if tree == nil {
		return "", []SemanticError{}
	}
	codegen := NewCodeGenerator(symbolTable, verbose, sourceFilename)
	var finalIR strings.Builder

	moduleID := strings.TrimSuffix(filepath.Base(sourceFilename), filepath.Ext(sourceFilename))
	finalIR.WriteString(fmt.Sprintf("; ModuleID = '%s'\n", moduleID))
	finalIR.WriteString(fmt.Sprintf("source_filename = \"%s\"\n\n", sourceFilename))

	codegen.Visit(tree)

	finalIR.WriteString(codegen.globalBuilder.String())
	finalIR.WriteString(codegen.mainBuilder.String())

	if codegen.verbose {
		fmt.Println(finalIR.String())
	}
	return finalIR.String(), codegen.errors
}

func (c *CodeGenerator) addError(msg string, line int) {
	c.errors = append(c.errors, SemanticError{msg, line})
}

func (c *CodeGenerator) Visit(tree antlr.ParseTree) interface{} {
	if c.verbose {
		fmt.Printf("Visiting: %T\n", tree)
	}
	return tree.Accept(c)
}

func (c *CodeGenerator) VisitChildren(node antlr.RuleNode) interface{} {
	if c.verbose {
		fmt.Printf("Visiting children of: %T\n", node)
	}
	for _, child := range node.GetChildren() {
		child.(antlr.ParseTree).Accept(c)
	}
	return nil
}

func (c *CodeGenerator) VisitProgram(ctx *parser.ProgramContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Program")
	}
	c.Visit(ctx.DataDivision())
	c.Visit(ctx.ProcedureDivision())
	return nil
}

func (c *CodeGenerator) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Paragraph")
	}
	for _, sentence := range ctx.AllSentence() {
		c.Visit(sentence)
	}
	return nil
}

func (c *CodeGenerator) VisitSentence(ctx *parser.SentenceContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Sentence")
	}
	return c.VisitChildren(ctx)
}

func (c *CodeGenerator) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Data Division")
	}
	for _, dataEntry := range ctx.AllDataEntry() {
		if field, ok := c.symbolTable.rootScope.fields[strings.ToUpper(dataEntry.Identifier().GetText())]; ok {
			if field.picture.isNumeric {
				c.globalBuilder.WriteString(fmt.Sprintf("@%s = common global i32 0, align 4\n", field.name))
			} else {
				c.globalBuilder.WriteString(fmt.Sprintf("@%s = common global [%d x i8] zeroinitializer, align 1\n", field.name, field.picture.length))
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Procedure Division")
	}
	var procBuilder strings.Builder
	procBuilder.WriteString("declare i32 @printf(i8*, ...)\n")
	procBuilder.WriteString("declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)\n\n")
	procBuilder.WriteString("@.str_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n")
	procBuilder.WriteString("@.str_nl = private unnamed_addr constant [2 x i8] c\"\\0A\\00\", align 1\n")

	c.globalBuilder.WriteString(procBuilder.String())

	c.mainBuilder.WriteString("\ndefine i32 @main() {\n")
	c.mainBuilder.WriteString("entry:\n")

	for _, p := range ctx.AllParagraph() {
		c.Visit(p)
	}
	for _, s := range ctx.AllSentence() {
		c.Visit(s)
	}

	c.mainBuilder.WriteString("  ret i32 0\n")
	c.mainBuilder.WriteString("}\n")

	return nil
}

func (c *CodeGenerator) VisitStatement(ctx *parser.StatementContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Statement")
	}
	return c.VisitChildren(ctx)
}

func (c *CodeGenerator) VisitDisplayStmt(ctx *parser.DisplayStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Display Stmt")
	}
	for _, item := range ctx.AllDisplayItem() {
		if id, ok := item.Expr().(*parser.IdExprContext); ok {
			if field, ok := c.symbolTable.rootScope.fields[strings.ToUpper(id.GetText())]; ok {
				if field.picture.isNumeric {
					tempReg := fmt.Sprintf("%%t%d", c.tempRegCounter)
					c.tempRegCounter++
					c.mainBuilder.WriteString(fmt.Sprintf("  %s = load i32, i32* @%s, align 4\n", tempReg, field.name))
					c.mainBuilder.WriteString(fmt.Sprintf("  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str_int, i32 0, i32 0), i32 %s)\n", tempReg))
				} else {
					formatStrName := fmt.Sprintf("@.str_format_var%d", c.strCounter)
					c.globalBuilder.WriteString(fmt.Sprintf("%s = private unnamed_addr constant [6 x i8] c\"%%.*s\\0A\\00\", align 1\n", formatStrName))
					c.strCounter++

					tempPtr := fmt.Sprintf("%%t%d", c.tempRegCounter)
					c.tempRegCounter++
					c.mainBuilder.WriteString(fmt.Sprintf("  %s = getelementptr inbounds [%d x i8], [%d x i8]* @%s, i32 0, i32 0\n", tempPtr, field.picture.length, field.picture.length, field.name))
					c.mainBuilder.WriteString(fmt.Sprintf("  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* %s, i32 0, i32 0), i32 %d, i8* %s)\n", formatStrName, field.picture.length, tempPtr))
				}
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitMoveStmt(ctx *parser.MoveStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Move Stmt")
	}
	to := ctx.ExprList(1).AllExpr()[0]
	from := ctx.ExprList(0).AllExpr()[0]

	if toField, ok := c.symbolTable.rootScope.fields[strings.ToUpper(to.GetText())]; ok {
		if fromLit, ok := from.(*parser.LitExprContext); ok {
			if !toField.picture.isNumeric {
				str := strings.Trim(fromLit.GetText(), "\"")
				strLen := len(str)

				strName := fmt.Sprintf("@.str%d", c.strCounter)
				c.strCounter++
				c.globalBuilder.WriteString(fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\", align 1\n", strName, strLen+1, str))

				destPtr := fmt.Sprintf("%%t%d", c.tempRegCounter)
				c.tempRegCounter++
				srcPtr := fmt.Sprintf("%%t%d", c.tempRegCounter)
				c.tempRegCounter++

				c.mainBuilder.WriteString(fmt.Sprintf("  %s = getelementptr inbounds [%d x i8], [%d x i8]* @%s, i32 0, i32 0\n", destPtr, toField.picture.length, toField.picture.length, toField.name))
				c.mainBuilder.WriteString(fmt.Sprintf("  %s = getelementptr inbounds [%d x i8], [%d x i8]* %s, i32 0, i32 0\n", srcPtr, strLen+1, strLen+1, strName))

				copySize := strLen
				if toField.picture.length < copySize {
					copySize = toField.picture.length
				}

				c.mainBuilder.WriteString(fmt.Sprintf("  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %s, i8* align 1 %s, i64 %d, i1 false)\n", destPtr, srcPtr, copySize))
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitStopStmt(ctx *parser.StopStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Stop Stmt")
	}
	return nil
}
