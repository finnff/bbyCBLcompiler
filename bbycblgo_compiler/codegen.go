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
	irBuilder      strings.Builder
	symbolTable    *SymbolTable
	errors         []SemanticError
	strCounter     int
	verbose        bool
	values         map[string]string
	sourceFilename string
}

// NewCodeGenerator creates a new code generator.
func NewCodeGenerator(symbolTable *SymbolTable, verbose bool, sourceFilename string) *CodeGenerator {
	return &CodeGenerator{
		BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
		symbolTable:       symbolTable,
		errors:            []SemanticError{},
		strCounter:        0,
		verbose:           verbose,
		values:            make(map[string]string),
		sourceFilename:    sourceFilename,
	}
}

// Generate generates LLVM IR from the parse tree.
func Generate(tree antlr.ParseTree, symbolTable *SymbolTable, verbose bool, sourceFilename string) (string, []SemanticError) {
	if tree == nil {
		return "", []SemanticError{}
	}
	codegen := NewCodeGenerator(symbolTable, verbose, sourceFilename)
	// Basic structure for a .ll file
	moduleID := strings.TrimSuffix(filepath.Base(sourceFilename), filepath.Ext(sourceFilename))
	codegen.irBuilder.WriteString(fmt.Sprintf("; ModuleID = '%s'\n", moduleID))
	codegen.irBuilder.WriteString(fmt.Sprintf("source_filename = \"%s\"\n\n", sourceFilename))

	codegen.Visit(tree)

	if codegen.verbose {
		fmt.Println(codegen.irBuilder.String())
	}
	return codegen.irBuilder.String(), codegen.errors
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
	return c.VisitChildren(ctx)
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
		if field, ok := c.symbolTable.rootScope.fields[dataEntry.Identifier().GetText()]; ok {
			if field.picture.isNumeric {
				c.irBuilder.WriteString(fmt.Sprintf("@%s = common global i32 0, align 4\n", field.name))
			} else {
				c.irBuilder.WriteString(fmt.Sprintf("@%s = common global [%d x i8] zeroinitializer, align 1\n", field.name, field.picture.length))
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Procedure Division")
	}
	c.irBuilder.WriteString("declare i32 @printf(i8*, ...)\n\n")
	c.irBuilder.WriteString("@.str_int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n")

	c.irBuilder.WriteString("define i32 @main() {\n")
	c.irBuilder.WriteString("entry:\n")

	c.VisitChildren(ctx)

	c.irBuilder.WriteString("  ret i32 0\n")
	c.irBuilder.WriteString("}\n")
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
		if lit, ok := item.Expr().(*parser.LitExprContext); ok {
			str := lit.GetText()
			str = strings.Trim(str, "\"")
			str = strings.ReplaceAll(str, "\n", "\\0A")
			str = strings.ReplaceAll(str, "\r", "\\0D")

			// Create a global constant for the string
			strName := fmt.Sprintf("@.str%d", c.strCounter)
			c.strCounter++
			c.irBuilder.WriteString(fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\", align 1\n", strName, len(str)+1, str))

			// Call printf
			c.irBuilder.WriteString(fmt.Sprintf("  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([%d x i8], [%d x i8]* %s, i32 0, i32 0))\n", len(str)+1, len(str)+1, strName))
		} else if id, ok := item.Expr().(*parser.IdExprContext); ok {
			if field, ok := c.symbolTable.rootScope.fields[id.GetText()]; ok {
				if field.picture.isNumeric {
					c.irBuilder.WriteString(fmt.Sprintf("  %%t%d = load i32, i32* @%s, align 4\n", c.strCounter, field.name))
					c.irBuilder.WriteString(fmt.Sprintf("  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str_int, i32 0, i32 0), i32 %%t%d)\n", c.strCounter))
					c.strCounter++
				} else {
					if val, ok := c.values[field.name]; ok {
						strName := fmt.Sprintf("@.str%d", c.strCounter)
						c.strCounter++
						c.irBuilder.WriteString(fmt.Sprintf("%s = private unnamed_addr constant [%d x i8] c\"%s\\00\", align 1\n", strName, len(val)+1, val))
						c.irBuilder.WriteString(fmt.Sprintf("  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([%d x i8], [%d x i8]* %s, i32 0, i32 0))\n", len(val)+1, len(val)+1, strName))
					}
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

	if c.verbose {
		fmt.Printf("Moving %s to %s\n", from.GetText(), to.GetText())
	}

	if toField, ok := c.symbolTable.rootScope.fields[to.GetText()]; ok {
		if fromLit, ok := from.(*parser.LitExprContext); ok {
			if toField.picture.isNumeric {
				c.irBuilder.WriteString(fmt.Sprintf("  store i32 %s, i32* @%s, align 4\n", fromLit.GetText(), toField.name))
			} else {
				str := fromLit.GetText()
				str = strings.Trim(str, "\"")
				c.values[toField.name] = str
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitStopStmt(ctx *parser.StopStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Stop Stmt")
	}
	c.irBuilder.WriteString("  ret i32 0\n")
	return nil
}
