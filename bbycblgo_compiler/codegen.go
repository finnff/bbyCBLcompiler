package main

import (
	"bbycblgo_compiler/parser"
	"fmt"
	"path/filepath"
	"strings"
	"sync"

	"github.com/antlr4-go/antlr/v4"
	"tinygo.org/x/go-llvm"
)

var llvmInit sync.Once

// CodeGenerator holds the state for LLVM IR generation.
type CodeGenerator struct {
	*parser.BasebbyCBLVisitor
	context        llvm.Context
	module         llvm.Module
	builder        llvm.Builder
	targetData     llvm.TargetData
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

	llvmInit.Do(func() {
		llvm.InitializeAllTargets()
		llvm.InitializeAllTargetMCs()
		llvm.InitializeAllAsmParsers()
		llvm.InitializeAllAsmPrinters()
		llvm.InitializeNativeTarget()
		llvm.InitializeNativeAsmPrinter()
	})

	codegen := NewCodeGenerator(symbolTable, verbose, sourceFilename)
	codegen.context = llvm.NewContext()
	defer codegen.context.Dispose()

	var target llvm.Target
	var err error

	defaultTriple := llvm.DefaultTargetTriple()
	target, err = llvm.GetTargetFromTriple(defaultTriple)
	if err != nil {
		fallbacks := []string{
			"x86_64-unknown-linux-gnu",
			"x86_64-pc-linux-gnu",
			"x86_64-linux-gnu",
		}
		for _, fallback := range fallbacks {
			target, err = llvm.GetTargetFromTriple(fallback)
			if err == nil {
				defaultTriple = fallback
				break
			}
		}
		if err != nil {
			return "", []SemanticError{{
				msg:  fmt.Sprintf("Unable to find suitable target: %v", err),
				line: 0,
			}}
		}
	}

	targetMachine := target.CreateTargetMachine(defaultTriple, "", "", llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault)
	codegen.targetData = targetMachine.CreateTargetData()

	moduleID := strings.TrimSuffix(filepath.Base(sourceFilename), filepath.Ext(sourceFilename))
	codegen.module = codegen.context.NewModule(moduleID)
	codegen.module.SetDataLayout(codegen.targetData.String())
	codegen.module.SetTarget(defaultTriple)

	codegen.builder = codegen.context.NewBuilder()
	defer codegen.builder.Dispose()

	codegen.Visit(tree)

	if err := llvm.VerifyModule(codegen.module, llvm.PrintMessageAction); err != nil {
		return "", []SemanticError{{
			msg:  fmt.Sprintf("LLVM module verification failed: %v", err),
			line: 0,
		}}
	}

	finalIR := codegen.module.String()
	if codegen.verbose {
		fmt.Println(finalIR)
	}
	return finalIR, codegen.errors
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
				global := llvm.AddGlobal(c.module, c.context.Int32Type(), field.name)
				global.SetInitializer(llvm.ConstInt(c.context.Int32Type(), 0, false))
				global.SetAlignment(4)
			} else {
				globalType := llvm.ArrayType(c.context.Int8Type(), field.picture.length)
				global := llvm.AddGlobal(c.module, globalType, field.name)
				global.SetInitializer(llvm.ConstNull(globalType))
				global.SetAlignment(1)
			}
		}
	}
	return nil
}

func (c *CodeGenerator) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Procedure Division")
	}

	// Declare printf
	printfType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{llvm.PointerType(c.context.Int8Type(), 0)}, true)
	llvm.AddFunction(c.module, "printf", printfType)

	// Declare llvm.memcpy
	memcpyType := llvm.FunctionType(c.context.VoidType(), []llvm.Type{
		llvm.PointerType(c.context.Int8Type(), 0), // dest
		llvm.PointerType(c.context.Int8Type(), 0), // src
		c.context.Int64Type(),                     // len
		c.context.Int1Type(),                      // isvolatile
	}, false)
	llvm.AddFunction(c.module, "llvm.memcpy.p0i8.p0i8.i64", memcpyType)

	// Create main function
	mainFuncType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{}, false)
	mainFunc := llvm.AddFunction(c.module, "main", mainFuncType)
	entryBlock := c.context.AddBasicBlock(mainFunc, "entry")
	c.builder.SetInsertPointAtEnd(entryBlock)

	for _, p := range ctx.AllParagraph() {
		c.Visit(p)
	}
	for _, s := range ctx.AllSentence() {
		c.Visit(s)
	}

	c.builder.CreateRet(llvm.ConstInt(c.context.Int32Type(), 0, false))

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
				printf := c.module.NamedFunction("printf")
				if field.picture.isNumeric {
					formatStr := c.builder.CreateGlobalStringPtr("%d\n", ".str_int")
					loadedVar := c.builder.CreateLoad(c.module.NamedGlobal(field.name).GlobalValueType(), c.module.NamedGlobal(field.name), "")
					c.builder.CreateCall(printf.GlobalValueType(), printf, []llvm.Value{formatStr, loadedVar}, "")
				} else {
					formatStr := c.builder.CreateGlobalStringPtr("%.*s\n", fmt.Sprintf(".str_format_var%d", c.strCounter))
					c.strCounter++

					destPtr := c.module.NamedGlobal(field.name)
					indices := []llvm.Value{
						llvm.ConstInt(c.context.Int32Type(), 0, false),
						llvm.ConstInt(c.context.Int32Type(), 0, false),
					}
					gep := c.builder.CreateInBoundsGEP(llvm.PointerType(c.context.Int8Type(), 0), destPtr, indices, "")

					c.builder.CreateCall(printf.GlobalValueType(), printf, []llvm.Value{
						formatStr,
						llvm.ConstInt(c.context.Int32Type(), uint64(field.picture.length), false),
						gep,
					}, "")
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

				// Create a global constant for the string literal
				strName := fmt.Sprintf(".str%d", c.strCounter)
				c.strCounter++
				globalStr := c.builder.CreateGlobalStringPtr(str, strName)

				destPtr := c.module.NamedGlobal(toField.name)

				indices := []llvm.Value{
					llvm.ConstInt(c.context.Int32Type(), 0, false),
					llvm.ConstInt(c.context.Int32Type(), 0, false),
				}

				destGEP := c.builder.CreateInBoundsGEP(llvm.PointerType(c.context.Int8Type(), 0), destPtr, indices, "")
				srcGEP := c.builder.CreateInBoundsGEP(llvm.PointerType(c.context.Int8Type(), 0), globalStr, indices, "")

				copySize := strLen
				if toField.picture.length < copySize {
					copySize = toField.picture.length
				}

				memcpy := c.module.NamedFunction("llvm.memcpy.p0i8.p0i8.i64")
				c.builder.CreateCall(memcpy.GlobalValueType(), memcpy, []llvm.Value{
					destGEP,
					srcGEP,
					llvm.ConstInt(c.context.Int64Type(), uint64(copySize), false),
					llvm.ConstInt(c.context.Int1Type(), 0, false),
				}, "")
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
