package main

import (
	"bbycblgo_compiler/parser"
	"fmt"
	"path/filepath"
	"strconv"
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
	mainEntryBlock llvm.BasicBlock
	printfFunc     llvm.Value
	memcpyFunc     llvm.Value
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

func (c *CodeGenerator) getFieldSymbol(name string, line int) *FieldSymbol {
	uname := strings.ToUpper(name)
	if fields, ok := c.symbolTable.rootScope.fields[uname]; ok && len(fields) > 0 {
		// For now, just return the first field found. 
		// A more robust solution would involve resolving qualified names.
		return fields[0]
	}
	c.addError(fmt.Sprintf("Field '%s' not found in symbol table", name), line)
	return nil
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
	if ctx.DataDivision() != nil {
		c.Visit(ctx.DataDivision())
	}
	if ctx.ProcedureDivision() != nil {
		c.Visit(ctx.ProcedureDivision())
	}
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
	// Ensure the builder's insert point is at the end of the main function's entry block
	if !c.mainEntryBlock.IsNil() {
		c.builder.SetInsertPointAtEnd(c.mainEntryBlock)
	}
	return c.VisitChildren(ctx)
}

func (c *CodeGenerator) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Data Division")
	}
	for _, dataEntry := range ctx.AllDataEntry() {
		fieldName := dataEntry.Identifier().GetText()
		if field := c.getFieldSymbol(fieldName, dataEntry.GetStart().GetLine()); field != nil {
			var global llvm.Value
			if field.picture.isNumeric {
				global = llvm.AddGlobal(c.module, c.context.Int32Type(), field.name)
				global.SetInitializer(llvm.ConstInt(c.context.Int32Type(), 0, false))
				global.SetAlignment(4)
			} else {
				globalType := llvm.ArrayType(c.context.Int8Type(), field.picture.length)
				global = llvm.AddGlobal(c.module, globalType, field.name)
				global.SetInitializer(llvm.ConstNull(globalType))
				global.SetAlignment(1)
			}
			if c.verbose {
				fmt.Printf("Created global variable: %s (Value: %v)\n", global.Name(), global)
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
	c.printfFunc = llvm.AddFunction(c.module, "printf", printfType)
	if c.printfFunc.IsNil() {
		c.addError("Failed to declare printf function", ctx.GetStart().GetLine())
		return nil
	}
	if c.verbose {
		fmt.Printf("Declared printf function: %v (Type: %v)\n", c.printfFunc, c.printfFunc.GlobalValueType())
	}

	// Declare llvm.memcpy.p0.p0.i64
	memcpyType := llvm.FunctionType(c.context.VoidType(), []llvm.Type{
		llvm.PointerType(c.context.Int8Type(), 0), // dest
		llvm.PointerType(c.context.Int8Type(), 0), // src
		c.context.Int64Type(),                     // len
		c.context.Int1Type(),                      // isvolatile
	}, false)
	c.memcpyFunc = c.module.NamedFunction("llvm.memcpy.p0.p0.i64")
	if c.memcpyFunc.IsNil() {
		// If not found, add it. This handles cases where it might not be implicitly declared by LLVM.
		c.memcpyFunc = llvm.AddFunction(c.module, "llvm.memcpy.p0.p0.i64", memcpyType)
		if c.memcpyFunc.IsNil() {
			c.addError("Failed to declare llvm.memcpy.p0.p0.i64 function", ctx.GetStart().GetLine())
			return nil
		}
	}

	// Create main function
	mainFuncType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{}, false)
	mainFunc := llvm.AddFunction(c.module, "main", mainFuncType)
	c.mainEntryBlock = c.context.AddBasicBlock(mainFunc, "entry")
	c.builder.SetInsertPointAtEnd(c.mainEntryBlock)

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
			// Handle identifier (field) case
			fieldName := id.GetText()
			if field := c.getFieldSymbol(fieldName, id.GetStart().GetLine()); field != nil {
				if c.printfFunc.IsNil() {
					c.addError("printf function not found", id.GetStart().GetLine())
					continue
				}
				destPtr := c.module.NamedGlobal(field.name)

				if destPtr.IsNil() {
					if c.verbose {
						fmt.Printf("Display error: Global variable %s not found\n", field.name)
					}
					continue
				}

				if field.picture.isNumeric {
					formatStr := c.builder.CreateGlobalStringPtr("%d", ".str_int")
					loadedVar := c.builder.CreateLoad(c.context.Int32Type(), destPtr, "")
					c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{formatStr, loadedVar}, "")
					if c.verbose {
						fmt.Printf("Generated display for numeric field: %s\n", field.name)
					}
				} else {
					formatStr := c.builder.CreateGlobalStringPtr("%.*s", fmt.Sprintf(".str_format_var%d", c.strCounter))
					c.strCounter++

					indices := []llvm.Value{
						llvm.ConstInt(c.context.Int32Type(), 0, false),
						llvm.ConstInt(c.context.Int32Type(), 0, false),
					}
					gep := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), field.picture.length), destPtr, indices, "")

					c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{
						formatStr,
						llvm.ConstInt(c.context.Int32Type(), uint64(field.picture.length), false),
						gep,
					}, "")
					if c.verbose {
						fmt.Printf("Generated display for string field: %s\n", field.name)
					}
				}
			}
		} else if lit, ok := item.Expr().(*parser.LitExprContext); ok {
			// Handle literal case - NOW AT THE CORRECT LEVEL!
			if c.printfFunc.IsNil() {
				c.addError("printf function not found", lit.GetStart().GetLine())
				continue
			}

			if c.verbose {
				fmt.Printf("Printf function in DisplayStmt (literal): %v (Type: %v)\n", c.printfFunc, c.printfFunc.GlobalValueType())
			}

			str := strings.Trim(lit.GetText(), "\"")
			strLen := len(str)

			// For DISPLAY with multiple items, just print the string without newline
			// The newline should only be added after all items are printed
			formatStr := c.builder.CreateGlobalStringPtr("%.*s", fmt.Sprintf(".str_format_lit%d", c.strCounter))
			c.strCounter++
			globalStr := c.builder.CreateGlobalStringPtr(str, fmt.Sprintf(".str_lit%d", c.strCounter))
			c.strCounter++

			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				llvm.ConstInt(c.context.Int32Type(), 0, false),
			}
			gep := c.builder.CreateInBoundsGEP(globalStr.GlobalValueType(), globalStr, indices, "")

			c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{
				formatStr,
				llvm.ConstInt(c.context.Int32Type(), uint64(strLen), false),
				gep,
			}, "")
			if c.verbose {
				fmt.Printf("Generated display for string literal: %s\n", str)
			}
		}
	}

	// Add a newline after all display items (unless WITH NO ADVANCING is specified)
	// For now, always add newline - you can check for WITH NO ADVANCING later
	newlineStr := c.builder.CreateGlobalStringPtr("\n", fmt.Sprintf(".str_newline%d", c.strCounter))
	c.strCounter++
	c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{newlineStr}, "")

	return nil
}

func (c *CodeGenerator) VisitMoveStmt(ctx *parser.MoveStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Move Stmt")
	}

	to := ctx.ExprList(1).AllExpr()[0]
	from := ctx.ExprList(0).AllExpr()[0]

	if c.verbose {
		fmt.Printf("Move from: %T (%s) to: %T (%s)\n", from, from.GetText(), to, to.GetText())
	}

	toField := c.getFieldSymbol(to.GetText(), to.GetStart().GetLine())
	if toField == nil {
		if c.verbose {
			fmt.Printf("Move error: Destination field %s not found in symbol table\n", to.GetText())
		}
		return nil
	}
	if c.verbose {
		fmt.Printf("Found destination field %s in symbol table\n", toField.name)
	}

	destPtr := c.module.NamedGlobal(toField.name)
	if c.verbose {
		fmt.Printf("Retrieved global variable %s: %v (Type: %v, IsGlobal: %t)\n", toField.name, destPtr, destPtr.Type(), destPtr.IsAGlobalVariable())
	}
	if destPtr.IsNil() {
		if c.verbose {
			fmt.Printf("Move error: Global variable %s not found or is nil after retrieval\n", toField.name)
		}
		return nil
	}

	if fromLit, ok := from.(*parser.LitExprContext); ok {
		if c.verbose {
			fmt.Printf("Source is literal: %s\n", fromLit.GetText())
		}
		if toField.picture.isNumeric {
			if c.verbose {
				fmt.Printf("Generating move for numeric field: %s\n", toField.name)
			}
			numStr := fromLit.GetText()
			val, err := strconv.ParseInt(numStr, 10, 32)
			if err != nil {
				c.addError(fmt.Sprintf("Invalid numeric literal: %s", numStr), from.GetStart().GetLine())
				return nil
			}
			c.builder.CreateStore(llvm.ConstInt(c.context.Int32Type(), uint64(val), true), destPtr)
			if c.verbose {
				fmt.Println("Generated store instruction for numeric move")
			}
		} else {
			if c.verbose {
				fmt.Printf("Generating move for string field: %s\n", toField.name)
			}
			str := strings.Trim(fromLit.GetText(), "\"")
			strLen := len(str)

			strName := fmt.Sprintf(".str%d", c.strCounter)
			c.strCounter++
			globalStr := c.builder.CreateGlobalStringPtr(str, strName)

			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				llvm.ConstInt(c.context.Int32Type(), 0, false),
			}

			// Reconstruct the destination pointer type as a pointer to an array of i8
			destType := llvm.PointerType(llvm.ArrayType(c.context.Int8Type(), toField.picture.length), 0)
			destGEP := c.builder.CreateInBoundsGEP(destType, destPtr, indices, "")

			// Reconstruct the source pointer type as a pointer to an array of i8
			srcType := llvm.PointerType(llvm.ArrayType(c.context.Int8Type(), int(uint64(strLen+1))), 0) // +1 for null terminator
			srcGEP := c.builder.CreateInBoundsGEP(srcType, globalStr, indices, "")

			copySize := strLen
			if toField.picture.length < copySize {
				copySize = toField.picture.length
			}

			if c.memcpyFunc.IsNil() {
				c.addError("llvm.memcpy.p0.p0.i64 function not found", from.GetStart().GetLine())
				return nil
			}
			c.builder.CreateCall(c.memcpyFunc.GlobalValueType(), c.memcpyFunc, []llvm.Value{
				destGEP,
				srcGEP,
				llvm.ConstInt(c.context.Int64Type(), uint64(copySize), false),
				llvm.ConstInt(c.context.Int1Type(), 0, false),
			}, "")
			if c.verbose {
				fmt.Println("Generated memcpy call for string move")
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