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
	stopped        bool
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
		stopped:           false,
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

func (c *CodeGenerator) checkNil(val llvm.Value, msg string, line int) bool {
	if val.IsNil() {
		c.addError(fmt.Sprintf("%s is nil", msg), line)
		return true
	}
	if val.Type().IsNil() {
		c.addError(fmt.Sprintf("type of %s is nil", msg), line)
		return true
	}
	return false
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
	if c.checkNil(c.printfFunc, "printf function", ctx.GetStart().GetLine()) {
		return nil
	}
	if c.verbose {
		fmt.Printf("Declared printf function: %v\n", c.printfFunc)
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
		c.memcpyFunc = llvm.AddFunction(c.module, "llvm.memcpy.p0.p0.i64", memcpyType)
		if c.checkNil(c.memcpyFunc, "memcpy function", ctx.GetStart().GetLine()) {
			return nil
		}
	}

	// Create main function
	mainFuncType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{}, false)
	mainFunc := llvm.AddFunction(c.module, "main", mainFuncType)
	if c.checkNil(mainFunc, "main function", ctx.GetStart().GetLine()) {
		return nil
	}
	c.mainEntryBlock = c.context.AddBasicBlock(mainFunc, "entry")
	c.builder.SetInsertPointAtEnd(c.mainEntryBlock);

	for _, p := range ctx.AllParagraph() {
		c.Visit(p)
		if c.stopped {
			return nil
		}
	}
	for _, s := range ctx.AllSentence() {
		c.Visit(s)
		if c.stopped {
			return nil
		}
	}

	// If we haven't stopped, we need a return statement
	if !c.stopped {
		c.builder.CreateRet(llvm.ConstInt(c.context.Int32Type(), 0, false))
	}

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
		val := c.Visit(item.Expr())
		if val == nil {
			continue
		}
		value, ok := val.(llvm.Value)
		if !ok || c.checkNil(value, "display item", item.GetStart().GetLine()) {
			continue
		}

		if id, ok := item.Expr().(*parser.IdExprContext); ok {
			fieldName := id.GetText()
			if field := c.getFieldSymbol(fieldName, id.GetStart().GetLine()); field != nil {
				if field.picture.isNumeric {
					formatStr := c.builder.CreateGlobalStringPtr("%d", ".str_int")
					if c.checkNil(formatStr, "format string for numeric display", item.GetStart().GetLine()) {
						continue
					}
					c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{formatStr, value}, "")
				} else {
					formatStr := c.builder.CreateGlobalStringPtr("%.*s", fmt.Sprintf(".str_format_var%d", c.strCounter))
					if c.checkNil(formatStr, "format string for string display", item.GetStart().GetLine()) {
						continue
					}
					c.strCounter++
					destPtr := c.module.NamedGlobal(field.name)
					indices := []llvm.Value{
						llvm.ConstInt(c.context.Int32Type(), 0, false),
						llvm.ConstInt(c.context.Int32Type(), 0, false),
					}
					gep := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), field.picture.length), destPtr, indices, "")
					if c.checkNil(gep, "GEP for string display", item.GetStart().GetLine()) {
						continue
					}
					c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{
						formatStr,
						llvm.ConstInt(c.context.Int32Type(), uint64(field.picture.length), false),
						gep,
					}, "")
				}
			}
		} else if lit, ok := item.Expr().(*parser.LitExprContext); ok {
			litStr := lit.GetText()
			if _, err := strconv.ParseInt(litStr, 10, 32); err == nil {
				formatStr := c.builder.CreateGlobalStringPtr("%d", ".str_int")
				if c.checkNil(formatStr, "format string for numeric literal display", item.GetStart().GetLine()) {
					continue
				}
				c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{formatStr, value}, "")
			} else {
				str := strings.Trim(lit.GetText(), "\"")
				strLen := len(str)
				formatStr := c.builder.CreateGlobalStringPtr("%.*s", fmt.Sprintf(".str_format_lit%d", c.strCounter))
				if c.checkNil(formatStr, "format string for string literal display", item.GetStart().GetLine()) {
					continue
				}
				c.strCounter++
				globalStr := c.builder.CreateGlobalStringPtr(str, fmt.Sprintf(".str_lit%d", c.strCounter))
				if c.checkNil(globalStr, "global string for literal display", item.GetStart().GetLine()) {
					continue
				}
				c.strCounter++
				c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{
					formatStr,
					llvm.ConstInt(c.context.Int32Type(), uint64(strLen), false),
					globalStr,
				}, "")
			}
		}
	}

	newlineStr := c.builder.CreateGlobalStringPtr("\n", fmt.Sprintf(".str_newline%d", c.strCounter))
	if c.checkNil(newlineStr, "newline string", ctx.GetStart().GetLine()) {
		return nil
	}
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

	toField := c.getFieldSymbol(to.GetText(), to.GetStart().GetLine())
	if toField == nil {
		return nil
	}
	destPtr := c.module.NamedGlobal(toField.name)
	if c.checkNil(destPtr, "destination pointer in move", to.GetStart().GetLine()) {
		return nil
	}

	fromVal := c.Visit(from)
	if fromVal == nil {
		return nil
	}
	value, ok := fromVal.(llvm.Value)
	if !ok || c.checkNil(value, "source value in move", from.GetStart().GetLine()) {
		return nil
	}

	if toField.picture.isNumeric {
		c.builder.CreateStore(value, destPtr)
	} else {
		// String move
		str := strings.Trim(from.GetText(), "\"")
		strLen := len(str)

		strName := fmt.Sprintf(".str%d", c.strCounter)
		c.strCounter++
		globalStr := c.builder.CreateGlobalStringPtr(str, strName)
		if c.checkNil(globalStr, "global string for move", from.GetStart().GetLine()) {
			return nil
		}

		indices := []llvm.Value{
			llvm.ConstInt(c.context.Int32Type(), 0, false),
			llvm.ConstInt(c.context.Int32Type(), 0, false),
		}

		destGEP := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), toField.picture.length), destPtr, indices, "")
		if c.checkNil(destGEP, "dest GEP for move", from.GetStart().GetLine()) {
			return nil
		}
		srcGEP := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), int(uint64(strLen+1))), globalStr, indices, "")
		if c.checkNil(srcGEP, "src GEP for move", from.GetStart().GetLine()) {
			return nil
		}

		copySize := strLen
		if toField.picture.length < copySize {
			copySize = toField.picture.length
		}

		c.builder.CreateCall(c.memcpyFunc.GlobalValueType(), c.memcpyFunc, []llvm.Value{
			destGEP,
			srcGEP,
			llvm.ConstInt(c.context.Int64Type(), uint64(copySize), false),
			llvm.ConstInt(c.context.Int1Type(), 0, false),
		}, "")
	}

	return nil
}



func (c *CodeGenerator) VisitStopStmt(ctx *parser.StopStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Stop Stmt")
	}
	c.builder.CreateRet(llvm.ConstInt(c.context.Int32Type(), 0, false))
	c.stopped = true
	return nil
}

func (c *CodeGenerator) VisitIfStmt(ctx *parser.IfStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting If Stmt")
	}

	conditionVal := c.Visit(ctx.Condition())
	if conditionVal == nil {
		c.addError("condition expression is nil", ctx.GetStart().GetLine())
		return nil
	}
	condition, ok := conditionVal.(llvm.Value)
	if !ok || condition.IsNil() {
		c.addError("condition is not a valid llvm value", ctx.GetStart().GetLine())
		return nil
	}

	startFunc := c.builder.GetInsertBlock().Parent()
	thenBlock := c.context.AddBasicBlock(startFunc, "then")
	mergeBlock := c.context.AddBasicBlock(startFunc, "ifcont")
	elseBlock := mergeBlock

	if ctx.ELSE() != nil {
		elseBlock = c.context.AddBasicBlock(startFunc, "else")
	}

	c.builder.CreateCondBr(condition, thenBlock, elseBlock)

	c.builder.SetInsertPointAtEnd(thenBlock)
	for _, stmt := range ctx.AllStatement() {
		c.Visit(stmt)
	}
	c.builder.CreateBr(mergeBlock)

	if ctx.ELSE() != nil {
		c.builder.SetInsertPointAtEnd(elseBlock)
	}

	c.builder.SetInsertPointAtEnd(mergeBlock)

	return nil
}

func (c *CodeGenerator) VisitCondition(ctx *parser.ConditionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Condition")
	}
	// For now, we only handle simple conditions.
	// In the future, we’ll handle AND/OR/XOR here.
	if len(ctx.AllSimpleCond()) > 0 {
		return c.Visit(ctx.SimpleCond(0))
	}
	c.addError("complex conditions not yet supported", ctx.GetStart().GetLine())
	return nil
}

func (c *CodeGenerator) VisitSimpleCond(ctx *parser.SimpleCondContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Simple Cond")
	}

	leftVal := c.Visit(ctx.Expr(0))
	if leftVal == nil {
		c.addError("left expression in condition is nil", ctx.GetStart().GetLine())
		return llvm.Value{}
	}
	left, ok := leftVal.(llvm.Value)
	if !ok || c.checkNil(left, "left expression", ctx.GetStart().GetLine()) {
		c.addError("left expression in condition is not a valid llvm value", ctx.GetStart().GetLine())
		return llvm.Value{}
	}

	rightVal := c.Visit(ctx.Expr(1))
	if rightVal == nil {
		c.addError("right expression in condition is nil", ctx.GetStart().GetLine())
		return llvm.Value{}
	}
	right, ok := rightVal.(llvm.Value)
	if !ok || c.checkNil(right, "right expression", ctx.GetStart().GetLine()) {
		c.addError("right expression in condition is not a valid llvm value", ctx.GetStart().GetLine())
		return llvm.Value{}
	}

	if c.verbose {
		fmt.Printf("  Left: %v (Type: %v)\n", left, left.Type())
		fmt.Printf("  Right: %v (Type: %v)\n", right, right.Type())
	}

	var pred llvm.IntPredicate
	switch {
	case ctx.Comparator().EQ() != nil:
		pred = llvm.IntEQ
	case ctx.Comparator().GT() != nil:
		pred = llvm.IntSGT
	case ctx.Comparator().LT() != nil:
		pred = llvm.IntSLT
	case ctx.Comparator().GE() != nil:
		pred = llvm.IntSGE
	case ctx.Comparator().LE() != nil:
		pred = llvm.IntSLE
	default:
		c.addError(fmt.Sprintf("Unsupported comparator: %s", ctx.Comparator().GetText()), ctx.GetStart().GetLine())
		return llvm.Value{}
	}

	return c.builder.CreateICmp(pred, left, right, "cmptmp")
}

func (c *CodeGenerator) VisitIdExpr(ctx *parser.IdExprContext) interface{} {
	if c.verbose {
		fmt.Printf("Visiting IdExpr: %s\n", ctx.GetText())
	}
	fieldName := ctx.GetText()
	if field := c.getFieldSymbol(fieldName, ctx.GetStart().GetLine()); field != nil {
		destPtr := c.module.NamedGlobal(field.name)
		if destPtr.IsNil() {
			c.addError(fmt.Sprintf("Global variable %s not found", field.name), ctx.GetStart().GetLine())
			return llvm.Value{}
		}
		if field.picture.isNumeric {
			val := c.builder.CreateLoad(c.context.Int32Type(), destPtr, "")
			if c.verbose {
				fmt.Printf("  Loaded IdExpr %s as %v\n", ctx.GetText(), val)
			}
			return val
		}
		if c.verbose {
			fmt.Printf("  Returning pointer for IdExpr %s\n", ctx.GetText())
		}
		return destPtr
	}
	return llvm.Value{}
}

func (c *CodeGenerator) VisitLitExpr(ctx *parser.LitExprContext) interface{} {
	litStr := ctx.GetText()
	if c.verbose {
		fmt.Printf("Visiting LitExpr: %s\n", litStr)
	}
	if val, err := strconv.ParseInt(litStr, 10, 32); err == nil {
		ret := llvm.ConstInt(c.context.Int32Type(), uint64(val), true)
		if c.verbose {
			fmt.Printf("  Parsed LitExpr %s as %v\n", litStr, ret)
		}
		return ret
	}
	if strings.HasPrefix(litStr, "\"") && strings.HasSuffix(litStr, "\"") {
		str := strings.Trim(litStr, "\"")
		strName := fmt.Sprintf(".str_lit%d", c.strCounter)
		c.strCounter++
		ret := c.builder.CreateGlobalStringPtr(str, strName)
		if c.verbose {
			fmt.Printf("  Created global string for LitExpr %s\n", litStr)
		}
		return ret
	}
	c.addError(fmt.Sprintf("Unsupported literal type: %s", litStr), ctx.GetStart().GetLine())
	return llvm.Value{}
}