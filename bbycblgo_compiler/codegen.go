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
	localVars      map[string]llvm.Value
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
		localVars:         make(map[string]llvm.Value),
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

func (c *CodeGenerator) getFieldSymbol(name string, line int) (*FieldSymbol, string) {
	// Check for subscript
	subscript := ""
	if strings.Contains(name, "(") {
		parts := strings.Split(name, "(")
		name = parts[0]
		subscript = strings.TrimRight(parts[1], ")")
	}

	uname := strings.ToUpper(name)
	if fields, ok := c.symbolTable.rootScope.fields[uname]; ok && len(fields) > 0 {
		// For now, just return the first field found. 
		// A more robust solution would involve resolving qualified names.
		return fields[0], subscript
	}
	c.addError(fmt.Sprintf("Field '%s' not found in symbol table", name), line)
	return nil, ""
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
	return c.VisitChildren(ctx)
}

func (c *CodeGenerator) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Data Division")
	}
	for _, dataEntry := range ctx.AllDataEntry() {
		fieldName := dataEntry.Identifier().GetText()
		field, _ := c.getFieldSymbol(fieldName, dataEntry.GetStart().GetLine())
		if field != nil {
			var global llvm.Value
			if field.occurs > 0 {
				if field.picture.isNumeric {
					global = llvm.AddGlobal(c.module, llvm.ArrayType(c.context.Int32Type(), field.occurs), field.name)
					global.SetInitializer(llvm.ConstNull(llvm.ArrayType(c.context.Int32Type(), field.occurs)))
					global.SetAlignment(4)
				} else {
					globalType := llvm.ArrayType(llvm.ArrayType(c.context.Int8Type(), field.picture.length), field.occurs)
					global = llvm.AddGlobal(c.module, globalType, field.name)
					global.SetInitializer(llvm.ConstNull(globalType))
					global.SetAlignment(1)
				}
			} else {
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
			field, _ := c.getFieldSymbol(fieldName, id.GetStart().GetLine())
			if field != nil {
				if strings.ContainsAny(field.picture.raw, "9S") { // Heuristic for numeric
					destPtr := c.module.NamedGlobal(field.name)
					loadedVar := c.builder.CreateLoad(c.context.Int32Type(), destPtr, "")
					formatStr := c.builder.CreateGlobalStringPtr("%d", ".str_int")
					if c.checkNil(formatStr, "format string for numeric display", item.GetStart().GetLine()) {
						continue
					}
					c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{formatStr, loadedVar}, "")
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

	// Add a newline after all display items unless WITH NO ADVANCING is specified
	if ctx.WithNoAdvancingClause() == nil {
		newlineStr := c.builder.CreateGlobalStringPtr("\n", fmt.Sprintf(".str_newline%d", c.strCounter))
		if c.checkNil(newlineStr, "newline string", ctx.GetStart().GetLine()) {
			return nil
		}
		c.strCounter++
		c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{newlineStr}, "")
	}

	return nil
}

func (c *CodeGenerator) VisitMoveStmt(ctx *parser.MoveStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Move Stmt")
	}

	from := ctx.ExprList(0).AllExpr()[0]
	fromVal := c.Visit(from)
	if fromVal == nil {
		return nil
	}
	value, ok := fromVal.(llvm.Value)
	if !ok || c.checkNil(value, "source value in move", from.GetStart().GetLine()) {
		return nil
	}

	for _, to := range ctx.ExprList(1).AllExpr() {
		toField, toSubscript := c.getFieldSymbol(to.GetText(), to.GetStart().GetLine())
		if toField == nil {
			continue
		}
		destPtr := c.module.NamedGlobal(toField.name)
		if c.checkNil(destPtr, "destination pointer in move", to.GetStart().GetLine()) {
			continue
		}

		var finalDestPtr llvm.Value
		if toSubscript != "" {
			// Handle array access
			idx, err := strconv.Atoi(toSubscript)
			if err != nil {
				c.addError(fmt.Sprintf("Invalid subscript: %s", toSubscript), to.GetStart().GetLine())
				continue
			}
			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				llvm.ConstInt(c.context.Int32Type(), uint64(idx-1), false), // COBOL arrays are 1-based
			}
			finalDestPtr = c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int32Type(), toField.occurs), destPtr, indices, "")
		} else {
			finalDestPtr = destPtr
		}

		if fromId, ok := from.(*parser.IdExprContext); ok {
			switch strings.ToUpper(fromId.GetText()) {
			case "HIGH-VALUES":
				if strings.ContainsAny(toField.picture.raw, "9S") { // Heuristic for numeric
					valStr := ""
					for _, char := range toField.picture.raw {
						if char == '9' {
							valStr += "9"
						}
					}
					val, _ := strconv.ParseInt(valStr, 10, 64)
					c.builder.CreateStore(llvm.ConstInt(c.context.Int32Type(), uint64(val), true), finalDestPtr)
				} else { // Alphanumeric
					char := llvm.ConstInt(c.context.Int8Type(), 0xff, false)
					for i := 0; i < toField.picture.length; i++ {
						indices := []llvm.Value{
							llvm.ConstInt(c.context.Int32Type(), 0, false),
							llvm.ConstInt(c.context.Int32Type(), uint64(i), false),
						}
						ptr := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), toField.picture.length), finalDestPtr, indices, "")
						c.builder.CreateStore(char, ptr)
					}
				}
				continue
			case "LOW-VALUES":
				// Similar logic for LOW-VALUES
				if strings.ContainsAny(toField.picture.raw, "9S") { // Heuristic for numeric
					c.builder.CreateStore(llvm.ConstInt(c.context.Int32Type(), 0, true), finalDestPtr)
				} else { // Alphanumeric
					char := llvm.ConstInt(c.context.Int8Type(), 0x00, false)
					for i := 0; i < toField.picture.length; i++ {
						indices := []llvm.Value{
							llvm.ConstInt(c.context.Int32Type(), 0, false),
							llvm.ConstInt(c.context.Int32Type(), uint64(i), false),
						}
						ptr := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), toField.picture.length), finalDestPtr, indices, "")
						c.builder.CreateStore(char, ptr)
					}
				}
				continue
			}
		}

		if toField.picture.isNumeric {
			c.builder.CreateStore(value, finalDestPtr)
		} else {
			// String move
			str := strings.Trim(from.GetText(), "\"")
			strLen := len(str)

			strName := fmt.Sprintf(".str%d", c.strCounter)
			c.strCounter++
			globalStr := c.builder.CreateGlobalStringPtr(str, strName)
			if c.checkNil(globalStr, "global string for move", from.GetStart().GetLine()) {
				continue
			}

			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				llvm.ConstInt(c.context.Int32Type(), 0, false),
			}

			destGEP := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), toField.picture.length), finalDestPtr, indices, "")
			if c.checkNil(destGEP, "dest GEP for move", from.GetStart().GetLine()) {
				continue
			}
			srcGEP := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int8Type(), int(uint64(strLen+1))), globalStr, indices, "")
			if c.checkNil(srcGEP, "src GEP for move", from.GetStart().GetLine()) {
				continue
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
	}

	return nil
}

func (c *CodeGenerator) VisitSubtractStmt(ctx *parser.SubtractStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Subtract Stmt")
	}

	// Get the value to subtract (e.g., 1). This comes from the first exprList.
	subtractExpr := ctx.ExprList(0).AllExpr()[0]
	subtractValRes := c.Visit(subtractExpr)
	if subtractValRes == nil {
		c.addError("Subtract value is nil", subtractExpr.GetStart().GetLine())
		return nil
	}
	subtractVal, ok := subtractValRes.(llvm.Value)
	if !ok || c.checkNil(subtractVal, "subtract value", subtractExpr.GetStart().GetLine()) {
		return nil
	}

	// The second expression must be an identifier that we can get a pointer to.
	// This comes from the second exprList.
	fromExpr := ctx.ExprList(1).AllExpr()[0]
	fromIdCtx, ok := fromExpr.(*parser.IdExprContext)
	if !ok {
		c.addError("SUBTRACT target must be an identifier", fromExpr.GetStart().GetLine())
		return nil
	}

	fieldName := fromIdCtx.GetText()
	field, subscript := c.getFieldSymbol(fieldName, fromIdCtx.GetStart().GetLine())
	if field == nil {
		return nil // Error already added
	}

	if !field.picture.isNumeric {
		c.addError(fmt.Sprintf("Cannot perform SUBTRACT on non-numeric field '%s'", field.name), fromIdCtx.GetStart().GetLine())
		return nil
	}

	destPtr := c.module.NamedGlobal(field.name)
	if c.checkNil(destPtr, "subtract target global", fromIdCtx.GetStart().GetLine()) {
		return nil
	}

	var finalDestPtr llvm.Value
	if subscript != "" {
		var idx llvm.Value
		// Check if subscript is a literal integer
		if val, err := strconv.Atoi(subscript); err == nil {
			idx = llvm.ConstInt(c.context.Int32Type(), uint64(val-1), false) // COBOL is 1-based
		} else {
			// Assume it's a variable
			if local, ok := c.localVars[strings.ToUpper(subscript)]; ok {
				loadedIdx := c.builder.CreateLoad(c.context.Int32Type(), local, "")
				idx = c.builder.CreateSub(loadedIdx, llvm.ConstInt(c.context.Int32Type(), 1, false), "") // Adjust for 1-based index
			} else {
				c.addError(fmt.Sprintf("Subscript variable '%s' not found", subscript), fromIdCtx.GetStart().GetLine())
				return nil
			}
		}
		indices := []llvm.Value{
			llvm.ConstInt(c.context.Int32Type(), 0, false),
			idx,
		}
		finalDestPtr = c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int32Type(), field.occurs), destPtr, indices, "")
	} else {
		finalDestPtr = destPtr
	}

	// Perform the subtraction
	currentVal := c.builder.CreateLoad(c.context.Int32Type(), finalDestPtr, "")
	newVal := c.builder.CreateSub(currentVal, subtractVal, "subtmp")
	c.builder.CreateStore(newVal, finalDestPtr)

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

func (c *CodeGenerator) VisitNextSentenceStmt(ctx *parser.NextSentenceStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Next Sentence Stmt")
	}
	// This is a simplification. A more robust implementation would involve
	// creating basic blocks for each sentence and branching to them.
	return nil
}

func (c *CodeGenerator) VisitCondition(ctx *parser.ConditionContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Condition")
	}

	if len(ctx.AllSimpleCond()) == 0 {
		return nil
	}

	// Handle the first condition
	firstCond := ctx.SimpleCond(0)
	left := c.Visit(firstCond.Expr(0))
	result := c.Visit(firstCond).(llvm.Value)
	firstComparator := firstCond.Comparator()

	// Chain the rest of the conditions with OR
	for i := 1; i < len(ctx.AllSimpleCond()); i++ {
		nextCondCtx := ctx.SimpleCond(i)
		// This is a contracted IF, so we need to use the left from the first condition
		if len(nextCondCtx.AllExpr()) == 1 {
			right := c.Visit(nextCondCtx.Expr(0)).(llvm.Value)
			var pred llvm.IntPredicate
			comparator := firstComparator
			if nextCondCtx.Comparator() != nil {
				comparator = nextCondCtx.Comparator()
			}

			switch {
			case comparator.EQ() != nil:
				pred = llvm.IntEQ
			case comparator.GT() != nil:
				pred = llvm.IntSGT
			case comparator.LT() != nil:
				pred = llvm.IntSLT
			case comparator.GE() != nil:
				pred = llvm.IntSGE
			case comparator.LE() != nil:
				pred = llvm.IntSLE
			default:
				c.addError(fmt.Sprintf("Unsupported comparator: %s", comparator.GetText()), nextCondCtx.GetStart().GetLine())
				return llvm.Value{}
			}
			nextCond := c.builder.CreateICmp(pred, left.(llvm.Value), right, "")
			result = c.builder.CreateOr(result, nextCond, "")
		} else {
			nextCond := c.Visit(nextCondCtx).(llvm.Value)
			result = c.builder.CreateOr(result, nextCond, "")
		}
	}

	return result
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

	// Check for local variables (loop counters)
	if local, ok := c.localVars[strings.ToUpper(fieldName)]; ok {
		return c.builder.CreateLoad(c.context.Int32Type(), local, "")
	}

	field, subscript := c.getFieldSymbol(fieldName, ctx.GetStart().GetLine())
	if field != nil {
		destPtr := c.module.NamedGlobal(field.name)
		if destPtr.IsNil() {
			c.addError(fmt.Sprintf("Global variable %s not found", field.name), ctx.GetStart().GetLine())
			return llvm.Value{}
		}

		if subscript != "" {
			// Handle array access
			var idx llvm.Value
			if val, err := strconv.Atoi(subscript); err == nil {
				idx = llvm.ConstInt(c.context.Int32Type(), uint64(val-1), false) // COBOL arrays are 1-based
			} else {
				// It's a variable subscript
				if local, ok := c.localVars[strings.ToUpper(subscript)]; ok {
					idx = c.builder.CreateLoad(c.context.Int32Type(), local, "")
					idx = c.builder.CreateSub(idx, llvm.ConstInt(c.context.Int32Type(), 1, false), "") // COBOL arrays are 1-based
				} else {
					c.addError(fmt.Sprintf("Subscript variable '%s' not found", subscript), ctx.GetStart().GetLine())
					return llvm.Value{}
				}
			}

			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				idx,
			}
			ptr := c.builder.CreateInBoundsGEP(llvm.ArrayType(c.context.Int32Type(), field.occurs), destPtr, indices, "")
			if field.picture.isNumeric {
				return c.builder.CreateLoad(c.context.Int32Type(), ptr, "")
			}
			return ptr
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

func (c *CodeGenerator) VisitLoopStmt(ctx *parser.LoopStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Loop Stmt")
	}

	startFunc := c.builder.GetInsertBlock().Parent()
	loopHeader := c.context.AddBasicBlock(startFunc, "loop.header")
	loopBody := c.context.AddBasicBlock(startFunc, "loop.body")
	loopExit := c.context.AddBasicBlock(startFunc, "loop.exit")

	// --- VARYING Clause ---
	var counterPtr llvm.Value
	var limit, step llvm.Value
	hasVarying := false

	// Find the VARYING clause in the loop content
	for _, content := range ctx.AllLoopContent() {
		if content.LoopControl() != nil && content.LoopControl().VaryingClause() != nil {
			hasVarying = true
			varying := content.LoopControl().VaryingClause()
			counterName := varying.IdentifierSegment().GetText()

			// Create an alloca for the loop counter in the entry block
			entryBuilder := c.context.NewBuilder()
			entryBuilder.SetInsertPointAtEnd(c.mainEntryBlock)
			counterPtr = entryBuilder.CreateAlloca(c.context.Int32Type(), counterName)
			entryBuilder.Dispose()

			c.localVars[strings.ToUpper(counterName)] = counterPtr

			// Get start, end, and step values
			startVal := llvm.ConstInt(c.context.Int32Type(), 1, true) // Default start is 1
			if len(varying.AllExpr()) > 0 {
				startVal = c.Visit(varying.Expr(0)).(llvm.Value)
			}

			limit = llvm.ConstInt(c.context.Int32Type(), 2147483647, true) // Default limit is max int
			if len(varying.AllExpr()) > 1 {
				limit = c.Visit(varying.Expr(1)).(llvm.Value)
			}

			step = llvm.ConstInt(c.context.Int32Type(), 1, true) // Default step is 1
			if len(varying.AllExpr()) > 2 {
				step = c.Visit(varying.Expr(2)).(llvm.Value)
			}

			c.builder.CreateStore(startVal, counterPtr)
			break // Assume only one VARYING clause for now
		}
	}

	c.builder.CreateBr(loopHeader)
	c.builder.SetInsertPointAtEnd(loopHeader)

	// --- Loop Condition ---
	if hasVarying {
		counter := c.builder.CreateLoad(c.context.Int32Type(), counterPtr, "")
		cond := c.builder.CreateICmp(llvm.IntSLE, counter, limit, "loop.cond")
		c.builder.CreateCondBr(cond, loopBody, loopExit)
	} else {
		// Infinite loop
		c.builder.CreateBr(loopBody)
	}

	// --- Loop Body ---
	c.builder.SetInsertPointAtEnd(loopBody)
	for _, content := range ctx.AllLoopContent() {
		if content.Sentence() != nil {
			c.Visit(content.Sentence())
		} else if content.Statement() != nil {
			c.Visit(content.Statement())
		}
	}

	// --- Loop Update ---
	if hasVarying {
		counter := c.builder.CreateLoad(c.context.Int32Type(), counterPtr, "")
		nextVal := c.builder.CreateAdd(counter, step, "next.val")
		c.builder.CreateStore(nextVal, counterPtr)
	}

	c.builder.CreateBr(loopHeader)
	c.builder.SetInsertPointAtEnd(loopExit)

	return nil
}

func (c *CodeGenerator) VisitVaryingClause(ctx *parser.VaryingClauseContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Varying Clause")
	}
	return nil
}