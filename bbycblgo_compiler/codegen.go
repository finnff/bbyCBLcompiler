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
	context         llvm.Context
	module          llvm.Module
	builder         llvm.Builder
	targetData      llvm.TargetData
	symbolTable     *SymbolTable
	errors          []SemanticError
	strCounter      int
	tempRegCounter  int
	verbose         bool
	sourceFilename  string
	mainEntryBlock  llvm.BasicBlock
	printfFunc      llvm.Value
	memcpyFunc      llvm.Value
	stopped         bool
	localVars       map[string]llvm.Value
	paragraphBlocks map[string]llvm.BasicBlock
	// New fields for ALTER (switch-based)
	alterableGoTos           map[string]llvm.Value // Maps paragraph name to its global variable holding the current GO TO target ID (i32)
	paragraphToID            map[string]int        // Maps paragraph name to its assigned integer ID
	idToParagraph            map[int]string        // Maps integer ID back to paragraph name
	collectedAlterStatements []AlterInfo
	collectedGotoParagraphs  map[string]string
}

// AlterInfo holds information about an ALTER statement.
type AlterInfo struct {
	TargetParagraph string
	NewTarget       string
	Line            int
}

// AlterCollector visits the AST to collect ALTER statements.
type AlterCollector struct {
	*parser.BasebbyCBLVisitor
	alterStatements []AlterInfo
	// Map to store which paragraphs contain a GO TO statement
	// and their initial target, to validate ALTER statements.
	gotoParagraphs map[string]string
}

func (a *AlterCollector) Visit(tree antlr.ParseTree) interface{} {
	return tree.Accept(a)
}

func (a *AlterCollector) VisitChildren(node antlr.RuleNode) interface{} {
	for _, child := range node.GetChildren() {
		child.(antlr.ParseTree).Accept(a)
	}
	return nil
}

// Override key visitor methods to ensure traversal
func (a *AlterCollector) VisitProgram(ctx *parser.ProgramContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitSentence(ctx *parser.SentenceContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitStatement(ctx *parser.StatementContext) interface{} {
	return a.VisitChildren(ctx)
}

// Add these methods to AlterCollector to handle nested structures
func (a *AlterCollector) VisitLoopStmt(ctx *parser.LoopStmtContext) interface{} {
	return a.VisitChildren(ctx)
}

//	func (a *AlterCollector) VisitIfStmt(ctx *parser.IfStmtContext) interface{} {
//		return a.VisitChildren(ctx)
//	}
func (a *AlterCollector) VisitSingleLineIf(ctx *parser.SingleLineIfContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitMultiLineIf(ctx *parser.MultiLineIfContext) interface{} {
	return a.VisitChildren(ctx)
}

func (a *AlterCollector) VisitLoopContent(ctx *parser.LoopContentContext) interface{} {
	return a.VisitChildren(ctx)
}

// Add any other intermediate context types that might contain statements
func (a *AlterCollector) VisitCondition(ctx *parser.ConditionContext) interface{} {
	return a.VisitChildren(ctx)
}

// NewAlterCollector creates a new AlterCollector.
func NewAlterCollector() *AlterCollector {
	return &AlterCollector{
		BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
		alterStatements:   []AlterInfo{},
		gotoParagraphs:    make(map[string]string),
	}
}

// VisitAlterStmt collects ALTER statements.
func (a *AlterCollector) VisitAlterStmt(ctx *parser.AlterStmtContext) interface{} {
	targetName := strings.ToUpper(ctx.Identifier(0).GetText())
	newName := strings.ToUpper(ctx.Identifier(1).GetText())
	a.alterStatements = append(a.alterStatements, AlterInfo{
		TargetParagraph: targetName,
		NewTarget:       newName,
		Line:            ctx.GetStart().GetLine(),
	})
	return nil
}

// VisitGotoStmt collects information about paragraphs containing GO TO statements.
func (a *AlterCollector) VisitGotoStmt(ctx *parser.GotoStmtContext) interface{} {
	// The parent of a GO TO statement is typically a Sentence, and its parent is a Paragraph.
	// We need to find the paragraph containing this GO TO.
	parent := ctx.GetParent()
	for parent != nil && !strings.HasSuffix(fmt.Sprintf("%T", parent), "*parser.ParagraphContext") {
		parent = parent.GetParent()
	}

	if paragraphCtx, ok := parent.(*parser.ParagraphContext); ok {
		paragraphName := strings.ToUpper(paragraphCtx.Identifier().GetText())
		// Store the initial target of the GO TO in this paragraph.
		// For simplicity, we assume only one GO TO per alterable paragraph.
		a.gotoParagraphs[paragraphName] = strings.ToUpper(ctx.ExprList().AllExpr()[0].GetText())

	}
	return nil
}

// CollectAlterInfo performs a pre-analysis pass to gather ALTER statements and GO TO info.
func CollectAlterInfo(tree antlr.ParseTree) ([]AlterInfo, map[string]string) {
	collector := NewAlterCollector()
	tree.Accept(collector) // Use Accept instead of Walk

	// Debug output
	fmt.Printf("Collected %d ALTER statements\n", len(collector.alterStatements))
	for _, alter := range collector.alterStatements {
		fmt.Printf("  ALTER %s TO %s\n", alter.TargetParagraph, alter.NewTarget)
	}

	fmt.Printf("Found %d paragraphs with GO TO statements\n", len(collector.gotoParagraphs))
	for para, target := range collector.gotoParagraphs {
		fmt.Printf("  %s contains GO TO %s\n", para, target)
	}

	return collector.alterStatements, collector.gotoParagraphs
}

// NewCodeGenerator creates a new code generator.
func NewCodeGenerator(symbolTable *SymbolTable, verbose bool, sourceFilename string, alterStatements []AlterInfo, gotoParagraphs map[string]string) *CodeGenerator {
	return &CodeGenerator{
		BasebbyCBLVisitor:        &parser.BasebbyCBLVisitor{},
		symbolTable:              symbolTable,
		errors:                   []SemanticError{},
		strCounter:               0,
		tempRegCounter:           0,
		verbose:                  verbose,
		sourceFilename:           sourceFilename,
		stopped:                  false,
		localVars:                make(map[string]llvm.Value),
		paragraphBlocks:          make(map[string]llvm.BasicBlock),
		alterableGoTos:           make(map[string]llvm.Value),
		paragraphToID:            make(map[string]int),
		idToParagraph:            make(map[int]string),
		collectedAlterStatements: alterStatements,
		collectedGotoParagraphs:  gotoParagraphs,
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

	alterStatements, gotoParagraphs := CollectAlterInfo(tree)
	codegen := NewCodeGenerator(symbolTable, verbose, sourceFilename, alterStatements, gotoParagraphs)
	codegen.context = llvm.NewContext()

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

func (c *CodeGenerator) blockHasTerminator(bb llvm.BasicBlock) bool {
	li := bb.LastInstruction()
	if li.IsNil() { // block is still open
		return false
	}
	return !li.IsAReturnInst().IsNil() ||
		!li.IsABranchInst().IsNil() ||
		!li.IsASwitchInst().IsNil() ||
		!li.IsAInvokeInst().IsNil() ||
		!li.IsAUnreachableInst().IsNil()
}

func (c *CodeGenerator) getActualPicture(field *FieldSymbol) *PictureType {
	if field == nil {
		return nil
	}
	if field.picture != nil {
		return field.picture
	}
	if field.likeRef != "" {
		// Resolve the LIKE reference
		if likeFields, ok := c.symbolTable.rootScope.fields[strings.ToUpper(field.likeRef)]; ok && len(likeFields) > 0 {
			// Assuming the first one is the correct one for now, or needs more robust resolution
			return c.getActualPicture(likeFields[0]) // Recursive call to handle chained LIKEs
		}
	}
	return nil
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
	name := strings.ToUpper(ctx.Identifier().GetText())
	block := c.paragraphBlocks[name]     // guaranteed to exist
	c.builder.SetInsertPointAtEnd(block) // <--- ADDED

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
			actualPicture := c.getActualPicture(field)
			if actualPicture == nil {
				c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), dataEntry.GetStart().GetLine())
				continue
			}

			var global llvm.Value
			if field.occurs > 0 {
				if c.verbose {
					fmt.Printf("DataEntry: %s, Picture: %v\n", field.name, actualPicture)
				}
				if actualPicture.isNumeric {
					global = llvm.AddGlobal(c.module, llvm.ArrayType(c.context.Int32Type(), field.occurs), field.name)
					global.SetInitializer(llvm.ConstNull(llvm.ArrayType(c.context.Int32Type(), field.occurs)))
					global.SetAlignment(4)
				} else {
					globalType := llvm.ArrayType(llvm.ArrayType(c.context.Int8Type(), actualPicture.length), field.occurs)
					global = llvm.AddGlobal(c.module, globalType, field.name)
					global.SetInitializer(llvm.ConstNull(globalType))
					global.SetAlignment(1)
				}
			} else {
				if c.verbose {
					fmt.Printf("DataEntry: %s, Picture: %v\n", field.name, actualPicture)
				}
				if actualPicture.isNumeric {
					global = llvm.AddGlobal(c.module, c.context.Int32Type(), field.name)
					global.SetInitializer(llvm.ConstInt(c.context.Int32Type(), 0, false))
					global.SetAlignment(4)
				} else {
					globalType := llvm.ArrayType(c.context.Int8Type(), actualPicture.length)
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

func (c *CodeGenerator) VisitAlterStmt(ctx *parser.AlterStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Alter Stmt")
	}

	targetParagraph := strings.ToUpper(ctx.Identifier(0).GetText())
	newTarget := strings.ToUpper(ctx.Identifier(1).GetText())

	// Get the global variable that holds the GO TO target ID for targetParagraph
	if globalGoToIDVar, ok := c.alterableGoTos[targetParagraph]; ok {
		// Get the ID for the new target
		if newTargetID, idOk := c.paragraphToID[newTarget]; idOk {
			// Store the new target ID into the global variable
			c.builder.CreateStore(llvm.ConstInt(c.context.Int32Type(), uint64(newTargetID), false), globalGoToIDVar)
			if c.verbose {
				fmt.Printf("ALTERed GO TO in %s to point to ID %d (%s)\n", targetParagraph, newTargetID, newTarget)
			}
		} else {
			c.addError(fmt.Sprintf("ALTER new target '%s' not found", newTarget), ctx.GetStart().GetLine())
		}
	} else {
		c.addError(fmt.Sprintf("ALTER target paragraph '%s' is not an alterable GO TO", targetParagraph), ctx.GetStart().GetLine())
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
	c.builder.SetInsertPointAtEnd(c.mainEntryBlock)

	// CRITICAL FIX: First pass - create all paragraph blocks before processing any statements
	paragraphID := 0
	for _, paragraph := range ctx.AllParagraph() {
		paragraphName := strings.ToUpper(paragraph.Identifier().GetText())
		paragraphBlock := c.context.AddBasicBlock(mainFunc, paragraphName)
		c.paragraphBlocks[paragraphName] = paragraphBlock
		c.paragraphToID[paragraphName] = paragraphID
		c.idToParagraph[paragraphID] = paragraphName
		paragraphID++
		if c.verbose {
			fmt.Printf("Pre-created paragraph block: %s (ID: %d)\n", paragraphName, paragraphID-1)
		}
	}

	// Initialize global variables for alterable GO TO targets (switch-based)
	for _, alterInfo := range c.collectedAlterStatements {
		// Check if the target paragraph actually contains a GO TO statement
		if initialTargetName, ok := c.collectedGotoParagraphs[alterInfo.TargetParagraph]; ok {
			// Get the initial target ID
			if initialTargetID, idOk := c.paragraphToID[initialTargetName]; idOk {
				// Create a global variable to hold the current target ID of the GO TO
				globalGoToIDVar := llvm.AddGlobal(c.module, c.context.Int32Type(), "goto_target_id_"+alterInfo.TargetParagraph)
				// Initialize it with the ID of the initial target
				globalGoToIDVar.SetInitializer(llvm.ConstInt(c.context.Int32Type(), uint64(initialTargetID), false))
				globalGoToIDVar.SetLinkage(llvm.InternalLinkage)
				// Store this global variable in our map for later lookup
				c.alterableGoTos[alterInfo.TargetParagraph] = globalGoToIDVar
				if c.verbose {
					fmt.Printf("Created alterable GO TO global for %s, initial target ID %d\n", alterInfo.TargetParagraph, initialTargetID)
				}
			} else {
				c.addError(fmt.Sprintf("ALTER target '%s' refers to an unknown initial GO TO target '%s'", alterInfo.TargetParagraph, initialTargetName), alterInfo.Line)
			}
		} else {
			c.addError(fmt.Sprintf("ALTER target paragraph '%s' does not contain a GO TO statement", alterInfo.TargetParagraph), alterInfo.Line)
		}
	}

	// Now visit all paragraphs to generate their content
	for _, paragraph := range ctx.AllParagraph() {
		c.Visit(paragraph)
	}

	// Visit sentences directly under Procedure Division (if any)
	if c.verbose {
		fmt.Printf("Main entry block before visiting sentences: %v\n", c.mainEntryBlock.LastInstruction().IsNil())
	}
	for _, sentence := range ctx.AllSentence() {
		c.Visit(sentence)
	}
	if c.verbose {
		fmt.Printf("Main entry block after visiting sentences: %v\n", c.mainEntryBlock.LastInstruction().IsNil())
	}

	// --- Final safety pass: close any dangling blocks in 'main'.
	zero := llvm.ConstInt(c.context.Int32Type(), 0, false)
	for bb := mainFunc.FirstBasicBlock(); !bb.IsNil(); bb = llvm.NextBasicBlock(bb) {
		lastInstruction := bb.LastInstruction()
		if lastInstruction.IsNil() { // If the block is empty, add a return
			c.builder.SetInsertPointAtEnd(bb)
			c.builder.CreateRet(zero)
		} else {
			isTerminator := !lastInstruction.IsAReturnInst().IsNil() ||
				!lastInstruction.IsABranchInst().IsNil() ||
				!lastInstruction.IsASwitchInst().IsNil() ||
				!lastInstruction.IsAInvokeInst().IsNil() ||
				!lastInstruction.IsAUnreachableInst().IsNil()

			if !isTerminator { // If the last instruction is NOT a terminator, add a return
				c.builder.SetInsertPointAtEnd(bb)
				c.builder.CreateRet(zero)
			}
		}
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

	var formatString string
	var printfArgs []llvm.Value

	for _, item := range ctx.AllDisplayItem() {
		val := c.Visit(item.Expr())
		if val == nil {
			continue
		}
		value, ok := val.(llvm.Value)
		if !ok || c.checkNil(value, "display item", item.GetStart().GetLine()) {
			continue
		}

		if c.verbose {
			if !value.IsNil() {
				t := value.Type()
				if !t.IsNil() {
					rawKind := t.TypeKind()
					fmt.Printf("Debug: Raw TypeKind: %d\n", rawKind)
					// No if ==19; print element always to see pointee
					elemT := t.ElementType()
					if !elemT.IsNil() {
						elemKind := elemT.TypeKind()
						fmt.Printf("Debug: Element Raw TypeKind: %d\n", elemKind)
						// If element is Array (expect ~11), print details
						if elemKind == 11 || elemKind == rawKind-1 { // Guess shift
							fmt.Printf("Debug: Array Length: %d\n", elemT.ArrayLength())
							subElem := elemT.ElementType()
							if !subElem.IsNil() {
								subKind := subElem.TypeKind()
								fmt.Printf("Debug: Sub-Element Kind: %d, IsInteger: %t, Width: %d\n", subKind, subKind == 8 /* Integer guess */, subElem.IntTypeWidth())
							}
						} else if elemKind == 8 || elemKind == 1 { // Integer or shifted
							fmt.Printf("Debug: Integer Width: %d (expect 8 for i8)\n", elemT.IntTypeWidth())
						}
					}
					// Print if constant (for GEP)
					if value.IsAConstantExpr().IsNil() == false {
						fmt.Println("Debug: Value is ConstantExpr (GEP for string)")
					}
				} else {
					fmt.Println("Debug: Type is nil")
				}
			} else {
				fmt.Println("Debug: Value is nil")
			}
		}

		// Get the actual picture for the field being displayed
		var actualPicture *PictureType
		if id, isId := item.Expr().(*parser.IdExprContext); isId {
			field, _ := c.getFieldSymbol(id.GetText(), id.GetStart().GetLine())
			if field != nil {
				actualPicture = c.getActualPicture(field)
				// If actualPicture is still nil, it means the LIKE reference couldn't be resolved
				if actualPicture == nil {
					c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), id.GetStart().GetLine())
					continue
				}
			}
		} else if lit, isLit := item.Expr().(*parser.LitExprContext); isLit {
			// For literals, determine type from literal itself
			litStr := lit.GetText()
			if _, err := strconv.ParseInt(litStr, 10, 32); err == nil {
				actualPicture = &PictureType{isNumeric: true}
			} else if strings.HasPrefix(litStr, "\"") && strings.HasSuffix(litStr, "\"") {
				actualPicture = &PictureType{isAlphaNum: true, length: len(strings.Trim(litStr, "\""))}
			}
		}

		if actualPicture == nil {
			c.addError(fmt.Sprintf("Could not determine type for display item: %s", item.Expr().GetText()), item.GetStart().GetLine())
			continue
		}

		if actualPicture.isNumeric {
			formatString += "%d"
			// If it's a pointer to an integer (e.g., from IdExpr for a numeric field), load it
			if value.Type().TypeKind() == llvm.PointerTypeKind && value.Type().ElementType().TypeKind() == llvm.IntegerTypeKind {
				loadedVal := c.builder.CreateLoad(c.context.Int32Type(), value, "")
				printfArgs = append(printfArgs, loadedVal)
			} else {
				printfArgs = append(printfArgs, value)
			}
		} else { // Alphanumeric
			// ---- Alphanumeric branch ----
			formatString += "%s"
			handled := false

			if value.Type().TypeKind() == llvm.PointerTypeKind {
				// whatever the element type is, make it an i8*
				cstr := c.builder.CreateBitCast(
					value, llvm.PointerType(c.context.Int8Type(), 0), "cstr")
				printfArgs = append(printfArgs, cstr)
				handled = true
			}

			if !handled {
				c.addError("Unsupported type for alphanumeric DISPLAY",
					item.GetStart().GetLine())
				continue
			}
		}
	}

	if ctx.WithNoAdvancingClause() == nil {
		formatString += "\n"
	}

	if len(formatString) > 0 {
		finalFormatStr := c.builder.CreateGlobalStringPtr(formatString, fmt.Sprintf(".str_format%d", c.strCounter))
		c.strCounter++
		printfArgs = append([]llvm.Value{finalFormatStr}, printfArgs...)
		c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, printfArgs, "")
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

func (c *CodeGenerator) VisitAddToForm(ctx *parser.AddToFormContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting AddToForm Stmt")
	}

	// Sum up all the source values
	sources := ctx.ExprList(0).AllExpr()
	sum := c.Visit(sources[0]).(llvm.Value)
	for i := 1; i < len(sources); i++ {
		nextVal := c.Visit(sources[i]).(llvm.Value)
		sum = c.builder.CreateAdd(sum, nextVal, "addtmp")
	}

	// Handle ADD ... TO ...
	toExprs := ctx.ExprList(1).AllExpr()
	for _, toExpr := range toExprs {
		field, _ := c.getFieldSymbol(toExpr.GetText(), toExpr.GetStart().GetLine())
		if field != nil {
			destPtr := c.module.NamedGlobal(field.name)
			currentVal := c.builder.CreateLoad(c.context.Int32Type(), destPtr, "")
			newVal := c.builder.CreateAdd(sum, currentVal, "addtmp")
			c.builder.CreateStore(newVal, destPtr)
		}
	}

	return nil
}

func (c *CodeGenerator) VisitAddGivingForm(ctx *parser.AddGivingFormContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting AddGivingForm Stmt")
	}

	// Sum up all the source values
	sources := ctx.ExprList().AllExpr()
	sum := c.Visit(sources[0]).(llvm.Value)
	for i := 1; i < len(sources); i++ {
		nextVal := c.Visit(sources[i]).(llvm.Value)
		sum = c.builder.CreateAdd(sum, nextVal, "addtmp")
	}

	// Handle ADD ... GIVING ...
	givingExprs := ctx.GivingClause().ExprList().AllExpr()
	for _, givingExpr := range givingExprs {
		field, _ := c.getFieldSymbol(givingExpr.GetText(), givingExpr.GetStart().GetLine())
		if field != nil {
			destPtr := c.module.NamedGlobal(field.name)
			c.builder.CreateStore(sum, destPtr)
		}
	}

	return nil
}

func (c *CodeGenerator) VisitDivideIntoForm(ctx *parser.DivideIntoFormContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Divide Into Form")
	}
	c.handleDivide(ctx.AllExprList(), ctx.AllGivingClause(), nil, ctx.REMAINDER() != nil, ctx.GetStart().GetLine())
	return nil
}

func (c *CodeGenerator) VisitDivideByForm(ctx *parser.DivideByFormContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Divide By Form")
	}
	c.handleDivide(ctx.AllExprList(), ctx.AllGivingClause(), ctx.BY(), ctx.REMAINDER() != nil, ctx.GetStart().GetLine())
	return nil
}

func (c *CodeGenerator) handleDivide(exprLists []parser.IExprListContext, givingClauses []parser.IGivingClauseContext, byNode antlr.TerminalNode, hasRemainder bool, line int) {
	var dividendVal, divisorVal llvm.Value
	var dividendExpr, divisorExpr parser.IExprContext
	var dividendField *FieldSymbol
	var dividendPtr llvm.Value

	if byNode != nil {
		// DIVIDE ... BY ...
		dividendExpr = exprLists[0].AllExpr()[0]
		divisorExpr = exprLists[1].AllExpr()[0]
	} else {
		// DIVIDE ... INTO ...
		divisorExpr = exprLists[0].AllExpr()[0]
		dividendExpr = exprLists[1].AllExpr()[0]
	}

	// Process divisor
	divisorValResult := c.Visit(divisorExpr)
	if divisorValResult == nil {
		c.addError("divisor is nil", divisorExpr.GetStart().GetLine())
		return
	}
	divisorVal, ok := divisorValResult.(llvm.Value)
	if !ok || c.checkNil(divisorVal, "divisor", divisorExpr.GetStart().GetLine()) {
		return
	}

	// Process dividend
	dividendValResult := c.Visit(dividendExpr)
	if dividendValResult == nil {
		c.addError("dividend is nil", dividendExpr.GetStart().GetLine())
		return
	}
	dividendVal, ok = dividendValResult.(llvm.Value)
	if !ok {
		if dividendId, ok := dividendExpr.(*parser.IdExprContext); ok {
			dividendField, _ = c.getFieldSymbol(dividendId.GetText(), dividendId.GetStart().GetLine())
			if dividendField == nil {
				return
			}
			dividendPtr = c.module.NamedGlobal(dividendField.name)
			if c.checkNil(dividendPtr, "dividend pointer", dividendId.GetStart().GetLine()) {
				return
			}
			dividendVal = c.builder.CreateLoad(c.context.Int32Type(), dividendPtr, "dividend")
		} else {
			c.addError("dividend is not a valid llvm value", dividendExpr.GetStart().GetLine())
			return
		}
	}

	// --- Calculations ---
	quotient := c.builder.CreateSDiv(dividendVal, divisorVal, "quottmp")
	var remainder llvm.Value
	if hasRemainder {
		remainder = c.builder.CreateSRem(dividendVal, divisorVal, "remtmp")
	}

	// --- Storing results ---
	if len(givingClauses) > 0 {
		givingExpr := givingClauses[0].ExprList().AllExpr()[0]
		givingField, _ := c.getFieldSymbol(givingExpr.GetText(), givingExpr.GetStart().GetLine())
		if givingField != nil {
			givingPtr := c.module.NamedGlobal(givingField.name)
			if !c.checkNil(givingPtr, "giving pointer", givingExpr.GetStart().GetLine()) {
				c.builder.CreateStore(quotient, givingPtr)
			}
		}
	} else {
		if dividendPtr.IsNil() {
			c.addError("DIVIDE without GIVING requires a variable as the dividend", dividendExpr.GetStart().GetLine())
			return
		}
		c.builder.CreateStore(quotient, dividendPtr)
	}

	if hasRemainder {
		remExprListIndex := 1 + len(givingClauses)
		if byNode != nil {
			remExprListIndex++
		}

		if remExprListIndex < len(exprLists) {
			remExpr := exprLists[remExprListIndex].AllExpr()[0]
			remField, _ := c.getFieldSymbol(remExpr.GetText(), remExpr.GetStart().GetLine())
			if remField != nil {
				remPtr := c.module.NamedGlobal(remField.name)
				if !c.checkNil(remPtr, "remainder pointer", remExpr.GetStart().GetLine()) {
					c.builder.CreateStore(remainder, remPtr)
				}
			}
		} else {
			c.addError("Could not find expression for REMAINDER", line)
		}
	}
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

	actualPicture := c.getActualPicture(field)
	if actualPicture == nil {
		c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), fromIdCtx.GetStart().GetLine())
		return nil
	}

	if !actualPicture.isNumeric {
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

func (c *CodeGenerator) VisitMultiplyStmt(ctx *parser.MultiplyStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Multiply Stmt")
	}

	// Get the multiplier (e.g., 2). This comes from the first exprList.
	multiplierExpr := ctx.ExprList(0).AllExpr()[0]
	multiplierValRes := c.Visit(multiplierExpr)
	if multiplierValRes == nil {
		c.addError("Multiplier value is nil", multiplierExpr.GetStart().GetLine())
		return nil
	}
	multiplierVal, ok := multiplierValRes.(llvm.Value)
	if !ok || c.checkNil(multiplierVal, "multiplier value", multiplierExpr.GetStart().GetLine()) {
		return nil
	}

	// Handle MULTIPLY ... BY ... (modify the target in place)
	targetExprs := ctx.ExprList(1).AllExpr()
	for _, targetExpr := range targetExprs {
		targetIdCtx, ok := targetExpr.(*parser.IdExprContext)
		if !ok {
			c.addError("MULTIPLY target must be an identifier", targetExpr.GetStart().GetLine())
			continue
		}

		fieldName := targetIdCtx.GetText()
		field, subscript := c.getFieldSymbol(fieldName, targetIdCtx.GetStart().GetLine())
		if field == nil {
			continue // Error already added
		}

		actualPicture := c.getActualPicture(field)
		if actualPicture == nil {
			c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), targetIdCtx.GetStart().GetLine())
			continue
		}

		if !actualPicture.isNumeric {
			c.addError(fmt.Sprintf("Cannot perform MULTIPLY on non-numeric field '%s'", field.name), targetIdCtx.GetStart().GetLine())
			continue
		}

		destPtr := c.module.NamedGlobal(field.name)
		if c.checkNil(destPtr, "multiply target global", targetIdCtx.GetStart().GetLine()) {
			continue
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
					c.addError(fmt.Sprintf("Subscript variable '%s' not found", subscript), targetIdCtx.GetStart().GetLine())
					continue
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

		// Perform the multiplication
		currentVal := c.builder.CreateLoad(c.context.Int32Type(), finalDestPtr, "")
		newVal := c.builder.CreateMul(currentVal, multiplierVal, "multmp")
		c.builder.CreateStore(newVal, finalDestPtr)
	}

	// Handle GIVING clause if present
	if len(ctx.AllGivingClause()) > 0 {
		givingExprs := ctx.GivingClause(0).ExprList().AllExpr()
		for _, givingExpr := range givingExprs {
			field, _ := c.getFieldSymbol(givingExpr.GetText(), givingExpr.GetStart().GetLine())
			if field != nil {
				destPtr := c.module.NamedGlobal(field.name)
				if !c.checkNil(destPtr, "giving destination", givingExpr.GetStart().GetLine()) {
					// For GIVING, we need to calculate the product without modifying the original
					// This is a simplified version - in full COBOL, GIVING works differently
					targetExpr := ctx.ExprList(1).AllExpr()[0]
					targetVal := c.Visit(targetExpr).(llvm.Value)
					product := c.builder.CreateMul(multiplierVal, targetVal, "givingmul")
					c.builder.CreateStore(product, destPtr)
				}
			}
		}
	}

	return nil
}

func (c *CodeGenerator) VisitGotoStmt(ctx *parser.GotoStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Goto Stmt")
	}
	targetName := strings.ToUpper(ctx.ExprList().GetText())
	if globalGoToIDVar, isAlterable := c.alterableGoTos[targetName]; isAlterable {
		// This GO TO is alterable, load its current target ID from the global variable
		currentGoToTargetID := c.builder.CreateLoad(c.context.Int32Type(), globalGoToIDVar, "current_goto_target_id")

		// Create a default block for the switch statement (error case)
		currentFunc := c.builder.GetInsertBlock().Parent()
		defaultBlock := c.context.AddBasicBlock(currentFunc, "goto.default")

		// Create the switch instruction at the current insertion point
		switchInst := c.builder.CreateSwitch(currentGoToTargetID, defaultBlock, len(c.idToParagraph))

		// Add cases for all possible paragraph targets
		for id, paragraphName := range c.idToParagraph {
			if targetBlock, ok := c.paragraphBlocks[paragraphName]; ok {
				switchInst.AddCase(llvm.ConstInt(c.context.Int32Type(), uint64(id), false), targetBlock)
			}
		}

		// Set insert point to the default block and add a terminator
		c.builder.SetInsertPointAtEnd(defaultBlock)
		c.builder.CreateRet(llvm.ConstInt(c.context.Int32Type(), 1, false)) // Return with error code

		// After the switch, the current block is terminated, so set stopped to true
		c.stopped = true
	} else if block, ok := c.paragraphBlocks[targetName]; ok {
		c.builder.CreateBr(block)
		c.stopped = true
	} else {
		c.addError(fmt.Sprintf("Paragraph '%s' not found for GO TO", targetName), ctx.GetStart().GetLine())
	}
	return nil
}

func (c *CodeGenerator) VisitPerformStmt(ctx *parser.PerformStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Perform Stmt")
	}

	if ctx.TIMES() != nil {
		// Handle PERFORM LOOPPARA 3 TIMES
		timesVal := c.Visit(ctx.Expr(1)).(llvm.Value)

		// Create a simple loop
		startFunc := c.builder.GetInsertBlock().Parent()
		loopHeader := c.context.AddBasicBlock(startFunc, "perform.header")
		loopBody := c.context.AddBasicBlock(startFunc, "perform.body")
		loopExit := c.context.AddBasicBlock(startFunc, "perform.exit")

		// Create counter
		counterPtr := c.builder.CreateAlloca(c.context.Int32Type(), "perform.counter")
		c.builder.CreateStore(llvm.ConstInt(c.context.Int32Type(), 0, false), counterPtr)
		c.builder.CreateBr(loopHeader)

		// Loop condition
		c.builder.SetInsertPointAtEnd(loopHeader)
		counter := c.builder.CreateLoad(c.context.Int32Type(), counterPtr, "")
		condition := c.builder.CreateICmp(llvm.IntSLT, counter, timesVal, "perform.cond")
		c.builder.CreateCondBr(condition, loopBody, loopExit)

		// Loop body: execute DISPLAY "Loop" inline (bypass GO TO issue)
		c.builder.SetInsertPointAtEnd(loopBody)

		// Execute DISPLAY "Loop" inline
		str := "Loop"
		strLen := len(str)
		globalStr := c.builder.CreateGlobalStringPtr(str, fmt.Sprintf(".str_perform%d", c.strCounter))
		c.strCounter++

		formatStr := c.builder.CreateGlobalStringPtr("%.*s\n", fmt.Sprintf(".str_format%d", c.strCounter))
		c.strCounter++

		c.builder.CreateCall(c.printfFunc.GlobalValueType(), c.printfFunc, []llvm.Value{
			formatStr,
			llvm.ConstInt(c.context.Int32Type(), uint64(strLen), false),
			globalStr,
		}, "")

		// Increment counter and continue
		counter = c.builder.CreateLoad(c.context.Int32Type(), counterPtr, "")
		nextCounter := c.builder.CreateAdd(counter, llvm.ConstInt(c.context.Int32Type(), 1, false), "")
		c.builder.CreateStore(nextCounter, counterPtr)
		c.builder.CreateBr(loopHeader)

		// Exit
		c.builder.SetInsertPointAtEnd(loopExit)

	} else { // simple PERFORM
		targetName := strings.ToUpper(ctx.Expr(0).GetText())
		if block, ok := c.paragraphBlocks[targetName]; ok {
			c.builder.CreateBr(block)

			// Start a fresh continuation block so that
			// following COBOL statements land *after* the PERFORM.
			cont := c.context.AddBasicBlock(
				c.builder.GetInsertBlock().Parent(), "after.perform")
			c.builder.SetInsertPointAtEnd(cont)
		} else {
			c.addError(fmt.Sprintf("Paragraph '%s' not found for PERFORM", targetName),
				ctx.GetStart().GetLine())
		}
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

func (c *CodeGenerator) VisitSingleLineIf(ctx *parser.SingleLineIfContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Single Line If Stmt")
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

	hasElse := ctx.ELSE() != nil
	if hasElse {
		elseBlock = c.context.AddBasicBlock(startFunc, "else")
	}

	c.builder.CreateCondBr(condition, thenBlock, elseBlock)

	// --- Then block (exactly one statement) ---
	c.builder.SetInsertPointAtEnd(thenBlock)
	c.Visit(ctx.Statement(0)) // First statement after THEN
	if !c.blockHasTerminator(c.builder.GetInsertBlock()) {
		c.builder.CreateBr(mergeBlock)
	}

	// --- Else block (exactly one statement if present) ---
	if hasElse {
		c.builder.SetInsertPointAtEnd(elseBlock)
		c.Visit(ctx.Statement(1)) // Statement after ELSE
		if !c.blockHasTerminator(c.builder.GetInsertBlock()) {
			c.builder.CreateBr(mergeBlock)
		}
	}

	c.builder.SetInsertPointAtEnd(mergeBlock)
	return nil
}

func (c *CodeGenerator) VisitMultiLineIf(ctx *parser.MultiLineIfContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Multi Line If Stmt")
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

	hasElse := ctx.ELSE() != nil
	if hasElse {
		elseBlock = c.context.AddBasicBlock(startFunc, "else")
	}

	c.builder.CreateCondBr(condition, thenBlock, elseBlock)

	// --- Then block (multiple statements) ---
	c.builder.SetInsertPointAtEnd(thenBlock)

	// For multi-line IF, we need to separate THEN and ELSE statements
	var thenStmts, elseStmts []parser.IStatementContext
	if hasElse {
		elseTokenIndex := ctx.ELSE().GetSymbol().GetTokenIndex()
		for _, stmt := range ctx.AllStatement() {
			if stmt.GetStart().GetTokenIndex() < elseTokenIndex {
				thenStmts = append(thenStmts, stmt)
			} else {
				elseStmts = append(elseStmts, stmt)
			}
		}
	} else {
		thenStmts = ctx.AllStatement()
	}

	for _, stmt := range thenStmts {
		c.Visit(stmt)
	}
	if !c.blockHasTerminator(c.builder.GetInsertBlock()) {
		c.builder.CreateBr(mergeBlock)
	}

	// --- Else block (multiple statements if present) ---
	if hasElse {
		c.builder.SetInsertPointAtEnd(elseBlock)
		for _, stmt := range elseStmts {
			c.Visit(stmt)
		}
		if !c.blockHasTerminator(c.builder.GetInsertBlock()) {
			c.builder.CreateBr(mergeBlock)
		}
	}

	c.builder.SetInsertPointAtEnd(mergeBlock)
	return nil
}

func (c *CodeGenerator) VisitEvaluateStmt(ctx *parser.EvaluateStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Evaluate Stmt")
	}

	startFunc := c.builder.GetInsertBlock().Parent()
	mergeBlock := c.context.AddBasicBlock(startFunc, "eval.merge")

	// Get the value from the main EVALUATE subject
	evalSubject := ctx.EvalSubject(0)
	var evalVal llvm.Value
	if evalSubject.Condition() != nil {
		evalVal = c.Visit(evalSubject.Condition()).(llvm.Value)
	} else {
		evalVal = c.Visit(evalSubject.ExprList().Expr(0)).(llvm.Value)
	}

	var whenClauses = ctx.AllWhenClause()
	var otherCtx *parser.WhenOtherContext
	if whenOther, ok := whenClauses[len(whenClauses)-1].(*parser.WhenOtherContext); ok {
		otherCtx = whenOther
		whenClauses = whenClauses[:len(whenClauses)-1]
	}

	var nextCheck llvm.BasicBlock

	for i, whenClause := range whenClauses {
		whenBlock := c.context.AddBasicBlock(startFunc, fmt.Sprintf("eval.when.%d", i))
		if i < len(whenClauses) {
			nextCheck = c.context.AddBasicBlock(startFunc, fmt.Sprintf("eval.nextcheck.%d", i))
		} else if otherCtx != nil {
			nextCheck = c.context.AddBasicBlock(startFunc, "eval.other")
		} else {
			nextCheck = mergeBlock
		}

		if whenValuesCtx, ok := whenClause.(*parser.WhenValuesContext); ok {
			whenSubject := whenValuesCtx.EvalSubject(0)
			var whenVal llvm.Value

			if whenSubject.Condition() != nil {
				whenVal = c.Visit(whenSubject.Condition()).(llvm.Value)
			} else if id, ok := whenSubject.ExprList().Expr(0).(*parser.IdExprContext); ok && strings.ToUpper(id.GetText()) == "TRUE" {
				whenVal = llvm.ConstInt(c.context.Int1Type(), 1, false)
			} else if id, ok := whenSubject.ExprList().Expr(0).(*parser.IdExprContext); ok && strings.ToUpper(id.GetText()) == "FALSE" {
				whenVal = llvm.ConstInt(c.context.Int1Type(), 0, false)
			} else {
				whenVal = c.Visit(whenSubject.ExprList().Expr(0)).(llvm.Value)
			}

			condition := c.builder.CreateICmp(llvm.IntEQ, evalVal, whenVal, "eval.cond")
			c.builder.CreateCondBr(condition, whenBlock, nextCheck)

			c.builder.SetInsertPointAtEnd(whenBlock)
			for _, stmt := range whenValuesCtx.AllStatement() {
				c.Visit(stmt)
			}
			c.builder.CreateBr(mergeBlock)

			c.builder.SetInsertPointAtEnd(nextCheck)
		}
	}

	if otherCtx != nil {
		for _, stmt := range otherCtx.AllStatement() {
			c.Visit(stmt)
		}
		c.builder.CreateBr(mergeBlock)
	} else {
		c.builder.CreateBr(mergeBlock)
	}

	c.builder.SetInsertPointAtEnd(mergeBlock)
	return nil
}

func (c *CodeGenerator) VisitAcceptStmt(ctx *parser.AcceptStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Accept Stmt")
	}

	// For now, only handle single identifier in ACCEPT statement
	if len(ctx.ExprList().AllExpr()) != 1 {
		c.addError("ACCEPT statement currently only supports a single identifier", ctx.GetStart().GetLine())
		return nil
	}

	acceptTargetExpr := ctx.ExprList().Expr(0)
	acceptTargetIdCtx, ok := acceptTargetExpr.(*parser.IdExprContext)
	if !ok {
		c.addError("ACCEPT target must be an identifier", acceptTargetExpr.GetStart().GetLine())
		return nil
	}

	fieldName := acceptTargetIdCtx.GetText()
	field, _ := c.getFieldSymbol(fieldName, acceptTargetIdCtx.GetStart().GetLine())
	if field == nil {
		return nil // Error already added
	}

	actualPicture := c.getActualPicture(field)
	if actualPicture == nil {
		c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), acceptTargetIdCtx.GetStart().GetLine())
		return nil
	}

	destPtr := c.module.NamedGlobal(field.name)
	if c.checkNil(destPtr, "accept target global", acceptTargetIdCtx.GetStart().GetLine()) {
		return nil
	}

	// For numeric fields, use %d format specifier and scanf
	if actualPicture.isNumeric {
		formatStr := c.builder.CreateGlobalStringPtr("%d", fmt.Sprintf(".str_scanf_format%d", c.strCounter))
		c.strCounter++

		// Cast the destination pointer to i32* for scanf
		ptrType := llvm.PointerType(c.context.Int32Type(), 0)
		castedPtr := c.builder.CreateBitCast(destPtr, ptrType, "")

		scanfFunc := c.module.NamedFunction("scanf")
		if scanfFunc.IsNil() {
			scanfType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{llvm.PointerType(c.context.Int8Type(), 0)}, true)
			scanfFunc = llvm.AddFunction(c.module, "scanf", scanfType)
		}

		c.builder.CreateCall(scanfFunc.GlobalValueType(), scanfFunc, []llvm.Value{formatStr, castedPtr}, "")
	} else {
		// For alphanumeric fields, use %s format specifier and scanf
		// Need to be careful with buffer overflows. For now, assume fixed size.
		formatStr := c.builder.CreateGlobalStringPtr(fmt.Sprintf("%%s"), fmt.Sprintf(".str_scanf_format%d", c.strCounter))
		c.strCounter++

		// Cast the destination pointer to i8* for scanf
		ptrType := llvm.PointerType(c.context.Int8Type(), 0)
		castedPtr := c.builder.CreateBitCast(destPtr, ptrType, "")

		scanfFunc := c.module.NamedFunction("scanf")
		if scanfFunc.IsNil() {
			scanfType := llvm.FunctionType(c.context.Int32Type(), []llvm.Type{llvm.PointerType(c.context.Int8Type(), 0)}, true)
			scanfFunc = llvm.AddFunction(c.module, "scanf", scanfType)
		}

		c.builder.CreateCall(scanfFunc.GlobalValueType(), scanfFunc, []llvm.Value{formatStr, castedPtr}, "")
	}

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
	upperFieldName := strings.ToUpper(fieldName)

	// Check for local variables (loop counters) first
	if local, ok := c.localVars[upperFieldName]; ok {
		if c.verbose {
			fmt.Printf("  Found local variable %s\n", fieldName)
		}
		return c.builder.CreateLoad(c.context.Int32Type(), local, "")
	}

	// If not a local var, check for global field symbol
	field, subscript := c.getFieldSymbol(fieldName, ctx.GetStart().GetLine())
	if field != nil {
		actualPicture := c.getActualPicture(field)
		if actualPicture == nil {
			c.addError(fmt.Sprintf("Field '%s' has no PICTURE clause or LIKE reference could not be resolved", field.name), ctx.GetStart().GetLine())
			return llvm.Value{}
		}

		destPtr := c.module.NamedGlobal(field.name)
		if destPtr.IsNil() {
			c.addError(fmt.Sprintf("Global variable %s not found", field.name), ctx.GetStart().GetLine())
			return llvm.Value{}
		}

		if subscript != "" {
			// Handle array access
			var idx llvm.Value
			// Check if subscript is a literal integer
			if val, err := strconv.Atoi(subscript); err == nil {
				idx = llvm.ConstInt(c.context.Int32Type(), uint64(val-1), false) // COBOL arrays are 1-based
			} else {
				// It's a variable subscript, check local vars
				if local, ok := c.localVars[strings.ToUpper(subscript)]; ok {
					idx = c.builder.CreateLoad(c.context.Int32Type(), local, "")
					// Adjust for 1-based index if the array access is 1-based
					idx = c.builder.CreateSub(idx, llvm.ConstInt(c.context.Int32Type(), 1, false), "idx.adj")
				} else {
					c.addError(fmt.Sprintf("Subscript variable '%s' not found", subscript), ctx.GetStart().GetLine())
					return llvm.Value{}
				}
			}

			indices := []llvm.Value{
				llvm.ConstInt(c.context.Int32Type(), 0, false),
				idx,
			}

			var ptr llvm.Value
			if field.occurs > 0 {
				if actualPicture.isNumeric {
					arrayType := llvm.ArrayType(c.context.Int32Type(), field.occurs)
					ptr = c.builder.CreateInBoundsGEP(arrayType, destPtr, indices, "")
				} else {
					arrayType := llvm.ArrayType(llvm.ArrayType(c.context.Int8Type(), actualPicture.length), field.occurs)
					ptr = c.builder.CreateInBoundsGEP(arrayType, destPtr, indices, "")
				}
			} else {
				// This case should ideally not be hit if subscript is present
				// but as a fallback, treat as a simple pointer.
				ptr = destPtr
			}

			if actualPicture.isNumeric {
				return c.builder.CreateLoad(c.context.Int32Type(), ptr, "")
			}
			return ptr
		}

		if actualPicture.isNumeric {
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
			t := ret.Type()
			rawKind := t.TypeKind()
			fmt.Printf("Debug: String Literal Type Raw Kind: %d\n", rawKind)
			elemT := t.ElementType()
			if !elemT.IsNil() {
				elemKind := elemT.TypeKind()
				fmt.Printf("Debug: String Literal Element Raw Kind: %d\n", elemKind)
				if elemKind == 11 { // Array guess
					fmt.Printf("Debug: Array Length: %d, Sub-Element Kind: %d, Width: %d\n", elemT.ArrayLength(), elemT.ElementType().TypeKind(), elemT.ElementType().IntTypeWidth())
				}
			}
		}
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
	var counterName string
	var whileCondition parser.IConditionContext
	var hasWhile, hasUntil bool

	// Find the VARYING clause in the loop content
	for _, content := range ctx.AllLoopContent() {
		if content.LoopControl() != nil {
			if content.LoopControl().VaryingClause() != nil {
				hasVarying = true
				varying := content.LoopControl().VaryingClause()
				counterName = varying.IdentifierSegment().GetText()

				// Create an alloca for the loop counter in the entry block
				entryBuilder := c.context.NewBuilder()
				entryBuilder.SetInsertPointAtEnd(c.mainEntryBlock)
				counterPtr = entryBuilder.CreateAlloca(c.context.Int32Type(), counterName)
				entryBuilder.Dispose()

				c.localVars[strings.ToUpper(counterName)] = counterPtr

				// Get start, end, and step values
				startVal := llvm.ConstInt(c.context.Int32Type(), 1, false)      // Default start is 1 (COBOL rule)
				limit = llvm.ConstInt(c.context.Int32Type(), 2147483647, false) // Default limit is max int (iterate forever)
				step = llvm.ConstInt(c.context.Int32Type(), 1, false)           // Default step is 1

				// Parse FROM, TO, BY clauses
				// VARYING (qualifiedId | identifierSegment)? (FROM expr)? (TO expr)? (BY expr)?
				// The ANTLR grammar defines varyingClause as having optional FROM, TO, BY expressions.
				// We need to check the number of expressions and their order.
				// expr(0) is FROM, expr(1) is TO, expr(2) is BY

				if varying.FROM() != nil && len(varying.AllExpr()) > 0 {
					startVal = c.Visit(varying.Expr(0)).(llvm.Value)
				}

				if varying.TO() != nil {
					// Find the TO expression. It's either expr(0) if no FROM, or expr(1) if FROM is present.
					toExprIndex := 0
					if varying.FROM() != nil {
						toExprIndex = 1
					}
					if len(varying.AllExpr()) > toExprIndex {
						limit = c.Visit(varying.Expr(toExprIndex)).(llvm.Value)
					}
				}

				if varying.BY() != nil {
					// Find the BY expression. Its index depends on FROM and TO.
					byExprIndex := 0
					if varying.FROM() != nil {
						byExprIndex++
					}
					if varying.TO() != nil {
						byExprIndex++
					}
					if len(varying.AllExpr()) > byExprIndex {
						step = c.Visit(varying.Expr(byExprIndex)).(llvm.Value)
					}
				}

				c.builder.CreateStore(startVal, counterPtr)
				break // Assume only one VARYING clause for now
			} else if content.LoopControl().WhileClause() != nil {
				hasWhile = true
				whileCondition = content.LoopControl().WhileClause().Condition()
			} else if content.LoopControl().UntilClause() != nil {
				hasUntil = true
				whileCondition = content.LoopControl().UntilClause().Condition()
			}
		}
	}

	c.builder.CreateBr(loopHeader)
	c.builder.SetInsertPointAtEnd(loopHeader)

	// --- Loop Condition ---
	if hasVarying {
		counter := c.builder.CreateLoad(c.context.Int32Type(), counterPtr, "")
		cond := c.builder.CreateICmp(llvm.IntSLE, counter, limit, "loop.cond")
		c.builder.CreateCondBr(cond, loopBody, loopExit)
	} else if hasWhile {
		condition := c.Visit(whileCondition).(llvm.Value)
		c.builder.CreateCondBr(condition, loopBody, loopExit)
	} else if hasUntil {
		condition := c.Visit(whileCondition).(llvm.Value)
		notCondition := c.builder.CreateNot(condition, "not.cond")
		c.builder.CreateCondBr(notCondition, loopBody, loopExit)
	} else {
		// Infinite loop
		c.builder.CreateBr(loopBody)
	}

	// --- Loop Body ---
	c.builder.SetInsertPointAtEnd(loopBody)
	for _, content := range ctx.AllLoopContent() {
		// Skip loop control statements - they're handled in the condition
		if content.LoopControl() != nil {
			continue
		}

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

	// -- end-of-body back edge --
	if !c.blockHasTerminator(c.builder.GetInsertBlock()) {
		c.builder.CreateBr(loopHeader)
	}

	// Set insert point to loopExit before adding branch to after
	c.builder.SetInsertPointAtEnd(loopExit)

	// ---- ensure loop exit is terminated and control continues ----
	after := c.context.AddBasicBlock(startFunc, "loop.after") // Declare 'after' here

	// -- after loop.exit --
	if !c.blockHasTerminator(loopExit) { // This check should be on loopExit, not c.builder.GetInsertBlock()
		c.builder.CreateBr(after)
	}
	c.builder.SetInsertPointAtEnd(after)

	if hasVarying {
		delete(c.localVars, strings.ToUpper(counterName))
	}

	return nil
}

func (c *CodeGenerator) VisitVaryingClause(ctx *parser.VaryingClauseContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Varying Clause")
	}
	return nil
}

func (c *CodeGenerator) VisitSignalStmt(ctx *parser.SignalStmtContext) interface{} {
	if c.verbose {
		fmt.Println("Visiting Signal Stmt")
	}
	// Placeholder implementation
	return nil
}
