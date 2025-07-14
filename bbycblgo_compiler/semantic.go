package main

import (
	"bbycblgo_compiler/parser"
	"fmt"
	"github.com/antlr4-go/antlr/v4"
	"strconv"
	"strings"
)

// --- Data Structures ---

// Scope represents a lexical scope, holding symbols and a pointer to its parent scope.
type Scope struct {
	fields map[string][]*FieldSymbol
	parent *Scope
	children []*Scope
}

// SymbolTable holds all symbols discovered in the program.
type SymbolTable struct {
	rootScope         *Scope
	paragraphs        map[string]*ParagraphSymbol // For quick lookup
	orderedParagraphs []*ParagraphSymbol          // For flow analysis
}

// FieldSymbol represents a data field in the DATA DIVISION.
type FieldSymbol struct {
	name        string
	level       int
	picture     *PictureType
	likeRef     string // For LIKE clause
	occurs      int    // For OCCURS clause
	initialized bool   // For tracking initialization
	parent      *FieldSymbol
	children    []*FieldSymbol
}

// PictureType stores parsed information from a PICTURE clause.
type PictureType struct {
	raw        string
	isNumeric  bool
	isAlpha    bool
	isAlphaNum bool
	length     int // Total length of the field
}

// ParagraphSymbol represents a paragraph in the PROCEDURE DIVISION.
type ParagraphSymbol struct {
	name   string
	params []*FieldSymbol
}

// SemanticError represents an error found during semantic analysis.
type SemanticError struct {
	msg  string
	line int
}

// --- Base Analyzer ---

const (
	AreaAStart = 7  // Column 8 (0-indexed)
	AreaAEnd   = 10 // Column 11
	AreaBStart = 11 // Column 12
)

// BaseAnalyzer provides common functionality for different visitor passes.
type BaseAnalyzer struct {
	*parser.BasebbyCBLVisitor
	errors      []SemanticError
	fixedFormat bool
}

func (v *BaseAnalyzer) addError(msg string, line int) {
	v.errors = append(v.errors, SemanticError{msg, line})
}

func (v *BaseAnalyzer) checkAreaA(token antlr.Token, constructName string) {
	if !v.fixedFormat {
		return
	}
	col := token.GetColumn()
	if col < AreaAStart || col > AreaAEnd {
		v.addError(fmt.Sprintf("%s must start in Area A (columns 8-11), but started in column %d", constructName, col+1), token.GetLine())
	}
}

func (v *BaseAnalyzer) checkAreaB(token antlr.Token, constructName string) {
	if !v.fixedFormat {
		return
	}
	col := token.GetColumn()
	if col < AreaBStart {
		v.addError(fmt.Sprintf("%s must start in Area B (columns 12 onwards), but started in column %d", constructName, col+1), token.GetLine())
	}
}

func (v *BaseAnalyzer) Visit(tree antlr.ParseTree) interface{} {
	return tree.Accept(v)
}

// --- Pass 1: Symbol Table Builder ---

type SymbolTableBuilder struct {
	*BaseAnalyzer // Embed BaseAnalyzer instead of BasebbyCBLVisitor directly
	symbolTable   *SymbolTable
	parentStack   []*FieldSymbol
	currentScope  *Scope
}

func NewSymbolTableBuilder() *SymbolTableBuilder {
	return &SymbolTableBuilder{
		BaseAnalyzer: &BaseAnalyzer{ // Initialize BaseAnalyzer
			BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
			errors:            []SemanticError{},
			fixedFormat:       false,
		},
		symbolTable: &SymbolTable{
			rootScope:         &Scope{fields: make(map[string][]*FieldSymbol)},
			paragraphs:        make(map[string]*ParagraphSymbol),
			orderedParagraphs: []*ParagraphSymbol{},
		},
		parentStack: []*FieldSymbol{},
	}
}

func (v *SymbolTableBuilder) VisitProgram(ctx *parser.ProgramContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *SymbolTableBuilder) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	return v.VisitChildren(ctx)
}

func (v *SymbolTableBuilder) VisitChildren(node antlr.RuleNode) interface{} {
	for _, child := range node.GetChildren() {
		if child == nil {
			continue // Skip nil children
		}
		if parseTreeChild, ok := child.(antlr.ParseTree); ok && parseTreeChild != nil {
			parseTreeChild.Accept(v) // Pass the concrete visitor (SymbolTableBuilder)
		}
	}
	return nil
}

func (v *SymbolTableBuilder) VisitDataEntry(ctx *parser.DataEntryContext) interface{} {
	levelStr := ctx.LevelNumber().GetText()
	level, err := strconv.Atoi(levelStr)
	if err != nil {
		v.addError(fmt.Sprintf("Level number '%s' is not a valid integer", levelStr), ctx.GetStart().GetLine())
		return nil
	}

	var name string
	if id := ctx.Identifier(); id != nil {
		name = id.GetText()
	} else {
		v.addError("Data entry without an identifier", ctx.GetStart().GetLine())
		return nil
	}

	if level == 1 { // Only level 01 must be in Area A
		v.checkAreaA(ctx.LevelNumber().GetStart(), fmt.Sprintf("Level number %s", levelStr))
	} else if level >= 2 && level <= 49 { // Levels 02-49 must be in Area B
		v.checkAreaB(ctx.LevelNumber().GetStart(), fmt.Sprintf("Level number %s", levelStr))
	}

	if !((level >= 0 && level <= 99)) { // Allow level 00-99
		v.addError(fmt.Sprintf("Invalid level number: must be 00-99, got %d", level), ctx.GetStart().GetLine())
		return nil
	}

	uname := strings.ToUpper(name) // Convert to uppercase once
	field := &FieldSymbol{name: uname, level: level} // Use uname for the field name

	// Manage parent stack and add to symbol table
	for len(v.parentStack) > 0 && v.parentStack[len(v.parentStack)-1].level >= level {
		v.parentStack = v.parentStack[:len(v.parentStack)-1]
	}

	if len(v.parentStack) > 0 {
		// This is a child field
		parent := v.parentStack[len(v.parentStack)-1]

		// Check for duplicate name among siblings
		for _, sibling := range parent.children {
			if sibling.name == uname {
				v.addError(fmt.Sprintf("Duplicate field name '%s' within the same parent scope", name), ctx.GetStart().GetLine())
				return nil // Stop processing this data entry
			}
		}

		field.parent = parent
		parent.children = append(parent.children, field)
	} else {
		// This is a top-level (01, 77, or 99) field
		currentSlice, ok := v.symbolTable.rootScope.fields[uname]
		if !ok {
			currentSlice = make([]*FieldSymbol, 0) // Initialize an empty slice
		}
		currentSlice = append(currentSlice, field)
		v.symbolTable.rootScope.fields[uname] = currentSlice
	}

	v.parentStack = append(v.parentStack, field) // Add current field to stack

	if picCtx := ctx.PictureClause(); picCtx != nil {
		if picPatternCtx := picCtx.PicturePattern(); picPatternCtx != nil {
			picPattern := picPatternCtx.GetText()
			field.picture = v.analyzePicture(picPattern, ctx.GetStart().GetLine())
		}
	}

	if likeCtx := ctx.LikeClause(); likeCtx != nil {
		allSegs := likeCtx.AllIdentifierSegment()
		if len(allSegs) > 0 {
			field.likeRef = allSegs[0].GetText()
			for i := 1; i < len(allSegs); i++ {
				field.likeRef += " OF " + allSegs[i].GetText()
			}
		}
	}

	if occursCtx := ctx.OccursClause(); occursCtx != nil {
		if num := occursCtx.NUMBER(); num != nil {
			occursStr := num.GetText()
			field.occurs, _ = strconv.Atoi(occursStr)
		}
	}

	return v.VisitChildren(ctx)
}

func (v *SymbolTableBuilder) analyzePicture(pic string, line int) *PictureType {
	pic = strings.ToUpper(pic) // Convert picture string to uppercase
	pt := &PictureType{raw: pic}
	hasNumeric, hasAlpha, hasAlphaNum := false, false, false
	sCount, vCount := 0, 0
	length := 0
	runes := []rune(pic)
	i := 0
	for i < len(runes) {
		char := runes[i]
		i++
		repetition := 1
		if i < len(runes) && runes[i] == '(' {
			start := i + 1
			end := start
			for end < len(runes) && runes[end] != ')' {
				end++
			}
			if end < len(runes) {
				numStr := string(runes[start:end])
				if num, err := strconv.Atoi(numStr); err == nil {
					repetition = num
					i = end + 1
				} else {
					v.addError(fmt.Sprintf("Invalid repetition factor in PICTURE: %s", numStr), line)
				}
			}
		}
		switch char {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			hasNumeric = true
			length += repetition
		case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'Q', 'R', 'T', 'U', 'W', 'Y': // Exclude S, X, Z, P for specific handling
			hasAlpha = true
			length += repetition
		case 'X':
			hasAlphaNum = true
			length += repetition
		case 'S':
			if i == 0 { // 'S' is a sign only if it's the first character
				sCount += 1
				hasNumeric = true
			} else { // Otherwise, it's an alphabetic character
				hasAlpha = true
				length += repetition
			}
		case 'V':
			vCount += 1
			hasNumeric = true
		case 'Z', 'P', '-':
			hasNumeric = true
			length += repetition
		default:
			v.addError(fmt.Sprintf("Invalid character '%c' in PICTURE clause", char), line)
		}
	}
	if sCount > 1 {
		v.addError("PICTURE clause can only contain only one 'S' for sign", line)
	}
	if vCount > 1 {
		v.addError("PICTURE clause can only contain one 'V' for an implicit decimal point", line)
	}
	
	
	pt.isNumeric = hasNumeric && !hasAlpha && !hasAlphaNum
	pt.isAlpha = hasAlpha
	pt.isAlphaNum = hasAlphaNum
	pt.length = length
	return pt
}

func (v *SymbolTableBuilder) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	name := ctx.Identifier().GetText()
	v.checkAreaA(ctx.Identifier().GetStart(), fmt.Sprintf("Paragraph '%s'", name))
	if _, exists := v.symbolTable.paragraphs[name]; exists {
		v.addError(fmt.Sprintf("Duplicate paragraph name '%s'", name), ctx.GetStart().GetLine())
		return v.VisitChildren(ctx)
	}
	paraSymbol := &ParagraphSymbol{name: name}
	procDiv := getProcedureDivisionAncestor(ctx)
	if procDiv != nil {
		if using := procDiv.UsingClause(); using != nil {
			for _, ex := range using.AllExpr() {
				segs := getIdentifierSegments(ex)
				if segs == nil {
					v.addError("Non-identifier in USING clause", ex.GetStart().GetLine())
					continue
				}
				if len(segs) > 1 {
					v.addError("Qualified identifier not allowed in USING clause", ex.GetStart().GetLine())
					continue
				}
				paramName := segs[0]
				if fieldSlice, exists := v.symbolTable.rootScope.fields[paramName]; exists {
					if len(fieldSlice) > 0 {
						// Use the first field in the slice
						paraSymbol.params = append(paraSymbol.params, fieldSlice[0])
					} else {
						v.addError(fmt.Sprintf("Parameter '%s' in USING clause for paragraph '%s' not found in Data Division", paramName, name), using.GetStart().GetLine())
					}
				} else {
					v.addError(fmt.Sprintf("Parameter '%s' in USING clause for paragraph '%s' not found in Data Division", paramName, name), using.GetStart().GetLine())
				}
			}
		}
	}
	v.symbolTable.paragraphs[name] = paraSymbol
	v.symbolTable.orderedParagraphs = append(v.symbolTable.orderedParagraphs, paraSymbol)
	return v.VisitChildren(ctx)
}

func getProcedureDivisionAncestor(node antlr.Tree) *parser.ProcedureDivisionContext {
	parent := node.GetParent()
	for parent != nil {
		if p, ok := parent.(*parser.ProcedureDivisionContext); ok {
			return p
		}
		parent = parent.GetParent()
	}
	return nil
}

// --- Pass 2: Semantic Checker ---

type SemanticChecker struct {
	*BaseAnalyzer
	symbolTable  *SymbolTable
	currentPara  *ParagraphSymbol
	cfg          map[string][]string
	hasStopRun   bool
	analyzedFlow bool
}

func NewSemanticChecker(symbolTable *SymbolTable) *SemanticChecker {
	return &SemanticChecker{
		BaseAnalyzer: &BaseAnalyzer{
			BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
			errors:            []SemanticError{},
			fixedFormat:       false,
		},
		symbolTable:  symbolTable,
		cfg:          make(map[string][]string),
		hasStopRun:   false,
		analyzedFlow: false,
	}
}

func (v *SemanticChecker) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	name := strings.ToUpper(ctx.Identifier().GetText())
	if p, exists := v.symbolTable.paragraphs[name]; exists {
		v.currentPara = p
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitChildren(node antlr.RuleNode) interface{} {
	for _, child := range node.GetChildren() {
		if child == nil {
			continue // Skip nil children
		}
		if parseTreeChild, ok := child.(antlr.ParseTree); ok && parseTreeChild != nil {
			parseTreeChild.Accept(v) // Pass the concrete visitor (SemanticChecker)
		}
	}
	return nil
}

func (v *SemanticChecker) VisitGotoStmt(ctx *parser.GotoStmtContext) interface{} {
	if v.currentPara == nil {
		return nil
	}
	if len(ctx.ExprList().AllExpr()) != 1 {
		v.addError("GO TO expects single identifier", ctx.GetStart().GetLine())
		return nil
	}
	ex := ctx.ExprList().Expr(0)
	segs := getIdentifierSegments(ex)
	if segs == nil || len(segs) > 1 {
		v.addError("GO TO target must be a simple identifier", ex.GetStart().GetLine())
		return nil
	}
	targetName := segs[0]
	if _, exists := v.symbolTable.paragraphs[targetName]; !exists {
		v.addError(fmt.Sprintf("GO TO target paragraph '%s' not found", targetName), ctx.GetStart().GetLine())
		return nil
	}
	v.cfg[v.currentPara.name] = append(v.cfg[v.currentPara.name], targetName)
	return nil
}

func (v *SemanticChecker) VisitStopStmt(ctx *parser.StopStmtContext) interface{} {
	v.hasStopRun = true
	if v.currentPara != nil {
		if _, exists := v.cfg[v.currentPara.name]; !exists {
			v.cfg[v.currentPara.name] = []string{}
		}
	}
	return nil
}
func (v *SemanticChecker) analyzeFlow() {
	if v.analyzedFlow || len(v.symbolTable.orderedParagraphs) == 0 {
		return
	}
	v.analyzedFlow = true
	for i := 0; i < len(v.symbolTable.orderedParagraphs)-1; i++ {
		current := v.symbolTable.orderedParagraphs[i]
		if _, endsInJump := v.cfg[current.name]; !endsInJump {
			next := v.symbolTable.orderedParagraphs[i+1]
			v.cfg[current.name] = append(v.cfg[current.name], next.name)
		}
	}
	reachable := make(map[string]bool)
	q := []string{v.symbolTable.orderedParagraphs[0].name}
	reachable[q[0]] = true
	for len(q) > 0 {
		currentName := q[0]
		q = q[1:]
		for _, neighbor := range v.cfg[currentName] {
			if !reachable[neighbor] {
				reachable[neighbor] = true
				q = append(q, neighbor)
			}
		}
	}
	for _, para := range v.symbolTable.orderedParagraphs {
		if !reachable[para.name] {
			v.addError(fmt.Sprintf("Unreachable paragraph '%s'", para.name), 0)
		}
	}
	if !v.hasStopRun {
		v.addError("Program does not contain a STOP RUN statement", 0)
	}
}

func (v *SemanticChecker) VisitDataEntry(ctx *parser.DataEntryContext) interface{} {
	name := ctx.Identifier().GetText()

	// Resolve the current field being processed
	currentField := v.getFieldFromExpr(ctx.Identifier(), ctx.GetStart().GetLine())
	if currentField == nil {
		// Error already added by getFieldFromExpr if not found
		return nil
	}

	if ctx.LikeClause() != nil {
		allSegs := ctx.LikeClause().AllIdentifierSegment()
		if len(allSegs) > 0 {
			// Resolve the target of the LIKE clause
			likeTarget := v.getFieldFromExpr(allSegs[0], ctx.GetStart().GetLine()) // Assuming simple identifier for now
			if likeTarget == nil {
				v.addError(fmt.Sprintf("Field '%s' in LIKE clause for '%s' not found", currentField.likeRef, name), ctx.GetStart().GetLine())
				return nil
			}
			if likeTarget == currentField {
				v.addError(fmt.Sprintf("Recursive LIKE clause for field '%s'", name), ctx.GetStart().GetLine())
				return nil
			}
			currentField.picture = likeTarget.picture
			currentField.children = deepCopyChildren(likeTarget.children, currentField)
		}
	}
	return nil
}

func (v *SemanticChecker) VisitStatement(ctx *parser.StatementContext) interface{} {
	v.checkAreaB(ctx.GetStart(), "Statement")
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitMoveStmt(ctx *parser.MoveStmtContext) interface{} {
	dests := ctx.ExprList(1).AllExpr()
	for _, dest := range dests {
		field := v.getFieldFromExpr(dest, dest.GetStart().GetLine())
		if field != nil {
			field.initialized = true
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitAddStmt(ctx *parser.AddStmtContext) interface{} {
	sources := ctx.ExprList(0).AllExpr()
	dests := ctx.ExprList(1).AllExpr()
	operands := append([]parser.IExprContext{}, sources...)
	operands = append(operands, dests...)
	for _, operand := range operands {
		if !v.isNumeric(operand) {
			v.addError(fmt.Sprintf("Operand '%s' in ADD statement must be numeric", operand.GetText()), operand.GetStart().GetLine())
		}
		field := v.getFieldFromExpr(operand, operand.GetStart().GetLine())
		if field != nil {
			if !field.initialized {
				v.addError(fmt.Sprintf("Variable '%s' may not have been initialized", operand.GetText()), operand.GetStart().GetLine())
			}
		}
	}
	givings := ctx.AllGivingClause()
	if len(givings) > 0 {
		for _, giving := range givings {
			for _, res := range giving.ExprList().AllExpr() {
				field := v.getFieldFromExpr(res, res.GetStart().GetLine())
				if field != nil {
					field.initialized = true
				}
			}
		}
	} else {
		for _, dest := range dests {
			field := v.getFieldFromExpr(dest, dest.GetStart().GetLine())
			if field != nil {
				field.initialized = true
			}
		}
		if len(dests) > 0 {
			last := dests[len(dests)-1]
			if _, ok := last.(*parser.LitExprContext); ok {
				v.addError("ADD statement with a literal as the final operand requires a GIVING clause", ctx.GetStart().GetLine())
			}
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitMultiplyStmt(ctx *parser.MultiplyStmtContext) interface{} {
	sources := ctx.ExprList(0).AllExpr()
	dests := ctx.ExprList(1).AllExpr()
	operands := append([]parser.IExprContext{}, sources...)
	operands = append(operands, dests...)
	for _, operand := range operands {
		if !v.isNumeric(operand) {
			v.addError(fmt.Sprintf("Operand '%s' in MULTIPLY statement must be numeric", operand.GetText()), operand.GetStart().GetLine())
		}
		field := v.getFieldFromExpr(operand, operand.GetStart().GetLine())
		if field != nil {
			if !field.initialized {
				v.addError(fmt.Sprintf("Variable '%s' may not have been initialized", operand.GetText()), operand.GetStart().GetLine())
			}
		}
	}
	givings := ctx.AllGivingClause()
	if len(givings) > 0 {
		for _, giving := range givings {
			for _, res := range giving.ExprList().AllExpr() {
				field := v.getFieldFromExpr(res, res.GetStart().GetLine())
				if field != nil {
					field.initialized = true
				}
			}
		}
	} else {
		for _, dest := range dests {
			field := v.getFieldFromExpr(dest, dest.GetStart().GetLine())
			if field != nil {
				field.initialized = true
			}
		}
		if len(dests) > 0 {
			last := dests[len(dests)-1]
			if _, ok := last.(*parser.LitExprContext); ok {
				v.addError("MULTIPLY statement with a literal as the final operand requires a GIVING clause", ctx.GetStart().GetLine())
			}
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitSubtractStmt(ctx *parser.SubtractStmtContext) interface{} {
	sources := ctx.ExprList(0).AllExpr()
	dests := ctx.ExprList(1).AllExpr()
	operands := append([]parser.IExprContext{}, sources...)
	operands = append(operands, dests...)
	for _, operand := range operands {
		if !v.isNumeric(operand) {
			v.addError(fmt.Sprintf("Operand '%s' in SUBTRACT statement must be numeric", operand.GetText()), operand.GetStart().GetLine())
		}
		field := v.getFieldFromExpr(operand, operand.GetStart().GetLine())
		if field != nil {
			if !field.initialized {
				v.addError(fmt.Sprintf("Variable '%s' may not have been initialized", operand.GetText()), operand.GetStart().GetLine())
			}
		}
	}
	givings := ctx.AllGivingClause()
	if len(givings) > 0 {
		for _, giving := range givings {
			for _, res := range giving.ExprList().AllExpr() {
				field := v.getFieldFromExpr(res, res.GetStart().GetLine())
				if field != nil {
					field.initialized = true
				}
			}
		}
	} else {
		for _, dest := range dests {
			field := v.getFieldFromExpr(dest, dest.GetStart().GetLine())
			if field != nil {
				field.initialized = true
			}
		}
		if len(dests) > 0 {
			last := dests[len(dests)-1]
			if _, ok := last.(*parser.LitExprContext); ok {
				v.addError("SUBTRACT statement with a literal as the final operand requires a GIVING clause", ctx.GetStart().GetLine())
			}
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitCallStmt(ctx *parser.CallStmtContext) interface{} {
	var callTargetName string
	var targetSymbol *ParagraphSymbol
	targetExpr := ctx.Expr()
	if lit, ok := targetExpr.(*parser.LitExprContext); ok {
		callTargetName = lit.GetText()
		if len(callTargetName) > 1 && (callTargetName[0] == '"' || callTargetName[0] == '\'') {
			callTargetName = callTargetName[1 : len(callTargetName)-1]
		}
	} else {
		segs := getIdentifierSegments(targetExpr)
		if segs == nil {
			v.addError("CALL target must be literal or identifier", ctx.GetStart().GetLine())
			return v.VisitChildren(ctx)
		}
		callTargetName = strings.ToUpper(segs[0])
		field := v.getFieldFromExpr(targetExpr, ctx.GetStart().GetLine())
		if field != nil {
			if !field.initialized {
				v.addError(fmt.Sprintf("Variable '%s' used in CALL statement may not be initialized", targetExpr.GetText()), ctx.GetStart().GetLine())
			}
		} else {
			v.addError(fmt.Sprintf("Identifier '%s' in CALL statement not found", targetExpr.GetText()), ctx.GetStart().GetLine())
		}
	}
	if sym, exists := v.symbolTable.paragraphs[callTargetName]; exists {
		targetSymbol = sym
	} else {
		fmt.Printf("Warning: Called program/paragraph '%s' not found in the current source. Assuming it is an external program.\n", callTargetName)
		return v.VisitChildren(ctx)
	}
	var callArgs []parser.IExprContext
	if using := ctx.UsingClause(); using != nil {
		callArgs = using.AllExpr()
	}
	if len(callArgs) != len(targetSymbol.params) {
		v.addError(fmt.Sprintf("Incorrect number of arguments for CALL to '%s'. Expected %d, but got %d", callTargetName, len(targetSymbol.params), len(callArgs)), ctx.GetStart().GetLine())
	} else {
		for _, arg := range callArgs {
			field := v.getFieldFromExpr(arg, arg.GetStart().GetLine())
			if field == nil {
				v.addError(fmt.Sprintf("Argument '%s' in CALL statement not found", arg.GetText()), arg.GetStart().GetLine())
			}
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) VisitDivideStmt(ctx *parser.DivideStmtContext) interface{} {
	sources := ctx.ExprList(0).AllExpr()
	dests := ctx.ExprList(1).AllExpr()
	operands := append([]parser.IExprContext{}, sources...)
	operands = append(operands, dests...)
	for _, operand := range operands {
		if !v.isNumeric(operand) {
			v.addError(fmt.Sprintf("Operand '%s' in DIVIDE statement must be numeric", operand.GetText()), operand.GetStart().GetLine())
		}
		field := v.getFieldFromExpr(operand, operand.GetStart().GetLine())
		if field != nil {
			if !field.initialized {
				v.addError(fmt.Sprintf("Variable '%s' may not have been initialized", operand.GetText()), operand.GetStart().GetLine())
			}
		}
	}
	givings := ctx.AllGivingClause()
	if len(givings) > 0 {
		for _, giving := range givings {
			for _, res := range giving.ExprList().AllExpr() {
				field := v.getFieldFromExpr(res, res.GetStart().GetLine())
				if field != nil {
					field.initialized = true
				}
			}
		}
	} else {
		for _, dest := range dests {
			field := v.getFieldFromExpr(dest, dest.GetStart().GetLine())
			if field != nil {
				field.initialized = true
			}
		}
		if len(dests) > 0 {
			last := dests[len(dests)-1]
			if _, ok := last.(*parser.LitExprContext); ok {
				v.addError("DIVIDE statement with a literal as the final operand requires a GIVING clause", ctx.GetStart().GetLine())
			}
		}
	}
	if ctx.REMAINDER() != nil {
		allExprLists := ctx.AllExprList()
		remainderExprList := allExprLists[len(allExprLists)-1]
		for _, rem := range remainderExprList.AllExpr() {
			field := v.getFieldFromExpr(rem, rem.GetStart().GetLine())
			if field != nil {
				field.initialized = true
			}
		}
	}
	return v.VisitChildren(ctx)
}
func (v *SemanticChecker) VisitEvaluateStmt(ctx *parser.EvaluateStmtContext) interface{} {
	subjectTypes := make([]*PictureType, 0)
	for _, subject := range ctx.AllEvalSubject() {
		if el := subject.ExprList(); el != nil {
			if len(el.AllExpr()) != 1 {
				v.addError("Multi-expression subjects not supported in EVALUATE", subject.GetStart().GetLine())
				continue
			}
			subjectTypes = append(subjectTypes, v.getExpressionType(el.Expr(0)))
		} else {
			// skip condition for type check
			subjectTypes = append(subjectTypes, nil)
		}
	}
	whenClauses := ctx.AllWhenClause()
	for _, when := range whenClauses {
		if wv, ok := when.(*parser.WhenValuesContext); ok {
			ev := wv.EvalSubject(0)
			var whenExprs []parser.IExprContext
			if el := ev.ExprList(); el != nil {
				whenExprs = el.AllExpr()
			} else {
				// skip for condition
				continue
			}
			if len(whenExprs) != len(subjectTypes) {
				v.addError(fmt.Sprintf("WHEN clause has %d conditions, but EVALUATE has %d subjects", len(whenExprs), len(subjectTypes)), when.GetStart().GetLine())
				continue
			}
			for i, whenExpr := range whenExprs {
				whenType := v.getExpressionType(whenExpr)
				if subjectTypes[i] != nil && whenType != nil {
					if subjectTypes[i].isNumeric != whenType.isNumeric ||
						subjectTypes[i].isAlpha != whenType.isAlpha ||
						subjectTypes[i].isAlphaNum != whenType.isAlphaNum {
						v.addError(fmt.Sprintf("Type mismatch in EVALUATE statement. Cannot compare '%s' with '%s'", ev.GetText(), whenExpr.GetText()), whenExpr.GetStart().GetLine())
					}
				}
			}
		}
	}
	return v.VisitChildren(ctx)
}

func (v *SemanticChecker) getExpressionType(expr parser.IExprContext) *PictureType {
	switch e := expr.(type) {
	case *parser.LitExprContext:
		lit := e.Literal()
		if lit.NUMBER() != nil || lit.FLOAT() != nil {
			return &PictureType{isNumeric: true}
		}
		if lit.STRING() != nil {
			return &PictureType{isAlphaNum: true}
		}
		return nil
	case *parser.IdExprContext:
		field := v.getFieldFromExpr(e, 0)
		if field != nil {
			return field.picture
		}
		return nil
	case *parser.QualifiedIdExprContext:
		field := v.getFieldFromExpr(e, 0)
		if field != nil {
			return field.picture
		}
		return nil
	default:
		return nil
	}
}

func (v *SemanticChecker) isNumeric(expr parser.IExprContext) bool {
	picType := v.getExpressionType(expr)
	return picType != nil && picType.isNumeric
}

func getIdentifierSegments(node antlr.ParseTree) []string {
	switch n := node.(type) {
	case *parser.IdExprContext:
		return []string{strings.ToUpper(n.GetText())}
	case *parser.QualifiedIdExprContext:
		segs := []string{}
		for _, seg := range n.QualifiedId().AllIdentifierSegment() {
			segs = append(segs, strings.ToUpper(seg.GetText()))
		}
		return segs
	case *parser.IdentifierContext: // Handle direct IdentifierContext
		return []string{strings.ToUpper(n.GetText())}
	case *parser.IdentifierSegmentContext: // Handle direct IdentifierSegmentContext
		return []string{strings.ToUpper(n.GetText())}
	default:
		return nil
	}
}

func (v *SemanticChecker) getFieldFromExpr(node antlr.ParseTree, line int) *FieldSymbol {
	segs := getIdentifierSegments(node)
	if segs == nil {
		return nil
	}
	return v.resolveQualifiedName(segs, line)
}

func (v *SemanticChecker) resolveQualifiedName(segs []string, line int) *FieldSymbol {
	if len(segs) == 0 {
		return nil
	}
	targetName := segs[0]
	qualifierNames := segs[1:]
	var candidates []*FieldSymbol

	// Iterate over all slices of FieldSymbols in rootScope.fields
	for _, fieldSlice := range v.symbolTable.rootScope.fields {
		// Iterate over each FieldSymbol within the slice
		for _, field := range fieldSlice {
			if field.name == targetName {
				candidates = append(candidates, field)
			}
		}
	}

	if len(candidates) == 0 {
		return nil
	}
	var matchingFields []*FieldSymbol
	for _, candidate := range candidates {
		if v.matchesQualifiers(candidate, qualifierNames) {
			matchingFields = append(matchingFields, candidate)
		}
	}
	if len(matchingFields) == 1 {
		return matchingFields[0]
	}
	if len(matchingFields) > 1 {
		v.addError(fmt.Sprintf("Ambiguous identifier '%s'", strings.Join(segs, " OF ")), line)
	}
	return nil
}

func (v *SemanticChecker) matchesQualifiers(field *FieldSymbol, qualifiers []string) bool {
	current := field.parent
	for _, qName := range qualifiers {
		if current == nil || current.name != qName {
			return false
		}
		current = current.parent
	}
	return true
}

func deepCopyChildren(children []*FieldSymbol, parent *FieldSymbol) []*FieldSymbol {
	if len(children) == 0 {
		return nil
	}
	newChildren := make([]*FieldSymbol, len(children))
	for i, child := range children {
		newChild := &FieldSymbol{
			name:        child.name,
			level:       child.level,
			picture:     child.picture,
			likeRef:     child.likeRef,
			occurs:      child.occurs,
			initialized: false,
			parent:      parent,
		}
		newChild.children = deepCopyChildren(child.children, newChild)
		newChildren[i] = newChild
	}
	return newChildren
}

// --- Main Analysis Function ---

func Analyze(tree antlr.ParseTree) (*SymbolTable, []SemanticError) {
	if tree == nil {
		return nil, []SemanticError{}
	}
	builder := NewSymbolTableBuilder()
	tree.Accept(builder)
	errors := builder.errors
	checker := NewSemanticChecker(builder.symbolTable)
	tree.Accept(checker)
	errors = append(errors, checker.errors...)
	checker.analyzeFlow()
	errors = append(errors, checker.errors...)
	return builder.symbolTable, errors
}
