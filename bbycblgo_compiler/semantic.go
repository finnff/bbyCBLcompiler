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
	fields map[string]*FieldSymbol
	parent *Scope
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
	fmt.Printf("Adding error: %s at line %d\n", msg, line)
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

func (v *BaseAnalyzer) VisitChildren(node antlr.RuleNode) interface{} {
	for _, child := range node.GetChildren() {
		child.(antlr.ParseTree).Accept(v)
	}
	return nil
}

// --- Pass 1: Symbol Table Builder ---

type SymbolTableBuilder struct {
	*BaseAnalyzer
	symbolTable *SymbolTable
	parentStack []*FieldSymbol
}

func NewSymbolTableBuilder() *SymbolTableBuilder {
	return &SymbolTableBuilder{
		BaseAnalyzer: &BaseAnalyzer{
			BasebbyCBLVisitor: &parser.BasebbyCBLVisitor{},
			errors:            []SemanticError{},
			fixedFormat:       true,
		},
		symbolTable: &SymbolTable{
			rootScope:         &Scope{fields: make(map[string]*FieldSymbol)},
			paragraphs:        make(map[string]*ParagraphSymbol),
			orderedParagraphs: []*ParagraphSymbol{},
		},
		parentStack: []*FieldSymbol{},
	}
}

func (v *SymbolTableBuilder) VisitIdentificationDivision(ctx *parser.IdentificationDivisionContext) interface{} {
	v.checkAreaA(ctx.GetStart(), "IDENTIFICATION DIVISION")
	return v.VisitChildren(ctx)
}

func (v *SymbolTableBuilder) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	v.checkAreaA(ctx.GetStart(), "DATA DIVISION")
	v.parentStack = []*FieldSymbol{}
	v.VisitChildren(ctx)
	return nil
}

func (v *SymbolTableBuilder) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	v.checkAreaA(ctx.GetStart(), "PROCEDURE DIVISION")
	return v.VisitChildren(ctx)
}

func (v *SymbolTableBuilder) VisitDataEntry(ctx *parser.DataEntryContext) interface{} {
	levelStr := ctx.LevelNumber().GetText()
	level, err := strconv.Atoi(levelStr)
	if err != nil {
		v.addError(fmt.Sprintf("Level number '%s' is not a valid integer", levelStr), ctx.GetStart().GetLine())
		return nil
	}

	if level == 1 || level == 77 {
		v.checkAreaA(ctx.LevelNumber().GetStart(), fmt.Sprintf("Level number %s", levelStr))
	} else if level >= 2 && level <= 49 {
		v.checkAreaB(ctx.LevelNumber().GetStart(), fmt.Sprintf("Level number %s", levelStr))
	}

	if !((level >= 1 && level <= 49) || level == 77) {
		v.addError(fmt.Sprintf("Invalid level number: must be 01-49 or 77, got %d", level), ctx.GetStart().GetLine())
		return nil
	}

	var name string
	if id := ctx.Identifier(); id != nil {
		name = id.GetText()
	} else {
		v.addError("Data entry without an identifier", ctx.GetStart().GetLine())
		return nil
	}

	if name != "" {
		if _, exists := v.symbolTable.rootScope.fields[name]; exists {
			v.addError(fmt.Sprintf("Duplicate field name '%s'", name), ctx.GetStart().GetLine())
		}
	}

	field := &FieldSymbol{name: name, level: level}

	for len(v.parentStack) > 0 && v.parentStack[len(v.parentStack)-1].level >= level {
		v.parentStack = v.parentStack[:len(v.parentStack)-1]
	}

	if len(v.parentStack) > 0 {
		parent := v.parentStack[len(v.parentStack)-1]
		if field.level <= parent.level {
			v.addError(fmt.Sprintf("Invalid level number %d for field '%s'. Must be greater than parent's level %d", field.level, field.name, parent.level), ctx.GetStart().GetLine())
		}
		field.parent = parent
		parent.children = append(parent.children, field)
	}
	v.parentStack = append(v.parentStack, field)

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

	if name != "" {
		v.symbolTable.rootScope.fields[name] = field
	}

	return nil
}

func (v *SymbolTableBuilder) analyzePicture(pic string, line int) *PictureType {
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
		case '9':
			hasNumeric = true
			length += repetition
		case 'A':
			hasAlpha = true
			length += repetition
		case 'X':
			hasAlphaNum = true
			length += repetition
		case 'S':
			sCount += 1
			hasNumeric = true
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
	isMixed := false
	for _, c := range pic {
		if (c == '9' || c == 'V' || c == 'Z' || c == 'P') && (hasAlpha || hasAlphaNum) {
			isMixed = true
			break
		}
	}
	if isMixed {
		v.addError("PICTURE clause cannot mix alphanumeric/alphabetic with numeric-only characters like '9'", line)
	}
	if hasAlpha && hasAlpha {
		v.addError("PICTURE clause cannot be both alphabetic ('A') and alphanumeric ('X')", line)
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
				if field, exists := v.symbolTable.rootScope.fields[paramName]; exists {
					paraSymbol.params = append(paraSymbol.params, field)
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
			fixedFormat:       true,
		},
		symbolTable:  symbolTable,
		cfg:          make(map[string][]string),
		hasStopRun:   false,
		analyzedFlow: false,
	}
}

func (v *SemanticChecker) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	name := ctx.Identifier().GetText()
	if p, exists := v.symbolTable.paragraphs[name]; exists {
		v.currentPara = p
	}
	return v.VisitChildren(ctx)
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
	field, exists := v.symbolTable.rootScope.fields[name]
	if !exists || field.likeRef == "" {
		return nil
	}
	likeTarget, targetExists := v.symbolTable.rootScope.fields[field.likeRef]
	if !targetExists {
		v.addError(fmt.Sprintf("Field '%s' in LIKE clause for '%s' not found", field.likeRef, name), ctx.GetStart().GetLine())
		return nil
	}
	if likeTarget == field {
		v.addError(fmt.Sprintf("Recursive LIKE clause for field '%s'", name), ctx.GetStart().GetLine())
		return nil
	}
	field.picture = likeTarget.picture
	field.children = deepCopyChildren(likeTarget.children, field)
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
		callTargetName = segs[0]
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

func getIdentifierSegments(expr parser.IExprContext) []string {
	switch e := expr.(type) {
	case *parser.IdExprContext:
		return []string{e.GetText()}
	case *parser.QualifiedIdExprContext:
		segs := []string{}
		for _, seg := range e.QualifiedId().AllIdentifierSegment() {
			segs = append(segs, seg.GetText())
		}
		return segs
	default:
		return nil
	}
}

func (v *SemanticChecker) getFieldFromExpr(expr parser.IExprContext, line int) *FieldSymbol {
	segs := getIdentifierSegments(expr)
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
	for _, field := range v.symbolTable.rootScope.fields {
		if field.name == targetName {
			candidates = append(candidates, field)
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

func Analyze(tree antlr.ParseTree) []SemanticError {
	if tree == nil {
		return []SemanticError{}
	}
	builder := NewSymbolTableBuilder()
	tree.Accept(builder)
	errors := builder.errors
	checker := NewSemanticChecker(builder.symbolTable)
	tree.Accept(checker)
	errors = append(errors, checker.errors...)
	checker.analyzeFlow()
	errors = append(errors, checker.errors...)
	return errors
}
