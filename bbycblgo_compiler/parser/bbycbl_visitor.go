// Code generated from bbyCBL.g4 by ANTLR 4.13.1. DO NOT EDIT.

package parser // bbyCBL

import "github.com/antlr4-go/antlr/v4"

// A complete Visitor for a parse tree produced by bbyCBLParser.
type bbyCBLVisitor interface {
	antlr.ParseTreeVisitor

	// Visit a parse tree produced by bbyCBLParser#program.
	VisitProgram(ctx *ProgramContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#identificationDivision.
	VisitIdentificationDivision(ctx *IdentificationDivisionContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#identificationClause.
	VisitIdentificationClause(ctx *IdentificationClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#freeFormText.
	VisitFreeFormText(ctx *FreeFormTextContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#dataDivision.
	VisitDataDivision(ctx *DataDivisionContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#dataEntry.
	VisitDataEntry(ctx *DataEntryContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#dataCopyStmt.
	VisitDataCopyStmt(ctx *DataCopyStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#levelNumber.
	VisitLevelNumber(ctx *LevelNumberContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#pictureClause.
	VisitPictureClause(ctx *PictureClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#picturePattern.
	VisitPicturePattern(ctx *PicturePatternContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#pictureElement.
	VisitPictureElement(ctx *PictureElementContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#likeClause.
	VisitLikeClause(ctx *LikeClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#occursClause.
	VisitOccursClause(ctx *OccursClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#procedureDivision.
	VisitProcedureDivision(ctx *ProcedureDivisionContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#usingClause.
	VisitUsingClause(ctx *UsingClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#paragraph.
	VisitParagraph(ctx *ParagraphContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#statement.
	VisitStatement(ctx *StatementContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#qualified.
	VisitQualified(ctx *QualifiedContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#single.
	VisitSingle(ctx *SingleContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#simpleId.
	VisitSimpleId(ctx *SimpleIdContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#evalSubject.
	VisitEvalSubject(ctx *EvalSubjectContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#acceptStmt.
	VisitAcceptStmt(ctx *AcceptStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#addToForm.
	VisitAddToForm(ctx *AddToFormContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#addGivingForm.
	VisitAddGivingForm(ctx *AddGivingFormContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#alterStmt.
	VisitAlterStmt(ctx *AlterStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#callStmt.
	VisitCallStmt(ctx *CallStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#copySource.
	VisitCopySource(ctx *CopySourceContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#copyStmt.
	VisitCopyStmt(ctx *CopyStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#replacePair.
	VisitReplacePair(ctx *ReplacePairContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#replaceBlock.
	VisitReplaceBlock(ctx *ReplaceBlockContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#equalDelim.
	VisitEqualDelim(ctx *EqualDelimContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#displayItem.
	VisitDisplayItem(ctx *DisplayItemContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#delimiterSpec.
	VisitDelimiterSpec(ctx *DelimiterSpecContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#valueSpec.
	VisitValueSpec(ctx *ValueSpecContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#displayStmt.
	VisitDisplayStmt(ctx *DisplayStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#withNoAdvancingClause.
	VisitWithNoAdvancingClause(ctx *WithNoAdvancingClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#divideIntoForm.
	VisitDivideIntoForm(ctx *DivideIntoFormContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#divideByForm.
	VisitDivideByForm(ctx *DivideByFormContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#evaluateStmt.
	VisitEvaluateStmt(ctx *EvaluateStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#givingClause.
	VisitGivingClause(ctx *GivingClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#gotoStmt.
	VisitGotoStmt(ctx *GotoStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#ifStmt.
	VisitIfStmt(ctx *IfStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#sentence.
	VisitSentence(ctx *SentenceContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#loopStmt.
	VisitLoopStmt(ctx *LoopStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#loopContent.
	VisitLoopContent(ctx *LoopContentContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#loopControl.
	VisitLoopControl(ctx *LoopControlContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#moveStmt.
	VisitMoveStmt(ctx *MoveStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#multiplyStmt.
	VisitMultiplyStmt(ctx *MultiplyStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#nextSentenceStmt.
	VisitNextSentenceStmt(ctx *NextSentenceStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#performStmt.
	VisitPerformStmt(ctx *PerformStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#signalDisable.
	VisitSignalDisable(ctx *SignalDisableContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#signalEnable.
	VisitSignalEnable(ctx *SignalEnableContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#signalExpr.
	VisitSignalExpr(ctx *SignalExprContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#stopStmt.
	VisitStopStmt(ctx *StopStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#subtractStmt.
	VisitSubtractStmt(ctx *SubtractStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#untilClause.
	VisitUntilClause(ctx *UntilClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#varyingClause.
	VisitVaryingClause(ctx *VaryingClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#varyingStmt.
	VisitVaryingStmt(ctx *VaryingStmtContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#whenOther.
	VisitWhenOther(ctx *WhenOtherContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#whenValues.
	VisitWhenValues(ctx *WhenValuesContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#whileClause.
	VisitWhileClause(ctx *WhileClauseContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#exprList.
	VisitExprList(ctx *ExprListContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#condition.
	VisitCondition(ctx *ConditionContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#simpleCond.
	VisitSimpleCond(ctx *SimpleCondContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#comparator.
	VisitComparator(ctx *ComparatorContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#identifierSegment.
	VisitIdentifierSegment(ctx *IdentifierSegmentContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#qualifiedId.
	VisitQualifiedId(ctx *QualifiedIdContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#QualifiedIdExpr.
	VisitQualifiedIdExpr(ctx *QualifiedIdExprContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#UMinus.
	VisitUMinus(ctx *UMinusContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#IdExpr.
	VisitIdExpr(ctx *IdExprContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#MulDiv.
	VisitMulDiv(ctx *MulDivContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#AddSub.
	VisitAddSub(ctx *AddSubContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#Parens.
	VisitParens(ctx *ParensContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#LitExpr.
	VisitLitExpr(ctx *LitExprContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#UPlus.
	VisitUPlus(ctx *UPlusContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#Exp.
	VisitExp(ctx *ExpContext) interface{}

	// Visit a parse tree produced by bbyCBLParser#literal.
	VisitLiteral(ctx *LiteralContext) interface{}
}
