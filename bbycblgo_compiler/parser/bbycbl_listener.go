// Code generated from bbyCBL.g4 by ANTLR 4.13.1. DO NOT EDIT.

package parser // bbyCBL

import "github.com/antlr4-go/antlr/v4"

// bbyCBLListener is a complete listener for a parse tree produced by bbyCBLParser.
type bbyCBLListener interface {
	antlr.ParseTreeListener

	// EnterProgram is called when entering the program production.
	EnterProgram(c *ProgramContext)

	// EnterIdentificationDivision is called when entering the identificationDivision production.
	EnterIdentificationDivision(c *IdentificationDivisionContext)

	// EnterIdentificationClause is called when entering the identificationClause production.
	EnterIdentificationClause(c *IdentificationClauseContext)

	// EnterFreeFormText is called when entering the freeFormText production.
	EnterFreeFormText(c *FreeFormTextContext)

	// EnterDataDivision is called when entering the dataDivision production.
	EnterDataDivision(c *DataDivisionContext)

	// EnterDataEntry is called when entering the dataEntry production.
	EnterDataEntry(c *DataEntryContext)

	// EnterDataCopyStmt is called when entering the dataCopyStmt production.
	EnterDataCopyStmt(c *DataCopyStmtContext)

	// EnterLevelNumber is called when entering the levelNumber production.
	EnterLevelNumber(c *LevelNumberContext)

	// EnterPictureClause is called when entering the pictureClause production.
	EnterPictureClause(c *PictureClauseContext)

	// EnterPicturePattern is called when entering the picturePattern production.
	EnterPicturePattern(c *PicturePatternContext)

	// EnterPictureElement is called when entering the pictureElement production.
	EnterPictureElement(c *PictureElementContext)

	// EnterLikeClause is called when entering the likeClause production.
	EnterLikeClause(c *LikeClauseContext)

	// EnterOccursClause is called when entering the occursClause production.
	EnterOccursClause(c *OccursClauseContext)

	// EnterProcedureDivision is called when entering the procedureDivision production.
	EnterProcedureDivision(c *ProcedureDivisionContext)

	// EnterUsingClause is called when entering the usingClause production.
	EnterUsingClause(c *UsingClauseContext)

	// EnterParagraph is called when entering the paragraph production.
	EnterParagraph(c *ParagraphContext)

	// EnterSentence is called when entering the sentence production.
	EnterSentence(c *SentenceContext)

	// EnterStatement is called when entering the statement production.
	EnterStatement(c *StatementContext)

	// EnterQualified is called when entering the qualified production.
	EnterQualified(c *QualifiedContext)

	// EnterSingle is called when entering the single production.
	EnterSingle(c *SingleContext)

	// EnterSimpleId is called when entering the simpleId production.
	EnterSimpleId(c *SimpleIdContext)

	// EnterEvalSubject is called when entering the evalSubject production.
	EnterEvalSubject(c *EvalSubjectContext)

	// EnterAcceptStmt is called when entering the acceptStmt production.
	EnterAcceptStmt(c *AcceptStmtContext)

	// EnterAddToForm is called when entering the addToForm production.
	EnterAddToForm(c *AddToFormContext)

	// EnterAddGivingForm is called when entering the addGivingForm production.
	EnterAddGivingForm(c *AddGivingFormContext)

	// EnterAlterStmt is called when entering the alterStmt production.
	EnterAlterStmt(c *AlterStmtContext)

	// EnterCallStmt is called when entering the callStmt production.
	EnterCallStmt(c *CallStmtContext)

	// EnterCopySource is called when entering the copySource production.
	EnterCopySource(c *CopySourceContext)

	// EnterCopyStmt is called when entering the copyStmt production.
	EnterCopyStmt(c *CopyStmtContext)

	// EnterReplacePair is called when entering the replacePair production.
	EnterReplacePair(c *ReplacePairContext)

	// EnterReplaceBlock is called when entering the replaceBlock production.
	EnterReplaceBlock(c *ReplaceBlockContext)

	// EnterEqualDelim is called when entering the equalDelim production.
	EnterEqualDelim(c *EqualDelimContext)

	// EnterDisplayItem is called when entering the displayItem production.
	EnterDisplayItem(c *DisplayItemContext)

	// EnterDelimiterSpec is called when entering the delimiterSpec production.
	EnterDelimiterSpec(c *DelimiterSpecContext)

	// EnterValueSpec is called when entering the valueSpec production.
	EnterValueSpec(c *ValueSpecContext)

	// EnterDisplayStmt is called when entering the displayStmt production.
	EnterDisplayStmt(c *DisplayStmtContext)

	// EnterWithNoAdvancingClause is called when entering the withNoAdvancingClause production.
	EnterWithNoAdvancingClause(c *WithNoAdvancingClauseContext)

	// EnterDivideIntoForm is called when entering the divideIntoForm production.
	EnterDivideIntoForm(c *DivideIntoFormContext)

	// EnterDivideByForm is called when entering the divideByForm production.
	EnterDivideByForm(c *DivideByFormContext)

	// EnterEvaluateStmt is called when entering the evaluateStmt production.
	EnterEvaluateStmt(c *EvaluateStmtContext)

	// EnterGivingClause is called when entering the givingClause production.
	EnterGivingClause(c *GivingClauseContext)

	// EnterGotoStmt is called when entering the gotoStmt production.
	EnterGotoStmt(c *GotoStmtContext)

	// EnterIfStmt is called when entering the ifStmt production.
	EnterIfStmt(c *IfStmtContext)

	// EnterLoopStmt is called when entering the loopStmt production.
	EnterLoopStmt(c *LoopStmtContext)

	// EnterLoopContent is called when entering the loopContent production.
	EnterLoopContent(c *LoopContentContext)

	// EnterLoopControl is called when entering the loopControl production.
	EnterLoopControl(c *LoopControlContext)

	// EnterMoveStmt is called when entering the moveStmt production.
	EnterMoveStmt(c *MoveStmtContext)

	// EnterMultiplyStmt is called when entering the multiplyStmt production.
	EnterMultiplyStmt(c *MultiplyStmtContext)

	// EnterNextSentenceStmt is called when entering the nextSentenceStmt production.
	EnterNextSentenceStmt(c *NextSentenceStmtContext)

	// EnterPerformStmt is called when entering the performStmt production.
	EnterPerformStmt(c *PerformStmtContext)

	// EnterSignalDisable is called when entering the signalDisable production.
	EnterSignalDisable(c *SignalDisableContext)

	// EnterSignalEnable is called when entering the signalEnable production.
	EnterSignalEnable(c *SignalEnableContext)

	// EnterSignalExpr is called when entering the signalExpr production.
	EnterSignalExpr(c *SignalExprContext)

	// EnterStopStmt is called when entering the stopStmt production.
	EnterStopStmt(c *StopStmtContext)

	// EnterSubtractStmt is called when entering the subtractStmt production.
	EnterSubtractStmt(c *SubtractStmtContext)

	// EnterUntilClause is called when entering the untilClause production.
	EnterUntilClause(c *UntilClauseContext)

	// EnterVaryingClause is called when entering the varyingClause production.
	EnterVaryingClause(c *VaryingClauseContext)

	// EnterVaryingStmt is called when entering the varyingStmt production.
	EnterVaryingStmt(c *VaryingStmtContext)

	// EnterWhenOther is called when entering the whenOther production.
	EnterWhenOther(c *WhenOtherContext)

	// EnterWhenValues is called when entering the whenValues production.
	EnterWhenValues(c *WhenValuesContext)

	// EnterWhileClause is called when entering the whileClause production.
	EnterWhileClause(c *WhileClauseContext)

	// EnterExprList is called when entering the exprList production.
	EnterExprList(c *ExprListContext)

	// EnterCondition is called when entering the condition production.
	EnterCondition(c *ConditionContext)

	// EnterSimpleCond is called when entering the simpleCond production.
	EnterSimpleCond(c *SimpleCondContext)

	// EnterComparator is called when entering the comparator production.
	EnterComparator(c *ComparatorContext)

	// EnterIdentifierSegment is called when entering the identifierSegment production.
	EnterIdentifierSegment(c *IdentifierSegmentContext)

	// EnterQualifiedId is called when entering the qualifiedId production.
	EnterQualifiedId(c *QualifiedIdContext)

	// EnterQualifiedIdExpr is called when entering the QualifiedIdExpr production.
	EnterQualifiedIdExpr(c *QualifiedIdExprContext)

	// EnterUMinus is called when entering the UMinus production.
	EnterUMinus(c *UMinusContext)

	// EnterIdExpr is called when entering the IdExpr production.
	EnterIdExpr(c *IdExprContext)

	// EnterMulDiv is called when entering the MulDiv production.
	EnterMulDiv(c *MulDivContext)

	// EnterAddSub is called when entering the AddSub production.
	EnterAddSub(c *AddSubContext)

	// EnterParens is called when entering the Parens production.
	EnterParens(c *ParensContext)

	// EnterLitExpr is called when entering the LitExpr production.
	EnterLitExpr(c *LitExprContext)

	// EnterUPlus is called when entering the UPlus production.
	EnterUPlus(c *UPlusContext)

	// EnterExp is called when entering the Exp production.
	EnterExp(c *ExpContext)

	// EnterLiteral is called when entering the literal production.
	EnterLiteral(c *LiteralContext)

	// ExitProgram is called when exiting the program production.
	ExitProgram(c *ProgramContext)

	// ExitIdentificationDivision is called when exiting the identificationDivision production.
	ExitIdentificationDivision(c *IdentificationDivisionContext)

	// ExitIdentificationClause is called when exiting the identificationClause production.
	ExitIdentificationClause(c *IdentificationClauseContext)

	// ExitFreeFormText is called when exiting the freeFormText production.
	ExitFreeFormText(c *FreeFormTextContext)

	// ExitDataDivision is called when exiting the dataDivision production.
	ExitDataDivision(c *DataDivisionContext)

	// ExitDataEntry is called when exiting the dataEntry production.
	ExitDataEntry(c *DataEntryContext)

	// ExitDataCopyStmt is called when exiting the dataCopyStmt production.
	ExitDataCopyStmt(c *DataCopyStmtContext)

	// ExitLevelNumber is called when exiting the levelNumber production.
	ExitLevelNumber(c *LevelNumberContext)

	// ExitPictureClause is called when exiting the pictureClause production.
	ExitPictureClause(c *PictureClauseContext)

	// ExitPicturePattern is called when exiting the picturePattern production.
	ExitPicturePattern(c *PicturePatternContext)

	// ExitPictureElement is called when exiting the pictureElement production.
	ExitPictureElement(c *PictureElementContext)

	// ExitLikeClause is called when exiting the likeClause production.
	ExitLikeClause(c *LikeClauseContext)

	// ExitOccursClause is called when exiting the occursClause production.
	ExitOccursClause(c *OccursClauseContext)

	// ExitProcedureDivision is called when exiting the procedureDivision production.
	ExitProcedureDivision(c *ProcedureDivisionContext)

	// ExitUsingClause is called when exiting the usingClause production.
	ExitUsingClause(c *UsingClauseContext)

	// ExitParagraph is called when exiting the paragraph production.
	ExitParagraph(c *ParagraphContext)

	// ExitSentence is called when exiting the sentence production.
	ExitSentence(c *SentenceContext)

	// ExitStatement is called when exiting the statement production.
	ExitStatement(c *StatementContext)

	// ExitQualified is called when exiting the qualified production.
	ExitQualified(c *QualifiedContext)

	// ExitSingle is called when exiting the single production.
	ExitSingle(c *SingleContext)

	// ExitSimpleId is called when exiting the simpleId production.
	ExitSimpleId(c *SimpleIdContext)

	// ExitEvalSubject is called when exiting the evalSubject production.
	ExitEvalSubject(c *EvalSubjectContext)

	// ExitAcceptStmt is called when exiting the acceptStmt production.
	ExitAcceptStmt(c *AcceptStmtContext)

	// ExitAddToForm is called when exiting the addToForm production.
	ExitAddToForm(c *AddToFormContext)

	// ExitAddGivingForm is called when exiting the addGivingForm production.
	ExitAddGivingForm(c *AddGivingFormContext)

	// ExitAlterStmt is called when exiting the alterStmt production.
	ExitAlterStmt(c *AlterStmtContext)

	// ExitCallStmt is called when exiting the callStmt production.
	ExitCallStmt(c *CallStmtContext)

	// ExitCopySource is called when exiting the copySource production.
	ExitCopySource(c *CopySourceContext)

	// ExitCopyStmt is called when exiting the copyStmt production.
	ExitCopyStmt(c *CopyStmtContext)

	// ExitReplacePair is called when exiting the replacePair production.
	ExitReplacePair(c *ReplacePairContext)

	// ExitReplaceBlock is called when exiting the replaceBlock production.
	ExitReplaceBlock(c *ReplaceBlockContext)

	// ExitEqualDelim is called when exiting the equalDelim production.
	ExitEqualDelim(c *EqualDelimContext)

	// ExitDisplayItem is called when exiting the displayItem production.
	ExitDisplayItem(c *DisplayItemContext)

	// ExitDelimiterSpec is called when exiting the delimiterSpec production.
	ExitDelimiterSpec(c *DelimiterSpecContext)

	// ExitValueSpec is called when exiting the valueSpec production.
	ExitValueSpec(c *ValueSpecContext)

	// ExitDisplayStmt is called when exiting the displayStmt production.
	ExitDisplayStmt(c *DisplayStmtContext)

	// ExitWithNoAdvancingClause is called when exiting the withNoAdvancingClause production.
	ExitWithNoAdvancingClause(c *WithNoAdvancingClauseContext)

	// ExitDivideIntoForm is called when exiting the divideIntoForm production.
	ExitDivideIntoForm(c *DivideIntoFormContext)

	// ExitDivideByForm is called when exiting the divideByForm production.
	ExitDivideByForm(c *DivideByFormContext)

	// ExitEvaluateStmt is called when exiting the evaluateStmt production.
	ExitEvaluateStmt(c *EvaluateStmtContext)

	// ExitGivingClause is called when exiting the givingClause production.
	ExitGivingClause(c *GivingClauseContext)

	// ExitGotoStmt is called when exiting the gotoStmt production.
	ExitGotoStmt(c *GotoStmtContext)

	// ExitIfStmt is called when exiting the ifStmt production.
	ExitIfStmt(c *IfStmtContext)

	// ExitLoopStmt is called when exiting the loopStmt production.
	ExitLoopStmt(c *LoopStmtContext)

	// ExitLoopContent is called when exiting the loopContent production.
	ExitLoopContent(c *LoopContentContext)

	// ExitLoopControl is called when exiting the loopControl production.
	ExitLoopControl(c *LoopControlContext)

	// ExitMoveStmt is called when exiting the moveStmt production.
	ExitMoveStmt(c *MoveStmtContext)

	// ExitMultiplyStmt is called when exiting the multiplyStmt production.
	ExitMultiplyStmt(c *MultiplyStmtContext)

	// ExitNextSentenceStmt is called when exiting the nextSentenceStmt production.
	ExitNextSentenceStmt(c *NextSentenceStmtContext)

	// ExitPerformStmt is called when exiting the performStmt production.
	ExitPerformStmt(c *PerformStmtContext)

	// ExitSignalDisable is called when exiting the signalDisable production.
	ExitSignalDisable(c *SignalDisableContext)

	// ExitSignalEnable is called when exiting the signalEnable production.
	ExitSignalEnable(c *SignalEnableContext)

	// ExitSignalExpr is called when exiting the signalExpr production.
	ExitSignalExpr(c *SignalExprContext)

	// ExitStopStmt is called when exiting the stopStmt production.
	ExitStopStmt(c *StopStmtContext)

	// ExitSubtractStmt is called when exiting the subtractStmt production.
	ExitSubtractStmt(c *SubtractStmtContext)

	// ExitUntilClause is called when exiting the untilClause production.
	ExitUntilClause(c *UntilClauseContext)

	// ExitVaryingClause is called when exiting the varyingClause production.
	ExitVaryingClause(c *VaryingClauseContext)

	// ExitVaryingStmt is called when exiting the varyingStmt production.
	ExitVaryingStmt(c *VaryingStmtContext)

	// ExitWhenOther is called when exiting the whenOther production.
	ExitWhenOther(c *WhenOtherContext)

	// ExitWhenValues is called when exiting the whenValues production.
	ExitWhenValues(c *WhenValuesContext)

	// ExitWhileClause is called when exiting the whileClause production.
	ExitWhileClause(c *WhileClauseContext)

	// ExitExprList is called when exiting the exprList production.
	ExitExprList(c *ExprListContext)

	// ExitCondition is called when exiting the condition production.
	ExitCondition(c *ConditionContext)

	// ExitSimpleCond is called when exiting the simpleCond production.
	ExitSimpleCond(c *SimpleCondContext)

	// ExitComparator is called when exiting the comparator production.
	ExitComparator(c *ComparatorContext)

	// ExitIdentifierSegment is called when exiting the identifierSegment production.
	ExitIdentifierSegment(c *IdentifierSegmentContext)

	// ExitQualifiedId is called when exiting the qualifiedId production.
	ExitQualifiedId(c *QualifiedIdContext)

	// ExitQualifiedIdExpr is called when exiting the QualifiedIdExpr production.
	ExitQualifiedIdExpr(c *QualifiedIdExprContext)

	// ExitUMinus is called when exiting the UMinus production.
	ExitUMinus(c *UMinusContext)

	// ExitIdExpr is called when exiting the IdExpr production.
	ExitIdExpr(c *IdExprContext)

	// ExitMulDiv is called when exiting the MulDiv production.
	ExitMulDiv(c *MulDivContext)

	// ExitAddSub is called when exiting the AddSub production.
	ExitAddSub(c *AddSubContext)

	// ExitParens is called when exiting the Parens production.
	ExitParens(c *ParensContext)

	// ExitLitExpr is called when exiting the LitExpr production.
	ExitLitExpr(c *LitExprContext)

	// ExitUPlus is called when exiting the UPlus production.
	ExitUPlus(c *UPlusContext)

	// ExitExp is called when exiting the Exp production.
	ExitExp(c *ExpContext)

	// ExitLiteral is called when exiting the literal production.
	ExitLiteral(c *LiteralContext)
}
