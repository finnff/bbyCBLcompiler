package main

import (
	"fmt"
	"strings"

	"bbycblgo_compiler/parser"
	"github.com/antlr4-go/antlr/v4"
)

type ASTPrinter struct {
	*parser.BasebbyCBLVisitor
	Indentation int
}

const (
	colorReset   = "\x1b[0m"
	colorCyan    = "\x1b[36m"
	colorMagenta = "\x1b[35m"
	colorYellow  = "\x1b[33m"
	colorBlue    = "\x1b[34m"
)

func (v *ASTPrinter) indent() string {
	return strings.Repeat("  ", v.Indentation)
}

func (v *ASTPrinter) Visit(tree antlr.ParseTree) interface{} {
	return tree.Accept(v)
}

func (v *ASTPrinter) VisitChildren(node antlr.RuleNode) interface{} {
	v.Indentation++
	for _, child := range node.GetChildren() {
		if child == nil {
			continue // Skip nil children
		}
		if parseTreeChild, ok := child.(antlr.ParseTree); ok && parseTreeChild != nil {
			parseTreeChild.Accept(v)
		}
	}
	v.Indentation--
	return nil
}

func (v *ASTPrinter) VisitTerminal(node antlr.TerminalNode) interface{} {
	// Do not print terminals to reduce verbosity
	return nil
}

func (v *ASTPrinter) VisitErrorNode(node antlr.ErrorNode) interface{} {
	fmt.Printf("%sError: %s\n", v.indent(), node.GetText())
	return nil
}

func (v *ASTPrinter) VisitProgram(ctx *parser.ProgramContext) interface{} {
	fmt.Printf("%s%sProgram%s\n", v.indent(), colorCyan, colorReset)
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitIdentificationDivision(ctx *parser.IdentificationDivisionContext) interface{} {
	fmt.Printf("%s%sIdentificationDivision%s\n", v.indent(), colorBlue, colorReset)
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitIdentificationClause(ctx *parser.IdentificationClauseContext) interface{} {
	fmt.Printf("%sIdentificationClause\n", v.indent())
	// Manually visit children to avoid crash
	if ctx.PROGRAM_ID() != nil {
		fmt.Printf("%s  PROGRAM-ID: %s\n", v.indent(), ctx.Identifier().GetText())
	}
	return nil
}

func (v *ASTPrinter) VisitDataDivision(ctx *parser.DataDivisionContext) interface{} {
	fmt.Printf("%s%sDataDivision%s\n", v.indent(), colorMagenta, colorReset)
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDataEntry(ctx *parser.DataEntryContext) interface{} {
	fmt.Printf("%sDataEntry: %s\n", v.indent(), ctx.Identifier().GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLevelNumber(ctx *parser.LevelNumberContext) interface{} {
	fmt.Printf("%sLevelNumber: %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitPictureClause(ctx *parser.PictureClauseContext) interface{} {
	fmt.Printf("%sPictureClause: %s\n", v.indent(), ctx.PicturePattern().GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitPicturePattern(ctx *parser.PicturePatternContext) interface{} {
	fmt.Printf("%sPicturePattern: %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitProcedureDivision(ctx *parser.ProcedureDivisionContext) interface{} {
	fmt.Printf("%s%sProcedureDivision%s\n", v.indent(), colorYellow, colorReset)
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitParagraph(ctx *parser.ParagraphContext) interface{} {
	fmt.Printf("%sParagraph: %s\n", v.indent(), ctx.Identifier().GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSentence(ctx *parser.SentenceContext) interface{} {
	fmt.Printf("%sSentence\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitStatement(ctx *parser.StatementContext) interface{} {
	fmt.Printf("%sStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDisplayStmt(ctx *parser.DisplayStmtContext) interface{} {
	fmt.Printf("%sDisplayStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDisplayItem(ctx *parser.DisplayItemContext) interface{} {
	fmt.Printf("%sDisplayItem\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitIfStmt(ctx *parser.IfStmtContext) interface{} {
	fmt.Printf("%sIfStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitCondition(ctx *parser.ConditionContext) interface{} {
	fmt.Printf("%sCondition\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSimpleCond(ctx *parser.SimpleCondContext) interface{} {
	fmt.Printf("%sSimpleCondition\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitComparator(ctx *parser.ComparatorContext) interface{} {
	fmt.Printf("%sComparator: %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitStopStmt(ctx *parser.StopStmtContext) interface{} {
	fmt.Printf("%sStopStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSubtractStmt(ctx *parser.SubtractStmtContext) interface{} {
	fmt.Printf("%sSubtractStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitAddToForm(ctx *parser.AddToFormContext) interface{} {
	fmt.Printf("%sAddStatement (TO)\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitAddGivingForm(ctx *parser.AddGivingFormContext) interface{} {
	fmt.Printf("%sAddStatement (GIVING)\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitMultiplyStmt(ctx *parser.MultiplyStmtContext) interface{} {

	fmt.Printf("%sMultiplyStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDivideIntoForm(ctx *parser.DivideIntoFormContext) interface{} {
	fmt.Printf("%sDivideStatement (INTO)\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDivideByForm(ctx *parser.DivideByFormContext) interface{} {
	fmt.Printf("%sDivideStatement (BY)\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLoopStmt(ctx *parser.LoopStmtContext) interface{} {
	fmt.Printf("%sLoopStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLoopContent(ctx *parser.LoopContentContext) interface{} {
	fmt.Printf("%sLoopContent\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitVaryingClause(ctx *parser.VaryingClauseContext) interface{} {
	fmt.Printf("%sVaryingClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitWhileClause(ctx *parser.WhileClauseContext) interface{} {
	fmt.Printf("%sWhileClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitUntilClause(ctx *parser.UntilClauseContext) interface{} {
	fmt.Printf("%sUntilClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLoopControl(ctx *parser.LoopControlContext) interface{} {
	fmt.Printf("%sLoopControl\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitGivingClause(ctx *parser.GivingClauseContext) interface{} {
	fmt.Printf("%sGivingClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitWithNoAdvancingClause(ctx *parser.WithNoAdvancingClauseContext) interface{} {
	fmt.Printf("%sWithNoAdvancingClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLikeClause(ctx *parser.LikeClauseContext) interface{} {
	fmt.Printf("%sLikeClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitOccursClause(ctx *parser.OccursClauseContext) interface{} {
	fmt.Printf("%sOccursClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitNextSentenceStmt(ctx *parser.NextSentenceStmtContext) interface{} {
	fmt.Printf("%sNextSentenceStmt\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitEvaluateStmt(ctx *parser.EvaluateStmtContext) interface{} {
	fmt.Printf("%sEvaluateStmt\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitQualifiedIdExpr(ctx *parser.QualifiedIdExprContext) interface{} {
	fmt.Printf("%sQualifiedIdExpr: %s\n", v.indent(), ctx.GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitIdentifierSegment(ctx *parser.IdentifierSegmentContext) interface{} {
	fmt.Printf("%sIdentifierSegment: %s\n", v.indent(), ctx.GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitUsingClause(ctx *parser.UsingClauseContext) interface{} {
	fmt.Printf("%sUsingClause\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitDataCopyStmt(ctx *parser.DataCopyStmtContext) interface{} {
	fmt.Printf("%sDataCopyStmt\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitEvalSubject(ctx *parser.EvalSubjectContext) interface{} {
	fmt.Printf("%sEvalSubject\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitWhenValues(ctx *parser.WhenValuesContext) interface{} {
	fmt.Printf("%sWhenValues\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitWhenOther(ctx *parser.WhenOtherContext) interface{} {
	fmt.Printf("%sWhenOther\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitQualifiedId(ctx *parser.QualifiedIdContext) interface{} {
	fmt.Printf("%sQualifiedId: %s\n", v.indent(), ctx.GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSimpleId(ctx *parser.SimpleIdContext) interface{} {
	fmt.Printf("%sSimpleId: %s\n", v.indent(), ctx.GetText())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitAddSub(ctx *parser.AddSubContext) interface{} {
	fmt.Printf("%sAddSub\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitUMinus(ctx *parser.UMinusContext) interface{} {
	fmt.Printf("%sUMinus\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitCopyStmt(ctx *parser.CopyStmtContext) interface{} {
	fmt.Printf("%sCopyStmt\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitMulDiv(ctx *parser.MulDivContext) interface{} {
	fmt.Printf("%sMulDiv\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSignalEnable(ctx *parser.SignalEnableContext) interface{} {
	fmt.Printf("%sSignalEnable\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitCopySource(ctx *parser.CopySourceContext) interface{} {
	fmt.Printf("%sCopySource\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitValueSpec(ctx *parser.ValueSpecContext) interface{} {
	fmt.Printf("%sValueSpec\n", v.indent())
	v.Indentation++
	if ctx.Expr() != nil {
		ctx.Expr().Accept(v)
	}
	if ctx.SIZE() != nil {
		fmt.Printf("%s  SIZE: %s\n", v.indent(), ctx.SIZE().GetText())
	}
	if ctx.SPACE() != nil {
		fmt.Printf("%s  SPACE: %s\n", v.indent(), ctx.SPACE().GetText())
	}
	v.Indentation--
	return nil
}

func (v *ASTPrinter) VisitDelimiterSpec(ctx *parser.DelimiterSpecContext) interface{} {
	fmt.Printf("%sDelimiterSpec\n", v.indent())
	v.Indentation++
	if ctx.DELIMITED_BY() != nil {
		fmt.Printf("%s  DELIMITED BY: %s\n", v.indent(), ctx.DELIMITED_BY().GetText())
		if ctx.ValueSpec() != nil {
			ctx.ValueSpec().Accept(v)
		}
	} else if ctx.DELIMITED() != nil && ctx.BY() != nil {
		fmt.Printf("%s  DELIMITED %s BY %s\n", v.indent(), ctx.DELIMITED().GetText(), ctx.BY().GetText())
		if ctx.ValueSpec() != nil {
			ctx.ValueSpec().Accept(v)
		}
	}
	v.Indentation--
	return nil
}

func (v *ASTPrinter) VisitExp(ctx *parser.ExpContext) interface{} {
	fmt.Printf("%sExp\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitSignalDisable(ctx *parser.SignalDisableContext) interface{} {
	fmt.Printf("%sSignalDisable\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitPerformStmt(ctx *parser.PerformStmtContext) interface{} {
	fmt.Printf("%sPerformStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitAlterStmt(ctx *parser.AlterStmtContext) interface{} {
	fmt.Printf("%sAlterStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitCallStmt(ctx *parser.CallStmtContext) interface{} {
	fmt.Printf("%sCallStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitAcceptStmt(ctx *parser.AcceptStmtContext) interface{} {
	fmt.Printf("%sAcceptStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitGotoStmt(ctx *parser.GotoStmtContext) interface{} {
	fmt.Printf("%sGotoStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitEqualDelim(ctx *parser.EqualDelimContext) interface{} {
	fmt.Printf("%sEqualDelim\n", v.indent())
	v.Indentation++
	if ctx.TRIPLE_EQUAL() != nil {
		fmt.Printf("%s  %s\n", v.indent(), ctx.TRIPLE_EQUAL().GetText())
	} else if ctx.DOUBLE_EQUAL() != nil {
		fmt.Printf("%s  %s\n", v.indent(), ctx.DOUBLE_EQUAL().GetText())
	}
	v.Indentation--
	return nil
}

func (v *ASTPrinter) VisitMoveStmt(ctx *parser.MoveStmtContext) interface{} {
	fmt.Printf("%sMoveStatement\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitParens(ctx *parser.ParensContext) interface{} {
	fmt.Printf("%sParens\n", v.indent())
	v.Indentation++
	if ctx.Expr() != nil {
		ctx.Expr().Accept(v)
	}
	v.Indentation--
	return nil
}

func (v *ASTPrinter) VisitReplaceBlock(ctx *parser.ReplaceBlockContext) interface{} {
	fmt.Printf("%sReplaceBlock\n", v.indent())
	v.Indentation++
	for _, delim := range ctx.AllEqualDelim() {
		if delim != nil {
			delim.Accept(v)
		}
	}
	v.Indentation--
	return nil

}

func (v *ASTPrinter) VisitReplacePair(ctx *parser.ReplacePairContext) interface{} {
	fmt.Printf("%sReplacePair\n", v.indent())
	v.Indentation++
	for _, block := range ctx.AllReplaceBlock() {
		if block != nil {
			block.Accept(v)
		}
	}
	if ctx.BY() != nil {
		fmt.Printf("%s  BY: %s\n", v.indent(), ctx.BY().GetText())
	}
	v.Indentation--
	return nil

}

func (v *ASTPrinter) VisitExprList(ctx *parser.ExprListContext) interface{} {
	fmt.Printf("%sExprList\n", v.indent())
	return v.VisitChildren(ctx)
}

func (v *ASTPrinter) VisitLitExpr(ctx *parser.LitExprContext) interface{} {
	fmt.Printf("%sLiteral: %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitIdExpr(ctx *parser.IdExprContext) interface{} {
	fmt.Printf("%sIdentifier: %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitQualified(ctx *parser.QualifiedContext) interface{} {
	fmt.Printf("%sIdentifier (Qualified): %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitSingle(ctx *parser.SingleContext) interface{} {
	fmt.Printf("%sIdentifier (Single): %s\n", v.indent(), ctx.GetText())
	return nil
}

func (v *ASTPrinter) VisitLiteral(ctx *parser.LiteralContext) interface{} {
	fmt.Printf("%sLiteral: %s\n", v.indent(), ctx.GetText())
	return nil
}
