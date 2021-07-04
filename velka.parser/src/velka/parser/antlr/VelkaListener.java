// Generated from Scheme.g by ANTLR 4.7.2
package velka.parser.antlr;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link VelkaParser}.
 */
public interface VelkaListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link VelkaParser#exprs}.
	 * @param ctx the parse tree
	 */
	void enterExprs(VelkaParser.ExprsContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#exprs}.
	 * @param ctx the parse tree
	 */
	void exitExprs(VelkaParser.ExprsContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(VelkaParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(VelkaParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#seq}.
	 * @param ctx the parse tree
	 */
	void enterSeq(VelkaParser.SeqContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#seq}.
	 * @param ctx the parse tree
	 */
	void exitSeq(VelkaParser.SeqContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(VelkaParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(VelkaParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#pair}.
	 * @param ctx the parse tree
	 */
	void enterPair(VelkaParser.PairContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#pair}.
	 * @param ctx the parse tree
	 */
	void exitPair(VelkaParser.PairContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#arrow}.
	 * @param ctx the parse tree
	 */
	void enterArrow(VelkaParser.ArrowContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#arrow}.
	 * @param ctx the parse tree
	 */
	void exitArrow(VelkaParser.ArrowContext ctx);
}