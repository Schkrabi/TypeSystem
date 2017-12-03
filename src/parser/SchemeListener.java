// Generated from Scheme.g by ANTLR 4.7
package parser;
import expression.*;
import types.TypeConcrete;
import types.TypeRepresentation;
import java.util.Map;
import java.util.HashMap;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SchemeParser}.
 */
public interface SchemeListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SchemeParser#exprs}.
	 * @param ctx the parse tree
	 */
	void enterExprs(SchemeParser.ExprsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#exprs}.
	 * @param ctx the parse tree
	 */
	void exitExprs(SchemeParser.ExprsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(SchemeParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(SchemeParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#seq}.
	 * @param ctx the parse tree
	 */
	void enterSeq(SchemeParser.SeqContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#seq}.
	 * @param ctx the parse tree
	 */
	void exitSeq(SchemeParser.SeqContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#typed}.
	 * @param ctx the parse tree
	 */
	void enterTyped(SchemeParser.TypedContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#typed}.
	 * @param ctx the parse tree
	 */
	void exitTyped(SchemeParser.TypedContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(SchemeParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(SchemeParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(SchemeParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(SchemeParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link SchemeParser#quote}.
	 * @param ctx the parse tree
	 */
	void enterQuote(SchemeParser.QuoteContext ctx);
	/**
	 * Exit a parse tree produced by {@link SchemeParser#quote}.
	 * @param ctx the parse tree
	 */
	void exitQuote(SchemeParser.QuoteContext ctx);
}