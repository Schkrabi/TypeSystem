// Generated from ./Velka.g4 by ANTLR 4.13.1
package velka.parser.antlr;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link VelkaParser}.
 */
public interface VelkaListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link VelkaParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(VelkaParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(VelkaParser.ProgramContext ctx);
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
	 * Enter a parse tree produced by {@link VelkaParser#argument_list}.
	 * @param ctx the parse tree
	 */
	void enterArgument_list(VelkaParser.Argument_listContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#argument_list}.
	 * @param ctx the parse tree
	 */
	void exitArgument_list(VelkaParser.Argument_listContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#bind}.
	 * @param ctx the parse tree
	 */
	void enterBind(VelkaParser.BindContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#bind}.
	 * @param ctx the parse tree
	 */
	void exitBind(VelkaParser.BindContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#bind_list}.
	 * @param ctx the parse tree
	 */
	void enterBind_list(VelkaParser.Bind_listContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#bind_list}.
	 * @param ctx the parse tree
	 */
	void exitBind_list(VelkaParser.Bind_listContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#special_form}.
	 * @param ctx the parse tree
	 */
	void enterSpecial_form(VelkaParser.Special_formContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#special_form}.
	 * @param ctx the parse tree
	 */
	void exitSpecial_form(VelkaParser.Special_formContext ctx);
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
	 * Enter a parse tree produced by {@link VelkaParser#application}.
	 * @param ctx the parse tree
	 */
	void enterApplication(VelkaParser.ApplicationContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#application}.
	 * @param ctx the parse tree
	 */
	void exitApplication(VelkaParser.ApplicationContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(VelkaParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(VelkaParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#type_arrow}.
	 * @param ctx the parse tree
	 */
	void enterType_arrow(VelkaParser.Type_arrowContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#type_arrow}.
	 * @param ctx the parse tree
	 */
	void exitType_arrow(VelkaParser.Type_arrowContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#type_atom}.
	 * @param ctx the parse tree
	 */
	void enterType_atom(VelkaParser.Type_atomContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#type_atom}.
	 * @param ctx the parse tree
	 */
	void exitType_atom(VelkaParser.Type_atomContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#representation_atom}.
	 * @param ctx the parse tree
	 */
	void enterRepresentation_atom(VelkaParser.Representation_atomContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#representation_atom}.
	 * @param ctx the parse tree
	 */
	void exitRepresentation_atom(VelkaParser.Representation_atomContext ctx);
	/**
	 * Enter a parse tree produced by {@link VelkaParser#type_tuple}.
	 * @param ctx the parse tree
	 */
	void enterType_tuple(VelkaParser.Type_tupleContext ctx);
	/**
	 * Exit a parse tree produced by {@link VelkaParser#type_tuple}.
	 * @param ctx the parse tree
	 */
	void exitType_tuple(VelkaParser.Type_tupleContext ctx);
}