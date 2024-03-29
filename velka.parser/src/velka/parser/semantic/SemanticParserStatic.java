package velka.parser.semantic;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.application.AndExpression;
import velka.core.application.CanDeconstructAs;
import velka.core.application.Construct;
import velka.core.application.Convert;
import velka.core.application.Deconstruct;
import velka.core.application.DefineConstructor;
import velka.core.application.DefineConversion;
import velka.core.application.DefineRepresentation;
import velka.core.application.DefineSymbol;
import velka.core.application.DefineType;
import velka.core.application.ExceptionExpr;
import velka.core.application.Extend;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.Loop;
import velka.core.application.OrExpression;
import velka.core.application.Recur;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.parser.antlr.SemanticNode;
import velka.parser.exceptions.UnexpectedExpressionException;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ThrowingFunction;

/**
 * This class contains auxiliary static methods and constants for SemanticParser
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class SemanticParserStatic {
	public static final String AND = AndExpression.AND;
	public static final String CAN_DECONSTRUCT_AS = CanDeconstructAs.CAN_DECONSTRUCT_AS;
	public static final String CONS = "cons";
	public static final String CONSTRUCT = Construct.CONSTRUCT;
	public static final String CONVERT = Convert.CONVERT;
	public static final String CONSTRUCTOR = DefineConstructor.CONSTRUCTOR;
	public static final String CONVERSION = DefineConversion.CONVERSION;
	public static final String DECONSTRUCT = Deconstruct.DECONSTRUCT;
	public static final String DEFINE = DefineSymbol.DEFINE;
	public static final String EAPPLY = AbstractionApplication.EAPPLY;
	public static final String ERROR = ExceptionExpr.ERROR;
	public static final String EXTEND = Extend.EXTEND;
	public static final String EXTENDED_LAMBDA = ExtendedLambda.EXTENDED_LAMBDA;
	public static final String GET = Get.GET;
	public static final String IF = IfExpression.IF;
	public static final String INSTANCE_OF = InstanceOf.INSTANCE_OF;
	public static final String INSTANCE_OF_REPRESENTATION = InstanceOfRepresentation.INSTANCE_OF_REPRESENTATION;
	public static final String LAMBDA = Lambda.LAMBDA;
	public static final String LET = "let";
	public static final String LET_AST = "let*";
	public static final String LET_TYPE = "let-type";
	public static final String LOOP = Loop.LOOP;
	public static final String OR = OrExpression.OR;
	public static final String RECUR = Recur.RECUR;
	public static final String REPRESENTATION = DefineRepresentation.REPRESENTATION;
	public static final String TUPLE = "tuple";
	public static final String TYPE = DefineType.TYPE;
	
	/**
	 * Unused special form. For testing purposes only!
	 */
	public static final String UNUSED = "unused";

	public static final Set<String> specialForms;

	static {
		specialForms = new TreeSet<String>();
		specialForms.add(REPRESENTATION);
		specialForms.add(TYPE);
		specialForms.add(EXTENDED_LAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
		specialForms.add(CONVERSION);
		specialForms.add(DEFINE);
		specialForms.add(CONS);
		specialForms.add(ERROR);
		specialForms.add(UNUSED);
		specialForms.add(AND);
		specialForms.add(OR);
		specialForms.add(CONSTRUCTOR);
		specialForms.add(CONSTRUCT);
		specialForms.add(CONVERT);
		specialForms.add(DECONSTRUCT);
		specialForms.add(CAN_DECONSTRUCT_AS);
		specialForms.add(LET_TYPE);
		specialForms.add(INSTANCE_OF);
		specialForms.add(INSTANCE_OF_REPRESENTATION);
		specialForms.add(EAPPLY);
		specialForms.add(GET);
		specialForms.add(TUPLE);
		specialForms.add(LET);
		specialForms.add(LET_AST);
		specialForms.add(LOOP);
		specialForms.add(RECUR);
		specialForms.add(EXTEND);
	}

	/**
	 * Returns true if given string is reserved word of a special form
	 * 
	 * @param symbol
	 * @return
	 */
	public static boolean isSpecialForm(String symbol) {
		return specialForms.contains(symbol);
	}

	/**
	 * Checks if given semantic node is special form reserved word
	 * 
	 * @param node inspected node
	 * @return true if node is a symbol node containing special form, false
	 *         otherwise
	 */
	public static boolean isSpecialForm(SemanticNode node) {
		try {
			return isSpecialForm(node.asSymbol());
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Checks if the variable-type list is fully typed
	 * 
	 * @param l
	 * @return false if some VariableTypePair has type of null, true otherwise
	 */
	public static boolean isArgListFullyTyped(List<TypeVariablePair> l) {
		for (TypeVariablePair p : l) {
			if (p.first instanceof TypeVariable) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Returns true if the VariableTypePair list is completely untyped - every pair
	 * has type of null. Returns false otherwise
	 * 
	 * @param l checked list
	 * @return true or false
	 */
	public static boolean isArgListUntypped(List<TypeVariablePair> l) {
		for (TypeVariablePair p : l) {
			if (!(p.first instanceof TypeVariable)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Gets the tail of a list
	 * 
	 * @param l
	 * @return
	 */
	public static <E> List<E> listTail(List<E> l) {
		return l.subList(1, l.size());
	}

	/**
	 * Gets the head of a list
	 * 
	 * @param l
	 * @return
	 */
	public static <E> E listHead(List<E> l) {
		return l.get(0);
	}

	/**
	 * Returns true if the semantic node is simple symbol
	 * 
	 * @param s
	 * @return
	 */
	public static boolean isSimpleSymbol(SemanticNode s) {
		return s.type == SemanticNode.NodeType.SYMBOL;
	}

	/**
	 * Parses simple list of formal arguments
	 * 
	 * @param l parsed argument list
	 * @return tuple of formal arguments
	 * @throws AppendableException
	 */
	public static Tuple parseArgsList(List<SemanticNode> l) throws AppendableException {
		try {
			return new Tuple(l.stream().map(ThrowingFunction.wrapper(x -> {
				try {
					return new Symbol(x.asSymbol());
				} catch (AppendableException e) {
					throw new UnexpectedExpressionException(x.toString());
				}
			})).collect(Collectors.toList()));
		} catch (RuntimeException e) {
			AppendableException ae = (AppendableException) e.getCause();
			throw ae;
		}
	}
}
