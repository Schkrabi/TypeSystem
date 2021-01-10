package velka.lang.semantic;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import velka.lang.exceptions.UnexpectedExpressionException;

import velka.lang.expression.Tuple;
import velka.lang.expression.Symbol;

import velka.lang.parser.SemanticNode;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.ThrowingFunction;

/**
 * This class contains auxiliary static methods and constants for SemanticParser
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class SemanticParserStatic {
	public static final String DEFINE = "define";
	public static final String DEFINE_TYPE = "type";
	public static final String DEFINE_REPRESENTATION = "representation";
	public static final String DEFINE_CONVERSION = "conversion";
	public static final String DEFINE_CONSTRUCTOR = "constructor";
	public static final String CONSTRUCT = "construct";
	public static final String CONVERT = "convert";
	public static final String LAMBDA = "lambda";
	public static final String EXTENDED_LAMBDA = "extended-lambda";
	public static final String IF = "if";
	public static final String CONS = "cons";
	public static final String ERROR = "error";
	public static final String AND = "and";
	public static final String OR = "or";
	public static final String DECONSTRUCT = "deconstruct";
	public static final String CAN_DECONSTRUCT_AS = "can-deconstruct-as";
	public static final String LET_TYPE = "let-type";
	public static final String INSTANCE_OF = "instance-of";
	public static final String INSTANCE_OF_REPRESENTATION = "instance-of-representation";
	/**
	 * Unused special form. For testing purposes only!
	 */
	public static final String UNUSED = "unused";

	public static final Set<String> specialForms;

	static {
		specialForms = new TreeSet<String>();
		specialForms.add(DEFINE_REPRESENTATION);
		specialForms.add(DEFINE_TYPE);
		specialForms.add(EXTENDED_LAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
		specialForms.add(DEFINE_CONVERSION);
		specialForms.add(DEFINE);
		specialForms.add(CONS);
		specialForms.add(ERROR);
		specialForms.add(UNUSED);
		specialForms.add(AND);
		specialForms.add(OR);
		specialForms.add(DEFINE_CONSTRUCTOR);
		specialForms.add(CONSTRUCT);
		specialForms.add(CONVERT);
		specialForms.add(DECONSTRUCT);
		specialForms.add(CAN_DECONSTRUCT_AS);
		specialForms.add(LET_TYPE);
		specialForms.add(INSTANCE_OF);
		specialForms.add(INSTANCE_OF_REPRESENTATION);
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
					throw new UnexpectedExpressionException(x);
				}
			})).collect(Collectors.toList()));
		} catch (RuntimeException e) {
			AppendableException ae = (AppendableException) e.getCause();
			throw ae;
		}
	}
}
