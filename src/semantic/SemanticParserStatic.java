package semantic;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Tuple;
import expression.Variable;

import parser.SemanticNode;
import types.TypeVariable;
import util.AppendableException;
import util.ThrowingFunction;

/**
 * This class contains auxiliary static methods and constants for SemanticParser
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class SemanticParserStatic {
	public static final String DEFTYPE = "deftype";
	public static final String DEFREP = "defrep";
	public static final String LAMBDA = "lambda";
	public static final String ELAMBDA = "elambda";
	public static final String IF = "if";
	public static final String DEFCONVERSION = "defconversion";
	public static final String DEFINE = "define";
	public static final String CONS = "cons";
	public static final String ERROR = "error";
	/**
	 * Unused special form. For testing purposes only!
	 */
	public static final String UNUSED = "unused";

	public static final Set<String> specialForms;

	static {
		specialForms = new TreeSet<String>();
		specialForms.add(DEFREP);
		specialForms.add(DEFTYPE);
		specialForms.add(ELAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
		specialForms.add(DEFCONVERSION);
		//specialForms.add(DEFCONSTRUCTOR);
		specialForms.add(DEFINE);
		specialForms.add(CONS);
		specialForms.add(ERROR);
		specialForms.add(UNUSED);
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
					return new Variable(x.asSymbol());
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
