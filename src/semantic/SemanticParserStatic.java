package semantic;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import expression.Expression;
import expression.Tuple;
import expression.Variable;

import parser.SemanticNode;
import types.TypeVariable;
import util.AppendableException;

/**
 * This class contains auxiliary static methods and constants for SemanticParser
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
	public static final String DEFCONSTRUCTOR = "defconstructor";
	public static final String DEFINE = "define";
	public static final String CONS = "cons";
	public static final String ERROR = "error";
	/**
	 * Unused special form. For testing purposes only!
	 */
	public static final String UNUSED = "unused";
	
	public static final Set<String> specialForms;
	
	static{
		specialForms = new TreeSet<String>();
		specialForms.add(DEFREP);
		specialForms.add(DEFTYPE);
		specialForms.add(ELAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
		specialForms.add(DEFCONVERSION);
		specialForms.add(DEFCONSTRUCTOR);
		specialForms.add(DEFINE);
		specialForms.add(CONS);
		specialForms.add(ERROR);
		specialForms.add(UNUSED);
	}
	
	/**
	 * Returns true if given string is reserved word of a special form
	 * @param symbol
	 * @return
	 */
	public static boolean isSpecialForm(String symbol){
		return specialForms.contains(symbol);
	}

	/**
	 * Checks if given semantic node is special form reserved word
	 * 
	 * @param node
	 *            inspected node
	 * @return true if node is a symbol node containing special form, false
	 *         otherwise
	 */
	public static boolean isSpecialForm(SemanticNode node) {
		if (node.type != SemanticNode.NodeType.SYMBOL) {
			return false;
		}
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
	 * @param l
	 *            checked list
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
	 * Filters the variables from the variableTypePair list
	 * 
	 * @param l
	 *            VariableTypePair list
	 * @return list of variables
	 */
	public static List<Variable> filterVariablesFromTypedArgsList(List<TypeVariablePair> l) {
		List<Variable> r = new ArrayList<Variable>();
		for (TypeVariablePair p : l) {
			r.add(p.second);
		}
		return r;
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
	 * @param l
	 * @return
	 */
	public static <E> E listHead(List<E> l) {
		return l.get(0);
	}

	/**
	 * Returns true if the semantic node is simple symbol
	 * @param s
	 * @return
	 */
	public static boolean isSimpleSymbol(SemanticNode s) {
		return s.type == SemanticNode.NodeType.SYMBOL;
	}

	/**
	 * Parses simple list of formal arguments
	 * @param l parsed argument list
	 * @return tuple of formal arguments
	 * @throws AppendableException
	 */
	public static Tuple parseArgsList(List<SemanticNode> l) throws AppendableException {		
		List<Expression> args = new LinkedList<Expression>();
		for (SemanticNode t : l) {
			if (t.type != SemanticNode.NodeType.SYMBOL) {
				throw new UnexpectedExpressionException(t);
			}
			args.add(new Variable(t.asSymbol()));
		}
		return new Tuple(args);
	}
}
