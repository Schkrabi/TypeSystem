package semantic;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import expression.Expression;
import expression.Tuple;
import expression.Variable;

import parser.SemanticNode;
import types.Type;
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
	
	public static final Set<String> specialForms;
	
	static{
		specialForms = new TreeSet<String>();
		specialForms.add(DEFREP);
		specialForms.add(DEFTYPE);
		specialForms.add(ELAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
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
	static boolean isSpecialForm(SemanticNode node) {
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
	static boolean isArgListFullyTyped(List<VariableTypePair> l) {
		for (VariableTypePair p : l) {
			if (p.type == null) {
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
	static boolean isArgListUntypped(List<VariableTypePair> l) {
		for (VariableTypePair p : l) {
			if (p.type != null) {
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
	static List<Variable> filterVariablesFromTypedArgsList(List<VariableTypePair> l) {
		List<Variable> r = new ArrayList<Variable>();
		for (VariableTypePair p : l) {
			r.add(p.variable);
		}
		return r;
	}

	/**
	 * Filters the types from the variableTypePair list
	 * 
	 * @param l
	 *            VariableTypePair list
	 * @return list of types
	 */
	static List<Type> filterTypesFromTypedArgsList(List<VariableTypePair> l) {
		List<Type> r = new ArrayList<Type>();
		for (VariableTypePair p : l) {
			r.add(p.type);
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
	static boolean isSimpleSymbol(SemanticNode s) {
		return s.type == SemanticNode.NodeType.SYMBOL;
	}

	/**
	 * Parses simple list of formal arguments
	 * @param l parsed argument list
	 * @return tuple of formal arguments
	 * @throws AppendableException
	 */
	static Tuple parseArgsList(List<SemanticNode> l) throws AppendableException {
		Expression[] args = new Expression[l.size()];
		int i = 0;
		for (SemanticNode t : l) {
			if (t.type != SemanticNode.NodeType.SYMBOL) {
				throw new UnexpectedExpressionException(t);
			}
			args[i] = new Variable(t.asSymbol());
			i++;
		}
		return new Tuple(args);
	}
}
