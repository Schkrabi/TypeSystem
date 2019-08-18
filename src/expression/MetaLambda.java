package expression;

import java.util.Comparator;

/**
 * Parent class for all applicable expresion (lambdas and extended lambdas)
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class MetaLambda extends Expression {

	/**
	 * Returns adequate simple lambda for given comparator
	 * @param c
	 * @return lambda expression
	 */
	public abstract Lambda getLambda(Comparator<? super Lambda> c);
	
	/**
	 * Returns true if the expression is applicable (is a function) otherwise returns false
	 * @param e inspected expression
	 * @return true or false
	 */
	public static boolean isApplicableExpression(Expression e){
		return e instanceof MetaLambda;
	}
}
