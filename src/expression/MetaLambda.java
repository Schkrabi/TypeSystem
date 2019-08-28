package expression;

/**
 * Parent class for all applicable expresion (lambdas and extended lambdas)
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class MetaLambda extends Expression {
	
	/**
	 * Returns true if the expression is applicable (is a function) otherwise returns false
	 * @param e inspected expression
	 * @return true or false
	 */
	public static boolean isApplicableExpression(Expression e){
		return e instanceof MetaLambda;
	}
}
