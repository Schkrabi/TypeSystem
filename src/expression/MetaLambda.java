package expression;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

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
	 * Returns adequate simple lambda expression for this applicable expression
	 * @return lambda expression
	 */
	public abstract Lambda getLambda();
	
	/**
	 * Returns true if the expression is applicable (is a function) otherwise returns false
	 * @param e inspected expression
	 * @return true or false
	 */
	public static boolean isApplicableExpression(Expression e){
		return e instanceof MetaLambda;
	}
	
	List<Class<? extends MetaLambda>> lambdaOrdering = Arrays.asList(Lambda.class, ExtendedLambda.class);
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof MetaLambda) {
			return (int) Math.signum(lambdaOrdering.indexOf(this.getClass()) - lambdaOrdering.indexOf(other.getClass()));
		}
		return super.compareTo(other);
	}
}
