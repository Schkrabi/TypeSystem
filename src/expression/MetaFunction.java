package expression;

import java.util.Comparator;

import util.AppendableException;

import interpretation.Environment;

/**
 * Expression for representing interpreted functions and Extended functions
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class MetaFunction extends Expression {
	/**
	 * Environment where the function was created
	 */
	public final Environment creationEnvironment;
	
	public MetaFunction(Environment creationEnvironment){
		this.creationEnvironment = creationEnvironment;
	}
	
	/**
	 * Returns implementation according to the comparator (if there are any alternative implementations)
	 * @param c comparator
	 * @return function
	 */
	public abstract Function getFunction(Comparator<? super Function> c);
	
	@Override
	public String toClojureCode() throws AppendableException {
		throw new AppendableException("Unable to compile evaluated function to clojure code");
	}
	
	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	/**
	 * Returns true if given expression is a function
	 * @param e
	 * @return
	 */
	public static boolean isFunction(Expression e){
		return e instanceof MetaFunction;
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof MetaFunction) {
			return this.creationEnvironment.equals(((MetaFunction) other).creationEnvironment);
		}
		return false;
	}
}
