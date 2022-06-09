package velka.core.application;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Function;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Class representing extend special form.
 * This special form is used for extending extended function by additional implementations.
 * Syntax of this special form is (extend <extended function> <implementation> <_cost function>)
 * _cost function will be implemented later.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Extend extends Expression implements Comparable<Expression> {

	/**
	 * Symbol of the special form
	 */
	public static final String EXTEND = "extend";
	
	/**
	 * Expresion that evaluates to extended function
	 */
	public final Expression extendedFunction;
	
	/**
	 * Expression that evaluates to function -> future implementation
	 */
	public final Expression implementation;
	
	public Extend(Expression extendedFunction, Expression implementation) {
		super();
		this.extendedFunction = extendedFunction;
		this.implementation = implementation;
	}
	
	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression extFunIntp = this.extendedFunction.interpret(env, typeEnv);
		
		if(!(extFunIntp instanceof ExtendedFunction)) {
			throw new AppendableException(
					this.extendedFunction.toString()
					+ " does not interpret to extended function, got: "
					+ extFunIntp.toString()
					+ " in "
					+ this.toString());
		}
		ExtendedFunction extendedFunctionIntp = (ExtendedFunction)extFunIntp;
		
		Expression implIntp = this.implementation.interpret(env, typeEnv);
		
		if(!(implIntp instanceof Function)) {
			throw new AppendableException(
					this.implementation.toString()
					+ " does not interpret to function, got: "
					+ implIntp.toString()
					+ " in "
					+ this.toString());
		}
		Function implementationIntp = (Function)implIntp;
		
		Set<Function> implementations = extendedFunctionIntp.getImplementationsAsFunctions();
		implementations.add(implementationIntp);
		
		@SuppressWarnings("unchecked")
		ExtendedFunction extendedFunction = 
				ExtendedFunction.makeExtendedFunction(implementations, extendedFunctionIntp.creationEnvironment);
		
		return extendedFunction;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> extendedFunctionInfered = this.extendedFunction.infer(env, typeEnv);
		Pair<Type, Substitution> implementationInfered = this.implementation.infer(env, typeEnv);
		
		Set<Type> representations;
		if ((extendedFunctionInfered.first instanceof RepresentationOr)
				&& ((RepresentationOr) extendedFunctionInfered.first).getRepresentations().stream()
						.allMatch(x -> x instanceof TypeArrow)) {
			representations = ((RepresentationOr)extendedFunctionInfered.first).getRepresentations();
		}
		else if(extendedFunctionInfered.first instanceof TypeArrow) {
			representations = new TreeSet<Type>();
			representations.add(extendedFunctionInfered.first);
		}
		else {
			throw new AppendableException(
					this.extendedFunction.toString()
					+ " must infer to Representation Or of type arrows or type arrow, got: "
					+ extendedFunctionInfered.first.toString()
					+ " in "
					+ this.toString());
		}
		
		representations.add(implementationInfered.first);
		
		RepresentationOr type = (RepresentationOr) RepresentationOr.makeRepresentationOr(representations);
		
		Optional<Substitution> s = extendedFunctionInfered.second.union(implementationInfered.second);
		
		if(!s.isPresent()) {
			throw new AppendableException(
						"Substitutions "
						+ extendedFunctionInfered.second.toString()
						+ " and "
						+ implementationInfered.second.toString()
						+ " cannot be merged, infered expressions were "
						+ this.extendedFunction.toString()
						+ " and "
						+ this.implementation.toString()
						+ " in "
						+ this.toString());
		}
		
		return new Pair<Type, Substitution>(type, s.get());
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		//TODO implement once the new elambda representation is in place
		return "";
	}
	
	@Override
	public int hashCode() {
		return this.extendedFunction.hashCode() * this.implementation.hashCode();
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Extend) {
			return this.extendedFunction.equals(((Extend)other).extendedFunction)
					&& this.implementation.equals(((Extend)other).implementation);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Extend) {
			Extend o = (Extend)other;
			int cmp = this.extendedFunction.compareTo(o.extendedFunction);
			if(cmp != 0) {
				return cmp;
			}
			cmp = this.implementation.compareTo(o.implementation);
			return cmp;
		}
		return super.compareTo(other);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("(");
		sb.append(Extend.EXTEND);
		sb.append(" ");
		sb.append(this.extendedFunction.toString());
		sb.append(" ");
		sb.append(this.implementation.toString());
		sb.append(")");
		return sb.toString();
	}
}
