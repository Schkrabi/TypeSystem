package velka.core.application;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.Function;
import velka.core.abstraction.Operators;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
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
	
	/**
	 * Cost function for implementation
	 */
	public final Optional<Expression> costFunction;
	
	public Extend(Expression extendedFunction, Expression implementation) {
		super();
		this.extendedFunction = extendedFunction;
		this.implementation = implementation;
		this.costFunction = Optional.empty();
	}
	
	public Extend(Expression extendedFunction, Expression implementation, Expression costFunction) {
		super();
		this.extendedFunction = extendedFunction;
		this.implementation = implementation;
		this.costFunction = Optional.of(costFunction);
	}
	
	private boolean isCostFunctionInferingCorrectly(Type implType, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if(this.costFunction.isEmpty()) {
			return true;
		}
		
		Expression costF = costFunction.get();
		Pair<Type, Substitution> infered = costF.infer(env, typeEnv);
		
		if(!infered.first.isApplicableType()){
			throw new AppendableException("Cost function "
					+ costF.toString() 
					+ " does not infer to applicable type, got "
					+ infered.first.toString()
					+ " in "
					+ this.toString());
		}
		TypeArrow costFunType = (TypeArrow)infered.first.removeRepresentationInformation();
		
		Optional<Substitution> o = Type.unifyTypes(costFunType.rtype, TypeAtom.TypeIntNative);
		if(o.isEmpty()) {
			throw new AppendableException("Cost function "
					+ costF.toString()
					+ " must return integer, got: "
					+ costFunType.rtype.toString()
					+ " in "
					+ this.toString());
		}
		
		TypeArrow implementationType = (TypeArrow)implType.removeRepresentationInformation();
		o = Type.unifyTypes(costFunType.ltype, implementationType.ltype);
		if(o.isEmpty()) {
			throw new AppendableException("Cost function "
					+ costF.toString()
					+ " argument type "
					+ costFunType.ltype.toString()
					+ " is not unyfiable with implementation " 
					+ this.implementation.toString()
					+ " argument type "
					+ implementationType.ltype.toString()
					+ " in "
					+ this.toString());
		}
		return true;
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
		
		Expression costFunction = this.getInterpretedCostFunction(env, typeEnv);
		
		Map<Function, Expression> implementations = extendedFunctionIntp.getImplementationsAsFunctions();
		implementations.put(implementationIntp, costFunction);
		
		ExtendedFunction extendedFunction = 
				ExtendedFunction.makeExtendedFunction(implementations, extendedFunctionIntp.creationEnvironment);
		
		return extendedFunction;
	}
	
	/**
	 * Gets interpreted cost function for this extend 
	 * @param env interpretation environment
	 * @param typeEnv type environment
	 * @return Interpreted function
	 * @throws AppendableException if anything goes wrong
	 */
	private Expression getInterpretedCostFunction(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if(this.costFunction.isPresent()) {
			return this.costFunction.get().interpret(env, typeEnv);
		}
		
		Expression defaultCost = ((Function)this.implementation.interpret(env, typeEnv))
				.defaultCostFunction().interpret(env, typeEnv);
		
		return defaultCost;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> extendedFunctionInfered = this.extendedFunction.infer(env, typeEnv);
		
		Set<Type> representations = getExtendedFunctionRepresentations(extendedFunctionInfered);
		
		Pair<Type, Substitution> implementationInfered = this.implementation.infer(env, typeEnv);
		
		representations.add(implementationInfered.first);
		
		Type type = RepresentationOr.makeRepresentationOr(representations); 
		
		this.isCostFunctionInferingCorrectly(implementationInfered.first, env, typeEnv);
		
		return new Pair<Type, Substitution>(type, extendedFunctionInfered.second.compose(implementationInfered.second));
	}

	/**
	 * Gets representations of modified extended functions
	 * @param extendedFunctionInfered inference result of extended function
	 * @return Set of representations
	 * @throws AppendableException if extended function is not valid
	 */
	private Set<Type> getExtendedFunctionRepresentations(Pair<Type, Substitution> extendedFunctionInfered)
			throws AppendableException {
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
		return representations;
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String extFun = "_extFun";
		String extFunType = "_extFunType";
		String impl_noCost = "_implNoCost";
		String impl = "_impl";
		String implType = "_implType";
		String code = ClojureHelper.letHelper(
				ClojureHelper.addTypeMetaInfo_str(
						ClojureHelper.applyClojureFunction(
								"conj",
								extFun,
								impl),
						ClojureHelper.applyClojureFunction(
								"if",
								ClojureHelper.isInstanceOfClass(
										extFunType,
										RepresentationOr.class),
								ClojureHelper.applyClojureFunction(
										".conjoin",
										extFunType,
										implType),
								ClojureHelper.applyClojureFunction(
										"velka.types.RepresentationOr/makeRepresentationOr",
										ClojureHelper.clojureSetHelper(
												extFunType,
												implType)))),
				new Pair<String, String>(
						extFun,
						this.extendedFunction.toClojureCode(env, typeEnv)),
				new Pair<String, String>(
						impl_noCost,
						this.implementation.toClojureCode(env, typeEnv)),
				new Pair<String, String>(
						impl,
						ClojureHelper.setCostFunction(
								impl_noCost,
								this.costFunction.isPresent() ?
										this.costFunction.get().toClojureCode(env, typeEnv) :
										ClojureHelper.applyClojureFunction(
												Operators.defaultCostFunction_full,
												impl_noCost))),
				new Pair<String, String>(
						implType,
						ClojureHelper.applyClojureFunction(
								ClojureCoreSymbols.getTypeClojureSymbol_full,
								impl)),
				new Pair<String, String>(
						extFunType,
						ClojureHelper.applyClojureFunction(
								ClojureCoreSymbols.getTypeClojureSymbol_full,
								extFun)));
		return code;
	}
	
	@Override
	public int hashCode() {
		return this.extendedFunction.hashCode() * this.implementation.hashCode();
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Extend) {
			return this.extendedFunction.equals(((Extend)other).extendedFunction)
					&& this.implementation.equals(((Extend)other).implementation)
					&& this.costFunction.equals(((Extend)other).costFunction);
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
			if(cmp != 0) {
				return cmp;
			}
			
			if(this.costFunction.isPresent()) {
				if(o.costFunction.isPresent()) {
					cmp = this.costFunction.get().compareTo(o.costFunction.get());
				}
				else {
					cmp = 1;
				}
			}
			else {
				cmp = o.costFunction.isPresent() ? -1 : 0;
			}
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

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}
}
