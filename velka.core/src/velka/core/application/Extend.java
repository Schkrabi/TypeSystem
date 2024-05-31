package velka.core.application;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.Function;
import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitInteger;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
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
	
	private static final Expression invalidCost = new Expression() {
		@Override
		public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return null;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return null;
		}

		@Override
		public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return null;
		}

		@Override
		protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			return null;
		}		
	};
	
	/**
	 * Cost function for implementation
	 */
	public final Expression costFunction;
	
	public Extend(Expression extendedFunction, Expression implementation) {
		super();
		this.extendedFunction = extendedFunction;
		this.implementation = implementation;
		this.costFunction = Extend.invalidCost;
	}
	
	public Extend(Expression extendedFunction, Expression implementation, Expression costFunction) {
		super();
		this.extendedFunction = extendedFunction;
		this.implementation = implementation;
		this.costFunction = costFunction;
	}
	
	private boolean isCostFunctionInferingCorrectly(Type implType, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		var argsType = ((TypeTuple)((TypeArrow)implType).ltype);
		Expression costF = this.getCostFunction(env, typeEnv);
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
		
		Expression costFunction = this.getCostFunction(env, typeEnv).interpret(env, typeEnv);
		
		Map<Function, Expression> implementations = extendedFunctionIntp.getImplementationsAsFunctions();
		implementations.put(implementationIntp, costFunction);
		
		ExtendedFunction extendedFunction = 
				ExtendedFunction.makeExtendedFunction(implementations, extendedFunctionIntp.creationEnvironment);
		
		return extendedFunction;
	}
	
	private Expression getCostFunction(Environment env, TypeEnvironment typeEnv) {
		if(this.costFunction == Extend.invalidCost) {
			try {
				var p = this.implementation.infer(env, typeEnv);
				var argsType = ((TypeTuple)((TypeArrow)p.first).ltype);
				return Lambda.constFun(argsType.size(), new LitInteger(1));
			}catch(AppendableException e) {
				throw new RuntimeException(e);
			}
		}
		return this.costFunction;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> extendedFunctionInfered = this.extendedFunction.infer(env, typeEnv);
		
		Set<Type> representations = getExtendedFunctionRepresentations(extendedFunctionInfered);
		
		Pair<Type, Substitution> implementationInfered = this.implementation.infer(env, typeEnv);
		
		representations.add(implementationInfered.first);
		
		Type type = RepresentationOr.makeRepresentationOr(representations); 
		
		this.isCostFunctionInferingCorrectly(implementationInfered.first, env, typeEnv);
		
		return new Pair<Type, Substitution>(type, Substitution.EMPTY);//extendedFunctionInfered.second.compose(implementationInfered.second));
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
		
		String costF;
		if(this.costFunction == Extend.invalidCost) {
			costF = this.getCostFunction(env, typeEnv).toClojureCode(env, typeEnv);
		}
		else {
			costF = this.costFunction.toClojureCode(env, typeEnv);
		}
		
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
				Pair.of(
						extFun,
						this.extendedFunction.toClojureCode(env, typeEnv)),
				Pair.of(
						impl_noCost,
						this.implementation.toClojureCode(env, typeEnv)),
				Pair.of(
						impl,
						ClojureHelper.setCostFunction(
								impl_noCost,
								costF)),
				Pair.of(
						implType,
						ClojureHelper.applyClojureFunction(
								ClojureCoreSymbols.getTypeClojureSymbol_full,
								impl)),
				Pair.of(
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
		if(this == other) return true;
		
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
			
			cmp = this.costFunction.compareTo(o.costFunction);
			
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

	/**
	 * Creates new extended function
	 * 
	 * @param implementations    function implementations
	 * @param rankingFunction    ranking function used for selecting implementation
	 * @param createdEnvironment environment where function was created
	 * @return new ExtendedFunction object
	 * @throws AppendableException thrown if argument types of function does not
	 *                             unify
	 */
	public static ExtendedFunction makeExtendedFunction(Collection<Function> implementations,
			Environment createdEnvironment, TypeEnvironment typeEnv) 
					throws AppendableException {		
		Map<Function, Expression> m = new TreeMap<Function, Expression>();
		for(Function f : implementations) {
			Expression e = Lambda.constFun(f.args.size(), new LitInteger(1)).interpret(createdEnvironment, typeEnv);
			m.put(f, e);
		}
		
		return ExtendedFunction.makeExtendedFunction(m, createdEnvironment);
	}
}
