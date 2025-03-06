package velka.core.application;

import java.util.Optional;
import com.sun.codemodel.JExpression;

import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.Function;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.RankAggregation;
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
public class Extend extends Expression implements Comparable<Expression>, CompileableToJava {

	/**
	 * Symbol of the special form
	 */
	public static final String EXTEND = "extend";
	
	/** Expresion that evaluates to extended function */
	public final Expression extendedFunction;
	
	/** Expression that evaluates to function -> future implementation */
	public final Expression implementation;
	
	/** Cost function for implementation */
	public final Expression costFunction;
	
	private static final Expression invalidCost = new Expression() {
		@Override
		public Expression interpret(Environment env) throws AppendableException {
			return null;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			return null;
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return null;
		}

		@Override
		protected Expression doConvert(Type from, Type to, Environment env)
				throws AppendableException {
			return null;
		}		
	};
	
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
	
	private boolean isCostFunctionInferingCorrectly(Type implType, Environment env) throws AppendableException {
		//var argsType = ((TypeTuple)((TypeArrow)implType).ltype);
		Expression costF = this.getCostFunction(env);
		Pair<Type, Substitution> infered = costF.infer(env);
		
		if(!infered.first.isApplicableType()){
			throw new AppendableException("Cost function "
					+ costF.toString() 
					+ " does not infer to applicable type, got "
					+ infered.first.toString()
					+ " in "
					+ this.toString());
		}
		TypeArrow costFunType = (TypeArrow)infered.first.removeRepresentationInformation();
		
		Optional<Substitution> o = Type.unifyTypes(costFunType.rtype, TypeAtom.TypeDoubleNative);
		if(o.isEmpty()) {
			throw new AppendableException("Cost function "
					+ costF.toString()
					+ " must return double, got: "
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
	public Expression interpret(Environment env) throws AppendableException {		
		var efi = this.extendedFunction.interpret(env);
		var impli = this.implementation.interpret(env);
		var costi = this.getCostFunction(env).interpret(env);
		
		if(efi instanceof ExtendedFunction ef) {
			if(impli instanceof Operator o) {
				var p = o.infer(env);
				var t = (TypeArrow)p.first;
				var pt = (TypeTuple)t.ltype;
				var parms = pt.stream().map(lt -> Pair.of(new velka.core.expression.Symbol(NameGenerator.next()), lt)).toList(); 
				
				impli = new Function(env,
						new AbstractionApplication(o, new Tuple(parms.stream().map(lp -> lp.first).toList())), parms);
			}
			
			if(impli instanceof Function impl) {
				if(costi instanceof VelkaAbstraction cost) {
					return ef.extend(impl, cost);
				}
				throw new AppendableException(
						new StringBuilder()
							.append(this.getCostFunction(env))
							.append(" does not interpret to VelkaAbstraction, got: ")
							.append(costi)
							.toString());
			}
			throw new AppendableException(
					this.implementation.toString()
					+ " does not interpret to function, got: "
					+ impli.toString()
					+ " in "
					+ this.toString());
		}
		throw new AppendableException(
				this.extendedFunction.toString()
				+ " does not interpret to extended function, got: "
				+ efi.toString()
				+ " in "
				+ this.toString());
	}
	
	private Expression getCostFunction(Environment env) {
		if(this.costFunction == Extend.invalidCost) {
			try {
				var p = this.implementation.infer(env);
				var argsType = ((TypeTuple)((TypeArrow)p.first).ltype);
				return Lambda.constFun(argsType.size(), new LitDouble(RankAggregation.instance().defaultImplementationRank()));
			}catch(AppendableException e) {
				throw new RuntimeException(e);
			}
		}
		return this.costFunction;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		var efInf = this.extendedFunction.infer(env);
		var implInf = this.implementation.infer(env);
		
		this.isCostFunctionInferingCorrectly(implInf.first, env);
		
		var t = RepresentationOr.or(efInf.first, implInf.first);
		return Pair.of(t, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var code = ClojureHelper.applyClojureFunction(
				".extend",
				this.extendedFunction.toClojureCode(env),
				this.implementation.toClojureCode(env),
				this.getCostFunction(env).toClojureCode(env));
		
		return code;
	}
	
	@Override
	public int hashCode() {
		return new StringBuilder()
				.append(this.extendedFunction.hashCode())
				.append(this.implementation.hashCode())
				.append(this.costFunction.hashCode())
				.toString().hashCode();
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
		return new StringBuilder()
			.append("(")
			.append(Extend.EXTEND)
			.append(" ")
			.append(this.extendedFunction.toString())
			.append(" ")
			.append(this.implementation.toString())
			.append(")")
			.toString();
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		throw new RuntimeException("doConvert is not implemented for extend");
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		var ef = (CompileableToJava)this.extendedFunction;
		var impl = (CompileableToJava)this.implementation;
		var cost = (CompileableToJava)this.getCostFunction(env);
		
		var _ef = ef.toJavaExpr(env);
		var ret = _ef.invoke("extend")
				.arg(impl.toJavaExpr(env))
				.arg(cost.toJavaExpr(env));
		
		return ret;
	}
}
