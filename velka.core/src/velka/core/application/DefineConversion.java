package velka.core.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.CostAggregation;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for defconversion special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineConversion extends Expression {
	
	/**
	 * Symbol for conversion special form
	 */
	public static final String CONVERSION = "conversion";

	/**
	 * Type from which we are converting
	 */
	public final TypeAtom from;
	/**
	 * Type to which expression is converting
	 */
	public final TypeAtom to;
	/**
	 * Arguments of the conversion
	 */
	public final Tuple args;
	/**
	 * Body of the conversion
	 */
	public final Expression body;
	
	/** Conversion cost */
	public final Expression cost;

	public DefineConversion(TypeAtom fromType, TypeAtom toType, Tuple args, Expression body) {
		super();
		this.from = fromType;
		this.to = toType;
		this.args = args;
		this.body = body;
		this.cost = new Lambda(new Tuple(new Symbol(NameGenerator.next())), new TypeTuple(this.from), new LitDouble(CostAggregation.instance().defaultConversionRank()));
	}
	
	public DefineConversion(TypeAtom fromType, TypeAtom toType, Tuple args, Expression body, Expression cost) {
		super();
		this.from = fromType;
		this.to = toType;
		this.args = args;
		this.body = body;
		this.cost = cost;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		final var lambda = this.makeConversionLambda(env);
		final var me = this;
		env.getTypeSystem().addConversion(this.from, this.to, 
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>(args.size());
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var appl = new AbstractionApplication(lambda, new Tuple(eargs));
						try {
							var eenv = (Environment)env;
							
							return appl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
					
				}, 
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>(args.size());
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var appl = new AbstractionApplication(me.cost, new Tuple(eargs));
						try {
							var eenv = (Environment)env;
							
							return appl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
					
				});
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.makeConversionLambda(env).infer(env);
		TypeArrow type = (TypeArrow) p.first;
		TypeTuple ttuple = new TypeTuple(Arrays.asList(this.from));
		Optional<Substitution> left = Type.unifyTypes(type.ltype, ttuple);
		if(left.isEmpty()) {
			throw new TypesDoesNotUnifyException(type.ltype, ttuple);
		}
		
		Optional<Substitution> right = Type.unifyTypes(type.rtype, this.to);
		if(right.isEmpty()) {
			throw new TypesDoesNotUnifyException(type.rtype, this.to);
		}
		
		return new Pair<Type, Substitution>(Expression.EMPTY_EXPRESSION.infer(env).first, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		final var lambda = this.makeConversionLambda(env);
		final var arg = "_arg";
		final var rhis = "_this";
		final var cenv = "_env";
		var code = ClojureHelper.applyClojureFunction(".addConversion", 
				ClojureCoreSymbols.typeSystem_full,
				this.from.clojureTypeRepresentation(),
				this.to.clojureTypeRepresentation(),
				ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
						Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), ClojureHelper.applyVelkaFunction_argsTuple(lambda.toClojureCode(env), 
								arg)))),
				ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
						Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), ClojureHelper.applyVelkaFunction_argsTuple(this.cost.toClojureCode(env), 
								arg)))));
		return code;
	}

	@Override
	public String toString() {
		return "(" + CONVERSION + " " + from.toString() + " " + to.toString() + " "
				+ this.args.toString() + " " + this.body.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineConversion) {
			return this.from.equals(((DefineConversion) other).from) && this.to.equals(((DefineConversion) other).to)
					&& this.args.equals(((DefineConversion) other).args) && this.body.equals(((DefineConversion) other).body);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.from.hashCode() * this.to.hashCode() * this.args.hashCode() * this.body.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineConversion) {
			int cmp = this.from.compareTo(((DefineConversion) other).from);
			if (cmp != 0)
				return cmp;
			cmp = this.to.compareTo(((DefineConversion) other).to);
			if (cmp != 0)
				return cmp;
			cmp = this.args.compareTo(((DefineConversion) other).args);
			if (cmp != 0)
				return cmp;
			return this.body.compareTo(((DefineConversion) other).body);
		}
		return super.compareTo(other);
	}
	
	/**
	 * Creates conversion lambda for this conversion definiton
	 * @param env environment where lambda is created
	 * @return lambda expression
	 */
	private Lambda makeConversionLambda(Environment env) {
		return new Lambda(this.args, new TypeTuple(Arrays.asList(this.from)), this.body);
	}

	/**
	 * Returns true if given lambda is conversion between given from and to types.
	 * Otherwise returns false.
	 * 
	 * @param lambda tested lambda
	 * @param from   type atom
	 * @param to     type atom
	 * @param env    environment
	 * @return true or false
	 * @throws AppendableException if there is issue during inference of lambda
	 */
	public static boolean isLambdaValidConversion(Lambda lambda, TypeAtom from, TypeAtom to, Environment env)
			throws AppendableException {
		Pair<Type, Substitution> infered = lambda.infer(env);
		TypeArrow expected = new TypeArrow(new TypeTuple(Arrays.asList(from)), to);
		return infered.first.equals(expected);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
	}
}
