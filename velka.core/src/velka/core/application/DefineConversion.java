package velka.core.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaTuple;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.RankAggregation;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for defconversion special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineConversion extends Expression implements CompileableToJava {
	
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
		try {
			this.cost = new Lambda(new LitDouble(RankAggregation.instance().defaultConversionRank()),
					List.of(Pair.of(new Symbol(NameGenerator.next()), this.from.removeRepresentationInformation())));
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
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
				new velka.util.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object _env) {
						var eargs = new ArrayList<Expression>(args.size());
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var appl = new AbstractionApplication(lambda, new Tuple(eargs));
						try {
							Environment eenv = (Environment)_env;
							
							return appl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
					
				}, 
				new velka.util.IConversionRanker() {
					
					@Override
					public double eval(Object arg) {						
						var appl = new AbstractionApplication(me.cost, new Tuple((Expression)arg));
						try {
							LitDouble ld = (LitDouble)appl.interpret(env);
							return ld.value;
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
				ClojureHelper.reify(velka.util.IEvalueable.class, 
						Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), ClojureHelper.applyVelkaFunction_argsTuple(lambda.toClojureCode(env), 
								arg)))),
				ClojureHelper.reify(velka.util.IConversionRanker.class, 
						Pair.of("eval", Pair.of(List.of(rhis, arg), 
								ClojureHelper.applyVelkaFunction(this.cost.toClojureCode(env), 
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
		return new Lambda(this.body,
				List.of(Pair.of((Symbol)this.args.get(0), this.from)));
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
		throw new RuntimeException("doConvert not implemented");
	}
	
	

	@Override
	public JExpression toJavaExpr(Environment env) {
		var ctjlbd = (CompileableToJava)this.makeConversionLambda(env);
		var ctjcst = (CompileableToJava)this.cost;
		
		var ievCl = CodeModelInstance.instance().ref(velka.util.IEvalueable.class);
		
		TypeArrow costType;
		try {
			costType = (TypeArrow)(this.cost.infer(env).first);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var costParmType = (TypeTuple)costType.ltype;
		
		var iConvRanker = CodeModelInstance.instance().anonymousClass(velka.util.IConversionRanker.class);
		
		var convRanker = iConvRanker.method(com.sun.codemodel.JMod.PUBLIC, double.class, "eval");
		var rnkParm = convRanker.param(Object.class, "arg");
		var rnkRet = convRanker
				.body().decl(
						CodeModelInstance.instance().DOUBLE, "ret", JExpr
								.cast(CodeModelInstance.instance().ref(Double.class),
										ctjcst.toJavaExpr(env).invoke("apply")
												.arg(VelkaTuple._velkaTuple(costParmType, rnkParm)))
								.invoke("doubleValue"));
		convRanker.body()._return(rnkRet);
		
		return JavaTypeSystem.codeInstance().invoke("addConversion")
				.arg(TypeUtil.instance().type2java(from))
				.arg(TypeUtil.instance().type2java(to))
				.arg(JExpr.cast(ievCl, ctjlbd.toJavaExpr(env)))
				.arg(JExpr._new(iConvRanker));
	}
}
