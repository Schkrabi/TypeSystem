package velka.core.application;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitComposite;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Class for expressions defining constructors
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineConstructor extends Expression {
	
	/**
	 * Symbol for constructor special form
	 */
	public static final String CONSTRUCTOR = "constructor";

	/**
	 * Constructor defined by this expression creates constructedType TypeAtom
	 */
	public final TypeAtom constructedType;

	/**
	 * Lambda constructing value of LitComposite of the TypeAtom
	 */
	public final Lambda constructionLambda;

	public DefineConstructor(TypeAtom constructedType, Lambda constructionLambda) {
		this.constructedType = constructedType;
		this.constructionLambda = new Lambda(constructionLambda.args, constructionLambda.argsType,
				new LitComposite(constructionLambda.body, constructedType)); 
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		env.getTypeSystem().addConstructor(
				constructedType, 
				this.constructionLambda.argsType, 
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>();
						args.stream().forEach(o -> eargs.add((Expression)o));
							
						var appl = new AbstractionApplication(constructionLambda, new Tuple(eargs));
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
		Pair<Type, Substitution> p = this.constructionLambda.infer(env);
		return new Pair<Type, Substitution>(Expression.EMPTY_EXPRESSION.infer(env).first, p.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		final var arg = "_arg";
		final var rhis = "_this";
		final var cenv = "_env";
		var code = ClojureHelper.applyClojureFunction(".addConstructor", 
				ClojureCoreSymbols.typeSystem_full,
				this.constructedType.clojureTypeRepresentation(),
				this.constructionLambda.argsType.clojureTypeRepresentation(),
				ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
						Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), 
								ClojureHelper.applyVelkaFunction_argsTuple(this.constructionLambda.toClojureCode(env), 
										arg
//										ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("empty?", arg), 
//												Tuple.EMPTY_TUPLE_CLOJURE, 
//												ClojureHelper.tupleHelper(arg))
										
										)))));		
		
		
		return code;
	}

	@Override
	public String toString() {
		return "(" + CONSTRUCTOR + " " + this.constructedType.toString() + " "
				+ this.constructionLambda.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineConstructor) {

			return this.constructedType.equals(((DefineConstructor) other).constructedType)
					&& this.constructionLambda.equals(((DefineConstructor) other).constructionLambda);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.constructedType.hashCode() * this.constructionLambda.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineConstructor) {
			int cmp = this.constructedType.compareTo(((DefineConstructor) other).constructedType);
			if (cmp != 0)
				return cmp;

			return this.constructionLambda.compareTo(((DefineConstructor) other).constructionLambda);
		}
		return super.compareTo(other);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
	}

}
