package velka.lang.application;

import velka.lang.abstraction.Lambda;
import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

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
		this.constructionLambda = constructionLambda;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		typeEnv.addConstructor(this.constructedType, this.constructionLambda, env);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.constructionLambda.infer(env, typeEnv);
		return new Pair<Type, Substitution>(Expression.EMPTY_EXPRESSION.infer(env, typeEnv).first, p.second);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		typeEnv.addConstructor(this.constructedType, this.constructionLambda, env);
		return "";
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

}
