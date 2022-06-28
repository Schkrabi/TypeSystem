package velka.core.application;

import velka.core.exceptions.IllegalDeconstructionException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitComposite;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Deconstruct special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Deconstruct extends Expression {
	
	/**
	 * Symbol for deconstruct special form
	 */
	public static final String DECONSTRUCT = "deconstruct";
	
	/**
	 * Deconstructed expression
	 */
	public final Expression argument;
	
	/**
	 * Type the argument should be deconstructed to
	 */
	public final Type as;
	
	public Deconstruct(Expression argument, Type as) {
		this.argument = argument;
		this.as = as;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression e = this.argument.interpret(env, typeEnv);
		if(!(e instanceof LitComposite)) {
			throw new IllegalDeconstructionException(e, this.as);
		}
		LitComposite lc = (LitComposite)e;
		Pair<Type, Substitution> p = lc.value.infer(env, typeEnv);
		
		if(Type.unifyTypes(p.first, this.as).isEmpty()) {
			throw new IllegalDeconstructionException(e, this.as);
		}
		
		return lc.value;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.argument.infer(env, typeEnv);
		return new Pair<Type, Substitution>(this.as, p.second);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return "(get " + this.argument.toClojureCode(env, typeEnv) + " 0)";
	}
	
	@Override
	public String toString() {
		return "(" + DECONSTRUCT + " " + this.argument.toString() + " " + this.as.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Deconstruct) {
			return this.argument.equals(((Deconstruct) other).argument)
					&& this.as.equals(((Deconstruct) other).as);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.argument.hashCode() * this.as.hashCode();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Deconstruct) {
			int cmp = this.argument.compareTo(((Deconstruct) other).argument);
			if(cmp != 0) {
				return cmp;
			}
			cmp = this.as.compareTo(((Deconstruct) other).as);
			return cmp;
		}
		return super.compareTo(other);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}
}
