package application;

import expression.Expression;
import interpretation.Environment;
import literal.LitComposite;
import semantic.SemanticParserStatic;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

/**
 * Deconstruct special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Deconstruct extends Expression {
	
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
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = this.argument.interpret(env);
		if(!(e instanceof LitComposite)) {
			throw new IllegalDeconstructionException(e, this.as);
		}
		LitComposite lc = (LitComposite)e;
		Pair<Type, Substitution> p = lc.value.infer(env);
		try {
			Type.unify(p.first, this.as);
		}catch(AppendableException ae) {
			throw new IllegalDeconstructionException(e, this.as);
		}
		return lc.value;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.argument.infer(env);
		return new Pair<Type, Substitution>(this.as, p.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return "(get " + this.argument.toClojureCode(env) + " 0)";
	}
	
	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DECONSTRUCT + " " + this.argument.toString() + " " + this.as.toString() + ")";
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
}
