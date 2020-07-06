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
	
	public Deconstruct(Expression argument) {
		this.argument = argument;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this.argument.value.interpret(env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return this.argument.value.infer(env);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return this.argument.value.toClojureCode(env);
	}
	
	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DECONSTRUCT + " " + this.argument.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Deconstruct) {
			return this.argument.equals(((Deconstruct) other).argument);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.argument.hashCode();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Deconstruct) {
			int cmp = this.argument.compareTo(((Deconstruct) other).argument);
			return cmp;
		}
		return super.compareTo(other);
	}
}
