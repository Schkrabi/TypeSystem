package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

public class PostponeInterpretation extends Expression {

	private final Expression postponed;
	private final Environment environment;

	public PostponeInterpretation(Expression postponed, Environment environment) {
		this.postponed = postponed;
		this.environment = environment;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this.postponed.interpret(this.environment);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return this.postponed.infer(this.environment);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		throw new AppendableException(this.getClass().getName() + " is not to be converted to Clojure!");
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof PostponeInterpretation) {
			return this.postponed.equals(((PostponeInterpretation) other).postponed)
					&& this.environment.equals(((PostponeInterpretation) other).environment);
		}
		return false;
	}

	@Override
	public String toString() {
		return "(postpone " + this.postponed.toString() + ")";
	}

	@Override
	public int compareTo(Expression e) {
		if (e instanceof PostponeInterpretation) {
			int cmp = this.postponed.compareTo(((PostponeInterpretation) e).postponed);
			if (cmp != 0)
				return cmp;

			return this.environment.compareTo(((PostponeInterpretation) e).environment);
		}
		return super.compareTo(e);
	}

	@Override
	public int hashCode() {
		return this.postponed.hashCode() * this.environment.hashCode();
	}
}
