package expression;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

import java.util.Arrays;

import interpretation.Environment;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends Application {

	public IfExpression(Expression condition, Expression trueBranch, Expression falseBranch) {
		super(IfWrapper.singleton, new Tuple(Arrays.asList(condition, trueBranch, falseBranch)));
	}

	/**
	 * Gets condition of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getCondition() {
		return this.args.get(0);
	}

	/**
	 * Gets true branch of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getTrueBranch() {
		return this.args.get(1);
	}

	/**
	 * Gets false branch of this ifExpression
	 * 
	 * @return
	 */
	protected Expression getFalseBranch() {
		return this.args.get(2);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		LitBoolean b = (LitBoolean) this.getCondition().interpret(env);
		if (b.value) {
			return this.getTrueBranch().interpret(env);
		} else {
			return this.getFalseBranch().interpret(env);
		}
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof IfExpression) {
			return this.args.compareTo(((IfExpression) other).args);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof IfExpression) {
			return super.equals(other);
		}
		return false;
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(if ");

		s.append(this.getCondition().toClojureCode(env));
		s.append(" ");

		Expression trueBranch = this.getTrueBranch();
		Pair<Type, Substitution> trueType = trueBranch.infer(env);

		s.append(this.getTrueBranch().toClojureCode(env));
		s.append(" ");

		Expression falseBranch = this.getFalseBranch();
		Pair<Type, Substitution> falseType = falseBranch.infer(env);

		if (!falseType.first.apply(trueType.second).equals(trueType.first.apply(falseType.second))) {
			s.append(falseType.first.convertTo(falseBranch, trueType.first).toClojureCode(env));
		} else {
			s.append(this.getFalseBranch().toClojureCode(env));
		}

		s.append(")");

		return s.toString();
	}

	/**
	 * Wrapper for if
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static final class IfWrapper extends Expression {

		public static final IfWrapper singleton = new IfWrapper();

		@Override
		public Expression interpret(Environment env) {
			return this;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable branchType = new TypeVariable(NameGenerator.next());
			TypeTuple argsType = new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, branchType, branchType));

			return new Pair<Type, Substitution>(new TypeArrow(argsType, branchType), Substitution.EMPTY);
		}

		@Override
		protected String toClojureCode(Environment env) {
			return "if";
		}

		@Override
		public String toString() {
			return "if";
		}

	}
}
