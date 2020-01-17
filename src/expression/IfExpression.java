package expression;

import types.RepresentationOr;
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
	 * @return expression
	 */
	protected Expression getCondition() {
		return this.args.get(0);
	}
	
	/**
	 * Gets true branch of this ifExpression
	 * @return expression
	 */
	protected Expression getTrueBranch() {
		return this.args.get(1);
	}
	
	/**
	 * Gets false branch of this ifExpression
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
	public String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(if ");
		
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		s.append(this.getCondition().toClojureCode(TypeAtom.TypeBoolNative, env));
		s.append(" ");
		s.append(this.getTrueBranch().toClojureCode(tv, env));
		s.append(" ");
		s.append(this.getFalseBranch().toClojureCode(tv, env));
		s.append(")");
		
		return s.toString();
	}
	
	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException{		
		Pair<Type, Substitution> condInfered = this.getCondition().infer(env);
		Pair<Type, Substitution> trueInfered = this.getTrueBranch().infer(env);
		Pair<Type, Substitution> falseInfered = this.getFalseBranch().infer(env);
		
		Substitution condSubst = condInfered.second.union(Type.unify(condInfered.first, TypeAtom.TypeBoolNative));
		Substitution branchSubst = trueInfered.second.union(falseInfered.second).union(Type.unify(trueInfered.first, falseInfered.first));
		
		Type trueFinal = trueInfered.first.apply(branchSubst);
		Type falseFinal = falseInfered.first.apply(branchSubst);
		
		return new Pair<Type, Substitution>(RepresentationOr.makeRepresentationOr(Arrays.asList(trueFinal, falseFinal)),
					condSubst.union(branchSubst));
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
		public String toClojureCode() {
			return this.toClojureCode(null, Environment.topLevelEnvironment);
		}
		
		@Override
		protected String toClojureCode(Type expectedType, Environment env) {
			return "if";
		}
		
		@Override
		public String toString() {
			return "if";
		}

	}
}
