package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends Expression {

	/**
	 * Condition of the if expression
	 */
	public final Expression condition;
	/**
	 * Branch for interpretation for the true condition
	 */
	public final Expression trueBranch;
	/**
	 * Branch for interpretation for the false condition
	 */
	public final Expression falseBranch;

	public IfExpression(Expression condition, Expression trueBranch,
			Expression falseBranch) {
		this.condition = condition;
		this.trueBranch = trueBranch;
		this.falseBranch = falseBranch;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		LitBoolean b = (LitBoolean) this.condition.interpret(env);
		if (b.value) {
			return this.trueBranch.interpret(env);
		} else {
			return this.falseBranch.interpret(env);
		}
	}

	@Override
	public String toString() {
		return "if " + this.condition.toString() + " then "
				+ this.trueBranch.toString() + " else "
				+ this.falseBranch.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type condType = this.condition.infer();
		Type tBranchType = this.trueBranch.infer();
		Type fBranchType = this.falseBranch.infer();

		if (!Type.unify(tBranchType, fBranchType)) {
			throw new Exception("Types of if branches do to unify, got: "
					+ tBranchType + " " + fBranchType);
		}

		if (!Type.unify(TypeConcrete.TypeBool, condType)) {
			throw new Exception("Condition of if do not unify with Bool got: "
					+ condType);
		}

		this.setType(tBranchType);

		return tBranchType;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return new IfExpression(
				this.condition.substituteTopLevelVariables(topLevel),
				this.trueBranch.substituteTopLevelVariables(topLevel),
				this.falseBranch.substituteTopLevelVariables(topLevel));
	}
}
