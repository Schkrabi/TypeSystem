package expression;

import types.Type;
import types.TypeConcrete;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
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
	public Type infer(Environment env) throws AppendableException {
		Type condType = this.condition.infer(env);
		Type tBranchType = this.trueBranch.infer(env);
		Type fBranchType = this.falseBranch.infer(env);

		try {
			if(!Type.unify(tBranchType, fBranchType).isPresent()) {
				throw new TypesDoesNotUnifyException(tBranchType, fBranchType);
			}
			if(!Type.unify(TypeConcrete.TypeBool, condType).isPresent()) {
				throw new TypesDoesNotUnifyException(TypeConcrete.TypeBool, condType);
			}
		}catch(AppendableException e) {
			e.appendMessage("in " + this.toString());
			throw e;
		}

		this.setType(tBranchType);

		return tBranchType;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IfExpression(
				this.condition.substituteTopLevelVariables(topLevel),
				this.trueBranch.substituteTopLevelVariables(topLevel),
				this.falseBranch.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder();
		s.append("(if ");
		s.append(this.condition.toClojureCode());
		s.append(' ');
		s.append(this.trueBranch.toClojureCode());
		s.append(' ');
		s.append(this.falseBranch.toClojureCode());
		s.append(')');
		return s.toString();
	}
}
