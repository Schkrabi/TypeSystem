package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class IfExpression extends Expression {
	
	public final Expression condition;
	public final Expression trueBranch;
	public final Expression falseBranch;
	
	public IfExpression(Expression condition, Expression trueBranch, Expression falseBranch) {
		this.condition = condition;
		this.trueBranch = trueBranch;
		this.falseBranch = falseBranch;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		LitBoolean b = (LitBoolean)this.condition.interpret(env);
		if(b.value) {
			return this.trueBranch.interpret(env);
		}
		else {
			return this.falseBranch.interpret(env);
		}
	}

	@Override
	public String toString() {
		return "if " + this.condition.toString() + " then " + this.trueBranch.toString() + " else " + this.falseBranch.toString();
	}

	@Override
	public Type infer() throws Exception {		
		Type condType = this.condition.infer();
		Type tBranchType = this.trueBranch.infer();
		Type fBranchType = this.falseBranch.infer();
		
		if(!Type.unify(tBranchType, fBranchType)) {
			throw new Exception("Types of if branches do to unify, got: " + tBranchType + " " + fBranchType);
		}
		
		if(!Type.unify(TypeConcrete.TypeBool, condType)) {
			throw new Exception("Condition of if do not unify with Bool got: " + condType);
		}
		
		return tBranchType;
	}
}
