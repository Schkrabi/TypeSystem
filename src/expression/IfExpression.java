package expression;

import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import types.TypeVariable;
import util.NameGenerator;
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
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		return new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeBool, tv, tv}), tv);
	}
}
