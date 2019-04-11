package expression;

import types.Type;
import types.TypeConcrete;
import types.TypesDoesNotUnifyException;
import util.AppendableException;

import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

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
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			
			if(this.typeHypothesis != null) {
				Map<Expression, Type> tmp = new TreeMap<Expression, Type>();
				
				Map<Expression, Type> condInfer = this.condition.infer(env);
				Map<Expression, Type> tBranchInfer = this.trueBranch.infer(env);
				Map<Expression, Type> fBranchInfer = this.falseBranch.infer(env);
				
				Optional<Type> o = Type.unify(tBranchInfer.get(this.trueBranch), fBranchInfer.get(this.falseBranch));
				if(!o.isPresent()) {
					throw new TypesDoesNotUnifyException(tBranchInfer.get(this.trueBranch), fBranchInfer.get(this.falseBranch));
				}
				
				Optional<Type> t = Type.unify(TypeConcrete.TypeBool, condInfer.get(this.condition));
				if(!t.isPresent()) {
					throw new TypesDoesNotUnifyException(TypeConcrete.TypeBool, condInfer.get(this.condition));
				}
				
				tmp.putAll(condInfer);
				tmp.putAll(tBranchInfer);
				tmp.putAll(fBranchInfer);
				tmp.put(this,  o.get());
				
				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		}catch(AppendableException e) {
			e.appendMessage("in " + e);
			throw e;
		}
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
