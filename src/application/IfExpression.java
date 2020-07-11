package application;

import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

import java.util.Arrays;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import literal.LitBoolean;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends SpecialFormApplication {
	
	public IfExpression(Expression condition, Expression trueBranch, Expression falseBranch) {
		super(new Tuple(Arrays.asList(condition, trueBranch, falseBranch)));
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
	public boolean equals(Object other) {
		if (other instanceof IfExpression) {
			return super.equals(other);
		}
		return false;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> condInfered = this.getCondition().infer(env);
		Pair<Type, Substitution> trueInfered = this.getTrueBranch().infer(env);
		Pair<Type, Substitution> falseInfered = this.getFalseBranch().infer(env);
		
		Substitution condSubstitution = Type.unify(condInfered.first, TypeAtom.TypeBool);
		Substitution branchSubstitution = Type.unify(trueInfered.first, falseInfered.first);
		
		Substitution s = Substitution.EMPTY;
		s = s.union(condInfered.second);
		s = s.union(trueInfered.second);
		s = s.union(falseInfered.second);
		s = s.union(condSubstitution);
		s = s.union(branchSubstitution);
		
		return new Pair<Type, Substitution>(trueInfered.first.apply(s), s);
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException {
		Expression cond = convertedArgs.get(0);
		Expression trueBranch = convertedArgs.get(1);
		Expression falseBranch = convertedArgs.get(2);
		
		StringBuilder s = new StringBuilder("(if ");

		s.append("(get ");
		s.append(cond.toClojureCode(env));
		s.append(" 0)");
		s.append(" ");

		Pair<Type, Substitution> trueType = trueBranch.infer(env);

		s.append(trueBranch.toClojureCode(env));
		s.append(" ");

		Pair<Type, Substitution> falseType = falseBranch.infer(env);

		if (!falseType.first.apply(trueType.second).equals(trueType.first.apply(falseType.second))) {
			s.append(falseType.first.convertTo(falseBranch, trueType.first).toClojureCode(env));
		} else {
			s.append(falseBranch.toClojureCode(env));
		}

		s.append(")");

		return s.toString();
	}

	@Override
	protected String applicatedToString() {
		return "if";
	}

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException {
		LitBoolean cond = (LitBoolean)convertedArgs.get(0).interpret(evaluationEnvironment);
		Expression trueBranch = convertedArgs.get(1);
		Expression falseBranch = convertedArgs.get(2);
		
		if (cond.value) {
			return trueBranch.interpret(evaluationEnvironment);
		} 
		
		Pair<Type, Substitution> retInfered = this.infer(evaluationEnvironment);
		Pair<Type, Substitution> falseInfered = falseBranch.infer(evaluationEnvironment);
		
		if(retInfered.first.equals(falseInfered.first)) {
			return falseBranch.interpret(evaluationEnvironment);
		}
		return falseInfered.first.convertTo(falseBranch, retInfered.first).interpret(evaluationEnvironment);	
	}

	@Override
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		TypeVariable v = new TypeVariable(NameGenerator.next());
		return new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, v, v));
	}
}
