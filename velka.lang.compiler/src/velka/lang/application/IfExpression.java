package velka.lang.application;

import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

import java.util.Arrays;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression iCond = this.getCondition().interpret(env, typeEnv);
		if(!(iCond instanceof LitBoolean)) {
			Pair<Type, Substitution> inf = iCond.infer(env, typeEnv);
			iCond = Conversions.convert(inf.first, iCond, TypeAtom.TypeBoolNative, typeEnv);
			iCond = iCond.interpret(env, typeEnv);
		}
		LitBoolean cond = (LitBoolean)iCond;
		if(cond.value) {
			return this.getTrueBranch().interpret(env, typeEnv);
		}
		
		Expression iFalse = this.getFalseBranch().interpret(env, typeEnv);
		Pair<Type, Substitution> inf = this.infer(env, typeEnv);
		Pair<Type, Substitution> fInf = iFalse.infer(env, typeEnv);
		
		iFalse = Conversions.convert(fInf.first, iFalse, inf.first, typeEnv);
		iFalse = iFalse.interpret(env, typeEnv);
		return iFalse;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> argsInfered = this.args.infer(env, typeEnv);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		Type argsExpected = new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, tv, tv));
		
		Substitution s = Type.unifyTypes(argsInfered.first, argsExpected); 
		s = s.union(argsInfered.second);
		
		return new Pair<Type, Substitution>(tv.apply(s), s);
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression cond = convertedArgs.get(0);
		Expression trueBranch = convertedArgs.get(1);
		Expression falseBranch = convertedArgs.get(2);
		
		StringBuilder s = new StringBuilder("(if ");

		s.append("(get ");
		s.append(cond.toClojureCode(env, typeEnv));
		s.append(" 0)");
		s.append(" ");

		Pair<Type, Substitution> trueType = trueBranch.infer(env, typeEnv);

		s.append(trueBranch.toClojureCode(env, typeEnv));
		s.append(" ");

		Pair<Type, Substitution> falseType = falseBranch.infer(env, typeEnv);

		if (!falseType.first.apply(trueType.second).equals(trueType.first.apply(falseType.second))) {
			s.append(Conversions.convert(falseType.first, falseBranch, trueType.first, typeEnv).toClojureCode(env, typeEnv));
		} else {
			s.append(falseBranch.toClojureCode(env, typeEnv));
		}

		s.append(")");

		return s.toString();
	}

	@Override
	protected String applicatedToString() {
		return "if";
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof IfExpression) {
			return super.equals(other);
		}
		return false;
	}
}
