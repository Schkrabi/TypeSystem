package velka.core.application;

import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

import java.util.Arrays;
import java.util.Optional;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends SpecialFormApplication {
	
	/**
	 * Symbol for special form if
	 */
	public static final String IF = "if";
	
	public IfExpression(Expression condition, Expression trueBranch, Expression falseBranch) {
		super(new Tuple(Arrays.asList(condition, trueBranch, falseBranch)));
	}

	/**
	 * Gets condition of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getCondition() {
		return ((Tuple)this.args).get(0);
	}

	/**
	 * Gets true branch of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getTrueBranch() {
		return ((Tuple)this.args).get(1);
	}

	/**
	 * Gets false branch of this ifExpression
	 * 
	 * @return
	 */
	protected Expression getFalseBranch() {
		return ((Tuple)this.args).get(2);
	}
	
	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression iCond = this.getCondition().interpret(env, typeEnv);
		if(!(iCond instanceof LitBoolean)) {
			iCond = iCond.convert(TypeAtom.TypeBoolNative, env, typeEnv);
			iCond = iCond.interpret(env, typeEnv);
		}
		
		if(!(iCond instanceof LitBoolean)) {
			throw new AppendableException(iCond + " not a LitBoolean");
		}
		LitBoolean cond = (LitBoolean)iCond;
		if(cond.value) {
			return this.getTrueBranch().interpret(env, typeEnv);
		}
		
		Expression iFalse = this.getFalseBranch().interpret(env, typeEnv);
		Pair<Type, Substitution> inf = this.infer(env, typeEnv);
		
		iFalse = iFalse.convert(inf.first, env, typeEnv);
		iFalse = iFalse.interpret(env, typeEnv);
		return iFalse;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> argsInfered = this.args.infer(env, typeEnv);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		Type argsExpected = new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, tv, tv));
		
		Optional<Substitution> s = Type.unifyTypes(argsInfered.first, argsExpected);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(argsInfered.first, argsExpected);
		}

		Substitution composed = s.get().compose(argsInfered.second);
		
		return new Pair<Type, Substitution>(tv.apply(composed), composed);
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String cond = null;
		var ct = this.getCondition().infer(env, typeEnv).first;
		if(ct.equals(TypeAtom.TypeBoolNative)) {
			cond = this.getCondition().toClojureCode(env, typeEnv);
		}
		else {
			cond = ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full, 
					this.getCondition().toClojureCode(env, typeEnv));
		}
		
		var code = ClojureHelper.clojureIfHelper(
				cond, 
				this.getTrueBranch().toClojureCode(env, typeEnv), 
				this.getFalseBranch().toClojureCode(env, typeEnv));
		
		return code;
	}

	@Override
	protected String applicatedToString() {
		return IF;
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof IfExpression) {
			return super.equals(other);
		}
		return false;
	}
}
