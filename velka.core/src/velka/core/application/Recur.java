package velka.core.application;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for recur special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Recur extends Expression {
	
	public static final String RECUR = "recur";
	
	/**
	 * Values for rebinded values in the loop
	 */
	public final Tuple rebidings;
	
	public Recur(Tuple rebidings) {
		this.rebidings = rebidings;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression abstraction = env.getVariableValue(Loop.RECUR_MARK_SYMBOL);
		AbstractionApplication appl = new AbstractionApplication(abstraction, this.rebidings);
		
		return appl.interpret(env, typeEnv);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<String> args = new LinkedList<String>();
		for(Expression e : this.rebidings) {
			String argCode = e.toClojureCode(env, typeEnv);
			args.add(argCode);
		}
		
		String code = ClojureHelper.applyClojureFunction("recur", args);
		return code;
	}

	@Override
	public int hashCode() {
		return this.rebidings.hashCode();
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Recur) {
			return this.rebidings.equals(((Recur) other).rebidings);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Recur) {
			return this.rebidings.compareTo(((Recur) other).rebidings);
		}
		return super.compareTo(other);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("(");
		sb.append(RECUR);
		sb.append(" ");
		
		Iterator<Expression> i = this.rebidings.iterator();
		while(i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toString());
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append(")");
		return sb.toString();
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}
}
