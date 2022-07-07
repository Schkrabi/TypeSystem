package velka.core.abstraction;

import java.util.Iterator;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for representation of interpreted function
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Function extends Lambda implements Comparable<Expression> {

	public final Environment creationEnvironment;

	public Function(TypeTuple argsType, Tuple args, Expression body, Environment createdEnvironment) {
		super(args, argsType, body);
		this.creationEnvironment = createdEnvironment;
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		return this.doInferWithArgs(args, this.creationEnvironment, env, typeEnv);
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Function) {
			int cmp = this.argsType.compareTo(((Function) other).argsType);
			if (cmp != 0)
				return cmp;
			cmp = this.args.compareTo(((Function) other).args);
			if (cmp != 0)
				return cmp;
			cmp = this.body.compareTo(((Function) other).body);
			if (cmp != 0)
				return cmp;
			return this.creationEnvironment.compareTo(((Function) other).creationEnvironment);
		}
		return super.compareTo(other);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(FunctionInternal (");

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();

			if (t instanceof TypeVariable) {
				s.append(e.toString());
			} else {
				s.append('(');
				s.append(t.toString());
				s.append(' ');
				s.append(e.toString());
				s.append(')');
			}
		}

		s.append(") ");
		s.append(this.body.toString());
		s.append(' ');
		s.append(this.creationEnvironment);
		s.append(')');

		return s.toString();
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Function) {
			return this.creationEnvironment.equals(((Function) other).creationEnvironment) && super.equals(other);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.argsType.hashCode() * this.args.hashCode() * this.body.hashCode();
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Environment childEnvironment = Abstraction.lexicalClojure(this.args, args, this.creationEnvironment);
		return this.body.interpret(childEnvironment, typeEnv);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}
	
	@Override
	public Lambda defaultCostFunction() throws AppendableException {
		Lambda l = super.defaultCostFunction();
		return new Function(l.argsType, l.args, l.body, this.creationEnvironment);
	}
}
