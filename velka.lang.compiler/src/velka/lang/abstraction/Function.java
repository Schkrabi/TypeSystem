package velka.lang.abstraction;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeHolder;
import velka.lang.expression.Symbol;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

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
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = Environment.create(this.creationEnvironment);

			List<Type> l = new LinkedList<Type>();

			Iterator<Type> i = this.argsType.iterator();
			for (Expression e : this.args) {
				if (!(e instanceof Symbol)) {
					throw new AppendableException(e + " is not instance of " + Symbol.class.getName());
				}
				Type t = i.next();
				childEnv.put((Symbol) e, new TypeHolder(t));
				l.add(t);
			}

			Type argsType = new TypeTuple(l);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv, typeEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(bodyInfered.second)),
					bodyInfered.second);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
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
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Abstraction> rankingFunction) throws AppendableException {
		Environment childEnvironment = Abstraction.lexicalClojure(this.args, args, this.creationEnvironment);
		return this.body.interpret(childEnvironment, typeEnv);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}
}
