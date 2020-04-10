package abstraction;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import expression.Expression;
import expression.Tuple;
import expression.TypeHolder;
import expression.Symbol;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import interpretation.Environment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends Abstraction implements Comparable<Expression> {

	/**
	 * Formal arguments (names) of the lambda expression
	 */
	public final Tuple args;

	/**
	 * Body
	 */
	public final Expression body;

	/**
	 * Non mandatory type of the lambda arguments
	 */
	public final TypeTuple argsType;
	
	/**
	 * General identity lambda
	 */
	public static final Lambda identity = Lambda.makeIdentity(new TypeVariable(NameGenerator.next()));

	public Lambda(Tuple args, TypeTuple argsType, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = argsType;
	}

	@Override
	public Expression interpret(Environment env) {
		return new Function(this.argsType, this.args, this.body, env);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(lambda (");

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
		s.append(')');

		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = Environment.create(env);
			List<Type> argsTypeArr = new LinkedList<Type>();

			for (Expression e : this.args) {
				if (!(e instanceof Symbol)) {
					// TODO change throwable
					throw new AppendableException(e + " is not instance of " + Symbol.class.getName());
				}
				TypeVariable tv = new TypeVariable(NameGenerator.next());
				childEnv.put((Symbol) e, new TypeHolder(tv));
				argsTypeArr.add(tv);
			}

			Type argsType = new TypeTuple(argsTypeArr);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			// Now check if body was typed correctly according to user defined types of
			// arguments
			Substitution s = Type.unify(argsType, this.argsType);

			// Compose all substitutions in order to check if there are no collisions and
			// provide final substitution
			Substitution finalSubst = s.union(bodyInfered.second);

			argsType = argsType.apply(finalSubst);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(finalSubst)),
					finalSubst);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("`(");
		s.append("[");
		s.append(this.argsType.toClojureKey());
		s.append(" ~");
		s.append(this.toClojureFn(env));
		s.append("])");
		return s.toString();
	}

	/**
	 * Creates code of this lambda as simple lambda in Clojure (e.g. (fn [x] x))
	 * 
	 * @param expectedType expected type of this lambda
	 * @param env          environment where lambda is evaluated
	 * @return string containing lcojure code
	 * @throws AppendableException Thrown on unification error or when any argument
	 *                             is not a variable
	 */
	protected String toClojureFn(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append("(fn [");

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		Environment child = Environment.create(env);
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			if (!(e instanceof Symbol)) {
				// TODO change throwable
				throw new AppendableException("Invalid expression in lambda variable list!");
			}
			Symbol v = (Symbol) e;
			child.put(v, new TypeHolder(t));
			s.append(v.toClojureCode());
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append("] ");
		
		s.append(this.body.toClojureCode(child));
		s.append(')');
		return s.toString();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Lambda) {
			int cmp = this.args.compareTo(((Lambda) other).args);
			if (cmp != 0) {
				return cmp;
			}

			cmp = this.argsType.compareTo(((Lambda) other).argsType);
			if (cmp != 0) {
				return cmp;
			}

			return this.body.compareTo(((Lambda) other).body);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Lambda) {
			return this.args.equals(((Lambda) other).args) && this.body.equals(((Lambda) other).body)
					&& this.argsType.equals(((Lambda) other).argsType);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.args.hashCode() * this.argsType.hashCode() * this.body.hashCode();
	}

	@Override
	public TypeArrow getFunctionTypeWithRepresentations(TypeTuple argTypes, Environment env)
			throws AppendableException {
		return (TypeArrow) this.infer(env).first;
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException{
		Function f = (Function)this.interpret(env);
		return f.doSubstituteAndEvaluate(args, env);
	}
	
	/**
	 * Makes identity lambda with given type
	 * @param argType type of the identity arg
	 * @return identity lambda
	 */
	public static Lambda makeIdentity(Type argType) {
		Symbol symbol = new Symbol(NameGenerator.next());
		return new Lambda(new Tuple(Arrays.asList(symbol)), new TypeTuple(Arrays.asList(argType)), symbol);
	}
}
