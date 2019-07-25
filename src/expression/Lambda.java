package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
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
public class Lambda extends MetaLambda implements Comparable<Expression> {

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

	public Lambda(Variable arg, Expression body) {
		this.args = new Tuple(new Expression[] { arg });
		this.body = body;
		this.argsType = null;
	}

	public Lambda(Tuple args, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = null;
	}

	public Lambda(Tuple args, TypeTuple argsType, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = argsType;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Function f = new Function(this.argsType, this.args, this.body, env);
		f.infer(env);
		return f;
	}

	@Override
	public String toString() {
		return "lambda " + this.args.toString() + " " + this.body.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = new Environment(env);
			Type[] argsTypeArr = new Type[this.args.values.length];

			for (int i = 0; i < this.args.values.length; i++) {
				Expression e = this.args.values[i];
				if (!(e instanceof Variable)) {
					throw new AppendableException(e + " is not instance of " + Variable.class.getName());
				}
				TypeVariable tv = new TypeVariable(NameGenerator.next());
				childEnv.put((Variable) e, new TypeHolder(tv));
				argsTypeArr[i] = tv;
			}

			Type argsType = new TypeTuple(argsTypeArr);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			// Now check if body was typed correctly according to user defined types of
			// arguments
			Optional<Substitution> s = Type.unify(argsType, this.argsType);

			if (!s.isPresent()) {
				throw new TypesDoesNotUnifyException(argsType, this.argsType);
			}

			// Compose all substitutions in order to check if there are no collisions and
			// provide final substitution
			Substitution finalSubst = s.get().compose(bodyInfered.second);

			argsType = argsType.apply(finalSubst);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(finalSubst)),
					finalSubst);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder();
		s.append("(fn [");

		Iterator<Expression> i = this.args.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			if (!(e instanceof Variable)) {
				throw new Exception("Invalid expression in lambda variable list!");
			}
			Variable v = (Variable) e;
			s.append(v.toClojureCode());
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append("] ");
		s.append(this.body.toClojureCode());
		s.append(')');
		return s.toString();
	}

	@Override
	public int compareTo(Expression other) {
		if(other instanceof Lambda) {
			Lambda o = (Lambda)other;
			if (this.argsType == o.argsType) {
				return 0;
			}
			if (this.argsType == null) {
				return 1;
			}
			if (o.argsType == null) {
				return -1;
			}
	
			return this.argsType.compareTo(o.argsType);
		}
		return super.compareTo(other);
	}

	@Override
	public Lambda getLambda(Comparator<? super Lambda> c) {
		return this;
	}

	@Override
	public Lambda getLambda() {
		return this;
	}
}
