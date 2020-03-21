package application;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import literal.LitBoolean;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.AppendableException;
import util.Pair;
import util.ThrowingFunction;

/**
 * Expression for And special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class AndExpression extends Application {

	public AndExpression(Tuple args) {
		super(AndWrapper.singleton, args);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		try {
			if (this.args.equals(Tuple.EMPTY_TUPLE)) {
				return LitBoolean.TRUE;
			}

			List<LitBoolean> l = this.args.stream().map(ThrowingFunction.wrapper(x -> (LitBoolean) x.interpret(env)))
					.collect(Collectors.toList());

			for (LitBoolean b : l) {
				if (!b.value) {
					return LitBoolean.FALSE;
				}
			}

			return LitBoolean.TRUE;

		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			e.appendMessage(" in " + this);
			throw e;
		}
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Substitution agg = Substitution.EMPTY;
		for (Expression e : this.args) {
			Pair<Type, Substitution> p = e.infer(env);
			Substitution s = Type.unify(p.first, TypeAtom.TypeBoolNative);
			agg = agg.union(p.second).union(s);
		}

		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, agg);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(");
		s.append(AndWrapper.singleton.toClojureCode());
		s.append(' ');

		Iterator<Expression> i = this.args.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			s.append(e.toClojureCode(env));
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append(')');
		return s.toString();
	}

	/**
	 * Wrapper class for And expression
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class AndWrapper extends Expression {

		public static AndWrapper singleton = new AndWrapper();

		private AndWrapper() {
		};

		@Override
		public Expression interpret(Environment env) throws AppendableException {
			return this;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			throw new AppendableException(this.getClass().getName() + " should not be infered directly "
					+ AndExpression.class.getName() + " should be used for inference");
		}

		@Override
		public String toClojureCode(Environment env) {
			return "and";
		}

		@Override
		public String toString() {
			return "and";
		}
	}
}
