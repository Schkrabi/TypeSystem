package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
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
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();

			if (this.typeHypothesis == null) {
				Map<Expression, Type> tmp = new TreeMap<Expression, Type>();

				Map<Expression, Type> inferedArgs = this.args.infer(new Environment());
				Map<Expression, Type> inferedBody = this.body.infer(env);

				tmp.putAll(inferedBody);

				for (Map.Entry<Expression, Type> e : inferedArgs.entrySet()) {
					if(inferedBody.containsKey(e.getKey())) {
						Optional<Type> t = Type.unify(e.getValue(), inferedBody.get(e.getKey()));
	
						if (!t.isPresent()) {
							throw new TypesDoesNotUnifyException(e.getValue(), inferedBody.get(e.getKey()));
						}
	
						tmp.put(e.getKey(), t.get());
					}
					else {
						tmp.put(e.getKey(), e.getValue());
					}
				}

				if (this.argsType != null) {
					Optional<Type> o = Type.unify(this.argsType, tmp.get(this.args));

					if (!o.isPresent()) {
						throw new TypesDoesNotUnifyException(this.argsType, inferedBody.get(this.args));
					}

					tmp.put(this.args, o.get());
				}

				tmp.put(this, new TypeArrow(tmp.get(this.args), tmp.get(this.body).quantifyUnconstrainedVariables()));
				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
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
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Environment e = new Environment(topLevel);
		// Mask locally redefined variables
		for (Expression expr : this.args) {
			e.put((Variable) expr, expr);
		}

		return new Lambda(this.args, this.argsType, this.body.substituteTopLevelVariables(e));
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
