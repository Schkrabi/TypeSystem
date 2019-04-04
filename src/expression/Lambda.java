package expression;

import java.util.Comparator;
import java.util.Iterator;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import interpretation.Environment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends MetaLambda implements Comparable<Lambda>{

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
	public Type infer(Environment env) throws AppendableException {
		Type inferedArgsType = this.args.infer(new Environment());
		Type bodyType = this.body.infer(env);

		if(this.argsType != null) {
			try {
				if(!Type.unify(this.argsType, inferedArgsType).isPresent()) {
					throw new TypesDoesNotUnifyException(this.argsType, inferedArgsType);
				}
			}catch(AppendableException e) {
				e.appendMessage("in " + this.toString());
				throw e;
			}
		}

		Type t = new TypeArrow(this.argsType == null ? inferedArgsType : this.argsType, bodyType);

		for (TypeVariable v : t.getUnconstrainedVariables()) {
			t = new ForallType(v, t);
		}

		this.setType(t);

		return t;
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
	public int compareTo(Lambda o) {
		if(this.argsType == o.argsType) {
			return 0; 
		}
		if(this.argsType == null) {
			return 1;
		}
		if(o.argsType == null) {
			return -1;
		}
		
		return this.argsType.compareTo(o.argsType);
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
