package expression;

import java.util.Iterator;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import interpretation.Environment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends Expression implements Comparable<Lambda>{

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
		return this;
	}

	@Override
	public String toString() {
		return "lambda " + this.args.toString() + " " + this.body.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type inferedArgsType = this.args.infer();
		Type bodyType = this.body.infer();

		if (this.argsType != null && !Type.unify(this.argsType, inferedArgsType)) {
			throw new Exception("Infered arguments type " + inferedArgsType + " do not unify with specified args type "
					+ this.argsType + " in " + this);
		}

		Type t = new TypeArrow(this.argsType == null ? inferedArgsType : this.argsType, bodyType.getRep());

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
}
