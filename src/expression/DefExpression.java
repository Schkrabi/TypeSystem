package expression;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;
import types.Type;
import types.TypeTuple;
import util.AppendableException;

/**
 * Expression for interpretation and handling the define special form. Purely
 * for purposes of simplifying the code.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefExpression extends Expression {

	/**
	 * Name of defined expression
	 */
	public final Variable name;
	/**
	 * Defined expression
	 */
	public final Expression defined;

	public DefExpression(Variable name, Expression defined) {
		this.name = name;
		this.defined = defined;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression interpreted = this.defined.interpret(env);
		env.put(this.name, interpreted);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if(this.typeHypothesis == null) {
				Map<Expression, Type> tmp = this.defined.infer(env);
				tmp.put(this, TypeTuple.EMPTY_TUPLE);
				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);
			
			return hyp;
		}catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Environment e = new Environment(topLevel);
		e.put(this.name, this.defined);
		return new DefExpression(this.name, this.defined.substituteTopLevelVariables(e));
	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder("(def ");
		s.append(this.name.toClojureCode());
		s.append(" ");
		s.append(this.defined.toClojureCode());
		s.append(")");
		return s.toString();
	}

	@Override
	public String toString() {
		return "(define " + this.name.toString() + " " + this.defined.toString() + ")"; 
	}
}
