package expression;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;
import types.Type;
import util.AppendableException;

public class QuotedExpression extends Expression {

	public final Expression quoted;

	public QuotedExpression(Expression quoted) {
		this.quoted = quoted;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this.quoted;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if(this.typeHypothesis == null) {
			this.typeHypothesis = this.quoted.infer(env);
		}
		hyp.putAll(this.typeHypothesis);
		
		return hyp;
		}catch(AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toString() {
		return "'" + this.quoted.toString();
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new QuotedExpression(
				quoted.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		return this.quoted.toClojureCode();
	}

	@Override
	public int compareTo(Expression other) {
		if(other instanceof QuotedExpression) {
			QuotedExpression o = (QuotedExpression)other;
			return this.quoted.compareTo(o.quoted);
		}
		return super.compareTo(other);
	}
}
