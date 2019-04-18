package expression;

import types.Type;
import types.TypeConcrete;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;

/**
 * Variable expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Variable extends Expression implements Comparable<Expression> {

	/**
	 * Name of the variable
	 */
	public final String name;

	public Variable(String name) {
		this.name = name;
	}

	@Override
	public int compareTo(Expression o) {
		if(o instanceof Variable) {
			Variable other = (Variable)o;
			return this.name.compareTo(other.name);
		}
		return super.compareTo(o);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		if (!env.containsVariable(this)) {
			//throw new Exception("Unbound variable");
			return this; //??
		}
		return env.getVariableValue(this).interpret(env);
	}

	@Override
	public String toString() {
		return this.name;
	}
	
	@Override
	public Map<Expression, Type> getTypeHypothesis(Environment env) throws AppendableException{
		return this.infer(env);
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			
			if(this.typeHypothesis == null) {
				if(env.containsVariable(this)) {
					Expression e = env.getVariableValue(this);
					Map<Expression, Type> tmp = e.infer(env);
					hyp.putAll(tmp);
					hyp.put(this, tmp.get(e));
				}
				else {
					hyp.put(this, new TypeVariable(NameGenerator.next()).quantifyUnconstrainedVariables());
				}
			}
			else {
				hyp.putAll(this.typeHypothesis);
			}
			return hyp;
		}catch(AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		Expression e = topLevel.getVariableValue(this);
		if(e != null){
			return e;
		}
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return this.name;
	}

	/**
	 * Sets type of a variable, used in semantic parser for type annotated variables
	 * @param typeConcrete
	 * @throws AppendableException
	 */
	public void setType(TypeConcrete typeConcrete) throws AppendableException {
		if(this.typeHypothesis == null) {
			this.typeHypothesis = new TreeMap<Expression, Type>();	
		}		
		this.typeHypothesis.put(this, typeConcrete);
	}
}
