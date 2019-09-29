package interpretation;

import java.util.HashMap;
import java.util.Map;

import expression.Expression;
import expression.Variable;
import util.AppendableException;
import util.UnboundVariableException;

/**
 * Environment for variable binding during interpretation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Environment implements Comparable<Environment> {

	/**
	 * Parent environment of this environment
	 */
	public final Environment parent;
	
	private Map<Variable, Expression> bindings = new HashMap<Variable, Expression>();

	protected Environment(Environment parent) {
		this.parent = parent;
	}
	
	protected Environment(Environment parent, Environment initFrom) {
		this.parent = parent;
		this.bindings.putAll(initFrom.bindings);
	}

	protected Environment() {
		this.parent = null;
	}
	
	/**
	 * Adds binding to environment
	 * @param v bounded variable
	 * @param e bounded value
	 */
	public void put(Variable v, Expression e) {
		this.bindings.put(v, e);
	}
	
	@Override
	public String toString() {
		return this.bindings.keySet().toString();
	}

	/**
	 * Returns true if this is top level environment (has no parent environment),
	 * otherwise returns false
	 * 
	 * @return true or false
	 */
	public boolean isTopLevel() {
		return this.parent == null;
	}

	/**
	 * Returns true if the environment hierarchy contains binding for this variable,
	 * otherwise returns false
	 * 
	 * @param var searched variable
	 * @return true or false
	 */
	public boolean containsVariable(Variable var) {
		if (!this.bindings.containsKey(var)) {
			if (!this.isTopLevel()) {
				return this.parent.containsVariable(var);
			}
			return false;
		}
		return true;
	}

	/**
	 * Returns closest binding of the given variable in environment hierarchy. If no
	 * binding exists throws.
	 * 
	 * @param var searched variable
	 * @return Expression object
	 * @throws UnboundVariableException 
	 */
	public Expression getVariableValue(Variable var) throws UnboundVariableException {
		if (!this.bindings.containsKey(var)) {
			if (!this.isTopLevel()) {
				return this.parent.getVariableValue(var);
			}
			throw new UnboundVariableException(var);
		}
		return this.bindings.get(var);
	}

	@Override
	public int compareTo(Environment o) {
		if (this.parent != null || o.parent != null) {
			if (this.parent == null) {
				return -1;
			}
			if (o.parent == null) {
				return 1;
			}
			if (this.parent != o.parent) {
				return this.parent.compareTo(o.parent);
			}
		}
		int c;

		for (Map.Entry<Variable, Expression> e : this.bindings.entrySet()) {
			if (!o.bindings.containsKey(e.getKey())) {
				return -1;
			}
			c = o.bindings.get(e.getKey()).compareTo(e.getValue());
			if (c != 0) {
				return c;
			}
		}
		for (Map.Entry<Variable, Expression> e : o.bindings.entrySet()) {
			if (!this.bindings.containsKey(e.getKey())) {
				return 1;
			}
		}

		return 0;
	}
	
	/**
	 * Top level environment, only one exists
	 */
	public static Environment topLevelEnvironment = new Environment();
	
	/**
	 * Construction method for new environments
	 * @param parent
	 * @return new Environment instance
	 * @throws AppendableException
	 */
	public static Environment create(Environment parent) throws AppendableException {
		if(parent == null) {
			throw new AppendableException("Cannot create environment with null parent!");
		}
		return new Environment(parent);
	}
	
	/**
	 * Construction methods for new environments initailizing from previously existing environments
	 * @param parent
	 * @param initFrom
	 * @return new Environment instance
	 * @throws AppendableException
	 */
	public static Environment create(Environment parent, Environment initFrom) throws AppendableException {
		if(parent == null) {
			throw new AppendableException("Cannot create environment with null parent!");
		}
		return new Environment(parent, initFrom);
	}
	
}
