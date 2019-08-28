package interpretation;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.Variable;
import util.UnboundVariableException;

/**
 * Environment for variable binding during interpretation
 * 
 * @author r.SKRABAL
 *
 */
public class Environment extends TreeMap<Variable, Expression> implements Comparable<Environment> {

	/**
	 * Serial version id
	 */
	private static final long serialVersionUID = -1055970003823419530L;

	/**
	 * Parent environment of this environment
	 */
	public final Environment parent;

	public Environment(Environment parent) {
		this.parent = parent;
	}

	public Environment() {
		this.parent = null;
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
		if (!this.containsKey(var)) {
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
		if (!this.containsKey(var)) {
			if (!this.isTopLevel()) {
				return this.parent.getVariableValue(var);
			}
			throw new UnboundVariableException(var);
		}
		return this.get(var);
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
		int c = (int) Math.signum(this.entrySet().size() - o.entrySet().size());
		if (c != 0)
			return c;

		for (Map.Entry<Variable, Expression> e : this.entrySet()) {
			if (!o.containsKey(e.getKey())) {
				return -1;
			}
			c = o.get(e.getKey()).compareTo(e.getValue());
			if (c != 0) {
				return c;
			}
		}
		for (Map.Entry<Variable, Expression> e : o.entrySet()) {
			if (!this.containsKey(e.getKey())) {
				return 1;
			}
			c = this.get(e.getKey()).compareTo(e.getValue());
			if (c != 0) {
				return c;
			}
		}

		return 0;
	}
}
