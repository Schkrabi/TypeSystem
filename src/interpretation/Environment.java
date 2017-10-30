package interpretation;

import java.util.TreeMap;

import expression.Expression;
import expression.Variable;

/**
 * Environment for variable binding during interpretation
 * 
 * @author r.SKRABAL
 *
 */
public class Environment extends TreeMap<Variable, Expression> {

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
	 * @param var
	 *            searched variable
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
	 * binding exists returns null
	 * 
	 * @param var
	 *            searched variable
	 * @return Expression object or null
	 */
	public Expression getVariableValue(Variable var) {
		if (!this.containsKey(var)) {
			if (!this.isTopLevel()) {
				return this.parent.getVariableValue(var);
			}
			return null;
		}
		return this.get(var);
	}
}
