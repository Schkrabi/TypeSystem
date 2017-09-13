package interpretation;

import java.util.TreeMap;

import expression.Expression;
import expression.Variable;

public class Environment extends TreeMap<Variable, Expression> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1055970003823419530L;

	public final Environment parent;
	
	public Environment(Environment parent) {
		this.parent = parent;
	}
	
	public Environment() {
		this.parent = null;
	}
	
	public boolean isTopLevel() {
		return this.parent == null;
	}
	
	public boolean containsVariable(Variable var) {
		if(!this.containsKey(var)) {
			if(!this.isTopLevel()) {
				return this.parent.containsVariable(var);
			}
			return false;
		}
		return true;
	}
	
	public Expression getVariableValue(Variable var) {
		if(!this.containsKey(var)) {
			if(!this.isTopLevel()) {
				return this.parent.getVariableValue(var);
			}
			return null;
		}
		return this.get(var);
	}
}
