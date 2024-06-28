package velka.core.interpretation;

import java.util.HashMap;
import java.util.Map;

import velka.core.exceptions.UnboundVariableException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.types.typeSystem.TypeSystem;
import velka.util.AppendableException;

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

	private Map<Symbol, Expression> bindings = new HashMap<Symbol, Expression>();

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
	 * 
	 * @param v bounded variable
	 * @param e bounded value
	 */
	public void put(Symbol v, Expression e) {
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
	public boolean containsVariable(Symbol var) {
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
	public Expression getVariableValue(Symbol var) throws UnboundVariableException {
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

		for (Map.Entry<Symbol, Expression> e : this.bindings.entrySet()) {
			if (!o.bindings.containsKey(e.getKey())) {
				return -1;
			}
			c = o.bindings.get(e.getKey()).compareTo(e.getValue());
			if (c != 0) {
				return c;
			}
		}
		for (Map.Entry<Symbol, Expression> e : o.bindings.entrySet()) {
			if (!this.bindings.containsKey(e.getKey())) {
				return 1;
			}
		}

		return 0;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Environment) {
			if (this.parent != null) {
				return this.parent.equals(((Environment) other).parent)
						&& this.bindings.equals(((Environment) other).bindings);
			}
			return this.parent == ((Environment) other).parent && this.bindings.equals(((Environment) other).bindings);
		}
		return false;
	}

	@Override
	public int hashCode() {
		if (this.parent != null)
			return this.parent.hashCode() * ((Integer) this.bindings.size()).hashCode();
		return ((Integer) this.bindings.size()).hashCode();
	}

	/**
	 * Construction method for new environments
	 * 
	 * @param parent
	 * @return new Environment instance
	 * @throws AppendableException
	 */
	public static Environment create(Environment parent) throws AppendableException {
		if (parent == null) {
			throw new AppendableException("Cannot create environment with null parent!");
		}
		return new Environment(parent);
	}

	/**
	 * Construction methods for new environments initailizing from previously
	 * existing environments
	 * 
	 * @param parent
	 * @param initFrom
	 * @return new Environment instance
	 * @throws AppendableException
	 */
	public static Environment create(Environment parent, Environment initFrom) throws AppendableException {
		if (parent == null) {
			throw new AppendableException("Cannot create environment with null parent!");
		}
		return new Environment(parent, initFrom);
	}

	public TypeSystem getTypeSystem() {
		if(this.parent == null) {
			throw new RuntimeException("Missing top level environment");
		}
		
		return this.parent.getTypeSystem();
	}
}
