package interpretation;

import java.util.HashMap;
import java.util.Map;

import expression.Expression;
import expression.Function;
import expression.Variable;
import operators.Operator;
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
	
	public static void initTopLevelEnvitonment() {
		Environment env = Environment.topLevelEnvironment;
		env.put(new Variable(Operator.Addition.toString()), Operator.Addition);
		env.put(new Variable(Operator.Subtraction.toString()), Operator.Subtraction);
		env.put(new Variable(Operator.Multiplication.toString()), Operator.Multiplication);
		env.put(new Variable(Operator.Division.toString()), Operator.Division);
		env.put(new Variable(Operator.NumericEqual.toString()), Operator.NumericEqual);
		env.put(new Variable(Operator.LesserThan.toString()), Operator.LesserThan);
		env.put(new Variable(Operator.And.toString()), Operator.And);
		env.put(new Variable(Operator.Or.toString()), Operator.Or);
		env.put(new Variable(Operator.Not.toString()), Operator.Not);
		env.put(new Variable(Operator.BitAnd.toString()), Operator.BitAnd);
		env.put(new Variable(Operator.BitOr.toString()), Operator.BitOr);
		env.put(new Variable(Operator.Concantenation.toString()), Operator.Concantenation);
		env.put(new Variable(Operator.Car.toString()), Operator.Car);
		env.put(new Variable(Operator.Cdr.toString()), Operator.Cdr);
		env.put(new Variable("nil"), Expression.EMPTY_EXPRESSION);
		env.put(new Variable(Operator.Equals.toString()), Operator.Equals);
		
		env.put(new Variable("Int"), Function.IntConstructor);
		env.put(new Variable("Int:Native"), Function.IntNativeConstructor);
		env.put(new Variable("Int:String"), Function.IntStringConstructor);
		env.put(new Variable("Int:Roman"), Function.IntRomanConstructor);
		env.put(new Variable("String"), Function.StringConstructor);
		env.put(new Variable("String:Native"), Function.StringNativeConstructor);
		env.put(new Variable("Double"), Function.DoubleConstructor);
		env.put(new Variable("Double:Native"), Function.DoubleNativeConstructor);
		env.put(new Variable("Bool"), Function.BoolConstructor);
		env.put(new Variable("Bool:Native"), Function.BoolNativeConstructor);
	}
	
}
