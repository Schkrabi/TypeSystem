package velka.lang.interpretation;

import java.util.HashMap;
import java.util.Map;

import velka.lang.abstraction.Operator;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.langbase.JavaArrayList;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.exceptions.UnboundVariableException;

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

	public static Environment initTopLevelEnvitonment() {
		Environment env = new Environment();
		env.put(new Symbol(Operator.Addition.toString()), Operator.Addition);
		env.put(new Symbol(Operator.Subtraction.toString()), Operator.Subtraction);
		env.put(new Symbol(Operator.Multiplication.toString()), Operator.Multiplication);
		env.put(new Symbol(Operator.Division.toString()), Operator.Division);
		env.put(new Symbol(Operator.NumericEqual.toString()), Operator.NumericEqual);
		env.put(new Symbol(Operator.LesserThan.toString()), Operator.LesserThan);
		env.put(new Symbol(Operator.Not.toString()), Operator.Not);
		env.put(new Symbol(Operator.BitAnd.toString()), Operator.BitAnd);
		env.put(new Symbol(Operator.BitOr.toString()), Operator.BitOr);
		env.put(new Symbol(Operator.Concantenation.toString()), Operator.Concantenation);
		env.put(new Symbol(Operator.Car.toString()), Operator.Car);
		env.put(new Symbol(Operator.Cdr.toString()), Operator.Cdr);
		env.put(new Symbol("nil"), Expression.EMPTY_EXPRESSION);
		env.put(new Symbol(Operator.Equals.toString()), Operator.Equals);
		env.put(new Symbol(Operator.PrintlnOperator.toString()), Operator.PrintlnOperator);
		env.put(new Symbol(Operator.CanUnifyRepresentations.toString()), Operator.CanUnifyRepresentations);
		env.put(new Symbol(Operator.CanUnifyTypes.toString()), Operator.CanUnifyTypes);
		env.put(new Symbol(Operator.IsSameType.toString()), Operator.IsSameType);
		env.put(new Symbol(Operator.IsSameRepresentation.toString()), Operator.IsSameRepresentation);
		
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman)),
				Operator.IntNativeToIntRoman);
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntNative, TypeAtom.TypeIntString)),
				Operator.IntNativeToIntString);
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative)),
				Operator.IntRomanToIntNative);
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString)),
				Operator.IntRomanToIntString);
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntString, TypeAtom.TypeIntNative)),
				Operator.IntStringToIntNative);
		env.put(new Symbol(TypeEnvironment.makeConversionName(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman)),
				Operator.IntStringToIntRoman);
		
		JavaArrayList.initializeInEnvironment(env);
		
		return env;
	}

}
