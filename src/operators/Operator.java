package operators;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.Function;
import expression.LitBoolean;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;

/**
 * Expression for meta-language operators 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Operator extends Function {

	/**
	 * Symbol for the operator in language
	 */
	public final String symbol;
	
	public final String clojureSymbol;

	public Operator(TypeTuple argsType, Tuple args, String symbol, String clojureSymbol, OperatorWrapper body) {
		super(argsType, args, body, new Environment());
		this.symbol = symbol;
		this.clojureSymbol = clojureSymbol;
	}
	
	@Override
	public String toString() {
		return this.symbol;
	}
	
	@Override
	public String toClojureCode() {
		return this.clojureSymbol;
	}

	/**
	 * Addition (+) operator
	 */
	public static final Operator Addition = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "+", "+",
			OperatorWrapper.AddWrapper);
	

	/**
	 * Logical And (AND) operator
	 */
	public static final Operator And = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeBool, TypeConcrete.TypeBool }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "and", "and",
			OperatorWrapper.AndWrapper);

	/**
	 * Bit and (&) operator
	 */
	public static final Operator BitAnd = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "bit-and", "bit-and",
			OperatorWrapper.BitAndWrapper);

	/**
	 * Bit or (|) operator
	 */
	public static final Operator BitOr = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "bit-or", "bit-or",
			OperatorWrapper.BitOrWrapper);

	/**
	 * car operator
	 */
	public static final Operator Car = new Operator(
			new TypeTuple(new Type[] { new TypeTuple(new Type[] { new TypeVariable("_a"), new TypeVariable("_b") }) }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "car", "",
			OperatorWrapper.CarWrapper);

	/**
	 * cdr operator
	 */
	public static final Operator Cdr = new Operator(
			new TypeTuple(new Type[] { new TypeTuple(new Type[] { new TypeVariable("_a"), new TypeVariable("_b") }) }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "cdr", "", 
			OperatorWrapper.CdrWrapper);

	/**
	 * Concatenation operator
	 */
	public static final Operator Concantenation = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeString, TypeConcrete.TypeString }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "concat", "concat",
			OperatorWrapper.ConcatWrapper);

	/**
	 * Division (/) operator
	 */
	public static final Operator Division = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "/", "/",
			OperatorWrapper.DivWrapper);

	/**
	 * Equality operator
	 */
	public static final Operator Equals = new Operator(
			new TypeTuple(new Type[] { new TypeVariable("_a"), new TypeVariable("_a") }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "equals?", "=",
			OperatorWrapper.EqualsWrapper);

	/**
	 * Lesser than (<) operator
	 */
	public static final Operator LesserThan = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "<", "<",
			OperatorWrapper.LesserThanWrapper);

	/**
	 * Multiplication (*) operator
	 */
	public static final Operator Multiplication = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "*", "*",
			OperatorWrapper.MulWrapper);

	/**
	 * Not operator
	 */
	public static final Operator Not = new Operator(new TypeTuple(new Type[] { TypeConcrete.TypeBool }),
			new Tuple(new Expression[] { new Variable("_x") }), "not", "not", OperatorWrapper.NotWrapper);

	/**
	 * Numeric equal (=) operator
	 */
	public static final Operator NumericEqual = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "=", "=",
			OperatorWrapper.NumEqWrapper);

	/**
	 * Or operator
	 */
	public static final Operator Or = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeBool, TypeConcrete.TypeBool }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "or", "or",
			OperatorWrapper.OrWrapper);

	/**
	 * Subtraction (-) operator
	 */
	public static final Operator Subtraction = new Operator(
			new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
			new Tuple(new Expression[] { new Variable("_x"), new Variable("_y") }), "-", "-",
			OperatorWrapper.SubWrapper);

	/**
	 * Wrapper abstract class for meta-language operators body
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static abstract class OperatorWrapper extends Expression {

		/**
		 * Type of this operatorWrapper (return type of operation)
		 */
		public final Type type;

		protected OperatorWrapper(Type type) {
			this.type = type;
		}

		@Override
		public Map<Expression, Type> infer(Environment env) throws AppendableException {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				this.typeHypothesis = new TreeMap<Expression, Type>();
				this.typeHypothesis.put(this, this.type);
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
			return this;
		}

		@Override
		public String toClojureCode() throws AppendableException {
			throw new AppendableException("You cannot convert " + this.getClass().getName() + " to clojure code!");
		}

		@Override
		public int compareTo(Expression other) {
			if (other instanceof OperatorWrapper) {
				return (int) Math.signum(
						operatorsOrdering.indexOf(this.getClass()) - operatorsOrdering.indexOf(other.getClass()));
			}
			return super.compareTo(other);
		}
		
		/**
		 * Body of addition operator
		 */
		public static final OperatorWrapper AddWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value + y.value);
			}
		}; 
		
		/**
		 * Body of and operator
		 */
		public static final OperatorWrapper AndWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return x.value && y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}

		};
		
		/**
		 * Body of bit-and operator
		 */
		public static final OperatorWrapper BitAndWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value & y.value);
			}

		};
		
		/**
		 * Body of bit-or operator
		 */
		public static final OperatorWrapper BitOrWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value | y.value);
			}
		};
		
		/**
		 * Body of car operator
		 */
		public static final OperatorWrapper CarWrapper = new OperatorWrapper(new TypeVariable("_a")) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				Tuple x = (Tuple) (env.getVariableValue(new Variable("_x")).interpret(env));

				if (x == null) {
					return this;
				}

				if (x.values.length != 2) {
					throw new AppendableException("Argument of car is " + x + " pair expected");
				}

				return x.values[0];
			}
		};
		
		/**
		 * Body of cdr operator
		 */
		public static final OperatorWrapper CdrWrapper = new OperatorWrapper(new TypeVariable("_b")) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				Tuple x = (Tuple) (env.getVariableValue(new Variable("_x")).interpret(env));

				if (x == null) {
					return this;
				}

				if (x.values.length != 2) {
					throw new AppendableException("Argument of car is " + x + " pair expected");
				}

				return x.values[0];
			}
		};
		
		/**
		 * Body of concat operator
		 */
		public static final OperatorWrapper ConcatWrapper = new OperatorWrapper(TypeConcrete.TypeString) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitString x = (LitString) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitString y = (LitString) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitString(x.value + y.value);
			}
		};
		
		/**
		 * Body of division operator
		 */
		public static final OperatorWrapper DivWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value / y.value);
			}
		};
		
		/**
		 * Body of equals? operator
		 */
		public static final OperatorWrapper EqualsWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				Expression x = (env.getVariableValue(new Variable("_x")).interpret(env));
				Expression y = (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return x.equals(y) ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};
		
		/**
		 * Body of lesser than operator
		 */
		public static final OperatorWrapper LesserThanWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return x.value < y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}

			@Override
			public String toClojureCode() {
				return "<";
			}
		};
		
		/**
		 * Body of multiplication operator
		 */
		public static final OperatorWrapper MulWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value * y.value);
			}
		};
		
		/**
		 * Body of not operator
		 */
		public static final OperatorWrapper NotWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));

				if (x == null) {
					return this;
				}

				return !x.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};
		
		/**
		 * Body of numerical equal operator
		 */
		public static final OperatorWrapper NumEqWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return x.value == y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};
		
		/**
		 * Body of or operator
		 */
		public static final OperatorWrapper OrWrapper = new OperatorWrapper(TypeConcrete.TypeBool) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

				if (x == null || y == null) {
					return this;
				}

				return x.value || y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};
		
		/**
		 * Body of subtraction operator
		 */
		public static final OperatorWrapper SubWrapper = new OperatorWrapper(TypeConcrete.TypeInt) {

			@Override
			public Expression interpret(Environment env) throws Exception {
				LitInteger x = (LitInteger) env.getVariableValue(new Variable("_x"));
				LitInteger y = (LitInteger) env.getVariableValue(new Variable("_y"));

				if (x == null || y == null) {
					return this;
				}

				return new LitInteger(x.value - y.value);
			}
		};

		/**
		 * Ordering of operator types for compareTo
		 */
		private static List<Class<? extends Expression>> operatorsOrdering = Arrays.asList(
				AddWrapper.getClass(), AndWrapper.getClass(), BitAndWrapper.getClass(),
				BitOrWrapper.getClass(), CarWrapper.getClass(), CdrWrapper.getClass(),
				ConcatWrapper.getClass(), DivWrapper.getClass(),
				EqualsWrapper.getClass(), LesserThanWrapper.getClass(),
				MulWrapper.getClass(), NotWrapper.getClass(),
				NumEqWrapper.getClass(), OrWrapper.getClass(),
				SubWrapper.getClass());
	}
}
