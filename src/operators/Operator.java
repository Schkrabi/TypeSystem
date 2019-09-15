package operators;

import java.util.Arrays;

import expression.Expression;
import expression.Function;
import expression.LitBoolean;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.InvalidClojureCompilationException;
import util.NameGenerator;
import util.Pair;

/**
 * Expression for meta-language operators
 * 
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
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "+", "+", OperatorWrapper.AddWrapper);

	/**
	 * Logical And (AND) operator
	 */
	public static final Operator And = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "and", "and", OperatorWrapper.AndWrapper);

	/**
	 * Bit and (&) operator
	 */
	public static final Operator BitAnd = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "bit-and", "bit-and",
			OperatorWrapper.BitAndWrapper);

	/**
	 * Bit or (|) operator
	 */
	public static final Operator BitOr = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "bit-or", "bit-or",
			OperatorWrapper.BitOrWrapper);

	/**
	 * car operator
	 */
	public static final Operator Car = new Operator(
			new TypeTuple(Arrays.asList(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))))),
			new Tuple(Arrays.asList(new Variable("_x"))), "car", "", OperatorWrapper.CarWrapper);

	/**
	 * cdr operator
	 */
	public static final Operator Cdr = new Operator(
			new TypeTuple(Arrays.asList(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))))),
			new Tuple(Arrays.asList(new Variable("_x"))), "cdr", "", OperatorWrapper.CdrWrapper);

	/**
	 * Concatenation operator
	 */
	public static final Operator Concantenation = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "concat", "concat",
			OperatorWrapper.ConcatWrapper);

	/**
	 * Division (/) operator
	 */
	public static final Operator Division = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "/", "/", OperatorWrapper.DivWrapper);

	/**
	 * Equality operator
	 */
	public static final Operator Equals = new Operator(
			new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "equals?", "=",
			OperatorWrapper.EqualsWrapper);

	/**
	 * Lesser than (<) operator
	 */
	public static final Operator LesserThan = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "<", "<",
			OperatorWrapper.LesserThanWrapper);

	/**
	 * Multiplication (*) operator
	 */
	public static final Operator Multiplication = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "*", "*", OperatorWrapper.MulWrapper);

	/**
	 * Not operator
	 */
	public static final Operator Not = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "not", "not", OperatorWrapper.NotWrapper);

	/**
	 * Numeric equal (=) operator
	 */
	public static final Operator NumericEqual = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "=", "=", OperatorWrapper.NumEqWrapper);

	/**
	 * Or operator
	 */
	public static final Operator Or = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "or", "or", OperatorWrapper.OrWrapper);

	/**
	 * Subtraction (-) operator
	 */
	public static final Operator Subtraction = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "-", "-", OperatorWrapper.SubWrapper);

	/**
	 * Wrapper abstract class for meta-language operators body
	 * 
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			return new Pair<Type, Substitution>(this.type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode() throws AppendableException {
			throw new InvalidClojureCompilationException(this);
		}

		/**
		 * Body of addition operator
		 */
		public static final OperatorWrapper AddWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitInteger(x.value + y.value);
			}
		};

		/**
		 * Body of and operator
		 */
		public static final OperatorWrapper AndWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

				return x.value && y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}

		};

		/**
		 * Body of bit-and operator
		 */
		public static final OperatorWrapper BitAndWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitInteger(x.value & y.value);
			}

		};

		/**
		 * Body of bit-or operator
		 */
		public static final OperatorWrapper BitOrWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitInteger(x.value | y.value);
			}
		};

		/**
		 * Body of car operator
		 */
		public static final OperatorWrapper CarWrapper = new OperatorWrapper(new TypeVariable("_a")) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Tuple x = (Tuple) (env.getVariableValue(new Variable("_x")).interpret(env));

				return x.get(0);
			}
		};

		/**
		 * Body of cdr operator
		 */
		public static final OperatorWrapper CdrWrapper = new OperatorWrapper(new TypeVariable("_b")) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Tuple x = (Tuple) (env.getVariableValue(new Variable("_x")).interpret(env));

				return x.get(1);
			}
		};

		/**
		 * Body of concat operator
		 */
		public static final OperatorWrapper ConcatWrapper = new OperatorWrapper(TypeAtom.TypeStringNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitString x = (LitString) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitString y = (LitString) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitString(x.value + y.value);
			}
		};

		/**
		 * Body of division operator
		 */
		public static final OperatorWrapper DivWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitInteger(x.value / y.value);
			}
		};

		/**
		 * Body of equals? operator
		 */
		public static final OperatorWrapper EqualsWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression x = (env.getVariableValue(new Variable("_x")).interpret(env));
				Expression y = (env.getVariableValue(new Variable("_y")).interpret(env));

				return x.equals(y) ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};

		/**
		 * Body of lesser than operator
		 */
		public static final OperatorWrapper LesserThanWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return x.value < y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};

		/**
		 * Body of multiplication operator
		 */
		public static final OperatorWrapper MulWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return new LitInteger(x.value * y.value);
			}
		};

		/**
		 * Body of not operator
		 */
		public static final OperatorWrapper NotWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));

				return !x.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};

		/**
		 * Body of numerical equal operator
		 */
		public static final OperatorWrapper NumEqWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

				return x.value == y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};

		/**
		 * Body of or operator
		 */
		public static final OperatorWrapper OrWrapper = new OperatorWrapper(TypeAtom.TypeBoolNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
				LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

				return x.value || y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
			}
		};

		/**
		 * Body of subtraction operator
		 */
		public static final OperatorWrapper SubWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitInteger x = (LitInteger) env.getVariableValue(new Variable("_x")).interpret(env);
				LitInteger y = (LitInteger) env.getVariableValue(new Variable("_y")).interpret(env);

				return new LitInteger(x.value - y.value);
			}
		};
	}
}
