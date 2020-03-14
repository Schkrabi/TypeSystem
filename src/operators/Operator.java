package operators;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import expression.Expression;
import expression.Function;
import expression.LitBoolean;
import expression.LitComposite;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.InvalidClojureCompilationException;
import util.NameGenerator;
import util.Pair;
import util.RomanNumbers;
import util.UnboundVariableException;

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

	public Operator(TypeTuple argsType, Tuple args, String symbol, String clojureSymbol, Expression body) {
		super(argsType, args, body, Environment.topLevelEnvironment);
		this.symbol = symbol;
		this.clojureSymbol = clojureSymbol;
	}

	@Override
	public String toString() {
		return this.symbol;
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return "`([" + this.argsType.toClojure() + " ~" + this.clojureSymbol + "])";
	}

	/**
	 * Addition (+) operator
	 */
	public static final Operator Addition = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "+", "+", OperatorWrapper.AddWrapper);

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
			new Tuple(Arrays.asList(new Variable("_x"))), "car", "(fn [_x] (get _x 0))", OperatorWrapper.CarWrapper) {

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			TypeTuple argType = (TypeTuple) this.argsType.get(0);
			return new Pair<Type, Substitution>(new TypeArrow(this.argsType, argType.get(0)), Substitution.EMPTY);
		}
	};

	/**
	 * cdr operator
	 */
	public static final Operator Cdr = new Operator(
			new TypeTuple(Arrays.asList(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))))),
			new Tuple(Arrays.asList(new Variable("_x"))), "cdr", "(fn [_x] (get _x 1))", OperatorWrapper.CdrWrapper) {

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			TypeTuple argType = (TypeTuple) this.argsType.get(0);
			return new Pair<Type, Substitution>(new TypeArrow(this.argsType, argType.get(1)), Substitution.EMPTY);
		}
	};

	/**
	 * Concatenation operator
	 */
	public static final Operator Concantenation = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "concat", "str",
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
	 * Subtraction (-) operator
	 */
	public static final Operator Subtraction = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"), new Variable("_y"))), "-", "-", OperatorWrapper.SubWrapper);

	/**
	 * Println operator
	 */
	public static final Operator PrintlnOperator = new Operator(
			new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()))),
			new Tuple(Arrays.asList(new Variable("_arg"))), "println", "println", OperatorWrapper.PrintlnWrapper);

	/**
	 * Makes name of getter for certain type
	 * 
	 * @param type  type for which getter is constructed
	 * @param index index of member in type
	 * @return string containing getter name
	 */
	public static String getterName(TypeAtom type, int index) {
		return type.name.toString() + type.representation.toString() + "-" + index;
	}

	/**
	 * Creates getter operators for given types and its type members
	 * 
	 * @param type    typeAtom
	 * @param members members of given composite type
	 * @return list of getters
	 */
	public static List<Operator> makeGetters(TypeAtom type, TypeTuple members) {
		List<Operator> l = new ArrayList<Operator>();

		for (int i = 0; i < members.size(); i++) {
			l.add(Operator.makeTypeGetter(type, members, i));
		}
		return l;
	}

	/**
	 * Creates getter operator for given type its members and index
	 * 
	 * @param type    inspected type
	 * @param members member types of given type
	 * @param index   index of gotten type
	 * @return new Operator instance
	 */
	private static Operator makeTypeGetter(TypeAtom type, TypeTuple members, final int index) {
		OperatorWrapper body = new OperatorWrapper(members.get(index)) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitComposite lit = (LitComposite) env.getVariableValue(new Variable("_value")).interpret(env);
				return lit.value.get(index).interpret(env);
			}
		};

		String name = Operator.getterName(type, index);

		return new Operator(new TypeTuple(Arrays.asList(type)), new Tuple(Arrays.asList(new Variable("_value"))), name,
				name, body);
	}

	/**
	 * Creates getters for given type and its members
	 * 
	 * @param type    Constructed type
	 * @param members number of members
	 * @return String containing getter definitions
	 */
	public static String makeClojureGetterDefinitions(TypeAtom type, int members) {
		StringBuilder s = new StringBuilder();

		for (int i = 0; i < members; i++) {
			s.append(Operator.makeClojureGetterDefinition(type, i));
			if (i != members - 1) {
				s.append("\n");
			}
		}

		return s.toString();
	}

	/**
	 * Creates clojure definition of specific getter
	 * 
	 * @param type  type for which getter is made
	 * @param index index of gotten member
	 * @return string containing clojure definition of the getter
	 */
	private static String makeClojureGetterDefinition(TypeAtom type, int index) {
		return "(def " + Operator.getterName(type, index) + " `([[" + type.toClojure() + "] ~(fn [x] (get x " + index
				+ "))]))";
	}

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
		protected String toClojureCode(Environment env) throws InvalidClojureCompilationException {
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
		public static final OperatorWrapper CarWrapper = new OperatorWrapper(new TypeVariable(NameGenerator.next())) {

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

		/**
		 * Body of get operator
		 */
		public static final OperatorWrapper GetWrapper = new OperatorWrapper(new TypeVariable(NameGenerator.next())) {

			@Override
			public Expression interpret(Environment env) throws UnboundVariableException, AppendableException {
				LitComposite composite = (LitComposite) env.getVariableValue(new Variable("_composite")).interpret(env);
				LitInteger index = (LitInteger) env.getVariableValue(new Variable("_index")).interpret(env);

				return composite.value.get(index.value);
			}
		};

		/**
		 * Body of println operator
		 */
		public static final OperatorWrapper PrintlnWrapper = new OperatorWrapper(TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression e = env.getVariableValue(new Variable("_arg")).interpret(env);

				String s = e.toString();
				System.out.println(s);
				return new LitInteger(s.length());
			}
		};
	}

	/**
	 * Int:Native constructor
	 */
	public static Operator IntNativeConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Int:Native", "identity", new Variable("_x"));
	/**
	 * Int constructor (really constructs Int:Native)
	 */
	public static Operator IntConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Int", "identity", new Variable("_x"));
	/**
	 * Int:String constructor
	 */
	public static Operator IntStringConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Int:String", "(fn [_x] [_x])",
			new LitComposite(new Tuple(Arrays.asList(new Variable("_x"))), TypeAtom.TypeIntString));
	/**
	 * Int:Roman constructor
	 */
	public static Operator IntRomanConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Int:Roman", "(fn [_x] [_x])",
			new LitComposite(new Tuple(Arrays.asList(new Variable("_x"))), TypeAtom.TypeIntRoman));
	/**
	 * String:Native constructor
	 */
	public static Operator StringNativeConstructor = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Tuple(Arrays.asList(new Variable("_x"))),
			"String:Native", "identity", new Variable("_x"));
	/**
	 * String constructor (really constructs String:Native)
	 */
	public static Operator StringConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "String", "identity", new Variable("_x"));
	/**
	 * Double:Native constructor
	 */
	public static Operator DoubleNativeConstructor = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)), new Tuple(Arrays.asList(new Variable("_x"))),
			"Double:Native", "identity", new Variable("_x"));
	/**
	 * Double constructor (really constructs Double:Native)
	 */
	public static Operator DoubleConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Double", "identity", new Variable("_x"));
	/**
	 * Bool:Native constructor
	 */
	public static Operator BoolNativeConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Bool:Native", "identity", new Variable("_x"));
	/**
	 * Bool constructor (really constructs Bool:Native)
	 */
	public static Operator BoolConstructor = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), "Bool", "identity", new Variable("_x"));

	/**
	 * Conversion from Int:Native to Int:Roman
	 */
	public static final Operator IntNativeToIntRoman = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman),
			"(fn [_x] [(" + RomanNumbers.int2RomanClojure + " _x)])", ConversionWrapper.IntNativeToIntRomanWrapper);

	/**
	 * Conversion from Int:Native to Int:String
	 */
	public static final Operator IntNativeToIntString = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntNative, TypeAtom.TypeIntString),
			"(fn [_x] [(Integer/toString _x)])", ConversionWrapper.IntNativeToIntStringWrapper);

	/**
	 * Conversion from Int:Roman to Int:Native
	 */
	public static final Operator IntRomanToIntNative = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative),
			"(fn [_x] (" + RomanNumbers.roman2intClojure + " (get _x 0)))",
			ConversionWrapper.IntRomanToIntNativeWrapper);

	/**
	 * Conversion from Int:Roman to Int:String
	 */
	public static final Operator IntRomanToIntString = new Operator(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString),
			"(fn [_x] [(str (" + RomanNumbers.roman2intClojure + " (get _x 0)))])",
			ConversionWrapper.IntRomanToIntStringWrapper);

	/**
	 * Conversion from Int:String to Int:Native
	 */
	public static final Operator IntStringToIntNative = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntString, TypeAtom.TypeIntNative),
			"(fn [_x] (Integer/parseInt (get _x 0)))", ConversionWrapper.IntStringToIntNativeWrapper);

	/**
	 * Conversion from Int:String to Int:Roman
	 */
	public static final Operator IntStringToIntRoman = new Operator(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Tuple(Arrays.asList(ConversionWrapper.arg)),
			TypeEnvironment.makeConversionName(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman),
			"(fn [_x] [(" + RomanNumbers.int2RomanClojure + " (Integer/parseInt (get _x 0)))])",
			ConversionWrapper.IntStringToIntRomanWrapper);

	/**
	 * Wrapper for conversions
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	public abstract static class ConversionWrapper extends OperatorWrapper {
		protected ConversionWrapper(Type type) {
			super(type);
		}

		/**
		 * Unified conversion argument
		 */
		public static final Expression arg = new Variable("_x");

		/**
		 * Body of IntNativeToIntRoman
		 */
		public static final ConversionWrapper IntNativeToIntRomanWrapper = new ConversionWrapper(
				TypeAtom.TypeIntRoman) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression e = Operator.ConversionWrapper.arg.interpret(env);
				LitInteger i = (LitInteger) e;
				Literal l = new LitComposite(new Tuple(Arrays.asList(new LitString(RomanNumbers.int2roman(i.value)))),
						TypeAtom.TypeIntRoman);
				return l;
			}
		};

		/**
		 * Body of IntNativeToIntString
		 */
		public static final ConversionWrapper IntNativeToIntStringWrapper = new ConversionWrapper(
				TypeAtom.TypeIntString) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression e = Operator.ConversionWrapper.arg.interpret(env);
				LitInteger i = (LitInteger) e;
				Literal l = new LitComposite(new Tuple(Arrays.asList(new LitString(Integer.toString(i.value)))),
						TypeAtom.TypeIntString);
				return l;
			}
		};

		/**
		 * Body of IntRomanToIntNative
		 */
		public static final ConversionWrapper IntRomanToIntNativeWrapper = new ConversionWrapper(
				TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitComposite e = (LitComposite) Operator.ConversionWrapper.arg.interpret(env);
				LitString r = (LitString) e.value.get(0);
				Literal l = new LitInteger(RomanNumbers.roman2int(r.value));
				return l;
			}
		};

		/**
		 * Body of IntRomanToIntString
		 */
		public static final ConversionWrapper IntRomanToIntStringWrapper = new ConversionWrapper(
				TypeAtom.TypeIntString) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitComposite e = (LitComposite) Operator.ConversionWrapper.arg.interpret(env);
				LitString r = (LitString) e.value.get(0);
				Literal l = new LitComposite(
						new Tuple(Arrays.asList(new LitString(Integer.toString(RomanNumbers.roman2int(r.value))))),
						TypeAtom.TypeIntString);
				return l;
			}
		};

		/**
		 * Body of IntStringToIntRoman
		 */
		public static final ConversionWrapper IntStringToIntNativeWrapper = new ConversionWrapper(
				TypeAtom.TypeIntNative) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitComposite e = (LitComposite) Operator.ConversionWrapper.arg.interpret(env);
				LitString s = (LitString) e.value.get(0);
				Literal l = new LitInteger(Integer.parseInt(s.value));
				return l;
			}
		};

		/**
		 * Body of IntStringToIntRoman
		 */
		public static final ConversionWrapper IntStringToIntRomanWrapper = new ConversionWrapper(
				TypeAtom.TypeIntRoman) {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				LitComposite e = (LitComposite) Operator.ConversionWrapper.arg.interpret(env);
				LitString s = (LitString) e.value.get(0);
				Literal l = new LitComposite(
						new Tuple(Arrays.asList(new LitString(RomanNumbers.int2roman(Integer.parseInt(s.value))))),
						TypeAtom.TypeIntRoman);
				return l;
			}
		};

	}
}
