package velka.core.abstraction;

import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.XMLFormatter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Optional;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeSymbol;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * This class contains Velka's operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class Operators {
	
	/**
	 * Namespace for velka.clojure.operators
	 */
	public static final String NAMESPACE = "velka.clojure.operators";

	/**
	 * Addition (+) operator
	 */
	public static final Operator Addition = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger x = (LitInteger) args.get(0);
			LitInteger y = (LitInteger) args.get(1);

			return new LitInteger(x.value + y.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "+";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(x, y),
					LitInteger.clojureIntToClojureLitInteger(
							ClojureHelper.applyClojureFunction("unchecked-add",
									ClojureHelper.getLiteralInnerValue(x),
									ClojureHelper.getLiteralInnerValue(y))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-addition", NAMESPACE);
		}

	};
	/**
	 * Bit and (&) operator
	 */
	public static final Operator BitAnd = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value & arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-and";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(bit-and (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-and", NAMESPACE);
		}
	};
	/**
	 * Bit or (|) operator
	 */
	public static final Operator BitOr = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value | arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-or";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(bit-or (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-or", NAMESPACE);
		}
	};

	/**
	 * Bit shift right (shr) operator
	 */
	public static final Operator BitShiftRight = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_num _n] "
					+ LitInteger.clojureIntToClojureLitInteger("(bit-shift-right (first _num) (first _n))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			long res = num.value >> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shr";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-shr", NAMESPACE);
		}

	};
	
	/**
	 * Unsigned bit shift right (ushr) operator
	 */
	public static final Operator UnsignedBitShiftRight = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_num _n] "
					+ LitInteger.clojureIntToClojureLitInteger("(unsigned-bit-shift-right (first _num) (first _n))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			long res = num.value >>> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "ushr";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-unsigned-bit-shr", NAMESPACE);
		}

	};

	/**
	 * Bit shift left (shl) operator
	 */
	public static final Operator BitShiftLeft = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_num _n] "
					+ LitInteger.clojureIntToClojureLitInteger("(bit-shift-left (first _num) (first _n))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);

			LitInteger n;

			if (!(args.get(1) instanceof LitInteger)) {
				n = new LitInteger(1);
			} else {
				n = (LitInteger) args.get(1);
			}

			long res = num.value << n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shl";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-shr", NAMESPACE);
		}

	};

	/**
	 * Bit Not operator
	 */
	public static final Operator BitNot = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_val] " + LitInteger.clojureIntToClojureLitInteger("(bit-not (first _val))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger l = (LitInteger) args.get(0);

			long ret = ~l.value;
			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-not";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-not", NAMESPACE);
		}

	};

	/**
	 * Bit XOR operator
	 */
	public static final Operator BitXor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_val1 _val2] "
					+ LitInteger.clojureIntToClojureLitInteger("(bit-xor (first _val1) (first _val2))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger val1 = (LitInteger) args.get(0);
			LitInteger val2 = (LitInteger) args.get(1);

			long ret = val1.value ^ val2.value;

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-xor";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-xor", NAMESPACE);
		}
	};

	/**
	 * car operator
	 */
	public static final Operator Car = new Operator() {
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), left);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(0);
		}

		@Override
		public String toString() {
			return "car";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (first _x))";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-car", NAMESPACE);
		}
	};
	/**
	 * cdr operator
	 */
	public static final Operator Cdr = new Operator() {

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), right);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(1);
		}

		@Override
		public String toString() {
			return "cdr";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (second _x))";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-cdr", NAMESPACE);
		}
	};
	/**
	 * Concatenation operator
	 */
	public static final Operator Concantenation = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg0 = (LitString) args.get(0);
			LitString arg1 = (LitString) args.get(1);

			return new LitString(arg0.value + arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "concat";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitString.clojureStringToClojureLitString("(str (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-concat", NAMESPACE);
		}

	};
	/**
	 * Division (/) operator
	 */
	public static final Operator Division = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value / arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "/";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
							Arrays.asList(x, y),
							LitInteger.clojureIntToClojureLitInteger(
								ClojureHelper.applyClojureFunction("int",
										ClojureHelper.applyClojureFunction("/",
												ClojureHelper.getLiteralInnerValue(x),
												ClojureHelper.getLiteralInnerValue(y)))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-division", NAMESPACE);
		}

	};
	/**
	 * Equality operator
	 */
	public static final Operator Equals = new Operator() {

		private final TypeArrow type = new TypeArrow(
				new TypeTuple(
						Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
				TypeAtom.TypeBoolNative);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression arg0 = args.get(0);
			Expression arg1 = args.get(1);

			return arg0.equals(arg1) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "equals?";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitBoolean.clojureBooleanToClojureLitBoolean("(= (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-equals", NAMESPACE);
		}

	};
	/**
	 * Lesser than (<) operator
	 */
	public static final Operator LesserThan = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value < arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "<";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitBoolean.clojureBooleanToClojureLitBoolean("(< (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-lesser-than", NAMESPACE);
		}
	};
	/**
	 * Multiplication (*) operator
	 */
	public static final Operator Multiplication = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value * arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "*";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(x, y),
					LitInteger.clojureIntToClojureLitInteger(
							ClojureHelper.applyClojureFunction("unchecked-multiply",
									ClojureHelper.getLiteralInnerValue(x),
									ClojureHelper.getLiteralInnerValue(y))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-multiplication", NAMESPACE);
		}
	};
	/**
	 * Not operator
	 */
	public static final Operator Not = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);

			return arg.value ? LitBoolean.FALSE : LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		public String toString() {
			return "not";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + LitBoolean.clojureBooleanToClojureLitBoolean("(not (get _x 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-not", NAMESPACE);
		}
	};
	/**
	 * Numeric equal (=) operator
	 */
	public static final Operator NumericEqual = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value == arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "=";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x _y] " + LitBoolean.clojureBooleanToClojureLitBoolean("(= (get _x 0) (get _y 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-numeric-equals", NAMESPACE);
		}

	};
	/**
	 * Subtraction (-) operator
	 */
	public static final Operator Subtraction = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value - arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "-";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(x, y),
					LitInteger.clojureIntToClojureLitInteger(
							ClojureHelper.applyClojureFunction("unchecked-subtract",
									ClojureHelper.getLiteralInnerValue(x),
									ClojureHelper.getLiteralInnerValue(y))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-subtraction", NAMESPACE);
		}

	};
	/**
	 * Println operator
	 */
	public static final Operator PrintlnOperator = new Operator() {

		private final TypeArrow type = new TypeArrow(
				new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()))), TypeAtom.TypeIntNative);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression arg = (Expression) args.get(0);

			String s = arg.toString();
			System.out.println(s);

			return new LitInteger(s.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "println";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String expr = "_expr";
			String str = "_str";
			return ClojureHelper.fnHelper(Arrays.asList(expr), 
					ClojureHelper.letHelper(
							LitInteger.clojureIntToClojureLitInteger(
								ClojureHelper.applyClojureFunction("second",
										ClojureHelper.applyClojureFunction("doall",
												ClojureHelper.clojureVectorHelper(
														ClojureHelper.applyClojureFunction("clojure.core/println", str),
														ClojureHelper.applyClojureFunction("count", str))))), 
							new Pair<String, String>(str, ClojureHelper.applyClojureFunction("pr-str", expr)))
					);
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-println", NAMESPACE);
		}
	};
	/**
	 * can-unify-types operator
	 */
	public static final Operator CanUnifyTypes = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			TypeSymbol t1 = (TypeSymbol) args.get(0);
			TypeSymbol t2 = (TypeSymbol) args.get(1);

			if(Type.unifyTypes(t1.type, t2.type).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {

			String t1 = "_t1";
			String t2 = "_t2";
			String opt = "_opt";
			String fn = ClojureHelper.fnHelper(Arrays.asList(t1, t2), ClojureHelper.letHelper(
					LitBoolean.clojureBooleanToClojureLitBoolean(ClojureHelper.applyClojureFunction(".isPresent", opt)),
					new Pair<String, String>(opt, ClojureHelper.applyClojureFunction(
							"velka.types.Type/unifyTypes",
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t1),
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t2)))));

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return new Pair<Type, Substitution>(new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative), Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "can-unify-types";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-can-unify-types", NAMESPACE);
		}
	};
	/**
	 * can-unify-representations operator
	 */
	public static final Operator CanUnifyRepresentations = new Operator() {
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			TypeSymbol t1 = (TypeSymbol) args.get(0);
			TypeSymbol t2 = (TypeSymbol) args.get(1);

			if(Type.unifyRepresentation(t1.type, t2.type).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {

			String t1 = "_t1";
			String t2 = "_t2";
			String opt = "_opt";
			String fn = ClojureHelper.fnHelper(Arrays.asList(t1, t2), ClojureHelper.letHelper(
					LitBoolean.clojureBooleanToClojureLitBoolean(ClojureHelper.applyClojureFunction(".isPresent", opt)),
					new Pair<String, String>(opt, ClojureHelper.applyClojureFunction(
							"velka.types.Type/unifyRepresentation",
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t1),
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t2)))));

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return new Pair<Type, Substitution>(new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative), Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "can-unify-representations";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-can-unify-representations", NAMESPACE);
		}
	};
	/**
	 * is-same-type operator
	 */
	public static final Operator IsSameType = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env, typeEnv);
			Pair<Type, Substitution> p2 = e2.infer(env, typeEnv);

			if(Type.unifyTypes(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {

			String fn = "(fn [e1 e2] (let [opt (velka.types.Type/unifyTypes " + 
							"(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " e1) " +
							"(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " e2))] " +
								"(if (.isPresent opt) " + 
									LitBoolean.TRUE.toClojureCode(env, typeEnv) +
									LitBoolean.FALSE.toClojureCode(env, typeEnv) + ")))";

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow t = new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative);

			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "is-same-type";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-is-same-type", NAMESPACE);
		}

	};
	/**
	 * is-same-representation operator
	 */
	public static final Operator IsSameRepresentation = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env, typeEnv);
			Pair<Type, Substitution> p2 = e2.infer(env, typeEnv);

			if(Type.unifyRepresentation(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {

			String fn = "(fn [e1 e2] (let [opt (velka.types.Type/unifyRepresentation " + 
							"(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " e1) " +
							"(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " e2))] " +
								"(if (.isPresent opt) " + 
									LitBoolean.TRUE.toClojureCode(env, typeEnv) +
									LitBoolean.FALSE.toClojureCode(env, typeEnv) + ")))"; 

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow t = new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative);

			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "is-same-representation";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-is-same-representation", NAMESPACE);
		}

	};
	/**
	 * Operator for current nano timestamp
	 */
	public static Operator Timestamp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = ClojureHelper.fnHelper(Arrays.asList(),
					LitInteger.clojureIntToClojureLitInteger("(System/currentTimeMillis)"));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			return new LitInteger(System.currentTimeMillis());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "timestamp";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-timestamp", NAMESPACE);
		}

	};

	/**
	 * Operator for logging initialization
	 */
	public static Operator InitLogger = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_name] "
					+ "(let [logger (java.util.logging.Logger/getLogger java.util.logging.Logger/GLOBAL_LOGGER_NAME) "
					+ "rootLogger (java.util.logging.Logger/getLogger \"\") "
					+ "consoleHandler (first (.getHandlers rootLogger))] " + "(first (doall ["
					+ Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv) + " "
					+ "(if (instance? java.util.logging.ConsoleHandler consoleHandler) "
					+ "(.removeHandler rootLogger consoleHandler) " + "nil) "
					+ "(.setLevel logger java.util.logging.Level/INFO) "
					+ "(let [file (java.util.logging.FileHandler. (first _name)) "
					+ "formatter (java.util.logging.XMLFormatter.)] " + "(doall [(.setFormatter file formatter) "
					+ "(.addHandler logger file)]))]))))";

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

			// suppress the logging output to the console
			Logger rootLogger = Logger.getLogger("");
			Handler[] handlers = rootLogger.getHandlers();
			if (handlers.length > 0 && handlers[0] instanceof ConsoleHandler) {
				rootLogger.removeHandler(handlers[0]);
			}

			logger.setLevel(Level.INFO);

			LitString name = (LitString) args.get(0);
			FileHandler file = null;

			try {
				file = new FileHandler(name.value);
			} catch (Exception e) {
				AppendableException ae = new AppendableException("Error initalizing logger in file " + name.value);
				ae.initCause(e);
				throw ae;
			}

			Formatter formatter = new XMLFormatter();
			file.setFormatter(formatter);
			logger.addHandler(file);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "init-logger";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-init-logger", NAMESPACE);
		}

	};

	/**
	 * Log operator
	 */
	public static final Operator Log = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_msg] (first (doall [" + Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv) + " "
					+ "(.info (java.util.logging.Logger/getLogger java.util.logging.Logger/GLOBAL_LOGGER_NAME) (first _msg))])))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString s = (LitString) args.get(0);
			Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
			logger.info(s.value);
			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "log";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-log", NAMESPACE);
		}

	};

	/**
	 * to-str operator
	 */
	public static final Operator ToStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_arg] " + LitString.clojureStringToClojureLitString("(str _arg)") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression e = args.get(0);
			String s;
			
//			if(e instanceof LitInteger) {
//				s = Long.toString(((LitInteger)e).value);
//			}
//			else if(e instanceof LitDouble) {
//				s = Double.toString(((LitDouble)e).value);
//			}
//			else if(e instanceof LitBoolean) {
//				s = Boolean.toString(((LitBoolean)e).value);
//			}
//			else if(e instanceof LitString) {
//				s = ((LitString)e).value;
//			}
//			else {
				s = e.toString();
//			}
			
			return new LitString(s);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeVariable(NameGenerator.next())),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "to-str";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-to-str", NAMESPACE);
		}

	};

	/**
	 * Operator read-file
	 */
	public static final Operator ReadFile = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_path] " + LitString.clojureStringToClojureLitString("(slurp (first _path))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);

			String content = "";
			try {
				content = Files.readString(Path.of(arg.value));
			} catch (IOException ioe) {
				AppendableException e = new AppendableException(ioe.getMessage());
				e.initCause(ioe);
				throw e;
			}

			return new LitString(content);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "read-file";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-read-file", NAMESPACE);
		}

	};

	/**
	 * str-split operator
	 */
	public static final Operator StrSplit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_str _by] (" + ClojureCoreSymbols.tuple2velkaListSymbol_full + " (map (fn [_s] "
					+ LitString.clojureStringToClojureLitString("_s") + ") "
					+ "(clojure.string/split (first _str) (re-pattern (first _by))))))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString lsStr = (LitString) args.get(0);
			LitString lsBy = (LitString) args.get(1);

			String[] splitted = lsStr.value.split(lsBy.value);
			LinkedList<Expression> l = new LinkedList<Expression>();
			for(String s : splitted) {
				LitString ls = new LitString(s);
				l.add(ls);
			}

			return new LitComposite(new LitInteropObject(l), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "str-split";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-str-split", NAMESPACE);
		}

	};

	/**
	 * parse-int operator
	 */
	public static final Operator parseInt = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_str] " + LitInteger.clojureIntToClojureLitInteger("(Integer/parseInt (first _str))")
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);

			int i;
			try {
				i = Integer.parseInt(arg.value);
			}
			catch(java.lang.NumberFormatException e) {
				throw e;
			}

			return new LitInteger(i);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "parse-int";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-parse-int", NAMESPACE);
		}

	};
	
	/**
	 * Floating point division operator
	 */
	public static final Operator DivisionFloatingPoint = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(x, y), 
					LitDouble.clojureDoubleToClojureLitDouble("(/ (first " + x + ") (first " + y + "))"));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-div", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			double rslt = d1.value / d2.value;
			
			return new LitDouble(rslt);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "ddiv";
		}
		
	};
	
	/**
	 * Operator for coercing int to double
	 */
	public static final Operator intToDouble = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String i = "_i";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(i), 
					LitDouble.clojureDoubleToClojureLitDouble("(double (first " + i + "))"));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-to-double-clj", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			return new LitDouble((double)i.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "int-to-double";
		}
		
	};
	
	/**
	 * Floor operator
	 */
	public static final Operator floor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String d = "_d";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(d), 
					LitInteger.clojureIntToClojureLitInteger("(int (Math/floor (first " + d + ")))"));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-floor", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble d = (LitDouble)args.get(0);
			
			double floored = Math.floor(d.value);
			
			return new LitInteger((long)floored);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "floor";
		}
		
	};
	
	/**
	 * Operator for addition of double values
	 */
	public static final Operator dadd = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String d1 = "_d1";
			String d2 = "_d2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(d1, d2), 
					LitDouble.clojureDoubleToClojureLitDouble("(+ (first " + d1 + ") (first " + d2 + "))"));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-add", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			double sum = d1.value + d2.value;
			
			return new LitDouble(sum);
		}
		
		@Override
		public String toString() {
			return "dadd";
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	/**
	 * Operator for Double lesser than
	 */
	public static final Operator doubleLesserThan = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String d1 = "_d1";
			String d2 = "_d2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(d1, d2), 
					LitBoolean.clojureBooleanToClojureLitBoolean("(< (first " + d1 + ") (first " + d2 + "))"));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("double-lesser-than", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			if(d1.value < d2.value) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}
		
		@Override
		public String toString() {
			return "dlt";
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	public static final Operator modulo = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String i = "_i";
			String j = "_j";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(i, j), 
					LitInteger.clojureIntToClojureLitInteger(ClojureHelper.applyClojureFunction("mod", 
							ClojureHelper.getLiteralInnerValue(i),
							ClojureHelper.getLiteralInnerValue(j))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("integer-modulo", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			LitInteger j = (LitInteger)args.get(1);
			
			long res = i.value % j.value;
						
			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "mod";
		}
	};

}
