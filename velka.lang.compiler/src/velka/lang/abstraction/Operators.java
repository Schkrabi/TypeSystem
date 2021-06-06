package velka.lang.abstraction;

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
import java.util.Optional;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeSymbol;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitDouble;
import velka.lang.literal.LitInteger;
import velka.lang.literal.LitString;
import velka.lang.literal.Literal;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

/**
 * This class contains Velka's operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class Operators {

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
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(+ (get _x 0) (get _y 0))") + ")";
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
	};

	/**
	 * car operator
	 */
	public static final Operator Car = new Operator() {

		private final TypeVariable carType = new TypeVariable(NameGenerator.next());
		private final TypeArrow type = new TypeArrow(
				new TypeTuple(
						Arrays.asList(new TypeTuple(Arrays.asList(carType, new TypeVariable(NameGenerator.next()))))),
				carType);

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
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
			return "(fn [_x] (get _x 0))";
		}
	};
	/**
	 * cdr operator
	 */
	public static final Operator Cdr = new Operator() {

		private final TypeVariable cdrType = new TypeVariable(NameGenerator.next());
		private final TypeArrow type = new TypeArrow(
				new TypeTuple(
						Arrays.asList(new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()), cdrType)))),
				cdrType);

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
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
			return "(fn [_x] (get _x 1))";
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
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(/ (get _x 0) (get _y 0))") + ")";
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
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(* (get _x 0) (get _y 0))") + ")";
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
			return "(fn [_x _y] " + LitInteger.clojureIntToClojureLitInteger("(- (get _x 0) (get _y 0))") + ")";
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
			return "(fn [_x] " + LitInteger.clojureIntToClojureLitInteger("(println (lang-pstr _x))") + ")";
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

			String fn = "(fn [t1 t2] (let [opt (velka.lang.types.Type/unifyTypes (first t1) (first t2))] "
					+ "(if (.isPresent opt) "
					+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + " " + LitBoolean.FALSE.toClojureCode(env, typeEnv)
					+ ")))";

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

			String fn = "(fn [t1 t2] (let [opt (velka.lang.types.Type/unifyRepresentation (first t1) (first t2))] "
					+ "(if (.isPresent opt) "
					+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + " " + LitBoolean.FALSE.toClojureCode(env, typeEnv)
					+ ")))";

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

			String fn = "(fn [e1 e2] (let [opt (velka.lang.types.Type/unifyTypes " + 
							"(" + ClojureCodeGenerator.getTypeClojureSymbol + " e1) " +
							"(" + ClojureCodeGenerator.getTypeClojureSymbol + " e2))] " +
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

			String fn = "(fn [e1 e2] (let [opt (velka.lang.types.Type/unifyRepresentation " + 
							"(" + ClojureCodeGenerator.getTypeClojureSymbol + " e1) " +
							"(" + ClojureCodeGenerator.getTypeClojureSymbol + " e2))] " +
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

	};
	/**
	 * Int:Native constructor
	 */
	public static Operator IntNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:Native";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}

	};
	/**
	 * Int constructor (really constructs Int:Native)
	 */
	public static Operator IntConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}
	};
	/**
	 * Int:String constructor
	 */
	public static Operator IntStringConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return new LitComposite(arg, TypeAtom.TypeIntString);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:String";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + Literal.clojureValueToClojureLiteral("_x", TypeAtom.TypeIntString) + ")";
		}
	};
	/**
	 * Int:Roman constructor
	 */
	public static Operator IntRomanConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return new LitComposite(arg, TypeAtom.TypeIntRoman);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:Roman";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + Literal.clojureValueToClojureLiteral("_x", TypeAtom.TypeIntRoman) + ")";
		}

	};
	/**
	 * String:Native constructor
	 */
	public static Operator StringNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "String:Native";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}

	};
	/**
	 * String constructor (really constructs String:Native)
	 */
	public static Operator StringConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "String";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}
	};
	/**
	 * Double:Native constructor
	 */
	public static Operator DoubleNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble arg = (LitDouble) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
					TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Double:Native";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}
	};
	/**
	 * Double constructor (really constructs Double:Native)
	 */
	public static Operator DoubleConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitDouble arg = (LitDouble) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
					TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Double";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}

	};
	/**
	 * Bool:Native constructor
	 */
	public static Operator BoolNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Bool:Native";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}
	};
	/**
	 * Bool constructor (really constructs Bool:Native)
	 */
	public static Operator BoolConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Bool";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] (identity _x))";
		}

	};

	/**
	 * Operator for current nano timestamp
	 */
	public static Operator Timestamp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = LitInteger.clojureIntToClojureLitInteger("(System/nanoTime)");
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			return new LitInteger(System.nanoTime());
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

	};

	/**
	 * to-str operator
	 */
	public static final Operator ToStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_arg] " + LitString.clojureStringToClojureLitString("(lang-pstr _arg)") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression e = args.get(0);
			return new LitString(e.toString());
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

	};

	/**
	 * str-split operator
	 */
	public static final Operator StrSplit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_str _by] (" + ClojureCodeGenerator.tuple2velkaListSymbol + " (map (fn [_s] "
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

			LitComposite agg = ListNative.EMPTY_LIST_NATIVE;
			for (int i = splitted.length - 1; i >= 0; i--) {
				String s = splitted[i];
				LitString lsStep = new LitString(s);
				agg = new LitComposite(new Tuple(lsStep, agg), TypeAtom.TypeListNative);
			}

			return agg;
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

			int i = Integer.parseInt(arg.value);

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

	};

}
