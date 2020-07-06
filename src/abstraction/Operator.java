package abstraction;

import java.util.Arrays;

import expression.Expression;
import literal.LitBoolean;
import literal.LitComposite;
import literal.LitDouble;
import literal.LitInteger;
import literal.LitString;
import expression.Tuple;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import util.RomanNumbers;

/**
 * Expression for meta-language operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Operator extends Abstraction {

	/**
	 * Addition (+) operator
	 */
	public static final Operator Addition = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger x = (LitInteger) args.get(0);
			LitInteger y = (LitInteger) args.get(1);

			return new LitInteger(x.value + y.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "+";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(+ (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Bit and (&) operator
	 */
	public static final Operator BitAnd = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value & arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-and";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(bit-and (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Bit or (|) operator
	 */
	public static final Operator BitOr = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value | arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-or";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(bit-or (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
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
		public Pair<Type, Substitution> infer(Environment env) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(0);
		}

		@Override
		public String toString() {
			return "car";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (get _x 0))" + "{:lang-type " + p.first.clojureTypeRepresentation() + "})";
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
		public Pair<Type, Substitution> infer(Environment env) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(1);
		}

		@Override
		public String toString() {
			return "cdr";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (get _x 1))" + "{:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Concatenation operator
	 */
	public static final Operator Concantenation = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg0 = (LitString) args.get(0);
			LitString arg1 = (LitString) args.get(1);

			return new LitString(arg0.value + arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) {
			return "`([[" + TypeAtom.TypeStringNative.clojureTypeRepresentation() + " "
					+ TypeAtom.TypeStringNative.clojureTypeRepresentation() + "] ~str])";
		}

		@Override
		public String toString() {
			return "concat";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(str (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeStringNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Division (/) operator
	 */
	public static final Operator Division = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value / arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "/";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(/ (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression arg0 = args.get(0);
			Expression arg1 = args.get(1);

			return arg0.equals(arg1) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "equals?";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(= (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Lesser than (<) operator
	 */
	public static final Operator LesserThan = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value < arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "<";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(< (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Multiplication (*) operator
	 */
	public static final Operator Multiplication = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value * arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "*";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(* (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Not operator
	 */
	public static final Operator Not = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);

			return arg.value ? LitBoolean.FALSE : LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		public String toString() {
			return "not";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (with-meta [(not (get _x 0))] " + "{:lang-type "
					+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Numeric equal (=) operator
	 */
	public static final Operator NumericEqual = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value == arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "=";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(= (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Subtraction (-) operator
	 */
	public static final Operator Subtraction = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value - arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "-";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(- (get _x 0) (get _y 0))] " + "{:lang-type "
					+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Println operator
	 */
	public static final Operator PrintlnOperator = new Operator() {

		private final TypeArrow type = new TypeArrow(
				new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()))), TypeAtom.TypeIntNative);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression arg = (Expression) args.get(0);

			String s = arg.toString();
			System.out.println(s);

			return new LitInteger(s.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return "`([" + type.ltype.clojureTypeRepresentation() + " ~println])";
		}

		@Override
		public String toString() {
			return "println";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x _y] (with-meta [(println (get _x 0))] " + "{:lang-type "
					+ ((TypeArrow) p.first).rtype.clojureTypeRepresentation() + "}))" + "{:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Int:Native constructor
	 */
	public static Operator IntNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:Native";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Int constructor (really constructs Int:Native)
	 */
	public static Operator IntConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Int:String constructor
	 */
	public static Operator IntStringConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return new LitComposite(arg, TypeAtom.TypeIntString);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:String";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (with-meta [_x] {:lang-type "
					+ TypeAtom.TypeIntString.clojureTypeRepresentation() + "})) {:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Int:Roman constructor
	 */
	public static Operator IntRomanConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return new LitComposite(arg, TypeAtom.TypeIntRoman);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Int:Roman";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (with-meta [_x] {:lang-type "
					+ TypeAtom.TypeIntRoman.clojureTypeRepresentation() + "})) {:lang-type "
					+ p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * String:Native constructor
	 */
	public static Operator StringNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "String:Native";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}

	};
	/**
	 * String constructor (really constructs String:Native)
	 */
	public static Operator StringConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "String";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}
	};
	/**
	 * Double:Native constructor
	 */
	public static Operator DoubleNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitDouble arg = (LitDouble) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
					TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Double:Native";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Double constructor (really constructs Double:Native)
	 */
	public static Operator DoubleConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitDouble arg = (LitDouble) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
					TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Double";
		}
		
		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Bool:Native constructor
	 */
	public static Operator BoolNativeConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Bool:Native";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}
	};

	/**
	 * Bool constructor (really constructs Bool:Native)
	 */
	public static Operator BoolConstructor = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "Bool";
		}
		
		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta (fn [_x] (identity _x)) {:lang-type " + p.first.clojureTypeRepresentation() + "})";
		}

	};

	/**
	 * Conversion from Int:Native to Int:Roman
	 */
	public static final Operator IntNativeToIntRoman = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);

			return new LitComposite(new LitString(RomanNumbers.int2roman(arg.value)), TypeAtom.TypeIntRoman);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "IntNative2IntRoman";
		}
		
		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta " 
					+ "(fn [_x] (with-meta " 
						+ "[(with-meta "
							+ "(" + RomanNumbers.int2RomanClojure + " (get _x 0))"
							+ "{:lang-type " + TypeAtom.TypeStringNative.clojureTypeRepresentation() + "})]"
						+ "{:lang-type " + TypeAtom.TypeIntRoman.clojureTypeRepresentation() + "}))"
					+ "{:lang-type " + p.first.clojureTypeRepresentation() + "})";
							
		}

	};

	/**
	 * Conversion from Int:Native to Int:String
	 */
	public static final Operator IntNativeToIntString = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return new LitComposite(new LitString(Integer.toString(arg.value)), TypeAtom.TypeIntString);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "IntNative2IntString";
		}

		@Override
		protected String implementationsToClojure(Environment env) throws AppendableException {
			Pair<Type, Substitution> p = this.infer(env);
			return "(with-meta " 
					+ "(fn [_x] (with-meta " 
						+ "[(with-meta "
							+ "(Integer/toString (get _x 0))"
							+ "{:lang-type " + TypeAtom.TypeStringNative.clojureTypeRepresentation() + "})]"
						+ "{:lang-type " + TypeAtom.TypeIntString.clojureTypeRepresentation() + "}))"
					+ "{:lang-type " + p.first.clojureTypeRepresentation() + "})";
							
		}

	};

	/**
	 * Conversion from Int:Roman to Int:Native
	 */
	public static final Operator IntRomanToIntNative = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			return new LitInteger(RomanNumbers.roman2int(strArg.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return "`([[" + TypeAtom.TypeIntRoman.clojureTypeRepresentation() + "] ~(fn [_x] ("
					+ RomanNumbers.roman2intClojure + " _x))])";
		}

		@Override
		public String toString() {
			return "IntRoman2IntNative";
		}

	};

	/**
	 * Conversion from Int:Roman to Int:String
	 */
	public static final Operator IntRomanToIntString = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			int value = RomanNumbers.roman2int(strArg.value);
			return new LitComposite(new LitString(Integer.toString(value)), TypeAtom.TypeIntString);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return "`([[" + TypeAtom.TypeIntRoman.clojureTypeRepresentation() + "] ~(fn [_x] (str ("
					+ RomanNumbers.roman2intClojure + " _x)))])";
		}

		@Override
		public String toString() {
			return "IntRoman2IntString";
		}

	};

	/**
	 * Conversion from Int:String to Int:Native
	 */
	public static final Operator IntStringToIntNative = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;

			return new LitInteger(Integer.parseInt(strArg.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return "`([[" + TypeAtom.TypeIntString.clojureTypeRepresentation() + "] ~(fn [_x] (Integer/parseInt _x))])";
		}

		@Override
		public String toString() {
			return "IntString2IntNative";
		}
	};

	/**
	 * Conversion from Int:String to Int:Roman
	 */
	public static final Operator IntStringToIntRoman = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			int value = Integer.parseInt(strArg.value);
			return new LitComposite(new LitString(RomanNumbers.int2roman(value)), TypeAtom.TypeIntRoman);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) throws AppendableException {
			return "`([[" + TypeAtom.TypeIntString.clojureTypeRepresentation() + "] ~(fn [_x] ("
					+ RomanNumbers.int2RomanClojure + " (Integer/parseInt _x)))])";
		}

		@Override
		public String toString() {
			return "IntString2IntRoman";
		}

	};

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this;
	}
}
