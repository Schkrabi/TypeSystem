package velka.core.abstraction;

import java.util.Arrays;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.core.literal.Literal;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.Pair;
import velka.util.RomanNumbers;

/**
 * This class contains Velka's build-in conversions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class ConversionOperators {
	
	/**
	 * Namespace 
	 */
	public static final String NAMESPACE = "velka.clojure.conversions";

	/**
	 * Conversion from Int:Native to Int:Roman
	 */
	public static final Operator IntNativeToIntRoman = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
	
			return new LitComposite(new LitString(RomanNumbers.int2roman(arg.value)), TypeAtom.TypeIntRoman);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntNative2IntRoman";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + Literal.clojureValueToClojureLiteral(
					LitString.clojureStringToClojureLitString("(" + RomanNumbers.int2RomanClojure + " (get _x 0))"),
					TypeAtom.TypeIntRoman) + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-native-2-int-roman", NAMESPACE);
		}
	
	};
	/**
	 * Conversion from Int:Native to Int:String
	 */
	public static final Operator IntNativeToIntString = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return new LitComposite(new LitString(Long.toString(arg.value)), TypeAtom.TypeIntString);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntNative2IntString";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + Literal.clojureValueToClojureLiteral(
					LitString.clojureStringToClojureLitString("(Integer/toString (get _x 0))"), TypeAtom.TypeIntString)
					+ ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-native-2-int-string", NAMESPACE);
		}
	
	};
	/**
	 * Conversion from Int:Roman to Int:Native
	 */
	public static final Operator IntRomanToIntNative = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			return new LitInteger(RomanNumbers.roman2int(strArg.value));
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntRoman2IntNative";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + LitInteger
					.clojureIntToClojureLitInteger("(" + RomanNumbers.roman2intClojure + " (get (get _x 0) 0))") + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-roman-2-int-native", NAMESPACE);
		}
	};
	/**
	 * Conversion from Int:Roman to Int:String
	 */
	public static final Operator IntRomanToIntString = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			int value = RomanNumbers.roman2int(strArg.value);
			return new LitComposite(new LitString(Integer.toString(value)), TypeAtom.TypeIntString);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntRoman2IntString";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] "
					+ Literal.clojureValueToClojureLiteral(
							LitString.clojureStringToClojureLitString(
									"(str (" + RomanNumbers.roman2intClojure + " (get (get _x 0) 0)))"),
							TypeAtom.TypeIntString)
					+ ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-roman-2-int-string", NAMESPACE);
		}
	
	};
	/**
	 * Conversion from Int:String to Int:Native
	 */
	public static final Operator IntStringToIntNative = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
	
			return new LitInteger(Integer.parseInt(strArg.value));
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntString2IntNative";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + LitInteger.clojureIntToClojureLitInteger("(Integer/parseInt (get (get _x 0) 0))")
					+ ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-string-2-int-native", NAMESPACE);
		}
	};
	/**
	 * Conversion from Int:String to Int:Roman
	 */
	public static final Operator IntStringToIntRoman = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitComposite arg = (LitComposite) args.get(0);
			LitString strArg = (LitString) arg.value;
			int value = Integer.parseInt(strArg.value);
			return new LitComposite(new LitString(RomanNumbers.int2roman(value)), TypeAtom.TypeIntRoman);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "IntString2IntRoman";
		}
	
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(fn [_x] " + Literal.clojureValueToClojureLiteral(
					LitString.clojureStringToClojureLitString(
							"(" + RomanNumbers.int2RomanClojure + " (Integer/parseInt (get (get _x 0) 0)))"),
					TypeAtom.TypeIntRoman) + ")";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-string-2-int-roman", NAMESPACE);
		}
	};

}
