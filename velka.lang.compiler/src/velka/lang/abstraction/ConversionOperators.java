package velka.lang.abstraction;

import java.util.Arrays;
import java.util.Optional;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitInteger;
import velka.lang.literal.LitString;
import velka.lang.literal.Literal;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;
import velka.lang.util.RomanNumbers;

/**
 * This class contains Velka's build-in conversions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public final class ConversionOperators {

	/**
	 * Conversion from Int:Native to Int:Roman
	 */
	public static final Operator IntNativeToIntRoman = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	
	};
	/**
	 * Conversion from Int:Native to Int:String
	 */
	public static final Operator IntNativeToIntString = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	
	};
	/**
	 * Conversion from Int:Roman to Int:Native
	 */
	public static final Operator IntRomanToIntNative = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	};
	/**
	 * Conversion from Int:Roman to Int:String
	 */
	public static final Operator IntRomanToIntString = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	
	};
	/**
	 * Conversion from Int:String to Int:Native
	 */
	public static final Operator IntStringToIntNative = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	};
	/**
	 * Conversion from Int:String to Int:Roman
	 */
	public static final Operator IntStringToIntRoman = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
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
	};

}
