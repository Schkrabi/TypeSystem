package velka.core.abstraction;

import java.util.Arrays;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
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

/**
 * This class contains contructor operators
 * @author Mgr. Radomir Skrabal
 *
 */
public final class ConstructorOperators {

	/**
	 * Int:Native constructor
	 */
	public static Operator IntNativeConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-native", Operators.NAMESPACE);
		}
	
	};
	/**
	 * Int constructor (really constructs Int:Native)
	 */
	public static Operator IntConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int", Operators.NAMESPACE);
		}
	};
	/**
	 * Int:String constructor
	 */
	public static Operator IntStringConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-string", Operators.NAMESPACE);
		}
	};
	/**
	 * Int:Roman constructor
	 */
	public static Operator IntRomanConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-roman", Operators.NAMESPACE);
		}
	
	};
	/**
	 * String:Native constructor
	 */
	public static Operator StringNativeConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-string-native", Operators.NAMESPACE);
		}
	
	};
	/**
	 * String constructor (really constructs String:Native)
	 */
	public static Operator StringConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-string", Operators.NAMESPACE);
		}
	};
	/**
	 * Double:Native constructor
	 */
	public static Operator DoubleNativeConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-native", Operators.NAMESPACE);
		}
	};
	/**
	 * Double constructor (really constructs Double:Native)
	 */
	public static Operator DoubleConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double", Operators.NAMESPACE);
		}
	
	};
	/**
	 * Bool:Native constructor
	 */
	public static Operator BoolNativeConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bool-native", Operators.NAMESPACE);
		}
	};
	/**
	 * Bool constructor (really constructs Bool:Native)
	 */
	public static Operator BoolConstructor = new Operator() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bool", Operators.NAMESPACE);
		}
	
	};
	public static final String NAMESPACE = "velka.clojure.constructors";

}
