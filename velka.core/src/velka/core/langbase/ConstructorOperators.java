package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import velka.core.abstraction.Constructor;
import velka.core.exceptions.DuplicateTypeDefinitionException;
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
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains contructor operators
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("General constructors for basic Velka representations.") 
@Header("General Constructors")
public final class ConstructorOperators extends OperatorBank {

	public static final String NAMESPACE = "velka.clojure.constructors";
	
	/**
	 * Int:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor IntNativeConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-native", NAMESPACE);
		}
		
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			typeEnv.addPrimitiveConstructor(TypeAtom.TypeIntNative, env);
		}
	
	};
	
	/**
	 * Int constructor (really constructs Int:Native)
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor IntConstructor = new Constructor() {
	
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
			return new Symbol("velka-int", NAMESPACE);
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {	}
	};
	/**
	 * Int:String constructor
	 */
	@VelkaConstructor
	@Description("Constructs Int:String from string.") 
	@Name("Construct Int String") 
	@Syntax("(construct Int String <string>)")
	public static Constructor IntStringConstructor = new Constructor() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(List.of(arg), LitComposite.clojureLit(TypeAtom.TypeIntString, arg));
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-string", NAMESPACE);
		}
	};
	/**
	 * Int:Roman constructor
	 */
	@VelkaConstructor
	@Description("Construct Int Roman from string.") 
	@Name("Construct Int Roman") 
	@Syntax("(construct Int Roman <string>)")
	public static Constructor IntRomanConstructor = new Constructor() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(List.of(arg), LitComposite.clojureLit(TypeAtom.TypeIntRoman, arg));
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-int-roman", NAMESPACE);
		}
	
	};
	
	/**
	 * String:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor StringNativeConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-string-native", NAMESPACE);
		}
	
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			typeEnv.addPrimitiveConstructor(TypeAtom.TypeStringNative, env);
		}
	};
	
	/**
	 * String constructor (really constructs String:Native)
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor StringConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-string", NAMESPACE);
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		}
	};
	
	/**
	 * Double:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor DoubleNativeConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-native", NAMESPACE);
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			typeEnv.addPrimitiveConstructor(TypeAtom.TypeDoubleNative, env);
		}
	};
	
	/**
	 * Double constructor (really constructs Double:Native)
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor DoubleConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double", NAMESPACE);
		}
	
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		}
	};
	
	/**
	 * Bool:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor BoolNativeConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bool-native", NAMESPACE);
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			typeEnv.addPrimitiveConstructor(TypeAtom.TypeBoolNative, env);
		}
	};
	
	/**
	 * Bool constructor (really constructs Bool:Native)
	 */	
	@VelkaConstructor(showInDoc = false)
	public static Constructor BoolConstructor = new Constructor() {
	
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
			return "identity";
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bool", NAMESPACE);
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		}
	
	};

	public static final Path VELKA_CLOJURE_CONSTRUCTORS_PATH = Paths.get("velka", "clojure");

	public static final Path VELKA_CLOJURE_CONSTRUCTORS_NAME = Paths.get("constructors.clj");
	@Override
	public String getNamespace() {
		return NAMESPACE;
	}
	@Override
	public Path getPath() {
		return VELKA_CLOJURE_CONSTRUCTORS_PATH;
	}
	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_CONSTRUCTORS_NAME;
	}
	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		// Int
		typeEnv.addType(TypeAtom.TypeInt.name);
		typeEnv.addRepresentation(TypeAtom.TypeIntNative);
		typeEnv.addRepresentation(TypeAtom.TypeIntRoman);
		typeEnv.addRepresentation(TypeAtom.TypeIntString);

		// Bool
		typeEnv.addType(TypeAtom.TypeBool.name);
		typeEnv.addRepresentation(TypeAtom.TypeBoolNative);

		// String
		typeEnv.addType(TypeAtom.TypeString.name);
		typeEnv.addRepresentation(TypeAtom.TypeStringNative);

		// Double
		typeEnv.addType(TypeAtom.TypeDouble.name);
		typeEnv.addRepresentation(TypeAtom.TypeDoubleNative);	
	}
	
	private ConstructorOperators() {}
	private static ConstructorOperators instance = null;
	public static ConstructorOperators singleton() {
		if(instance == null) {
			instance = new ConstructorOperators();
		}
		return instance;
	}

}
