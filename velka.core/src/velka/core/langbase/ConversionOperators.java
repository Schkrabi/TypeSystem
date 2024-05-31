package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.RomanNumbers;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConversion;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains Velka's build-in conversions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("Conversions of build-in representations") 
@Header("General Conversions")
public final class ConversionOperators extends OperatorBank{
	
	/**
	 * Namespace 
	 */
	public static final String NAMESPACE = "velka.clojure.conversions";

	/**
	 * Conversion from Int:Native to Int:Roman
	 */
	@VelkaConversion
	@Description("Converts integer in native represetation into roman representation. This is shorthand for _(convert Int:Native Int:Roman arg)_.") 
	@Example("(IntNative2IntRoman 42) ; = \"XLII\"") 
	@Syntax("(IntNative2IntRoman <arg>)")
	public static final Conversion IntNativeToIntRoman = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					LitComposite.clojureLit(
							TypeAtom.TypeIntRoman, 
							ClojureHelper.applyClojureFunction(
									RomanNumbers.int2RomanClojure, 
									arg)));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-native-2-int-roman", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	
	};
	
	/**
	 * Conversion from Int:Native to Int:String
	 */
	@VelkaConversion
	@Description("Converts integer in native represetation into string representation. This is shorthand for _(convert Int:Native Int:String arg)_.") 
	@Example("(IntNative2IntString 42) ; = \"42\"") 
	@Syntax("(IntNative2IntString <arg>)")
	public static final Conversion IntNativeToIntString = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					LitComposite.clojureLit(
							TypeAtom.TypeIntString, 
							ClojureHelper.applyClojureFunction(
									"Integer/toString", 
									arg)));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-native-2-int-string", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	
	};
	
	/**
	 * Conversion from Int:Roman to Int:Native
	 */
	@VelkaConversion
	@Description("Converts integer in roman represetation into native representation. This is shorthand for _(convert Int:Roman Int:Native arg)_.") 
	@Example("(IntRoman2IntNative (construct Int Roman \"XLII\")) ; = 42") 
	@Syntax("(IntRoman2IntNative <arg>)")
	public static final Conversion IntRomanToIntNative = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction(
						RomanNumbers.roman2intClojure, 
						ClojureHelper.applyClojureFunction(
								"first", 
								arg)));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-roman-2-int-native", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	};
	
	/**
	 * Conversion from Int:Roman to Int:String
	 */
	@VelkaConversion
	@Description("Converts integer in roman represetation into string representation. This is shorthand for _(convert Int:Roman Int:String arg)_.") 
	@Example("(IntRoman2IntString (construct Int Roman \"XLII\")) ; = \"42\"") 
	@Syntax("(IntRoman2IntString <arg>)")
	public static final Conversion IntRomanToIntString = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					LitComposite.clojureLit(
							TypeAtom.TypeIntString, 
							ClojureHelper.applyClojureFunction(
								"Integer/toString",
								ClojureHelper.applyClojureFunction(
										RomanNumbers.roman2intClojure, 
										ClojureHelper.applyClojureFunction("first", arg)))));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-roman-2-int-string", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	
	};
	
	/**
	 * Conversion from Int:String to Int:Native
	 */
	@VelkaConversion
	@Description("Converts integer in string represetation into native representation. This is shorthand for _(convert Int:String Int:Native arg)_.") 
	@Example("(IntString2IntNative (construct Int String \"42\")) ; = 42") 
	@Syntax("(IntString2IntNative <arg>)")
	public static final Conversion IntStringToIntNative = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction(
						"Integer/parseInt", 
						ClojureHelper.applyClojureFunction(
								"first", 
								arg)));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-string-2-int-native", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	};
	
	/**
	 * Conversion from Int:String to Int:Roman
	 */
	@VelkaConversion
	@Description("Converts integer in string represetation into roman representation. This is shorthand for _(convert Int:String Int:Roman arg)_.") 
	@Example("(IntString2IntRoman (construct Int String \"42\")) => \"XLII\"") 
	@Syntax("(IntString2IntRoman <arg>)")
	public static final Conversion IntStringToIntRoman = new Conversion() {
	
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
			String arg = "_arg";
			return ClojureHelper.fnHelper(
					List.of(arg),
					LitComposite.clojureLit(
							TypeAtom.TypeIntString, 
							ClojureHelper.applyClojureFunction(
									RomanNumbers.int2RomanClojure, 
									ClojureHelper.applyClojureFunction(
											"Integer/parseInt",
											ClojureHelper.applyClojureFunction("first", arg)))));
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-string-2-int-roman", NAMESPACE);
		}

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitInteger(1));
		}
	};

	public static final Path VELKA_CLOJURE_CONVERSIONS_PATH = Paths.get("velka", "clojure");

	public static final Path VELKA_CLOJURE_CONVERSIONS_NAME = Paths.get("conversions.clj");

	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return VELKA_CLOJURE_CONVERSIONS_PATH;
	}

	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_CONVERSIONS_NAME;
	}

	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		//No types to initialize		
	}
	
	private ConversionOperators() {}
	private static ConversionOperators instance = null;
	public static ConversionOperators singleton() {
		if(instance == null) {
			instance = new ConversionOperators();
		}
		return instance;
	}

}
