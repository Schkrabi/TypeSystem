package velka.core.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.regex.Pattern;

import velka.core.abstraction.Operator;
import velka.core.exceptions.FallThroughException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.core.util.OperatorBankUtil;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains utilities to work with readers in Velka
 * @author r.skrabal
 *
 */
@VelkaOperatorBank
@Description("Operators for working with java.util.Scanner.") 
@Header("Scanner")
public class Scanner {

	/**
	 * Clojure namespace for JavaArrayList
	 */
	public static final String NAMESPACE = "velka.clojure.scanner";
	
	public static final TypeName TYPE_NAME = new TypeName("Scanner");
	
	public static final TypeAtom TYPE = new TypeAtom(TYPE_NAME, TypeRepresentation.NATIVE);
	
	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	
	/**
	 * Constructor
	 */
	@VelkaConstructor
	@Description("Constructs Scanner:Native.") 
	@Name("Constructs scanner for reading files.") 
	@Syntax("(construct Scanner Native <filename>)")
	public static final Operator constructor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String path = "_path";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(path),
					ClojureHelper.applyClojureFunction(
							"java.util.Scanner.",
							ClojureHelper.applyClojureFunction(
									"java.io.FileInputStream.",
									ClojureHelper.getLiteralInnerValue(path))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitString filename = (LitString)args.get(0);
			
			Path p = Path.of(filename.value);
			java.util.Scanner scanner = null;
			try {
				 scanner = new java.util.Scanner(p);
			} catch (IOException e) {
				throw new FallThroughException(e);
			}
			
			return new LitInteropObject(scanner);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TYPE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Scanner Native";
		}
		
	};
	
	private static final Symbol closeSymbol = new Symbol("close", NAMESPACE);
	public static final Symbol closeSymbol_out = new Symbol("scanner-native-close");
	
	@VelkaOperator
	@Description("Closes scanner.") 
	@Example("(scanner-native-close (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-close <scanner>)")
	public static final Operator close = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					ClojureHelper.applyClojureFunction(
							".close",
							ClojureHelper.getLiteralInnerValue(scanner)));							
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return closeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			scanner.close();
			
			return Tuple.EMPTY_TUPLE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeTuple.EMPTY_TUPLE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return closeSymbol_out.toString();
		}
	};
	
	private static final Symbol nextLineSymbol = new Symbol("next-line", NAMESPACE);
	public static final Symbol nextLineSymbol_out = new Symbol("scanner-native-next-line");
	
	@VelkaOperator
	@Description("Advances scanner past the current line and returns the input that was skipped.") 
	@Example("(scanner-native-next-line (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-next-line <scanner>)")
	public static final Operator nextLine = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitString.clojureLit(
						ClojureHelper.applyClojureFunction(
								".nextLine",
								ClojureHelper.getLiteralInnerValue(scanner))));							
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextLineSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			String line = scanner.nextLine();
			
			return new LitString(line); 
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextLineSymbol_out.toString();
		}
		
	};

//	There is an issue with printing delimiter string in interpretation and compilation, not critical right now
	private static final Symbol delimiterSymbol = new Symbol("delimiter", NAMESPACE);
	public static final Symbol delimiterSymbol_out = new Symbol("scanner-native-delimiter");
	
//	@VelkaOperator
	@Description("Returns the string representation of Pattern this Scanner is currently using to match delimiters.") 
	@Example("(scanner-native-delimiter (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-delimiter <scanner>)")
	public static final Operator delimiter = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".toString",
									ClojureHelper.applyClojureFunction(
											".delimiter",
											ClojureHelper.getLiteralInnerValue(scanner)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return delimiterSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			Pattern pattern = scanner.delimiter();
			
			return new LitString(pattern.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return delimiterSymbol_out.toString();
		}
	};
	
	private static final Symbol findInLineSymbol = new Symbol("find-in-line", NAMESPACE);
	public static final Symbol findInLineSymbol_out = new Symbol("scanner-native-find-in-line");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-in-line (construct Scanner Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-find-in-line <scanner> <pattern>)")
	public static final Operator findInLine = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String pattern = "_pattern";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, pattern),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
								".findInLine",
								ClojureHelper.getLiteralInnerValue(scanner),
								ClojureHelper.getLiteralInnerValue(pattern))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return findInLineSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString pattern = (LitString)args.get(1);
			
			String s;
			
			s = scanner.findInLine(pattern.value);
			
			return new LitString(s); 
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return findInLineSymbol_out.toString();
		}
	};
	
	private static final Symbol findWithinHorizonSymbol = new Symbol("find-within-horizon", NAMESPACE);
	public static final Symbol findWithinHorizonSymbol_out = new Symbol("scanner-native-find-within-horizon");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-within-horizon (construct Scanner Native \"test-file\") \"a*b\" 42)") 
	@Syntax("(scanner-native-find-within-horizon <scanner> <pattern> <horizon>)")
	public static final Operator findWithinHorizon = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String pattern = "_pattern";
			String horizon = "_horizon";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, pattern, horizon),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
								".findWithinHorizon",
								ClojureHelper.getLiteralInnerValue(scanner),
								ClojureHelper.getLiteralInnerValue(pattern),
								ClojureHelper.getLiteralInnerValue(horizon))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return findWithinHorizonSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString pattern = (LitString)args.get(1);
			LitInteger horizon = (LitInteger)args.get(2);
			
			String s;
			
			s = scanner.findWithinHorizon(pattern.value, (int) horizon.value);
			
			return new LitString(s); 
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative, TypeAtom.TypeIntNative), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		public String toString() {
			return findWithinHorizonSymbol_out.toString();
		}
		
	};
	
	private static final Symbol hasNextSymbol = new Symbol("has-next", NAMESPACE);
	public static final Symbol hasNextSymbol_out = new Symbol("scanner-native-has-next");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-within-horizon (construct Scanner Native \"test-file\") \"a*b\" 42)") 
	@Syntax("(scanner-native-find-within-horizon <scanner>)")
	public static final Operator hasNext = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNext",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			if(scanner.hasNext()) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return hasNextSymbol_out.toString();
		}
	};
	
	private static final Symbol hasNextPatternSymbol = new Symbol("has-next-pattern", NAMESPACE);
	public static final Symbol hasNextPatternSymbol_out = new Symbol("scanner-native-has-next-pattern");
	
	@VelkaOperator
	@Description("Returns true if the next token matches the pattern constructed from the specified string.") 
	@Example("(scanner-native-has-next-pattern (construct Scanner Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-has-next-pattern <scanner> <pattern>)")
	public static final Operator hasNextPattern = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String pattern = "_pattern";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, pattern),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNext",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(pattern))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextPatternSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString pattern = (LitString)args.get(1);
			
			if(scanner.hasNext(pattern.value)) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return hasNextPatternSymbol_out.toString();
		}
		
	};
	
	private static final Symbol hasNextBooleanSymbol = new Symbol("has-next-boolean", NAMESPACE);
	public static final Symbol hasNextBooleanSymbol_out = new Symbol("scanner-native-has-next-boolean");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as a boolean value using a case insensitive pattern created from the string \"true|false\".") 
	@Example("(scanner-native-has-next-boolean (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-boolean <scanner>)")
	public static final Operator hasNextBoolean = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNextBoolean",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextBooleanSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			if(scanner.hasNextBoolean()) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return hasNextBooleanSymbol_out.toString();
		}
		
	};
	
	private static final Symbol hasNextDoubleSymbol = new Symbol("has-next-double", NAMESPACE);
	public static final Symbol hasNextDoubleSymbol_out = new Symbol("scanner-native-has-next-double");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as a double value using the scanner-native-next-double function.") 
	@Example("(scanner-native-has-next-double (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-double <scanner>)")
	public static final Operator hasNextDouble = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNextDouble",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextDoubleSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			if(scanner.hasNextDouble()) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return hasNextDoubleSymbol_out.toString();
		}
	};
	
	private static final Symbol hasNextIntSymbol = new Symbol("has-next-int", NAMESPACE);
	public static final Symbol hasNextIntSymbol_out = new Symbol("scanner-native-has-next-int");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as an int value in the default radix using the scanner-native-next-int function.") 
	@Example("(scanner-native-has-next-int (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-int <scanner>)")	
	public static final Operator hasNextInt = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNextInt",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextIntSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			if(scanner.hasNextInt()) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return hasNextIntSymbol_out.toString();
		}
		
	};
	
	private static final Symbol hasNextIntRadixSymbol = new Symbol("has-next-int-radix", NAMESPACE);
	public static final Symbol hasNextIntRadixSymbol_out = new Symbol("scanner-native-has-next-int-radix");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as an int value in the specified radix using the next-int-radix function.") 
	@Example("(scanner-native-has-next-int-radix (construct Scanner Native \"test-file\" 16))") 
	@Syntax("(scanner-native-has-next-int-radix <scanner> <radix>)")	
	public static final Operator hasNextIntRadix = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String radix = "_radix";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, radix),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNextInt",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(radix))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextIntRadixSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitInteger radix = (LitInteger)args.get(1);
			
			if(scanner.hasNextInt((int)radix.value)) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeIntNative), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return hasNextIntRadixSymbol_out.toString();
		}
	};
	
	private static final Symbol hasNextLineSymbol = new Symbol("has-next-line", NAMESPACE);
	public static final Symbol hasNextLineSymbol_out = new Symbol("scanner-native-has-next-line");
	
	@VelkaOperator
	@Description("Returns true if there is another line in the input of this scanner.") 
	@Example("(scanner-native-has-next-line (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-line <scanner>)")
	public static final Operator hasNextLine = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									".hasNextLine",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return hasNextLineSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			
			if(scanner.hasNextLine()) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString(){
			return hasNextLineSymbol_out.toString();
		}
	};
	
	private static final Symbol nextSymbol = new Symbol("velka-next", NAMESPACE);
	public static final Symbol nextSymbol_out = new Symbol("scanner-native-next");
	
	@VelkaOperator
	@Description("Finds and returns the next complete token from this scanner.") 
	@Example("(scanner-native-next (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-next <scanner>)")
	public static final Operator next = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".next",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			String s = scanner.next();
			
			return new LitString(s);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextSymbol_out.toString();
		}
	};
	
	private static final Symbol nextPatternSymbol = new Symbol("next-pattern", NAMESPACE);
	public static final Symbol nextPatternSymbol_out = new Symbol("scanner-native-next-pattern");
	
	@VelkaOperator
	@Description("Returns the next token if it matches the pattern constructed from the specified string.") 
	@Example("(scanner-native-next-pattern (construct Scanner Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-next-pattern <scanner> <pattern>)")
	public static final Operator nextPattern = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String pattern = "_pattern";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, pattern),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".next",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(pattern))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextPatternSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString pattern = (LitString)args.get(1);
			
			String s = scanner.next(pattern.value);
			
			return new LitString(s);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextPatternSymbol_out.toString();
		}
	};
	
	private static final Symbol nextBoolSymbol = new Symbol("next-bool", NAMESPACE);
	public static final Symbol nextBoolSymbol_out = new Symbol("scanner-native-next-bool");
	
	@VelkaOperator
	@Description("Scans the next token of the input into a boolean value and returns that value.") 
	@Example("(scanner-native-next-bool (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-next-bool <scanner>)")
	public static final Operator nextBool = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextBoolean",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextBoolSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			if(scanner.nextBoolean()){
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextBoolSymbol_out.toString();
		}
		
	};
	
	private static final Symbol nextDoubleSymbol = new Symbol("next-double", NAMESPACE);
	public static final Symbol nextDoubleSymbol_out = new Symbol("scanner-native-next-double");
	
	@VelkaOperator
	@Description("Scans the next token of the input as a double.") 
	@Example("(scanner-native-next-double (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-next-double <scanner>)")
	public static final Operator nextDouble = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitDouble.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextDouble",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextDoubleSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			double d = scanner.nextDouble();
			
			return new LitDouble(d);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeDoubleNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextDoubleSymbol_out.toString();
		}
		
	};
	
	private static final Symbol nextIntSymbol = new Symbol("next-int", NAMESPACE);
	public static final Symbol nextIntSymbol_out = new Symbol("scanner-native-next-int");
	
	@VelkaOperator
	@Description("Scans the next token of the input as an int.") 
	@Example("(scanner-native-next-int (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-next-int <scanner>)")
	public static final Operator nextInt = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextInt",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextIntSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			long l = (long)scanner.nextInt();
			
			return new LitInteger(l);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextIntSymbol_out.toString();
		}
	};
	
	private static final Symbol radixSymbol = new Symbol("radix", NAMESPACE);
	public static final Symbol radixSymbol_out = new Symbol("scanner-native-radix");
	
	@VelkaOperator
	@Description("Returns this scanner's default radix.") 
	@Example("(scanner-native-radix (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-radix <scanner>)")
	public static final Operator radix = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".radix",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return radixSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			long l = (long)scanner.radix();
			
			return new LitInteger(l);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return radixSymbol_out.toString();
		}
	};
	
	private static final Symbol resetSymbol = new Symbol("reset", NAMESPACE);
	public static final Symbol resetSymbol_out = new Symbol("scanner-native-reset");
	
	@VelkaOperator
	@Description("Resets this scanner.") 
	@Example("(scanner-native-reset (construct Scanner Native \"test-file\"))") 
	@Syntax("(scanner-native-reset <scanner>)")
	public static final Operator reset = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner),
					LitComposite.clojureLit(
							TYPE,
							ClojureHelper.applyClojureFunction(
									".reset",
									ClojureHelper.getLiteralInnerValue(scanner))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return resetSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			scanner.reset();
			
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE), TYPE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return resetSymbol_out.toString();
		}
		
	};
	
	private static final Symbol skipSymbol = new Symbol("skip", NAMESPACE);
	public static final Symbol skipSymbol_out = new Symbol("scanner-native-skip");
	
	@VelkaOperator
	@Description("Skips input that matches a pattern constructed from the specified string.") 
	@Example("(scanner-native-skip (construct Scanner Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-skip <scanner> <pattern>)")
	public static final Operator skip = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String pattern = "_pattern";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, pattern),
					LitComposite.clojureLit(
							TYPE,
							ClojureHelper.applyClojureFunction(
									".skip",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(pattern))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return skipSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString pattern = (LitString)args.get(1);
			
			scanner.skip(pattern.value);
			
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative), TYPE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return skipSymbol_out.toString();
		}
	};
	
	//Delimiters not currently supported
	//NOT TESTED!
	private static final Symbol useDelimiterSymbol = new Symbol("use-delimiter", NAMESPACE);
	public static final Symbol useDelimiterSymbol_out = new Symbol("scanner-native-use-delimiter");
	
	//@VelkaOperator
	@Description("Sets this scanner's delimiting pattern to a pattern constructed from the specified String.") 
	@Example("(scanner-native-use-delimiter (construct Scanner Native \"test-file\") \"a\")") 
	@Syntax("(scanner-native-use-delimiter <scanner> <delimiter>)")
	public static final Operator useDelimiter = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String delimiter = "_delimiter";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, delimiter),
					LitComposite.clojureLit(
							TYPE,
							ClojureHelper.applyClojureFunction(
									".useDelimiter",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(delimiter))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return useDelimiterSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitString delimiter = (LitString)args.get(1);
			
			scanner.useDelimiter(delimiter.value);
			
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeStringNative), TYPE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return useDelimiterSymbol_out.toString();
		}
		
	};
	
	private static final Symbol useRadixSymbol = new Symbol("use-radix", NAMESPACE);
	public static final Symbol useRadixSymbol_out = new Symbol("scanner-native-use-radix");
	
	@VelkaOperator
	@Description("Sets this scanner's default radix to the specified radix.") 
	@Example("(scanner-native-use-radix (construct Scanner Native \"test-file\") \"a\")") 
	@Syntax("(scanner-native-use-radix <scanner> <delimiter>)")
	public static final Operator useRadix = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String scanner = "_scanner";
			String radix = "_radix";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(scanner, radix),
					LitComposite.clojureLit(
							TYPE,
							ClojureHelper.applyClojureFunction(
									".useRadix",
									ClojureHelper.getLiteralInnerValue(scanner),
									ClojureHelper.getLiteralInnerValue(radix))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return useRadixSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject lji = (LitInteropObject)lc.value;
			java.util.Scanner scanner = (java.util.Scanner)lji.javaObject;
			
			LitInteger radix = (LitInteger)args.get(1);
			
			scanner.useRadix((int)radix.value);
			
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TYPE, TypeAtom.TypeIntNative), TYPE);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return useRadixSymbol_out.toString();
		}
	};
	
	public static final Path VELKA_CLOJURE_SCANNER_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_SCANNER_NAME = Paths.get("scanner.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_SCANNER_PATH.resolve(VELKA_CLOJURE_SCANNER_NAME);
	
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, OperatorBankUtil.writeDefinitions(Scanner.class, NAMESPACE));
	}
}
