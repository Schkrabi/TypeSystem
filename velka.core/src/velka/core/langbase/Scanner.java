package velka.core.langbase;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.regex.Pattern;

import com.sun.codemodel.JExpr;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Operator;
import velka.core.exceptions.FallThroughException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.java.CodeModelInstance;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
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
public class Scanner extends OperatorBank{
	public static final Symbol constructorSymbol = new Symbol("velka_construct", Scanner.singleton().getNamespace());
	
	/**
	 * Constructor
	 */
	@VelkaConstructor
	@Description("Constructs Scanner:Native.") 
	@Name("Constructs scanner for reading files.") 
	@Syntax("(construct Scanner:Native <filename>)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String path = "_path";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(path),
					ClojureHelper.applyClojureFunction(
							"java.util.Scanner.",
							ClojureHelper.applyClojureFunction(
									"java.io.FileInputStream.",
									path)));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			LitString filename = (LitString)args.get(0);
			
			Path p = Path.of(filename.value);
			java.util.Scanner scanner = null;
			try {
				 scanner = new java.util.Scanner(p);
			} catch (IOException e) {
				throw new FallThroughException(e);
			}
			
			return new LitInteropObject(scanner, TypeAtom.TypeScannerNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeScannerNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Scanner:Native";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var pathCl = CodeModelInstance.instance().ref(Path.class);
			var scannerCl = CodeModelInstance.instance().ref(java.util.Scanner.class);
			var rteCl = CodeModelInstance.instance().ref(RuntimeException.class);
			var ioeCl = CodeModelInstance.instance().ref(IOException.class);
			
			var filename = mappedArgs.get(new Symbol("_0"));
			
			var path = method.body().decl(pathCl, "path", pathCl.staticInvoke("of").arg(filename));
			var scanner = method.body().decl(scannerCl, "scanner", JExpr._null());
			
			var _try = method.body()._try();
			_try.body().assign(scanner, JExpr._new(scannerCl).arg(path));
			var _catch = _try._catch(ioeCl);
			var _e = _catch.param("_e");
			_catch.body()._throw(JExpr._new(rteCl).arg(_e));
			
			method.body()._return(scanner);
		}
	};
	
	private static final Symbol closeSymbol = new Symbol("close", Scanner.singleton().getNamespace());
	public static final Symbol closeSymbol_out = new Symbol("scanner-native-close");
	
	@VelkaOperator
	@Description("Closes scanner.") 
	@Example("(scanner-native-close (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-close <scanner>)")
	public static final Operator close = Operator.wrapJavaMethod(java.util.Scanner.class, "close", "scanner-native-close",
			Scanner.singleton().getNamespace());
	
	private static final Symbol nextLineSymbol = new Symbol("next_line", Scanner.singleton().getNamespace());
	public static final Symbol nextLineSymbol_out = new Symbol("scanner-native-next-line");
	
	@VelkaOperator
	@Description("Advances scanner past the current line and returns the input that was skipped.") 
	@Example("(scanner-native-next-line (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-next-line <scanner>)")
	public static final Operator nextLine = Operator.wrapJavaMethod(java.util.Scanner.class, "nextLine", "scanner-native-next-line",
			Scanner.singleton().getNamespace());
	
	private static final Symbol findInLineSymbol = new Symbol("find_in_line", Scanner.singleton().getNamespace());
	public static final Symbol findInLineSymbol_out = new Symbol("scanner-native-find-in-line");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-in-line (construct Scanner:Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-find-in-line <scanner> <pattern>)")
	public static final Operator findInLine = Operator.wrapJavaMethod(java.util.Scanner.class, "findInLine", 
			"scanner-native-find-in-line", Scanner.singleton().getNamespace(), String.class);
	
	private static final Symbol findWithinHorizonSymbol = new Symbol("find_within_horizon", Scanner.singleton().getNamespace());
	public static final Symbol findWithinHorizonSymbol_out = new Symbol("scanner-native-find-within-horizon");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-within-horizon (construct Scanner:Native \"test-file\") \"a*b\" 42)") 
	@Syntax("(scanner-native-find-within-horizon <scanner> <pattern> <horizon>)")
	public static final Operator findWithinHorizon = Operator.wrapJavaMethod(java.util.Scanner.class, "findWithinHorizon",
			"scanner-native-find-within-horizon", Scanner.singleton().getNamespace(), String.class, int.class);
	
	private static final Symbol hasNextSymbol = new Symbol("has_next", Scanner.singleton().getNamespace());
	public static final Symbol hasNextSymbol_out = new Symbol("scanner-native-has-next");
	
	@VelkaOperator
	@Description("Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.") 
	@Example("(scanner-native-find-within-horizon (construct Scanner:Native \"test-file\") \"a*b\" 42)") 
	@Syntax("(scanner-native-find-within-horizon <scanner>)")
	public static final Operator hasNext = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNext", 
			"scanner-native-has-next", Scanner.singleton().getNamespace());
	
	private static final Symbol hasNextPatternSymbol = new Symbol("has_next_pattern", Scanner.singleton().getNamespace());
	public static final Symbol hasNextPatternSymbol_out = new Symbol("scanner-native-has-next-pattern");
	
	@VelkaOperator
	@Description("Returns true if the next token matches the pattern constructed from the specified string.") 
	@Example("(scanner-native-has-next-pattern (construct Scanner:Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-has-next-pattern <scanner> <pattern>)")
	public static final Operator hasNextPattern = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNext", 
			"scanner-native-has-next-pattern", Scanner.singleton().getNamespace(), String.class);
	
	private static final Symbol hasNextBooleanSymbol = new Symbol("has_next_boolean", Scanner.singleton().getNamespace());
	public static final Symbol hasNextBooleanSymbol_out = new Symbol("scanner-native-has-next-boolean");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as a boolean value using a case insensitive pattern created from the string \"true|false\".") 
	@Example("(scanner-native-has-next-boolean (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-boolean <scanner>)")
	public static final Operator hasNextBoolean = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNextBoolean",
			"scanner-native-has-next-boolean", Scanner.singleton().getNamespace());
	
	private static final Symbol hasNextDoubleSymbol = new Symbol("has_next_double", Scanner.singleton().getNamespace());
	public static final Symbol hasNextDoubleSymbol_out = new Symbol("scanner-native-has-next-double");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as a double value using the scanner-native-next-double function.") 
	@Example("(scanner-native-has-next-double (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-double <scanner>)")
	public static final Operator hasNextDouble = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNextDouble",
			"scanner-native-has-next-double", Scanner.singleton().getNamespace());
	
	private static final Symbol hasNextIntSymbol = new Symbol("has_next_int", Scanner.singleton().getNamespace());
	public static final Symbol hasNextIntSymbol_out = new Symbol("scanner-native-has-next-int");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as an int value in the default radix using the scanner-native-next-int function.") 
	@Example("(scanner-native-has-next-int (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-int <scanner>)")	
	public static final Operator hasNextInt = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNextInt",
			"scanner-native-has-next-int", Scanner.singleton().getNamespace());
	
	private static final Symbol hasNextIntRadixSymbol = new Symbol("has_next_int_radix", Scanner.singleton().getNamespace());
	public static final Symbol hasNextIntRadixSymbol_out = new Symbol("scanner-native-has-next-int-radix");
	
	@VelkaOperator
	@Description("Returns true if the next token in this scanner's input can be interpreted as an int value in the specified radix using the next-int-radix function.") 
	@Example("(scanner-native-has-next-int-radix (construct Scanner:Native \"test-file\" 16))") 
	@Syntax("(scanner-native-has-next-int-radix <scanner> <radix>)")	
	public static final Operator hasNextIntRadix = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNextInt",
			"scanner-native-has-next-int-radix", Scanner.singleton().getNamespace(), int.class);
	
	private static final Symbol hasNextLineSymbol = new Symbol("has_next_line", Scanner.singleton().getNamespace());
	public static final Symbol hasNextLineSymbol_out = new Symbol("scanner-native-has-next-line");
	
	@VelkaOperator
	@Description("Returns true if there is another line in the input of this scanner.") 
	@Example("(scanner-native-has-next-line (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-has-next-line <scanner>)")
	public static final Operator hasNextLine = Operator.wrapJavaMethod(java.util.Scanner.class, "hasNextLine",
			"scanner-native-has-next-line", Scanner.singleton().getNamespace());
	
	private static final Symbol nextSymbol = new Symbol("velka_next", Scanner.singleton().getNamespace());
	public static final Symbol nextSymbol_out = new Symbol("scanner-native-next");
	
	@VelkaOperator
	@Description("Finds and returns the next complete token from this scanner.") 
	@Example("(scanner-native-next (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-next <scanner>)")
	public static final Operator next = Operator.wrapJavaMethod(java.util.Scanner.class, "next",
			"scanner-native-next", Scanner.singleton().getNamespace());
	
	private static final Symbol nextPatternSymbol = new Symbol("next_pattern", Scanner.singleton().getNamespace());
	public static final Symbol nextPatternSymbol_out = new Symbol("scanner-native-next-pattern");
	
	@VelkaOperator
	@Description("Returns the next token if it matches the pattern constructed from the specified string.") 
	@Example("(scanner-native-next-pattern (construct Scanner:Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-next-pattern <scanner> <pattern>)")
	public static final Operator nextPattern = Operator.wrapJavaMethod(java.util.Scanner.class, "next",
			"scanner-native-next-pattern", Scanner.singleton().getNamespace(), String.class);
	
	private static final Symbol nextBoolSymbol = new Symbol("next_bool", Scanner.singleton().getNamespace());
	public static final Symbol nextBoolSymbol_out = new Symbol("scanner-native-next-boolean");
	
	@VelkaOperator
	@Description("Scans the next token of the input into a boolean value and returns that value.") 
	@Example("(scanner-native-next-bool (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-next-bool <scanner>)")
	public static final Operator nextBool = Operator.wrapJavaMethod(java.util.Scanner.class, "nextBoolean",
			"scanner-native-next-boolean", Scanner.singleton().getNamespace());
	
	private static final Symbol nextDoubleSymbol = new Symbol("next_double", Scanner.singleton().getNamespace());
	public static final Symbol nextDoubleSymbol_out = new Symbol("scanner-native-next-double");
	
	@VelkaOperator
	@Description("Scans the next token of the input as a double.") 
	@Example("(scanner-native-next-double (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-next-double <scanner>)")
	public static final Operator nextDouble = Operator.wrapJavaMethod(java.util.Scanner.class, "nextDouble",
			"scanner-native-next-double", Scanner.singleton().getNamespace());
	
	private static final Symbol nextIntSymbol = new Symbol("next_int", Scanner.singleton().getNamespace());
	public static final Symbol nextIntSymbol_out = new Symbol("scanner-native-next-int");
	
	@VelkaOperator
	@Description("Scans the next token of the input as an int.") 
	@Example("(scanner-native-next-int (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-next-int <scanner>)")
	public static final Operator nextInt = Operator.wrapJavaMethod(java.util.Scanner.class, "nextInt",
			"scanner-native-next-int", Scanner.singleton().getNamespace());
	
	private static final Symbol radixSymbol = new Symbol("radix", Scanner.singleton().getNamespace());
	public static final Symbol radixSymbol_out = new Symbol("scanner-native-radix");
	
	@VelkaOperator
	@Description("Returns this scanner's default radix.") 
	@Example("(scanner-native-radix (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-radix <scanner>)")
	public static final Operator radix = Operator.wrapJavaMethod(java.util.Scanner.class, "radix",
			"scanner-native-radix", Scanner.singleton().getNamespace());
	
	private static final Symbol resetSymbol = new Symbol("reset", Scanner.singleton().getNamespace());
	public static final Symbol resetSymbol_out = new Symbol("scanner-native-reset");
	
	@VelkaOperator
	@Description("Resets this scanner.") 
	@Example("(scanner-native-reset (construct Scanner:Native \"test-file\"))") 
	@Syntax("(scanner-native-reset <scanner>)")
	public static final Operator reset = Operator.wrapJavaMethod(java.util.Scanner.class, "reset",
			"scanner-native-reset", Scanner.singleton().getNamespace());
	
	private static final Symbol skipSymbol = new Symbol("skip", Scanner.singleton().getNamespace());
	public static final Symbol skipSymbol_out = new Symbol("scanner-native-skip");
	
	@VelkaOperator
	@Description("Skips input that matches a pattern constructed from the specified string.") 
	@Example("(scanner-native-skip (construct Scanner:Native \"test-file\") \"a*b\")") 
	@Syntax("(scanner-native-skip <scanner> <pattern>)")
	public static final Operator skip = Operator.wrapJavaMethod(java.util.Scanner.class, "skip",
			"scanner-native-skip", Scanner.singleton().getNamespace(), String.class);
	
	
	private static final Symbol useRadixSymbol = new Symbol("use_radix", Scanner.singleton().getNamespace());
	public static final Symbol useRadixSymbol_out = new Symbol("scanner-native-use-radix");
	
	@VelkaOperator
	@Description("Sets this scanner's default radix to the specified radix.") 
	@Example("(scanner-native-use-radix (construct Scanner:Native \"test-file\") \"a\")") 
	@Syntax("(scanner-native-use-radix <scanner> <delimiter>)")
	public static final Operator useRadix = Operator.wrapJavaMethod(java.util.Scanner.class, "useRadix",
			"scanner-native-use-radix", Scanner.singleton().getNamespace(), int.class);
	
	private Scanner() {};
	private static Scanner instance = null;
	public static Scanner singleton() {
		if(instance == null) {
			instance = new Scanner();
		}
		return instance;
	}

	@Override
	protected String name() {
		return "scanner";
	}
}
