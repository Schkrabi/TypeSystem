package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import expression.Application;
import expression.Expression;
import expression.LitComposite;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import interpretation.Environment;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import util.AppendableException;
import util.Pair;
import util.RomanNumbers;
import util.ThrowingFunction;

@SuppressWarnings("deprecation")
class TestComplex {

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
		Environment.initTopLevelEnvitonment();
		TypeEnvironment.initBasicTypes();
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testComplex() throws AppendableException {
		this.testInterpretString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(fact 5)",
				new LitInteger(120));

		this.testInterpretString("(deftype Name)" + "(defrep Unstructured Name (String:Native))"
				+ "(defrep Structured Name (String:Native String:Native))"
				+ "((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Unstructured \"Jan Novak\"))",
				new LitString("unstructured"));

		this.testInterpretString(
				"((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Structured \"Jan\" \"Novak\"))",
				new LitString("structured"));

		this.testInterpretString(
				"(defconversion Name:Structured Name:Unstructured (lambda ((Name:Structured x)) (Name:Unstructured (concat (NameStructured-0 x) (NameStructured-1 x)))))"
						+ "((lambda ((Name:Unstructured x)) x) (Name:Structured \"Jan\" \"Novak\"))",
				new LitComposite(new Tuple(Arrays.asList(new LitString("JanNovak"))),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured"))));

		this.testInterpretString(
				"(deftype List)" + "(defrep Native List (x List))" + "(defrep Empty List ())"
						+ "(List:Native 1 (List:Native 2 (List:Empty)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(
										new Tuple(Arrays.asList(new LitInteger(2),
												new LitComposite(Tuple.EMPTY_TUPLE,
														new TypeAtom(new TypeName("List"),
																new TypeRepresentation("Empty"))))),
										new TypeAtom(new TypeName("List"), TypeRepresentation.NATIVE)))),
						new TypeAtom(new TypeName("List"), TypeRepresentation.NATIVE)));

	}

	@Test
	void testClojure() throws AppendableException {
		// Literals
		this.testClojureCompile("0", "0");
		this.testClojureCompile("3.141521", "3.141521");
		this.testClojureCompile("#t", "true");
		this.testClojureCompile("#f", "false");
		this.testClojureCompile("\"Hello World\"", "\"Hello World\"");
		this.testClojureCompile("(Int:Roman \"XLII\")",
				"(" + Application.clojureEapply + " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"XLII\"])");
		// Unbound Variable
		this.testClojureCompile("variable", "variable");
		// Lambda
		this.testClojureCompileRegex("(lambda (x y) x)",
				TestComplex.escapeBrackets("`([[:\\w* :\\w*] ~(fn [x y] x)])"));
		// Application
		this.testClojureCompile("((lambda ((Int:Native x) (Int:Native y)) x) 42 21)", "(" + Application.clojureEapply
				+ " `([[:IntNative :IntNative] ~(fn [x y] x)]) [:IntNative :IntNative] [42 21])");
		// If
		this.testClojureCompile("(if #t 42 21)", "(if true 42 21)");
		this.testClojureCompile("(if #t (Int:Roman \"XLII\") (Int:String \"42\"))",
				"(if true (" + Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"XLII\"]) (IntString2IntRoman ("
						+ Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"42\"])))");
		// Cons
		this.testClojureCompile("(cons 21 21)", "[21 21]");
		// Exception
		this.testClojureCompile("(error \"error msg\")", "(throw (Throwable. \"error msg\"))");
		// Operators
		this.testClojureCompile("(+ 41 1)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~+]) [:IntNative :IntNative] [41 1])");
		this.testClojureCompile("(and #t #f)", "(and true false)");
		this.testClojureCompile("(bit-and 42 1)", "(" + Application.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-and]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(bit-or 42 1)", "(" + Application.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-or]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompileRegex("(car pair)", TestComplex.escapeBrackets(
				"(" + Application.clojureEapply + " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 0))]) [:\\w*] [pair])"));
		this.testClojureCompileRegex("(cdr pair)", TestComplex.escapeBrackets(
				"(" + Application.clojureEapply + " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 1))]) [:\\w*] [pair])"));
		this.testClojureCompile("(concat \"Hello\" \"World\")", "(" + Application.clojureEapply
				+ " `([[:StringNative :StringNative] ~str]) [:StringNative :StringNative] [\"Hello\" \"World\"])");
		this.testClojureCompile("(/ 84 2)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~/]) [:IntNative :IntNative] [84 2])");
		this.testClojureCompileRegex("(equals? 42 \"42\")", TestComplex.escapeBrackets(
				"(" + Application.clojureEapply + " `([[:\\w :\\w] ~=]) [:IntNative :StringNative] [42 \"42\"])"));
		this.testClojureCompile("(< 42 42)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~<]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(* 42 1)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~*]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(not #t)",
				"(" + Application.clojureEapply + " `([[:BoolNative] ~not]) [:BoolNative] [true])");
		this.testClojureCompile("(= 42 42)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~=]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(or #t #f)", "(or true false)");
		this.testClojureCompile("(- 43 1)",
				"(" + Application.clojureEapply + " `([[:IntNative :IntNative] ~-]) [:IntNative :IntNative] [43 1])");
		// Conversions
		this.testClojureCompile("(IntNative2IntRoman 42)", "(" + Application.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] [(" + RomanNumbers.int2RomanClojure + " _x)])]) [:IntNative] [42])");
		this.testClojureCompile("(IntNative2IntString 42)", "(" + Application.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] [(Integer/toString _x)])]) [:IntNative] [42])");
		this.testClojureCompile("(IntRoman2IntNative (Int:Roman \"XLII\"))",
				"(" + Application.clojureEapply + " `([[:IntRoman] ~(fn [_x] (" + RomanNumbers.roman2intClojure
						+ " (get _x 0)))]) [:IntRoman] [(" + Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntRoman2IntString (Int:Roman \"XLII\"))",
				"(" + Application.clojureEapply + " `([[:IntRoman] ~(fn [_x] (str (" + RomanNumbers.roman2intClojure
						+ " (get _x 0))))]) [:IntRoman] [(" + Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntString2IntNative (Int:String \"42\"))", "(" + Application.clojureEapply
				+ " `([[:IntString] ~(fn [_x] (Integer/parseInt (get _x 0)))]) [:IntString] [("
				+ Application.clojureEapply + " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"42\"])])");
		this.testClojureCompile("(IntString2IntRoman (Int:String \"42\"))",
				"(" + Application.clojureEapply + " `([[:IntString] ~(fn [_x] [(" + RomanNumbers.int2RomanClojure
						+ " (Integer/parseInt (get _x 0)))])]) [:IntString] [(" + Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"42\"])])");
		// Define
		this.testClojureCompile("(define answer 42)", "(def answer 42)");
		this.testClojureCompile("(defconversion Name:Structured Name:Unstructured (lambda (x) x))",
				"(def NameStructured2NameUnstructured `([[:NameStructured] ~(fn [x] x)]))");
		this.testClojureCompile("(deftype Name)", "");
		this.testClojureCompileRegex("(defrep Structured Name (String:Native String:Native))",
				TestComplex.escapeBrackets(
						"(def Name:Structured `([[:StringNative :StringNative] ~(fn [\\w* \\w*] [\\w* \\w*])]))\n"
								+ "(def NameStructured-0 `([[:NameStructured] ~(fn [\\w*] (get \\w* 0))]))\n"
								+ "(def NameStructured-1 `([[:NameStructured] ~(fn [\\w*] (get \\w* 1))]))"));
		// Extended Lambda
		this.testClojureCompile("(elambda ((Int x)) ((Int:Native) \"Native\"))",
				"`([[:IntNative] ~(fn [x] \"Native\")])");
		this.testClojureCompile(
				"((elambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (Int:String \"42\"))",
				"(" + Application.clojureEapply
						+ " `([[:IntNative] ~(fn [x] \"Native\")] [[:IntString] ~(fn [x] \"String\")]) [:IntString] [("
						+ Application.clojureEapply
						+ " `([[:StringNative] ~(fn [_x] [_x])]) [:StringNative] [\"42\"])])");

		// Recursion
		this.testClojureCompile("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))",
				"(def fact `([[:df] ~(fn [x] (if (" + Application.clojureEapply
						+ " `([[:IntNative :IntNative] ~=]) [:df :IntNative] [x 1]) x (" + Application.clojureEapply
						+ " `([[:IntNative :IntNative] ~*]) [:df :IntNative] [x (" + Application.clojureEapply
						+ " fact [:IntNative] [(" + Application.clojureEapply
						+ " `([[:IntNative :IntNative] ~-]) [:df :IntNative] [x 1])])])))]))");
	}

	private List<Expression> parseString(String s, SemanticParser semanticParser) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();

		return exprsContext.val.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x)))
				.collect(Collectors.toList());
	}

	private void testInterpretString(String code, Expression expected) throws AppendableException {
		SemanticParser semanticParser = new SemanticParser();
		Environment topLevel = Environment.topLevelEnvironment;
		Expression last = null;
		for (Expression e : this.parseString(code, semanticParser)) {
			@SuppressWarnings("unused")
			Pair<Type, Substitution> p = e.infer(topLevel);
			last = e.interpret(topLevel);
		}

		if (!last.equals(expected)) {
			fail("Interpretation of " + code + " yields " + last + " were expecting " + expected);
		}
	}

	private String compileToClojure(String code) throws AppendableException {
		SemanticParser semanticParser = new SemanticParser();
		List<Expression> l = this.parseString(code, semanticParser);
		StringBuilder s = new StringBuilder();

		Iterator<Expression> i = l.iterator();
		while (i.hasNext()) {
			s.append(i.next().toClojureCode());
			if (i.hasNext()) {
				s.append('\n');
			}
		}
		return s.toString();
	}

	private void testClojureCompile(String code, String expected) throws AppendableException {
		String s = this.compileToClojure(code);

		if (s.compareTo(expected) != 0) {
			fail("Clojure compilation test failed, compiling " + code + " expected " + expected + " got "
					+ s.toString());
		}
	}

	private void testClojureCompileRegex(String code, String regex) throws AppendableException {
		String s = this.compileToClojure(code);
		if (!s.matches(regex)) {
			fail("Clojure compilation test failed, compiling " + code + " do not match " + regex + " got "
					+ s.toString());
		}
	}

	private static String escapeBrackets(String s) {
		return s.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)").replaceAll("\\[", "\\\\[")
				.replaceAll("\\]", "\\\\]").replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
				.replaceAll("\\+", "\\\\+");
	}
}
