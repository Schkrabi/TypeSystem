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

import application.AbstractionApplication;
import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import literal.LitComposite;
import literal.LitInteger;
import literal.LitString;
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

		final TypeAtom linkedList = new TypeAtom(new TypeName("List"), new TypeRepresentation("Linked"));
		final LitComposite emptyList = new LitComposite(Tuple.EMPTY_TUPLE,
				new TypeAtom(new TypeName("List"), new TypeRepresentation("Empty")));

		this.testInterpretString(
				"(deftype List)" + "(defrep Linked List (A List))" + "(defrep Empty List ())"
						+ "(List:Linked 1 (List:Linked 2 (List:Empty)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));

		final LitComposite xlii = new LitComposite(new Tuple(Arrays.asList(new LitString("XLII"))),
				TypeAtom.TypeIntRoman);
		final LitComposite fortyTwoStr = new LitComposite(new Tuple(Arrays.asList(new LitString("42"))),
				TypeAtom.TypeIntString);
		final LitInteger fortyTwo = new LitInteger(42);

		this.testInterpretString(
				"(define x (List:Linked (Int:Roman \"XLII\") (List:Linked (Int:String \"42\") (List:Linked 42 (List:Empty)))))"
						+ "(define head-list (elambda ((List l))\n"
						+ "          					((List:Linked) (ListLinked-0 l))\n"
						+ "          					((List:Empty) (error \"Cannot make head of empty list!\"))))"
						+ "(head-list x)",
				xlii);

		this.testInterpretString(
				"(define tail-list (elambda ((List l)) \n"
						+ "          					((List:Linked) (ListLinked-1 l))\n"
						+ "          					((List:Empty) (error \"Cannot make tail of empty list!\"))))\n"
						+ "(tail-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwoStr,
								new LitComposite(new Tuple(Arrays.asList(fortyTwo, emptyList)), linkedList))),
						linkedList));

		this.testInterpretString(
				"(define build-list-aux (lambda (i n f)\n" + "            						(if (= i n)\n"
						+ "            							(List:Empty)\n"
						+ "            							(List:Linked (f i) (build-list-aux (+ i 1) n f)))))\n"
						+ "(build-list-aux 0 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));

		this.testInterpretString(
				"(define build-list (lambda (n f) (build-list-aux 0 n f)))\n" + "(build-list 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));

		this.testInterpretString(
				"(define append-list (lambda ((List l) x) \n" + "						(if (equals? l (List:Empty)) \n"
						+ "							(List:Linked x (List:Empty)) \n"
						+ "							(List:Linked (head-list l) (append-list (tail-list l) x)))))\n"
						+ "(append-list x 21)",
				new LitComposite(
						new Tuple(
								Arrays.asList(xlii,
										new LitComposite(
												new Tuple(
														Arrays.asList(fortyTwoStr,
																new LitComposite(
																		new Tuple(
																				Arrays.asList(fortyTwo,
																						new LitComposite(
																								new Tuple(Arrays.asList(
																										new LitInteger(
																												21),
																										emptyList)),
																								linkedList))),
																		linkedList))),
												linkedList))),
						linkedList));

		this.testInterpretString(
				"(define reverse-list (lambda ((List l)) \n" + "						(if (equals? l (List:Empty)) \n"
						+ "							(List:Empty) \n"
						+ "							(append-list (reverse-list (tail-list l)) (head-list l)))))\n"
						+ "(reverse-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwo,
								new LitComposite(new Tuple(Arrays.asList(fortyTwoStr,
										new LitComposite(new Tuple(Arrays.asList(xlii, emptyList)), linkedList))),
										linkedList))),
						linkedList));
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
				"(" + AbstractionApplication.clojureEapply + " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])");
		// Unbound Variable
		this.testClojureCompile("variable", "variable");
		// Lambda
		this.testClojureCompileRegex("(lambda (x y) x)",
				TestComplex.escapeBrackets("`([[:\\w* :\\w*] ~(fn [x y] x)])"));
		// Application
		this.testClojureCompile("((lambda ((Int:Native x) (Int:Native y)) x) 42 21)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~(fn [x y] x)]) [:IntNative :IntNative] [42 21])");
		// If
		this.testClojureCompile("(if #t 42 21)", "(if true 42 21)");
		this.testClojureCompile("(if #t (Int:Roman \"XLII\") (Int:String \"42\"))",
				"(if true (" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"]) ("
						+ AbstractionApplication.clojureEapply + " `([[:IntString] ~(fn [_x] (" + RomanNumbers.int2RomanClojure
						+ " (Integer/parseInt (get _x 0))))]) [:IntString] [(" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])]))");
		// Cons
		this.testClojureCompile("(cons 21 21)", "[21 21]");
		// Exception
		this.testClojureCompile("(error \"error msg\")", "(throw (Throwable. \"error msg\"))");
		// Operators
		this.testClojureCompile("(+ 41 1)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~+]) [:IntNative :IntNative] [41 1])");
		this.testClojureCompile("(and #t #f)", "(and true false)");
		this.testClojureCompile("(bit-and 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-and]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(bit-or 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-or]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompileRegex("(car pair)", TestComplex.escapeBrackets(
				"(" + AbstractionApplication.clojureEapply + " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 0))]) [:\\w*] [pair])"));
		this.testClojureCompileRegex("(cdr pair)", TestComplex.escapeBrackets(
				"(" + AbstractionApplication.clojureEapply + " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 1))]) [:\\w*] [pair])"));
		this.testClojureCompile("(concat \"Hello\" \"World\")", "(" + AbstractionApplication.clojureEapply
				+ " `([[:StringNative :StringNative] ~str]) [:StringNative :StringNative] [\"Hello\" \"World\"])");
		this.testClojureCompile("(/ 84 2)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~/]) [:IntNative :IntNative] [84 2])");
		this.testClojureCompileRegex("(equals? 42 \"42\")", TestComplex.escapeBrackets(
				"(" + AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:IntNative :StringNative] [42 \"42\"])"));
		this.testClojureCompile("(< 42 42)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~<]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(* 42 1)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~*]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(not #t)",
				"(" + AbstractionApplication.clojureEapply + " `([[:BoolNative] ~not]) [:BoolNative] [true])");
		this.testClojureCompile("(= 42 42)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~=]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(or #t #f)", "(or true false)");
		this.testClojureCompile("(- 43 1)",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntNative :IntNative] ~-]) [:IntNative :IntNative] [43 1])");
		// Conversions
		this.testClojureCompile("(IntNative2IntRoman 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] (" + RomanNumbers.int2RomanClojure + " _x))]) [:IntNative] [42])");
		this.testClojureCompile("(IntNative2IntString 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] [(Integer/toString _x)])]) [:IntNative] [42])");
		this.testClojureCompile("(IntRoman2IntNative (Int:Roman \"XLII\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] (" + RomanNumbers.roman2intClojure
						+ " (get _x 0)))]) [:IntRoman] [(" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntRoman2IntString (Int:Roman \"XLII\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] (str (" + RomanNumbers.roman2intClojure
						+ " (get _x 0))))]) [:IntRoman] [(" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntString2IntNative (Int:String \"42\"))", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntString] ~(fn [_x] (Integer/parseInt (get _x 0)))]) [:IntString] [("
				+ AbstractionApplication.clojureEapply + " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");
		this.testClojureCompile("(IntString2IntRoman (Int:String \"42\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntString] ~(fn [_x] (" + RomanNumbers.int2RomanClojure
						+ " (Integer/parseInt (get _x 0))))]) [:IntString] [(" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");
		// Define
		this.testClojureCompile("(define answer 42)", "(def answer 42)");
		this.testClojureCompile("(deftype Name2)", "");
		this.testClojureCompileRegex("(defrep Structured Name2 (String:Native String:Native))",
				TestComplex.escapeBrackets(
						"(def Name2:Structured `([[:StringNative :StringNative] ~(fn [\\w* \\w*] [\\w* \\w*])]))"));
		this.testClojureCompileRegex("(defrep Unstructured Name2 (String:Native))",
				TestComplex.escapeBrackets("(def Name2:Unstructured `([[:StringNative] ~(fn [\\w*] [\\w*])]))"));
		this.testClojureCompileRegex(
				"(defconversion Name2:Structured Name2:Unstructured"
						+ "(lambda (x) (Name2:Unstructured (concat (Name2Structured-0 x) (Name2Structured-1 x)))))",
				TestComplex.escapeBrackets("(def Name2Structured2Name2Unstructured `([[:Name2Structured] ~(fn [x] ("
						+ AbstractionApplication.clojureEapply + " Name2:Unstructured [:StringNative] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative :StringNative] ~str]) [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " Name2Structured-0 [:Name2Structured] [x]) ("
						+ AbstractionApplication.clojureEapply + " Name2Structured-1 [:Name2Structured] [x])])]))]))"));

		this.testClojureCompile("((lambda ((Name2:Unstructured x)) x) (Name2:Structured \"Jan\" \"Novak\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:Name2Unstructured] ~(fn [x] x)]) [:Name2Structured] [("
						+ AbstractionApplication.clojureEapply + " Name2Structured2Name2Unstructured [:Name2Structured] [("
						+ AbstractionApplication.clojureEapply
						+ " Name2:Structured [:StringNative :StringNative] [\"Jan\" \"Novak\"])])])");
		// Extended Lambda
		this.testClojureCompile("(elambda ((Int x)) ((Int:Native) \"Native\"))",
				"`([[:IntNative] ~(fn [x] \"Native\")])");
		this.testClojureCompile(
				"((elambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (Int:String \"42\"))",
				"(" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative] ~(fn [x] \"Native\")] [[:IntString] ~(fn [x] \"String\")]) [:IntString] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");

		// Recursion
		this.testClojureCompileRegex("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))",
				TestComplex.escapeBrackets("(def fact `([[:\\w*] ~(fn [x] (if (" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~=]) [:\\w* :IntNative] [x 1]) x (" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~\\*]) [:\\w* :IntNative] [x (" + AbstractionApplication.clojureEapply
						+ " fact [:IntNative] [(" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~-]) [:\\w* :IntNative] [x 1])])])))]))"));

		// Conversions
		this.testClojureCompile(
				"((elambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (Int:Roman \"XLII\") 66)",
				"(" + AbstractionApplication.clojureEapply
						+ " `([[:BoolNative :IntString :IntString] ~(fn [x y z] (if x z y))]) [:BoolNative :IntRoman :IntNative] [false ("
						+ AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] (str ("
						+ RomanNumbers.roman2intClojure + " (get _x 0))))]) [:IntRoman] [(" + AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative] ~(fn [_x] [(Integer/toString _x)])]) [:IntNative] [66])])");

		// List
		this.testClojureCompile("(deftype List2)", "");
		this.testClojureCompileRegex("(defrep Linked List2 (A List2))",
				TestComplex.escapeBrackets("(def List2:Linked `([[:A :List2\\*] ~(fn [\\w* \\w*] [\\w* \\w*])]))"));
		this.testClojureCompile("(defrep Empty List2 ())", "(def List2:Empty `([[] ~(fn [] [])]))");

		this.testClojureCompile(
				"(define x (List2:Linked (Int:Roman \"XLII\") (List2:Linked (Int:String \"42\") (List2:Linked 42 (List2:Empty)))))",
				"(def x (" + AbstractionApplication.clojureEapply + " List2:Linked [:IntRoman :List2Linked] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"]) ("
						+ AbstractionApplication.clojureEapply + " List2:Linked [:IntString :List2Linked] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"]) ("
						+ AbstractionApplication.clojureEapply + " List2:Linked [:IntNative :List2Empty] [42 ("
						+ AbstractionApplication.clojureEapply + " List2:Empty [] [])])])]))");

		this.testClojureCompile(
				"(define head-list2 (elambda ((List2 l))\n"
						+ "          					((List2:Linked) (List2Linked-0 l))\n"
						+ "          					((List2:Empty) (error \"Cannot make head of empty list!\"))))",
				"(def head-list2 `([[:List2Empty] ~(fn [l] (throw (Throwable. \"Cannot make head of empty list!\")))] [[:List2Linked] ~(fn [l] ("
						+ AbstractionApplication.clojureEapply + " List2Linked-0 [:List2Linked] [l]))]))");

		this.testClojureCompile(
				"(define tail-list2 (elambda ((List2 l)) \n"
						+ "          					((List2:Linked) (List2Linked-1 l))\n"
						+ "          					((List2:Empty) (error \"Cannot make tail of empty list!\"))))",
				"(def tail-list2 `([[:List2Empty] ~(fn [l] (throw (Throwable. \"Cannot make tail of empty list!\")))] [[:List2Linked] ~(fn [l] ("
						+ AbstractionApplication.clojureEapply + " List2Linked-1 [:List2Linked] [l]))]))");

		/*this.testClojureCompileRegex(
				"(define build-list2-aux (lambda (i n f)\n" + "            						(if (= i n)\n"
						+ "            							(List2:Empty)\n"
						+ "            							(List2:Linked (f i) (build-list2-aux (+ i 1) n f)))))",
				TestComplex.escapeBrackets("(def build-list2-aux `([[:\\w* :\\w* :\\w*] ~(fn [i n f] (if ("
						+ Application.clojureEapply + " `([[:IntNative :IntNative] ~=]) [:\\w* :\\w*] [i n]) ("
						+ Application.clojureEapply + " List2:Empty [] []) (List2Linked2List2Empty ("
						+ Application.clojureEapply + " List2:Linked [:\\w* :List2Empty] [(" + Application.clojureEapply
						+ " f [:\\w*] [i]) (" + Application.clojureEapply
						+ " build-list2-aux [:IntNative :\\w* :\\w*] [(" + Application.clojureEapply
						+ " `([[:IntNative :IntNative] ~+]) [:\\w* :IntNative] [i 1]) n f])]))))]))"));*/

		this.testClojureCompileRegex("(define build-list2 (lambda (n f) (build-list2-aux 0 n f)))",
				TestComplex.escapeBrackets("(def build-list2 `([[:\\w* :\\w*] ~(fn [n f] (" + AbstractionApplication.clojureEapply
						+ " build-list2-aux [:IntNative :\\w* :\\w*] [0 n f]))]))"));

		this.testClojureCompileRegex(
				"(define append-list2 (lambda ((List2 l) x) \n" + "(if (equals? l (List2:Empty)) \n"
						+ "							(List2:Linked x (List2:Empty)) \n"
						+ "							(List2:Linked (head-list2 l) (append-list2 (tail-list2 l) x)))))",
				TestComplex.escapeBrackets(
						"(def append-list2 `([[:List2\\* :\\w*] ~(fn [l x] (if (eapply `([[:\\w* :\\w*] ~=]) [:List2\\* :List2Empty] [l ("
								+ AbstractionApplication.clojureEapply + " List2:Empty [] [])]) (" + AbstractionApplication.clojureEapply
								+ " List2:Linked [:\\w* :List2Empty] [x (" + AbstractionApplication.clojureEapply
								+ " List2:Empty [] [])]) (" + AbstractionApplication.clojureEapply
								+ " List2:Linked [:\\w* :List2Linked] [(" + AbstractionApplication.clojureEapply
								+ " head-list2 [:List2\\*] [l]) (" + AbstractionApplication.clojureEapply
								+ " append-list2 [:\\w* :\\w*] [(" + AbstractionApplication.clojureEapply
								+ " tail-list2 [:List2\\*] [l]) x])])))]))"));

		/*this.testClojureCompile(
				"(define reverse-list2 (lambda ((List2 l)) \n"
						+ "						(if (equals? l (List2:Empty)) \n"
						+ "							(List2:Empty) \n"
						+ "							(append-list2 (reverse-list2 (tail-list2 l)) (head-list2 l)))))",
				"");*/
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
