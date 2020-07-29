package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
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

import abstraction.ExtendedFunction;
import abstraction.Function;
import application.AbstractionApplication;
import application.CanDeconstructAs;
import application.Construct;
import application.IfExpression;
import expression.Expression;
import expression.Symbol;
import expression.Tuple;
import interpretation.Environment;
import literal.LitBoolean;
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
import types.TypeArrow;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
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

		this.testInterpretString("(type Name)" + "(representation Unstructured Name)"
				+ "(constructor Name Unstructured ((String:Native x)) x)" + "(representation Structured Name)"
				+ "(constructor Name Structured ((String:Native x) (String:Native y)) (cons x y))"
				+ "((extended-lambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (construct Name Unstructured \"Jan Novak\"))",
				new LitString("unstructured"));

		this.testInterpretString(
				"((extended-lambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (construct Name Structured \"Jan\" \"Novak\"))",
				new LitString("structured"));

		this.testInterpretString(
				"(conversion Name:Structured Name:Unstructured ((Name:Structured x)) (construct Name Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
						+ "((lambda ((Name:Unstructured x)) x) (construct Name Structured \"Jan\" \"Novak\"))",
				new LitComposite(new LitString("JanNovak"),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured"))));

		final TypeAtom linkedList = new TypeAtom(new TypeName("List"), new TypeRepresentation("Linked"));
		final LitComposite emptyList = new LitComposite(Expression.EMPTY_EXPRESSION, linkedList);

		this.testInterpretString(
				"(type List)" + "(representation Linked List)" + "(constructor List Linked (x (List l)) (cons x l))"
						+ "(constructor List Linked () ())"
						+ "(construct List Linked 1 (construct List Linked 2 (construct List Linked)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));

		// final TypeAtom functionalList = new TypeAtom(new TypeName("List"), new
		// TypeRepresentation("Functional"));

		this.testInterpretString("(define fcons (lambda (x y) (lambda (f) (f x y))))"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))" + "(define fcdr (lambda (p) (p (lambda (x y) y))))"
				+ "(representation Functional List)" + "(constructor List Functional (x (List l)) (fcons x l))"
				+ "(constructor List Functional () ())", Expression.EMPTY_EXPRESSION);

		final LitComposite xlii = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		final LitComposite fortyTwoStr = new LitComposite(new LitString("42"), TypeAtom.TypeIntString);
		final LitInteger fortyTwo = new LitInteger(42);

		this.testInterpretString(
				"(define x (construct List Linked (construct Int Roman \"XLII\") (construct List Linked (construct Int String \"42\") (construct List Linked 42 (construct List Linked)))))"
						+ "(define head-list (extended-lambda ((List l)) "
						+ "((List:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked))))) "
						+ "((List:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list\") (fcar (deconstruct l ((((A B) #> C)) #> D)))))))"
						+ "(head-list x)",
				xlii);

		this.testInterpretString(
				"(define y (construct List Functional (construct Int Roman \"XLII\") (construct List Functional (construct Int String \"42\") (construct List Functional 42 (construct List Functional)))))"
						+ "(head-list y)",
				xlii);

		this.testInterpretString("(define tail-list (extended-lambda ((List l)) "
				+ "((List:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List:Linked)))))"
				+ "((List:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A B) #> C)) #> D)))))))"
				+ "(tail-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwoStr,
								new LitComposite(new Tuple(Arrays.asList(fortyTwo, emptyList)), linkedList))),
						linkedList));

		this.testInterpretString(
				"(define build-list-aux (lambda (i n f) " + "(if (= i n) " + "(construct List Linked)"
						+ "(construct List Linked (f i) (build-list-aux (+ i 1) n f)))))"
						+ "(build-list-aux 0 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));

		this.testInterpretString(
				"(define build-list (lambda (n f) (build-list-aux 0 n f)))" + "(build-list 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList));
		
		this.testInterpretString("(define empty-list? (extended-lambda ((List l)) " 
				+ "((List:Linked) (can-deconstruct-as l ())) "
				+ "((List:Functional) (can-deconstruct-as l ()))))"
				+ "(empty-list? x)",
				LitBoolean.FALSE);
		
		this.testInterpretString("(empty-list? y)", LitBoolean.FALSE);

		this.testInterpretString("(define append-list (lambda ((List l) x) " + "(if (empty-list? l) "
				+ "(construct List Linked x (construct List Linked)) "
				+ "(construct List Linked (head-list l) (append-list (tail-list l) x)))))" + "(append-list x 21)",
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
				"(define reverse-list (lambda ((List l)) " + "(if (empty-list? l) "
						+ "(construct List Linked) " + "(append-list (reverse-list (tail-list l)) (head-list l)))))"
						+ "(reverse-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwo,
								new LitComposite(new Tuple(Arrays.asList(fortyTwoStr,
										new LitComposite(new Tuple(Arrays.asList(xlii, emptyList)), linkedList))),
										linkedList))),
						linkedList));

		// Complex types
		this.testInterpretString("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)",
				new LitInteger(42));
		this.testInterpretString("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\")))) +)",
				new LitInteger(42));
		this.testInterpretString("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\"))))"
				+ "(lambda ((Int:String x) (Int:String y)) (construct Int String (concat (deconstruct x String:Native) (deconstruct y String:Native)))))",
				new LitComposite(new LitString("2121"), TypeAtom.TypeIntString));

		this.testInterpretString("((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\"))", new Tuple(
				Arrays.asList(new LitInteger(42), new LitComposite(new LitString("42"), TypeAtom.TypeIntString))));

		TypeName listTypeName = new TypeName("List");
		TypeAtom typeListLinkedAtom = new TypeAtom(listTypeName, new TypeRepresentation("Linked"));
		TypeAtom typeListFuntionalAtom = new TypeAtom(listTypeName, new TypeRepresentation("Functional"));

		this.testInterpretString(
				"(extended-lambda ((List l) (A x))" + "((List:Linked A) (if (can-deconstruct-as l ())"
						+ "(construct List Linked x (construct List Linked))"
						+ "(construct List Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List:Functional A) (if (can-deconstruct-as l ())"
						+ "(construct List Functional x (construct List Functional))"
						+ "(construct List Functional (head-list l) (append-list (tail-list l) x)))))",
				ExtendedFunction
						.makeExtendedFunction(Arrays.asList(
								new Function(
										new TypeTuple(Arrays.asList(typeListLinkedAtom, new TypeVariable("A"))),
										new Tuple(Arrays.asList(new Symbol("l"), new Symbol("x"))),
										new IfExpression(
												new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(typeListLinkedAtom,
														new Tuple(Arrays.asList(new Symbol("x"),
																new Construct(typeListLinkedAtom, Tuple.EMPTY_TUPLE)))),
												new Construct(typeListLinkedAtom,
														new Tuple(Arrays.asList(
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))),
																new AbstractionApplication(new Symbol("append-list"),
																		new Tuple(Arrays.asList(
																				new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l")))),
																				new Symbol("x")))))))),
										Environment.topLevelEnvironment),
								new Function(new TypeTuple(Arrays.asList(typeListFuntionalAtom, new TypeVariable("A"))),
										new Tuple(Arrays.asList(new Symbol("l"), new Symbol("x"))),
										new IfExpression(
												new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(typeListFuntionalAtom,
														new Tuple(Arrays.asList(new Symbol("x"),
																new Construct(typeListFuntionalAtom,
																		Tuple.EMPTY_TUPLE)))),
												new Construct(typeListFuntionalAtom,
														new Tuple(Arrays.asList(
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))),
																new AbstractionApplication(new Symbol("append-list"),
																		new Tuple(Arrays.asList(
																				new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l")))),
																				new Symbol("x")))))))),
										Environment.topLevelEnvironment)),
								Environment.topLevelEnvironment));

		this.testInterpretString("(extended-lambda ((List l))"
				+ "                        ((List:Linked) (if (can-deconstruct-as l ())"
				+ "                                            (construct List Linked)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))"
				+ "                        ((List:Functional) (if (can-deconstruct-as l ())"
				+ "                                            (construct List Functional)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l)))))",
				ExtendedFunction
						.makeExtendedFunction(
								Arrays.asList(
										new Function(new TypeTuple(Arrays.asList(typeListLinkedAtom)),
												new Tuple(Arrays.asList(new Symbol("l"))),
												new IfExpression(
														new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
														new Construct(typeListLinkedAtom, Tuple.EMPTY_TUPLE),
														new AbstractionApplication(new Symbol("append-list"),
																new Tuple(Arrays.asList(new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																		new AbstractionApplication(
																				new Symbol("head-list"),
																				new Tuple(Arrays
																						.asList(new Symbol("l")))))))),
												Environment.topLevelEnvironment),
										new Function(new TypeTuple(Arrays.asList(typeListFuntionalAtom)),
												new Tuple(Arrays.asList(new Symbol("l"))),
												new IfExpression(
														new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
														new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
														new AbstractionApplication(new Symbol("append-list"),
																new Tuple(Arrays.asList(new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																		new AbstractionApplication(
																				new Symbol("head-list"),
																				new Tuple(Arrays
																						.asList(new Symbol("l")))))))),
												Environment.topLevelEnvironment)),
								Environment.topLevelEnvironment));

		this.testInterpretString("(extended-lambda ((((A) #> B) f) (List l))"
				+ "                    ((((A) #> B) List:Linked) (if (can-deconstruct-as l ())"
				+ "                                                (construct List Linked)"
				+ "                                                (construct List Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List:Functional) (if (can-deconstruct-as l ())"
				+ "                                                    (construct List Functional)"
				+ "                                                    (construct List Functional (f (head-list l)) (map-list f (tail-list l))))))",
				ExtendedFunction.makeExtendedFunction(Arrays.asList(
						new Function(
								new TypeTuple(Arrays.asList(
										new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))),
												new TypeVariable("B")),
										typeListLinkedAtom)),
								new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))), new IfExpression(
										new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
										new Construct(typeListLinkedAtom, Tuple.EMPTY_TUPLE), new Construct(
												typeListLinkedAtom, new Tuple(Arrays.asList(
														new AbstractionApplication(
																new Symbol("f"),
																new Tuple(Arrays
																		.asList(new AbstractionApplication(
																				new Symbol("head-list"),
																				new Tuple(Arrays
																						.asList(new Symbol("l"))))))),
														new AbstractionApplication(new Symbol("map-list"),
																new Tuple(Arrays.asList(new Symbol("f"),
																		new AbstractionApplication(
																				new Symbol("tail-list"),
																				new Tuple(Arrays.asList(
																						new Symbol("l"))))))))))),
								Environment.topLevelEnvironment),
						new Function(new TypeTuple(
								Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))),
										new TypeVariable("B")), typeListFuntionalAtom)),
								new Tuple(Arrays.asList(new Symbol("f"),
										new Symbol("l"))),
								new IfExpression(
										new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
										new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
										new Construct(typeListFuntionalAtom,
												new Tuple(
														Arrays.asList(
																new AbstractionApplication(new Symbol("f"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("head-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																new AbstractionApplication(new Symbol("map-list"),
																		new Tuple(Arrays.asList(new Symbol("f"),
																				new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays
																								.asList(new Symbol(
																										"l"))))))))))),
								Environment.topLevelEnvironment)),
						Environment.topLevelEnvironment));

		this.testInterpretString("(define append-list-el (extended-lambda ((List l) (A x))"
				+ "                        ((List:Linked A) (if (can-deconstruct-as l ())"
				+ "                                                (construct List Linked x (construct List Linked))"
				+ "                                                (construct List Linked (head-list l) (append-list-el (tail-list l) x))))"
				+ "                        ((List:Functional A) (if (can-deconstruct-as l ())"
				+ "                                                (construct List Functional x (construct List Functional))"
				+ "                                                (construct List Functional (head-list l) (append-list-el (tail-list l) x))))))",
				Expression.EMPTY_EXPRESSION);
		this.testInterpretString("(head-list (append-list-el x 42))",
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman));
		this.testInterpretString("(head-list (append-list-el y 42))",
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman));

		this.testInterpretString("(define reverse-list-el (extended-lambda ((List l))\n"
				+ "                        ((List:Linked) (if (can-deconstruct-as l ())\n"
				+ "                                            (construct List Linked)\n"
				+ "                                            (append-list-el (reverse-list-el (tail-list l)) (head-list l))))\n"
				+ "                        ((List:Functional) (if (can-deconstruct-as l ())\n"
				+ "                                            (construct List Functional)\n"
				+ "                                            (append-list-el (reverse-list-el (tail-list l)) (head-list l))))))",
				Expression.EMPTY_EXPRESSION);
		this.testInterpretString("(reverse-list-el x)",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(42),
								new LitComposite(
										new Tuple(Arrays.asList(
												new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
												new LitComposite(new Tuple(Arrays.asList(
														new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
														new LitComposite(Expression.EMPTY_EXPRESSION,
																typeListLinkedAtom))),
														typeListLinkedAtom))),
										typeListLinkedAtom))),
						typeListLinkedAtom));
	}

	@Test
	void testClojure() throws AppendableException {
		// Literals
		this.testClojureCompile("0", "(with-meta [0] {:lang-type (lang-type-atom. \"Int\" \"Native\")})");
		this.testClojureCompile("3.141521",
				"(with-meta [3.141521] {:lang-type (lang-type-atom. \"Double\" \"Native\")})");
		this.testClojureCompile("#t", "(with-meta [true] {:lang-type (lang-type-atom. \"Bool\" \"Native\")})");
		this.testClojureCompile("#f", "(with-meta [false] {:lang-type (lang-type-atom. \"Bool\" \"Native\")})");
		this.testClojureCompile("\"Hello World\"",
				"(with-meta [\"Hello World\"] {:lang-type (lang-type-atom. \"String\" \"Native\")})");
		this.testClojureCompileExpression(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				"(with-meta [(with-meta [\"XLII\"] {:lang-type (lang-type-atom. \"String\" \"Native\")})] {:lang-type (lang-type-atom. \"Int\" \"Roman\")})");
		// Unbound Variable
		this.testClojureCompile("variable", "variable");
		// Lambda
		this.testClojureCompileRegex("(lambda (x y) x)", TestComplex.escapeBrackets(
				"(with-meta [(with-meta (fn [x y] x) {:lang-type (lang-type-arrow. [\\w* \\w*] \\w*)})] {:lang-type (lang-type-arrow. [\\\\w* \\\\w*] \\\\w*)})"));
		// Application
		this.testClojureCompile("((lambda ((Int:Native x) (Int:Native y)) x) 42 21)",
				"(" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~(fn [x y] x)]) [:IntNative :IntNative] [42 21])");
		// If
		this.testClojureCompile("(if #t 42 21)", "(if true 42 21)");
		this.testClojureCompile("(if #t (Int:Roman \"XLII\") (Int:String \"42\"))", "(if true ("
				+ AbstractionApplication.clojureEapply + " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"]) ("
				+ AbstractionApplication.clojureEapply + " `([[:IntString] ~(fn [_x] (" + RomanNumbers.int2RomanClojure
				+ " (Integer/parseInt _x)))]) [:IntString] [(" + AbstractionApplication.clojureEapply
				+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])]))");
		// Cons
		this.testClojureCompile("(cons 21 21)", "[21 21]");
		// Exception
		this.testClojureCompile("(error \"error msg\")", "(throw (Throwable. \"error msg\"))");
		// Operators
		this.testClojureCompile("(+ 41 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~+]) [:IntNative :IntNative] [41 1])");
		this.testClojureCompile("(and #t #f)", "(and true false)");
		this.testClojureCompile("(bit-and 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-and]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(bit-or 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~bit-or]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompileRegex("(car pair)", TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
				+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 0))]) [:\\w*] [pair])"));
		this.testClojureCompileRegex("(cdr pair)", TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
				+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 1))]) [:\\w*] [pair])"));
		this.testClojureCompile("(concat \"Hello\" \"World\")", "(" + AbstractionApplication.clojureEapply
				+ " `([[:StringNative :StringNative] ~str]) [:StringNative :StringNative] [\"Hello\" \"World\"])");
		this.testClojureCompile("(/ 84 2)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~/]) [:IntNative :IntNative] [84 2])");
		this.testClojureCompileRegex("(equals? 42 \"42\")",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[:\\w* :\\w*] ~=]) [:IntNative :StringNative] [42 \"42\"])"));
		this.testClojureCompile("(< 42 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~<]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(* 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~*]) [:IntNative :IntNative] [42 1])");
		this.testClojureCompile("(not #t)",
				"(" + AbstractionApplication.clojureEapply + " `([[:BoolNative] ~not]) [:BoolNative] [true])");
		this.testClojureCompile("(= 42 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~=]) [:IntNative :IntNative] [42 42])");
		this.testClojureCompile("(or #t #f)", "(or true false)");
		this.testClojureCompile("(- 43 1)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative :IntNative] ~-]) [:IntNative :IntNative] [43 1])");
		// Conversions
		this.testClojureCompile("(IntNative2IntRoman 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] (" + RomanNumbers.int2RomanClojure + " _x))]) [:IntNative] [42])");
		this.testClojureCompile("(IntNative2IntString 42)", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntNative] ~(fn [_x] (Integer/toString _x))]) [:IntNative] [42])");
		this.testClojureCompile("(IntRoman2IntNative (Int:Roman \"XLII\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] ("
						+ RomanNumbers.roman2intClojure + " _x))]) [:IntRoman] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntRoman2IntString (Int:Roman \"XLII\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] (str ("
						+ RomanNumbers.roman2intClojure + " _x)))]) [:IntRoman] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])])");
		this.testClojureCompile("(IntString2IntNative (Int:String \"42\"))", "(" + AbstractionApplication.clojureEapply
				+ " `([[:IntString] ~(fn [_x] (Integer/parseInt _x))]) [:IntString] [("
				+ AbstractionApplication.clojureEapply + " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");
		this.testClojureCompile("(IntString2IntRoman (Int:String \"42\"))",
				"(" + AbstractionApplication.clojureEapply + " `([[:IntString] ~(fn [_x] ("
						+ RomanNumbers.int2RomanClojure + " (Integer/parseInt _x)))]) [:IntString] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");
		// Define
		this.testClojureCompile("(define answer 42)", "(def answer 42)");
		this.testClojureCompile("(type Name2)", "nil");
		this.testClojureCompile("(representation Structured Name2)", "nil");
		this.testClojureCompile("(constructor Name2 Structured ((String:Native x) (String:Native y)) (cons x y))", "");
		this.testClojureCompile("(representation Unstructured Name2)", "nil");
		this.testClojureCompile("(constructor Name2 Unstructured ((String:Native x)) x)", "");
		this.testClojureCompile("(conversion Name2:Structured Name2:Unstructured"
				+ "((Name2:Structured x)) (construct Name2 Unstructured (concat (car (deconstruct x)) (cdr (deconstruct x)))))",
				"");

		this.testClojureCompileRegex(
				"((lambda ((Name2:Unstructured x)) x) (construct Name2 Structured \"Jan\" \"Novak\"))",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[:Name2Unstructured] ~(fn [x] x)]) [:Name2Structured] [("
						+ AbstractionApplication.clojureEapply + " `([[:Name2Structured] ~(fn [x] ("
						+ AbstractionApplication.clojureEapply + " `([[:StringNative] ~(fn [x] x)]) [:StringNative] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative :StringNative] ~str]) [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 0))]) [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:Name2Structured] [x])]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 1))]) [:\\w*] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:Name2Structured] [x])])])]))]) [:Name2Structured] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative :StringNative] ~(fn [x y] [x y])]) [:StringNative :StringNative] [\"Jan\" \"Novak\"])])])"));
		// Extended Lambda
		this.testClojureCompile("(extended-lambda ((Int x)) ((Int:Native) \"Native\"))",
				"`([[:IntNative] ~(fn [x] \"Native\")])");
		this.testClojureCompile(
				"((extended-lambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (Int:String \"42\"))",
				"(" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative] ~(fn [x] \"Native\")] [[:IntString] ~(fn [x] \"String\")]) [:IntString] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"42\"])])");

		// Recursion
		this.testClojureCompileRegex("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))",
				TestComplex.escapeBrackets("(def fact `([[:\\w*] ~(fn [x] (if (" + AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~=]) [:\\w* :IntNative] [x 1]) x ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~\\*]) [:\\w* :IntNative] [x ("
						+ AbstractionApplication.clojureEapply + " fact [:IntNative] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~-]) [:\\w* :IntNative] [x 1])])])))]))"));

		// Conversions
		this.testClojureCompile(
				"((extended-lambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (Int:Roman \"XLII\") 66)",
				"(" + AbstractionApplication.clojureEapply
						+ " `([[:BoolNative :IntString :IntString] ~(fn [x y z] (if x z y))]) [:BoolNative :IntRoman :IntNative] [false ("
						+ AbstractionApplication.clojureEapply + " `([[:IntRoman] ~(fn [_x] (str ("
						+ RomanNumbers.roman2intClojure + " _x)))]) [:IntRoman] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~identity]) [:StringNative] [\"XLII\"])]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative] ~(fn [_x] (Integer/toString _x))]) [:IntNative] [66])])");

		// List
		this.testClojureCompile("(type List2)", "nil");
		this.testClojureCompile("(representation Linked List2)", "nil");
		this.testClojureCompile("(constructor List2 Linked (x (List2 l)) (cons x l))", "");
		this.testClojureCompile("(constructor List2 Linked () ())", "");
		this.testClojureCompile("(representation Functional List2)", "nil");
		this.testClojureCompileRegex("(define fcons (lambda (x y) (lambda (f) (f x y))))",
				TestComplex.escapeBrackets("(def fcons `([[:\\w* :\\w*] ~(fn [x y] `([[:\\w*] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply + " f [:\\w* :\\w*] [x y]))]))]))"));
		this.testClojureCompileRegex("(define fcar (lambda (p) (p (lambda (x y) x))))",
				TestComplex.escapeBrackets("(def fcar `([[:\\w*] ~(fn [p] (" + AbstractionApplication.clojureEapply
						+ " p [[[:\\w* :\\w*] :-> :\\w*]] [`([[:\\w* :\\w*] ~(fn [x y] x)])]))]))"));
		this.testClojureCompileRegex("(define fcdr (lambda (p) (p (lambda (x y) y))))",
				TestComplex.escapeBrackets("(def fcdr `([[:\\w*] ~(fn [p] (" + AbstractionApplication.clojureEapply
						+ " p [[[:\\w* :\\w*] :-> :\\w*]] [`([[:\\w* :\\w*] ~(fn [x y] y)])]))]))"));
		this.testClojureCompile("(constructor List2 Functional (x (List2 l)) (fcons x l))", "");
		this.testClojureCompile("(constructor List2 Functional () ())", "");

		this.testClojureCompileRegex(
				"(define x (construct List2 Linked (construct Int Roman \"XLII\") (construct List2 Linked (construct Int String \"42\") (construct List2 Linked 42 (construct List2 Linked)))))",
				TestComplex.escapeBrackets("(def x (" + AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:IntRoman :List2Linked] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"XLII\"]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:IntString :List2Linked] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"42\"]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:IntNative :List2Linked] [42 ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] [])])])]))"));

		this.testClojureCompileRegex(
				"(define y (construct List2 Functional (construct Int Roman \"XLII\") (construct List2 Functional (construct Int String \"42\") (construct List2 Functional 42 (construct List2 Functional)))))",
				TestComplex.escapeBrackets("(def y (" + AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] (" + AbstractionApplication.clojureEapply
						+ " fcons [:\\w* :List2\\*] [x l]))]) [:IntRoman :List2Functional] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"XLII\"]) ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :List2\\*] ~(fn [x l] ("
						+ AbstractionApplication.clojureEapply
						+ " fcons [:\\w* :List2\\*] [x l]))]) [:IntString :List2Functional] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"42\"]) ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :List2\\*] ~(fn [x l] ("
						+ AbstractionApplication.clojureEapply
						+ " fcons [:\\w* :List2\\*] [x l]))]) [:IntNative :List2Functional] [42 ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] [])])])]))"));

		this.testClojureCompileRegex(
				"(define head-list2 (extended-lambda ((List2 l)) ((List2:Linked) (if (equals? (deconstruct l) ()) (error \"Cannot make head of empty list!\") (car (deconstruct l))))"
						+ "((List2:Functional) (if (equals? (deconstruct l) ()) (error \"Cannot make head of empty list!\") (fcar (deconstruct l))))))",
				TestComplex.escapeBrackets("(def head-list2 `([[:List2Functional] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l]) nil]) (throw (Throwable. \"Cannot make head of empty list!\")) ("
						+ AbstractionApplication.clojureEapply + " fcar [:\\w*] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l])])))] [[:List2Linked] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Linked] [l]) nil]) (throw (Throwable. \"Cannot make head of empty list!\")) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 0))]) [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2Linked] [l])])))]))"));

		this.testClojureCompileRegex(
				"(define tail-list2 (extended-lambda ((List2 l)) ((List2:Linked) (if (equals? (deconstruct l) ()) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l))))"
						+ "((List2:Functional) (if (equals? (deconstruct l) ()) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l))))))",
				TestComplex.escapeBrackets("(def tail-list2 `([[:List2Functional] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l]) nil]) (throw (Throwable. \"Cannot make tail of empty list!\")) ("
						+ AbstractionApplication.clojureEapply + " fcdr [:\\w*] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l])])))] [[:List2Linked] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Linked] [l]) nil]) (throw (Throwable. \"Cannot make tail of empty list!\")) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[[:\\w* :\\w*]] ~(fn [_x] (get _x 1))]) [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2Linked] [l])])))]))"));

		// This is interesting, extended lambda is not sufficient, when I might want to
		// return a different representation.
		// But on what would I base the representation?
		this.testClojureCompileRegex(
				"(define build-list2-aux (lambda (i n f) (if (= i n) (construct List2 Linked) (construct List2 Linked (f i) (build-list2-aux (+ i 1) n f)))))",
				TestComplex.escapeBrackets("(def build-list2-aux `([[:\\w* :\\w* :\\w*] ~(fn [i n f] (if ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~=]) [:\\w* :\\w*] [i n]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:\\w* :List2Linked] [("
						+ AbstractionApplication.clojureEapply + " f [:\\w*] [i]) ("
						+ AbstractionApplication.clojureEapply + " build-list2-aux [:IntNative :\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~+]) [:\\w* :IntNative] [i 1]) n f])])))]))"));

		this.testClojureCompileRegex("(define build-list2 (lambda (n f) (build-list2-aux 0 n f)))",
				TestComplex.escapeBrackets(
						"(def build-list2 `([[:\\w* :\\w*] ~(fn [n f] (" + AbstractionApplication.clojureEapply
								+ " build-list2-aux [:IntNative :\\w* :\\w*] [0 n f]))]))"));

		this.testClojureCompileRegex("(define append-list2 (lambda ((List2 l) x) \n"
				+ "(if (equals? (deconstruct l) ()) (construct List2 Linked x (construct List2 Linked))	(construct List2 Linked (head-list2 l) (append-list2 (tail-list2 l) x)))))",
				TestComplex.escapeBrackets("(def append-list2 `([[:List2\\* :\\w*] ~(fn [l x] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2\\*] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:\\w* :List2Linked] [x ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] [])]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:\\w* :List2Linked] [("
						+ AbstractionApplication.clojureEapply + " head-list2 [:List2\\*] [l]) ("
						+ AbstractionApplication.clojureEapply + " append-list2 [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " tail-list2 [:List2\\*] [l]) x])])))]))"));

		this.testClojureCompileRegex(
				"(define reverse-list2 (lambda ((List2 l)) (if (equals? (deconstruct l) ()) (construct List2 Linked) (append-list2 (reverse-list2 (tail-list2 l)) (head-list2 l)))))",
				TestComplex.escapeBrackets("(def reverse-list2 `([[:List2\\*] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2\\*] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply + " append-list2 [:List2Linked :\\w*] [("
						+ AbstractionApplication.clojureEapply + " reverse-list2 [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " tail-list2 [:List2\\*] [l])]) ("
						+ AbstractionApplication.clojureEapply + " head-list2 [:List2\\*] [l])])))]))"));

		this.testClojureCompileRegex("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[[[:IntNative :IntNative] :-> :IntNative]] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply
						+ " f [:IntNative :IntNative] [21 21]))]) [[[:IntNative :IntNative] :-> :IntNative]] [`([[:IntNative :IntNative] ~(fn [\\w* \\w*] ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~+]) [:IntNative :IntNative] [\\w* \\w*]))])])"));
		this.testClojureCompileRegex("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\")))) +)",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[[[:IntNative :IntNative] :-> :IntNative]] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply
						+ " f [:IntNative :IntNative] [21 21]))] [[[[:IntString :IntString] :-> :IntString]] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply + " f [:IntString :IntString] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"21\"]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"21\"])]))]) [[[:IntNative :IntNative] :-> :IntNative]] [`([[:IntNative :IntNative] ~(fn [\\w* \\w*] ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:IntNative :IntNative] ~+]) [:IntNative :IntNative] [\\w* \\w*]))])])"));
		this.testClojureCompileRegex("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\"))))"
				+ "(lambda ((Int:String x) (Int:String y)) (construct Int String (concat (deconstruct x) (deconstruct y)))))",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[[[:IntNative :IntNative] :-> :IntNative]] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply
						+ " f [:IntNative :IntNative] [21 21]))] [[[[:IntString :IntString] :-> :IntString]] ~(fn [f] ("
						+ AbstractionApplication.clojureEapply + " f [:IntString :IntString] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"21\"]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"21\"])]))]) [[[:IntString :IntString] :-> :IntString]] [`([[:IntString :IntString] ~(fn [\\w* \\w*] ("
						+ AbstractionApplication.clojureEapply + " `([[:IntString :IntString] ~(fn [x y] ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative :StringNative] ~str]) [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:IntString] [x]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:IntString] [y])])]))]) [:IntString :IntString] [\\w* \\w*]))])])"));

		this.testClojureCompileRegex("((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\"))",
				TestComplex.escapeBrackets("(" + AbstractionApplication.clojureEapply
						+ " `([[:A :B] ~(fn [x y] [x y])]) [:IntNative :IntString] [42 ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:StringNative] ~(fn [\\w*] \\w*)]) [:StringNative] [\"42\"])])"));

		this.testClojureCompileRegex(
				"(extended-lambda ((List2 l) (A x))" + "((List2:Linked A) (if (equals? (deconstruct l) ())"
						+ "(construct List2 Linked x (construct List2 Linked))"
						+ "(construct List2 Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List2:Functional A) (if (equals? (deconstruct l) ())"
						+ "(construct List2 Functional x (construct List2 Functional))"
						+ "(construct List2 Functional (head-list l) (append-list (tail-list l) x)))))",
				TestComplex.escapeBrackets("`([[:List2Functional :A] ~(fn [l x] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :List2\\*] ~(fn [x l] ("
						+ AbstractionApplication.clojureEapply
						+ " fcons [:\\w* :List2\\*] [x l]))]) [:A :List2Functional] [x ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] [])]) ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :List2\\*] ~(fn [x l] ("
						+ AbstractionApplication.clojureEapply + " fcons [:\\w* :List2\\*] [x l]))]) [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " head-list [:List2Functional] [l]) ("
						+ AbstractionApplication.clojureEapply + " append-list [:\\w* :A] [("
						+ AbstractionApplication.clojureEapply
						+ " tail-list [:List2Functional] [l]) x])])))] [[:List2Linked :A] ~(fn [l x] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2Linked] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:A :List2Linked] [x ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] [])]) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " head-list [:List2Linked] [l]) ("
						+ AbstractionApplication.clojureEapply + " append-list [:\\w* :A] [("
						+ AbstractionApplication.clojureEapply + " tail-list [:List2Linked] [l]) x])])))])"));

		this.testClojureCompileRegex("(extended-lambda ((List2 l))"
				+ "                        ((List2:Linked) (if (equals? (deconstruct l) ())"
				+ "                                            (construct List2 Linked)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))"
				+ "                        ((List2:Functional) (if (equals? (deconstruct l) ())"
				+ "                                            (construct List2 Functional)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l)))))",
				TestComplex.escapeBrackets("`([[:List2Functional] ~(fn [l] (if (" + AbstractionApplication.clojureEapply
						+ " `([[:\\w* :\\w*] ~=]) [:\\w* []] [(" + AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply + " append-list [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " reverse-list [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " tail-list [:List2Functional] [l])]) ("
						+ AbstractionApplication.clojureEapply
						+ " head-list [:List2Functional] [l])])))] [[:List2Linked] ~(fn [l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2Linked] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply + " append-list [:\\w* :\\w*] [("
						+ AbstractionApplication.clojureEapply + " reverse-list [:\\w*] [("
						+ AbstractionApplication.clojureEapply + " tail-list [:List2Linked] [l])]) ("
						+ AbstractionApplication.clojureEapply + " head-list [:List2Linked] [l])])))])"));

		this.testClojureCompileRegex("(extended-lambda ((((A) #> B) f) (List l))"
				+ "                    ((((A) #> B) List2:Linked) (if (equals? (deconstruct l) ())"
				+ "                                                (construct List2 Linked)"
				+ "                                                (construct List2 Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List2:Functional) (if (equals? (deconstruct l) ())"
				+ "                                                    (construct List2 Functional)"
				+ "                                                    (construct List2 Functional (f (head-list l)) (map-list f (tail-list l))))))",
				TestComplex.escapeBrackets("`([[[[:A] :-> :B] :List2Functional] ~(fn [f l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w*] ~identity]) [:List2Functional] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :List2\\*] ~(fn [x l] ("
						+ AbstractionApplication.clojureEapply + " fcons [:\\w* :List2\\*] [x l]))]) [:B :\\w*] [("
						+ AbstractionApplication.clojureEapply + " f [:\\w*] [(" + AbstractionApplication.clojureEapply
						+ " head-list [:List2Functional] [l])]) (" + AbstractionApplication.clojureEapply
						+ " map-list [[[:A] :-> :B] :\\w*] [f (" + AbstractionApplication.clojureEapply
						+ " tail-list [:List2Functional] [l])])])))] [[[[:A] :-> :B] :List2Linked] ~(fn [f l] (if ("
						+ AbstractionApplication.clojureEapply + " `([[:\\w* :\\w*] ~=]) [:\\w* []] [("
						+ AbstractionApplication.clojureEapply + " `([[:\\w*] ~identity]) [:List2Linked] [l]) nil]) ("
						+ AbstractionApplication.clojureEapply + " `([[] ~(fn [] nil)]) [] []) ("
						+ AbstractionApplication.clojureEapply
						+ " `([[:\\w* :List2\\*] ~(fn [x l] [x l])]) [:B :\\w*] [("
						+ AbstractionApplication.clojureEapply + " f [:\\w*] [(" + AbstractionApplication.clojureEapply
						+ " head-list [:List2Linked] [l])]) (" + AbstractionApplication.clojureEapply
						+ " map-list [[[:A] :-> :B] :\\w*] [f (" + AbstractionApplication.clojureEapply
						+ " tail-list [:List2Linked] [l])])])))])"));

		/*
		 * this.
		 * testClojureCompile("(define append-list2-el (extended-lambda ((List2 l) (A x))"
		 * +
		 * "                        ((List2:Linked A) (if (equals? (deconstruct l) ())"
		 * +
		 * "                                                (construct List2 Linked x (construct List2 Linked))"
		 * +
		 * "                                                (construct List2 Linked (head-list2 l) (append-list2-el (tail-list2 l) x))))"
		 * +
		 * "                        ((List2:Functional A) (if (equals? (deconstruct l) ())"
		 * +
		 * "                                                (construct List2 Functional x (construct List2 Functional))"
		 * +
		 * "                                                (construct List2 Functional (head-list2 l) (append-list2-el (tail-list2 l) x))))))"
		 * , ""); this.testClojureCompile("(head-list2 (append-list2-el x 42))", "");
		 * this.testClojureCompile("(head-list2 (append-list2-el y 42))", "");
		 */
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

	private String compileExpressionsToClojure(List<Expression> l) throws AppendableException {
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

	private String compileToClojure(String code) throws AppendableException {
		SemanticParser semanticParser = new SemanticParser();
		List<Expression> l = this.parseString(code, semanticParser);

		return this.compileExpressionsToClojure(l);
	}

	private void testClojureCodeCmp(String generatedCode, String expectedCode) {
		if (generatedCode.compareTo(expectedCode) != 0) {
			fail("Clojure compilation test failed, got " + generatedCode + " expected " + expectedCode);
		}
	}

	private void testClojureCompileExpression(Expression e, String expected) throws AppendableException {
		List<Expression> l = new LinkedList<Expression>();
		l.add(e);
		String s = this.compileExpressionsToClojure(l);
		this.testClojureCodeCmp(s, expected);
	}

	private void testClojureCompile(String code, String expected) throws AppendableException {
		String s = this.compileToClojure(code);

		this.testClojureCodeCmp(s, expected);
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
