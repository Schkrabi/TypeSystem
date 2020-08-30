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
				"(define x (construct List Linked (construct Int Roman \"XLII\") (construct List Linked (construct Int String \"42\") (construct List Linked 42 (construct List Linked)))))",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString(
				"(define y (construct List Functional (construct Int Roman \"XLII\") (construct List Functional (construct Int String \"42\") (construct List Functional 42 (construct List Functional)))))",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString(
				"(define is-list-empty (extended-lambda ((List l))\n"
						+ "                        ((List:Linked) (can-deconstruct-as l ()))\n"
						+ "                        ((List:Functional) (can-deconstruct-as l ()))))",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString("(is-list-empty x)", LitBoolean.FALSE);
		this.testInterpretString("(is-list-empty y)", LitBoolean.FALSE);
		this.testInterpretString("(is-list-empty (construct List Linked))", LitBoolean.TRUE);
		this.testInterpretString("(is-list-empty (construct List Functional))", LitBoolean.TRUE);

		this.testInterpretString("(define head-list (let-type (A) (extended-lambda ((List l))\n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString("(head-list x)", xlii);

		this.testInterpretString(
				"(define y (construct List Functional (construct Int Roman \"XLII\") (construct List Functional (construct Int String \"42\") (construct List Functional 42 (construct List Functional)))))"
						+ "(head-list y)",
				xlii);

		this.testInterpretString("(define tail-list (let-type (A) (extended-lambda ((List l)) \n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString("(tail-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwoStr,
								new LitComposite(new Tuple(Arrays.asList(fortyTwo, emptyList)), linkedList))),
						linkedList));

		this.testInterpretString("(head-list (tail-list y))", fortyTwoStr);

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

		this.testInterpretString("(define append-list (let-type (A) (extended-lambda ((List l) (A x))\n"
				+ "                        ((List:Linked A) (if (is-list-empty l)\n"
				+ "                            (construct List Linked x (construct List Linked))\n"
				+ "                            (construct List Linked (head-list l) (append-list (tail-list l) x))))\n"
				+ "						((List:Functional A) (if (is-list-empty l)\n"
				+ "                            (construct List Functional x (construct List Functional))\n"
				+ "                            (construct List Functional (head-list l) (append-list (tail-list l) x)))))))\n",
				Expression.EMPTY_EXPRESSION);

		this.testInterpretString("(append-list x 21)",
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
				"(define reverse-list (lambda ((List l)) " + "(if (is-list-empty l) " + "(construct List Linked) "
						+ "(append-list (reverse-list (tail-list l)) (head-list l)))))" + "(reverse-list x)",
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

		this.testInterpretString("(let-type (A B) ((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\")))", new Tuple(
				Arrays.asList(new LitInteger(42), new LitComposite(new LitString("42"), TypeAtom.TypeIntString))));

		TypeName listTypeName = new TypeName("List");
		TypeAtom typeListLinkedAtom = new TypeAtom(listTypeName, new TypeRepresentation("Linked"));
		TypeAtom typeListFuntionalAtom = new TypeAtom(listTypeName, new TypeRepresentation("Functional"));

		this.testInterpretString(
				"(let-type (A) (extended-lambda ((List l) (A x))" + "((List:Linked A) (if (can-deconstruct-as l ())"
						+ "(construct List Linked x (construct List Linked))"
						+ "(construct List Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List:Functional A) (if (can-deconstruct-as l ())"
						+ "(construct List Functional x (construct List Functional))"
						+ "(construct List Functional (head-list l) (append-list (tail-list l) x))))))",
				ExtendedFunction.makeExtendedFunction(
						Arrays.asList(
								new Function(new TypeTuple(Arrays.asList(typeListLinkedAtom, new TypeVariable("A"))),
										new Tuple(Arrays.asList(new Symbol("l"),
												new Symbol("x"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
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
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
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
						.makeExtendedFunction(Arrays.asList(
								new Function(new TypeTuple(Arrays.asList(typeListLinkedAtom)),
										new Tuple(Arrays.asList(new Symbol("l"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(typeListLinkedAtom, Tuple.EMPTY_TUPLE),
												new AbstractionApplication(new Symbol("append-list"),
														new Tuple(Arrays.asList(
																new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))))))),
										Environment.topLevelEnvironment),
								new Function(new TypeTuple(Arrays.asList(typeListFuntionalAtom)),
										new Tuple(Arrays.asList(new Symbol("l"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
												new AbstractionApplication(new Symbol("append-list"),
														new Tuple(Arrays.asList(
																new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))))))),
										Environment.topLevelEnvironment)),
								Environment.topLevelEnvironment));

		this.testInterpretString("(let-type (A B) (extended-lambda ((((A) #> B) f) (List l))"
				+ "                    ((((A) #> B) List:Linked) (if (can-deconstruct-as l ())"
				+ "                                                (construct List Linked)"
				+ "                                                (construct List Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List:Functional) (if (can-deconstruct-as l ())"
				+ "                                                    (construct List Functional)"
				+ "                                                    (construct List Functional (f (head-list l)) (map-list f (tail-list l)))))))",
				ExtendedFunction
						.makeExtendedFunction(
								Arrays.asList(
										new Function(
												new TypeTuple(Arrays.asList(new TypeArrow(
														new TypeTuple(Arrays.asList(new TypeVariable("A"))),
														new TypeVariable("B")), typeListLinkedAtom)),
												new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
												new IfExpression(
														new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
														new Construct(typeListLinkedAtom, Tuple.EMPTY_TUPLE),
														new Construct(typeListLinkedAtom, new Tuple(Arrays.asList(
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
												Environment.topLevelEnvironment),
										new Function(
												new TypeTuple(
														Arrays.asList(new TypeArrow(
																new TypeTuple(Arrays.asList(new TypeVariable("A"))),
																new TypeVariable("B")), typeListFuntionalAtom)),
												new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
												new IfExpression(
														new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
														new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
														new Construct(typeListFuntionalAtom, new Tuple(Arrays.asList(
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
				"(with-meta [(with-meta (fn [x y] x) {:lang-type (lang-type-arrow. [\"\\w*\" \"\\w*\"] \"\\w*\")})] {:lang-type (lang-type-arrow. [\"\\w*\" \"\\w*\"] \"\\w*\")})"));
		// Application
		this.testClojureCompile("((lambda ((Int:Native x) (Int:Native y)) x) 42 21)",
				"(" + AbstractionApplication.clojureEapply + " (with-meta [(with-meta (fn [x y] x) {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [21] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		// If
		this.testClojureCompile("(if #t 42 21)",
				"(if (get (with-meta [true] {:lang-type " + TypeAtom.TypeBoolNative.clojureTypeRepresentation()
						+ "}) 0) (with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}) (with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}))");
		this.testClojureCompileNoCmp("(if #t (construct Int Roman \"XLII\") (construct Int String \"42\"))");
		// Cons
		this.testClojureCompile("(cons 21 21)",
				"(with-meta [(with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}) (with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "})");
		// Exception
		this.testClojureCompile("(error \"error msg\")",
				"(throw (Throwable. (get (with-meta [\"error msg\"] {:lang-type "
						+ TypeAtom.TypeStringNative.clojureTypeRepresentation() + "}) 0)))");
		// Operators
		this.testClojureCompile("(+ 41 1)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(+ (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [41] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [1] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompile("(and #t #f)", "(with-meta [(and (get (with-meta [true] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0) (get (with-meta [false] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0))]{:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})");
		this.testClojureCompile("(bit-and 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(bit-and (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [1] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type " + (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompileNoCmp("(bit-or 42 1)");
		this.testClojureCompileNoCmp("(car pair)");
		this.testClojureCompileNoCmp("(cdr pair)");
		this.testClojureCompileNoCmp("(concat \"Hello\" \"World\")");
		this.testClojureCompile("(/ 84 2)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(/ (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [84] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [2] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompileNoCmp("(equals? 42 \"42\")");
		this.testClojureCompile("(< 42 42)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(< (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompile("(* 42 1)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(* (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [1] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompile("(not #t)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x] (with-meta [(not (get _x 0))] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative))
						.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative))
						.clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [true] {:lang-type " + TypeAtom.TypeBoolNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative))).clojureTypeRepresentation() + "}) "
				+ AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompile("(= 42 42)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(= (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [42] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		this.testClojureCompile("(or #t #f)", "(with-meta [(or (get (with-meta [true] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0) (get (with-meta [false] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0))]{:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})");
		this.testClojureCompile("(- 43 1)",
				"(" + AbstractionApplication.clojureEapply
						+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(- (get _x 0) (get _y 0))] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
								TypeAtom.TypeIntNative)).clojureTypeRepresentation()
						+ "}) (with-meta [(with-meta [43] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}) (with-meta [1] {:lang-type "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")");
		// Conversions
		this.testClojureCompileNoCmp("(IntNative2IntRoman 42)");
		this.testClojureCompileNoCmp("(IntNative2IntString 42)");
		this.testClojureCompileNoCmp("(IntRoman2IntNative (Int:Roman \"XLII\"))");
		this.testClojureCompileNoCmp("(IntRoman2IntString (Int:Roman \"XLII\"))");
		this.testClojureCompileNoCmp("(IntString2IntNative (Int:String \"42\"))");
		this.testClojureCompileNoCmp("(IntString2IntRoman (Int:String \"42\"))");
		// Define
		this.testClojureCompile("(define answer 42)", "(def answer (with-meta [42] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))");
		this.testClojureCompile("(type Name2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompile("(representation Structured Name2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompile("(constructor Name2 Structured ((String:Native x) (String:Native y)) (cons x y))", "");
		this.testClojureCompile("(representation Unstructured Name2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompile("(constructor Name2 Unstructured ((String:Native x)) x)", "");
		this.testClojureCompile("(conversion Name2:Structured Name2:Unstructured"
				+ "((Name2:Structured x)) (construct Name2 Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))",
				"");

		this.testClojureCompileNoCmp(
				"((lambda ((Name2:Unstructured x)) x) (construct Name2 Structured \"Jan\" \"Novak\"))");
		// Extended Lambda
		this.testClojureCompileNoCmp("(extended-lambda ((Int x)) ((Int:Native) \"Native\"))");
		this.testClojureCompileNoCmp(
				"((extended-lambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (Int:String \"42\"))");

		// Recursion
		this.testClojureCompileNoCmp("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))");

		// Conversions
		this.testClojureCompileNoCmp(
				"((extended-lambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (Int:Roman \"XLII\") 66)");

		// List
		this.testClojureCompile("(type List2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompile("(representation Linked List2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompile("(constructor List2 Linked (x (List2 l)) (cons x l))", "");
		this.testClojureCompile("(constructor List2 Linked () ())", "");
		this.testClojureCompile("(representation Functional List2)", "(with-meta [] {:lang-type []})");
		this.testClojureCompileNoCmp("(define fcons (lambda (x y) (lambda (f) (f x y))))");
		this.testClojureCompileNoCmp("(define fcar (lambda (p) (p (lambda (x y) x))))");
		this.testClojureCompile("(constructor List2 Functional (x (List2 l)) (fcons x l))", "");
		this.testClojureCompile("(constructor List2 Functional () ())", "");

		this.testClojureCompileNoCmp(
				"(define x (construct List2 Linked (construct Int Roman \"XLII\") (construct List2 Linked (construct Int String \"42\") (construct List2 Linked 42 (construct List2 Linked)))))");

		this.testClojureCompileNoCmp(
				"(define y (construct List2 Functional (construct Int Roman \"XLII\") (construct List2 Functional (construct Int String \"42\") (construct List2 Functional 42 (construct List2 Functional)))))");

		this.testClojureCompileNoCmp(
				"(define head-list2 (let-type (A) (extended-lambda ((List2 l)) ((List2:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List2:Linked)))))"
						+ "((List2:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List2:Functional) #> List2:Functional)) #> List2:Functional))))))))");

		this.testClojureCompileNoCmp(
				"(define tail-list2 (let-type (A) (extended-lambda ((List2 l)) ((List2:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List2:Linked)))))"
						+ "((List2:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A List2:Functional) #> List2:Functional)) #> List2:Functional))))))))");

		// This is interesting, extended lambda is not sufficient, when I might want to
		// return a different representation.
		// But on what would I base the representation?
		this.testClojureCompileNoCmp(
				"(define build-list2-aux (lambda (i n f) (if (= i n) (construct List2 Linked) (construct List2 Linked (f i) (build-list2-aux (+ i 1) n f)))))");

		this.testClojureCompileNoCmp("(define build-list2 (lambda (n f) (build-list2-aux 0 n f)))");

		this.testClojureCompileNoCmp("(define append-list2 (lambda ((List2 l) x) \n"
				+ "(if (can-deconstruct-as l ()) (construct List2 Linked x (construct List2 Linked)) (construct List2 Linked (head-list2 l) (append-list2 (tail-list2 l) x)))))");

		this.testClojureCompileNoCmp(
				"(define reverse-list2 (lambda ((List2 l)) (if (can-deconstruct-as l ()) (construct List2 Linked) (append-list2 (reverse-list2 (tail-list2 l)) (head-list2 l)))))");

		this.testClojureCompileNoCmp("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)");
		this.testClojureCompileNoCmp("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\")))) +)");
		this.testClojureCompileNoCmp("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\"))))"
				+ "(lambda ((Int:String x) (Int:String y)) (construct Int String (concat (deconstruct x String:Native) (deconstruct y String:Native)))))");

		this.testClojureCompileNoCmp("(let-type (A B) ((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\")))");

		this.testClojureCompileNoCmp(
				"(let-type (A) (extended-lambda ((List2 l) (A x))" + "((List2:Linked A) (if (can-deconstruct-as l ())"
						+ "(construct List2 Linked x (construct List2 Linked))"
						+ "(construct List2 Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List2:Functional A) (if (can-deconstruct-as l ())"
						+ "(construct List2 Functional x (construct List2 Functional))"
						+ "(construct List2 Functional (head-list l) (append-list (tail-list l) x))))))");

		this.testClojureCompileNoCmp("(extended-lambda ((List2 l))"
				+ "                        ((List2:Linked) (if (can-deconstruct-as l ())"
				+ "                                            (construct List2 Linked)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))"
				+ "                        ((List2:Functional) (if (can-deconstruct-as l ())"
				+ "                                            (construct List2 Functional)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l)))))");

		this.testClojureCompileNoCmp("(let-type (A B) (extended-lambda ((((A) #> B) f) (List2 l))"
				+ "                    ((((A) #> B) List2:Linked) (if (can-deconstruct-as l ())"
				+ "                                                (construct List2 Linked)"
				+ "                                                (construct List2 Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List2:Functional) (if (can-deconstruct-as l ())"
				+ "                                                    (construct List2 Functional)"
				+ "                                                    (construct List2 Functional (f (head-list l)) (map-list f (tail-list l)))))))");

		this.testClojureCompileNoCmp("(println (cons 42 \"42\"))");
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

	private void testClojureCompileNoCmp(String code) throws AppendableException {
		this.compileToClojure(code);
	}

	private static String escapeBrackets(String s) {
		return s.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)").replaceAll("\\[", "\\\\[")
				.replaceAll("\\]", "\\\\]").replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
				.replaceAll("\\+", "\\\\+");
	}
}
