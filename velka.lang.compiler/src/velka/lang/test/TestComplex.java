package velka.lang.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.lang.abstraction.ExtendedFunction;
import velka.lang.abstraction.Function;
import velka.lang.application.AbstractionApplication;
import velka.lang.application.CanDeconstructAs;
import velka.lang.application.Construct;
import velka.lang.application.IfExpression;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitInteger;
import velka.lang.literal.LitString;
import velka.lang.parser.SchemeLexer;
import velka.lang.parser.SchemeParser;
import velka.lang.parser.SchemeParser.ExprsContext;
import velka.lang.semantic.SemanticParser;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;
import velka.lang.util.ThrowingFunction;

class TestComplex {

	@Test
	@DisplayName("Test Recursion")
	void testRecursion() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testInterpretString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(fact 5)",
				new LitInteger(120), env, typeEnv);

		TestComplex.testClojureCompileNoCmp("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))", env,
				typeEnv);

		TestComplex.testClojureCompileClj(
				"(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(println (fact 5))", "120\n");
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(println (fact 5))");
	}

	@Test
	@DisplayName("Test Basic Extended lambda")
	void testExtemdedLambda() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testInterpretString("(type Name)" + "(representation Unstructured Name)"
				+ "(constructor Name Unstructured ((String:Native x)) x)" + "(representation Structured Name)"
				+ "(constructor Name Structured ((String:Native x) (String:Native y)) (cons x y))"
				+ "((extended-lambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (construct Name Unstructured \"Jan Novak\"))",
				new LitString("unstructured"), env, typeEnv);

		TestComplex.testInterpretString(
				"((extended-lambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (construct Name Structured \"Jan\" \"Novak\"))",
				new LitString("structured"), env, typeEnv);

		TestComplex.testInterpretString(
				"(conversion Name:Structured Name:Unstructured ((Name:Structured x)) (construct Name Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
						+ "((lambda ((Name:Unstructured x)) x) (construct Name Structured \"Jan\" \"Novak\"))",
				new LitComposite(new LitString("JanNovak"),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured"))),
				env, typeEnv);

		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println ((lambda ((String:Native x) (Int:String y)) x) \"test\" (construct Int String \"1984\")))\n"
						+ "(println ((extended-lambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (construct Int Roman \"XLII\") 66))\n"
						+ "");
	}

	@Test
	@DisplayName("Test User List Interpretation")
	void testComplexList() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);

		TypeName listTypeName = new TypeName("List");

		final TypeAtom linkedList = new TypeAtom(listTypeName, new TypeRepresentation("Linked"));
		final TypeAtom typeListFuntionalAtom = new TypeAtom(listTypeName, new TypeRepresentation("Functional"));
		final LitComposite emptyList = new LitComposite(Expression.EMPTY_EXPRESSION, linkedList);

		TestComplex.testInterpretString(
				"(representation Linked List)" + "(constructor List Linked (x (List l)) (cons x l))"
						+ "(constructor List Linked () ())"
						+ "(construct List Linked 1 (construct List Linked 2 (construct List Linked)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env, typeEnv);

		TestComplex.testInterpretString("(define fcons (lambda (x y) (lambda (f) (f x y))))"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))" + "(define fcdr (lambda (p) (p (lambda (x y) y))))"
				+ "(representation Functional List)" + "(constructor List Functional (x (List l)) (fcons x l))"
				+ "(constructor List Functional () ())", Expression.EMPTY_EXPRESSION, env, typeEnv);

		final LitComposite xlii = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		final LitComposite fortyTwoStr = new LitComposite(new LitString("42"), TypeAtom.TypeIntString);
		final LitInteger fortyTwo = new LitInteger(42);

		TestComplex.testInterpretString(
				"(define x (construct List Linked (construct Int Roman \"XLII\") (construct List Linked (construct Int String \"42\") (construct List Linked 42 (construct List Linked)))))",
				Expression.EMPTY_EXPRESSION, env, typeEnv);

		TestComplex.testInterpretString(
				"(define y (construct List Functional (construct Int Roman \"XLII\") (construct List Functional (construct Int String \"42\") (construct List Functional 42 (construct List Functional)))))",
				Expression.EMPTY_EXPRESSION, env, typeEnv);
		TestComplex.testInterpretString(
				"(define is-list-empty (extended-lambda ((List l))\n"
						+ "                        ((List:Linked) (can-deconstruct-as l ()))\n"
						+ "                        ((List:Functional) (can-deconstruct-as l ()))))",
				Expression.EMPTY_EXPRESSION, env, typeEnv);
		TestComplex.testInterpretString("(is-list-empty x)", LitBoolean.FALSE, env, typeEnv);
		TestComplex.testInterpretString("(is-list-empty y)", LitBoolean.FALSE, env, typeEnv);
		TestComplex.testInterpretString("(is-list-empty (construct List Linked))", LitBoolean.TRUE, env, typeEnv);
		TestComplex.testInterpretString("(is-list-empty (construct List Functional))", LitBoolean.TRUE, env, typeEnv);

		TestComplex.testInterpretString("(define head-list (let-type (A) (extended-lambda ((List l))\n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))",
				Expression.EMPTY_EXPRESSION, env, typeEnv);
		TestComplex.testInterpretString("(head-list x)", xlii, env, typeEnv);
		TestComplex.testInterpretString("(head-list y)", xlii, env, typeEnv);

		TestComplex.testInterpretString("(define tail-list (let-type (A) (extended-lambda ((List l)) \n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))",
				Expression.EMPTY_EXPRESSION, env, typeEnv);

		TestComplex.testInterpretString("(tail-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwoStr,
								new LitComposite(new Tuple(Arrays.asList(fortyTwo, emptyList)), linkedList))),
						linkedList),
				env, typeEnv);
		TestComplex.testInterpretString("(head-list (tail-list y))", fortyTwoStr, env, typeEnv);

		TestComplex.testInterpretString(
				"(define build-list-aux (lambda (i n f) " + "(if (= i n) " + "(construct List Linked)"
						+ "(construct List Linked (f i) (build-list-aux (+ i 1) n f)))))"
						+ "(build-list-aux 0 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env, typeEnv);

		TestComplex.testInterpretString(
				"(define build-list (lambda (n f) (build-list-aux 0 n f)))" + "(build-list 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env, typeEnv);

		TestComplex.testInterpretString(
				"(let-type (A) (extended-lambda ((List l) (A x))"
						+ "((List:Linked A) (if (can-deconstruct-as l ())"
						+ "(construct List Linked x (construct List Linked))"
						+ "(construct List Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List:Functional A) (if (can-deconstruct-as l ())"
						+ "(construct List Functional x (construct List Functional))"
						+ "(construct List Functional (head-list l) (append-list (tail-list l) x))))))",
				ExtendedFunction
						.makeExtendedFunction(Arrays.asList(
								new Function(new TypeTuple(Arrays.asList(linkedList, new TypeVariable("A"))),
										new Tuple(Arrays.asList(new Symbol("l"), new Symbol("x"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(linkedList,
														new Tuple(Arrays.asList(new Symbol("x"),
																new Construct(linkedList, Tuple.EMPTY_TUPLE)))),
												new Construct(linkedList,
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
										env),
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
										env)),
								env),
				env, typeEnv);

		TestComplex.testInterpretString("(define append-list (let-type (A) (extended-lambda ((List l) (A x))\n"
				+ "                        ((List:Linked A) (if (is-list-empty l)\n"
				+ "                            (construct List Linked x (construct List Linked))\n"
				+ "                            (construct List Linked (head-list l) (append-list (tail-list l) x))))\n"
				+ "						((List:Functional A) (if (is-list-empty l)\n"
				+ "                            (construct List Functional x (construct List Functional))\n"
				+ "                            (construct List Functional (head-list l) (append-list (tail-list l) x)))))))\n",
				Expression.EMPTY_EXPRESSION, env, typeEnv);

		TestComplex
				.testInterpretString("(append-list x 21)",
						new LitComposite(
								new Tuple(
										Arrays.asList(xlii,
												new LitComposite(
														new Tuple(
																Arrays.asList(fortyTwoStr,
																		new LitComposite(
																				new Tuple(Arrays.asList(fortyTwo,
																						new LitComposite(
																								new Tuple(Arrays.asList(
																										new LitInteger(
																												21),
																										emptyList)),
																								linkedList))),
																				linkedList))),
														linkedList))),
								linkedList),
						env, typeEnv);

		TestComplex.testInterpretString("(extended-lambda ((List l))"
				+ "                        ((List:Linked) (if (can-deconstruct-as l ())"
				+ "                                            (construct List Linked)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))"
				+ "                        ((List:Functional) (if (can-deconstruct-as l ())"
				+ "                                            (construct List Functional)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l)))))",
				ExtendedFunction.makeExtendedFunction(Arrays.asList(
						new Function(new TypeTuple(Arrays.asList(linkedList)),
								new Tuple(Arrays
										.asList(new Symbol("l"))),
								new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
										new Construct(linkedList,
												Tuple.EMPTY_TUPLE),
										new AbstractionApplication(new Symbol(
												"append-list"),
												new Tuple(
														Arrays.asList(
																new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))))))),
								env),
						new Function(new TypeTuple(Arrays.asList(typeListFuntionalAtom)),
								new Tuple(Arrays
										.asList(new Symbol("l"))),
								new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
										new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
										new AbstractionApplication(
												new Symbol("append-list"),
												new Tuple(
														Arrays.asList(
																new AbstractionApplication(
																		new Symbol("reverse-list"),
																		new Tuple(Arrays
																				.asList(new AbstractionApplication(
																						new Symbol("tail-list"),
																						new Tuple(Arrays.asList(
																								new Symbol("l"))))))),
																new AbstractionApplication(new Symbol("head-list"),
																		new Tuple(Arrays.asList(new Symbol("l")))))))),
								env)),
						env),
				env, typeEnv);

		TestComplex.testInterpretString(
				"(define reverse-list (lambda ((List l)) "
						+ "(if (is-list-empty l) " + "(construct List Linked) "
						+ "(append-list (reverse-list (tail-list l)) (head-list l)))))" + "(reverse-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwo,
								new LitComposite(new Tuple(Arrays.asList(fortyTwoStr,
										new LitComposite(new Tuple(Arrays.asList(xlii, emptyList)), linkedList))),
										linkedList))),
						linkedList),
				env, typeEnv);

		// map
		TestComplex.testInterpretString("(let-type (A B) (extended-lambda ((((A) #> B) f) (List l))"
				+ "                    ((((A) #> B) List:Linked) (if (can-deconstruct-as l ())"
				+ "                                                (construct List Linked)"
				+ "                                                (construct List Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List:Functional) (if (can-deconstruct-as l ())"
				+ "                                                    (construct List Functional)"
				+ "                                                    (construct List Functional (f (head-list l)) (map-list f (tail-list l)))))))",
				ExtendedFunction.makeExtendedFunction(
						Arrays.asList(
								new Function(
										new TypeTuple(Arrays.asList(
												new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))),
														new TypeVariable("B")),
												linkedList)),
										new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(linkedList, Tuple.EMPTY_TUPLE),
												new Construct(linkedList,
														new Tuple(Arrays.asList(
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
										env),
								new Function(
										new TypeTuple(Arrays.asList(
												new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))),
														new TypeVariable("B")),
												typeListFuntionalAtom)),
										new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
										new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
												new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
												new Construct(typeListFuntionalAtom, new Tuple(Arrays.asList(
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
										env)),
						env),
				env, typeEnv);
	}

	@Test
	@DisplayName("Test Complex Types")
	void testComplexTypes() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testInterpretString("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)",
				new LitInteger(42), env, typeEnv);
		TestComplex.testInterpretString("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\")))) +)",
				new LitInteger(42), env, typeEnv);
		TestComplex.testInterpretString("((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
				+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\"))))"
				+ "(lambda ((Int:String x) (Int:String y)) (construct Int String (concat (deconstruct x String:Native) (deconstruct y String:Native)))))",
				new LitComposite(new LitString("2121"), TypeAtom.TypeIntString), env, typeEnv);
		TestComplex.testInterpretString(
				"(let-type (A B) ((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\")))",
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				env, typeEnv);

		TestComplex.assertIntprtAndCompPrintSameValues("(type Name)\n" + "(representation Unstructured Name)\n"
				+ "(constructor Name Unstructured ((String:Native x)) x)\n" + "(representation Structured Name)\n"
				+ "(constructor Name Structured ((String:Native x) (String:Native y)) (cons x y))\n"
				+ "(println ((extended-lambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (construct Name Unstructured \"Jan Novak\")))\n"
				+ "(println ((extended-lambda (x) ((Name:Structured) \"structured\")) (construct Name Structured \"Jan\" \"Novak\")))\n"
				+ "(conversion Name:Structured Name:Unstructured ((Name:Structured x)) (construct Name Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))\n"
				+ "(println ((lambda ((Name:Unstructured x)) x) (construct Name Structured \"Jan\" \"Novak\")))\n"
				+ "");
	}

	@Test
	@DisplayName("Clojure Literals")
	void testClojureLiterals() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testClojureCompile("0",
				"(with-meta [0] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"Int\") (new velka.lang.types.TypeRepresentation \"Native\"))})",
				env, typeEnv);
		TestComplex.testClojureCompile("3.141521",
				"(with-meta [3.141521] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"Double\") (new velka.lang.types.TypeRepresentation \"Native\"))})",
				env, typeEnv);
		TestComplex.testClojureCompile("#t",
				"(with-meta [true] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"Bool\") (new velka.lang.types.TypeRepresentation \"Native\"))})",
				env, typeEnv);
		TestComplex.testClojureCompile("#f",
				"(with-meta [false] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"Bool\") (new velka.lang.types.TypeRepresentation \"Native\"))})",
				env, typeEnv);
		TestComplex.testClojureCompile("\"Hello World\"",
				"(with-meta [\"Hello World\"] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"String\") (new velka.lang.types.TypeRepresentation \"Native\"))})",
				env, typeEnv);
		TestComplex.testClojureCompileExpression(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				"(with-meta [(with-meta [\"XLII\"] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"String\") (new velka.lang.types.TypeRepresentation \"Native\"))})] {:lang-type (new velka.lang.types.TypeAtom (new velka.lang.types.TypeName \"Int\") (new velka.lang.types.TypeRepresentation \"Roman\"))})",
				env, typeEnv);

		TestComplex.testClojureCompile("variable", "variable", env, typeEnv);
	}

	@Test
	@DisplayName("Clojure Special Forms and Applications")
	void testSpecialFormsAndApplication() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		// Lambda
		TestComplex.testClojureCompileRegex("(lambda (x y) x)", TestComplex.escapeBrackets(
				"(with-meta [(with-meta (fn [x y] x) {:lang-type (new velka.lang.types.TypeArrow (new velka.lang.types.TypeTuple [(new velka.lang.types.TypeVariable \"\\w*\") (new velka.lang.types.TypeVariable \"\\w*\")]) (new velka.lang.types.TypeVariable \"\\w*\"))})] {:lang-type (new velka.lang.types.TypeArrow (new velka.lang.types.TypeTuple [(new velka.lang.types.TypeVariable \"\\w*\") (new velka.lang.types.TypeVariable \"\\w*\")]) (new velka.lang.types.TypeVariable \"\\w*\"))})"),
				env, typeEnv);
		// Application
		TestComplex.testClojureCompile("((lambda ((Int:Native x) (Int:Native y)) x) 42 21)",
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
						+ "}) " + AbstractionApplication.clojureRankingFunction + ")",
				env, typeEnv);
		// If
		TestComplex.testClojureCompile("(if #t 42 21)",
				"(if (get (with-meta [true] {:lang-type " + TypeAtom.TypeBoolNative.clojureTypeRepresentation()
						+ "}) 0) (with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}) (with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}))",
				env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(if #t (construct Int Roman \"XLII\") (construct Int String \"42\"))", env,
				typeEnv);
		// Cons
		TestComplex.testClojureCompile("(cons 21 21)",
				"(with-meta [(with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "}) (with-meta [21] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
						+ "})] {:lang-type "
						+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
								.clojureTypeRepresentation()
						+ "})",
				env, typeEnv);
		// Exception
		TestComplex.testClojureCompile("(error \"error msg\")",
				"(throw (Throwable. (get (with-meta [\"error msg\"] {:lang-type "
						+ TypeAtom.TypeStringNative.clojureTypeRepresentation() + "}) 0)))",
				env, typeEnv);
		// And
		TestComplex.testClojureCompile("(and #t #f)", "(with-meta [(and (get (with-meta [true] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0) (get (with-meta [false] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0))]{:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})", env, typeEnv);
		// Or
		TestComplex.testClojureCompile("(or #t #f)", "(with-meta [(or (get (with-meta [true] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0) (get (with-meta [false] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "}) 0))]{:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})", env, typeEnv);

		// Define
		TestComplex.testClojureCompile("(define answer 42)",
				"(def answer (with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation() + "}))",
				env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(type Name2)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(representation Structured Name2)", env, typeEnv);
		TestComplex.testClojureCompile(
				"(constructor Name2 Structured ((String:Native x) (String:Native y)) (cons x y))", "", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(representation Unstructured Name2)", env, typeEnv);
		TestComplex.testClojureCompile("(constructor Name2 Unstructured ((String:Native x)) x)", "", env, typeEnv);
		TestComplex.testClojureCompile("(conversion Name2:Structured Name2:Unstructured"
				+ "((Name2:Structured x)) (construct Name2 Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))",
				"", env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"((lambda ((Name2:Unstructured x)) x) (construct Name2 Structured \"Jan\" \"Novak\"))", env, typeEnv);
		// Extended Lambda
		TestComplex.testClojureCompileNoCmp("(extended-lambda ((Int x)) ((Int:Native) \"Native\"))", env, typeEnv);
		TestComplex.testClojureCompileNoCmp(
				"((extended-lambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (Int:String \"42\"))",
				env, typeEnv);
	}

	@Test
	@DisplayName("Clojure Operators")
	void testClojureOperators() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testClojureCompile("(+ 41 1)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(+ (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [41] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [1] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompile("(bit-and 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(bit-and (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [1] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(bit-or 42 1)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(car pair)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(cdr pair)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(concat \"Hello\" \"World\")", env, typeEnv);
		TestComplex.testClojureCompile("(/ 84 2)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(/ (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [84] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [2] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(equals? 42 \"42\")", env, typeEnv);
		TestComplex.testClojureCompile("(< 42 42)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(< (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompile("(* 42 1)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(* (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [1] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompile("(not #t)", "(" + AbstractionApplication.clojureEapply
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
				+ AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompile("(= 42 42)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(= (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeBoolNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [42] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);
		TestComplex.testClojureCompile("(- 43 1)", "(" + AbstractionApplication.clojureEapply
				+ " (with-meta [(with-meta (fn [_x _y] (with-meta [(- (get _x 0) (get _y 0))] {:lang-type "
				+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + "})){:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						TypeAtom.TypeIntNative)).clojureTypeRepresentation()
				+ "}) (with-meta [(with-meta [43] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "}) (with-meta [1] {:lang-type " + TypeAtom.TypeIntNative.clojureTypeRepresentation()
				+ "})] {:lang-type "
				+ (new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)))
						.clojureTypeRepresentation()
				+ "}) " + AbstractionApplication.clojureRankingFunction + ")", env, typeEnv);

		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (+ 21 21))\n" + "(println (* 1 42))\n" + "(println (/ 84 2))\n" + "(println (- 63 21))\n"
						+ "(println (and #t #f))\n" + "(println (bit-and 42 1))\n" + "(println (bit-or 42 1))\n"
						+ "(println (concat \"Hello \" \"World\"))\n" + "(println (equals? 42 \"42\"))\n"
						+ "(println (< 42 42))\n" + "(println (not #t))\n" + "(println (= 42 42))\n"
						+ "(println (or #t #f))\n" + "(println (cons 42 \"42\"))\n"
						+ "(println (car (cons 42 \"42\")))\n" + "(println (cdr (cons 42 \"42\")))");
	}

	@Test
	@DisplayName("Clojure Conversions")
	void testClojureConversions() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.testClojureCompileNoCmp("(IntNative2IntRoman 42)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(IntNative2IntString 42)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(IntRoman2IntNative (Int:Roman \"XLII\"))", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(IntRoman2IntString (Int:Roman \"XLII\"))", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(IntString2IntNative (Int:String \"42\"))", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(IntString2IntRoman (Int:String \"42\"))", env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"((extended-lambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (Int:Roman \"XLII\") 66)",
				env, typeEnv);

		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (convert Int:Native Int:String 42))\n" + "(println (convert Int:Native Int:Roman 42))\n"
						+ "(println (convert Int:String Int:Native (construct Int String \"42\")))\n"
						+ "(println (convert Int:String Int:Roman (construct Int String \"42\")))\n"
						+ "(println (convert Int:Roman Int:Native (construct Int Roman \"XLII\")))\n"
						+ "(println (convert Int:Roman Int:String (construct Int Roman \"XLII\")))");
	}

	@Test
	@DisplayName("Clojure List")
	void testListClojure() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		// List
		TestComplex.testClojureCompileNoCmp("(type List2)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(representation Linked List2)", env, typeEnv);
		TestComplex.testClojureCompile("(constructor List2 Linked (x (List2 l)) (cons x l))", "", env, typeEnv);
		TestComplex.testClojureCompile("(constructor List2 Linked () ())", "", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(representation Functional List2)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(define fcons (lambda (x y) (lambda (f) (f x y))))", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(define fcar (lambda (p) (p (lambda (x y) x))))", env, typeEnv);
		TestComplex.testClojureCompile("(constructor List2 Functional (x (List2 l)) (fcons x l))", "", env, typeEnv);
		TestComplex.testClojureCompile("(constructor List2 Functional () ())", "", env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(define x (construct List2 Linked (construct Int Roman \"XLII\") (construct List2 Linked (construct Int String \"42\") (construct List2 Linked 42 (construct List2 Linked)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(define y (construct List2 Functional (construct Int Roman \"XLII\") (construct List2 Functional (construct Int String \"42\") (construct List2 Functional 42 (construct List2 Functional)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(define head-list2 (let-type (A) (extended-lambda ((List2 l)) ((List2:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List2:Linked)))))"
						+ "((List2:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List2:Functional) #> List2:Functional)) #> List2:Functional))))))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(define tail-list2 (let-type (A) (extended-lambda ((List2 l)) ((List2:Linked) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List2:Linked)))))"
						+ "((List2:Functional) (if (can-deconstruct-as l ()) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A List2:Functional) #> List2:Functional)) #> List2:Functional))))))))",
				env, typeEnv);

		// This is interesting, extended lambda is not sufficient, when I might want to
		// return a different representation.
		// But on what would I base the representation?
		TestComplex.testClojureCompileNoCmp(
				"(define build-list2-aux (lambda (i n f) (if (= i n) (construct List2 Linked) (construct List2 Linked (f i) (build-list2-aux (+ i 1) n f)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp("(define build-list2 (lambda (n f) (build-list2-aux 0 n f)))", env,
				typeEnv);

		TestComplex.testClojureCompileNoCmp("(define append-list2 (lambda ((List2 l) x) \n"
				+ "(if (can-deconstruct-as l ()) (construct List2 Linked x (construct List2 Linked)) (construct List2 Linked (head-list2 l) (append-list2 (tail-list2 l) x)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(define reverse-list2 (lambda ((List2 l)) (if (can-deconstruct-as l ()) (construct List2 Linked) (append-list2 (reverse-list2 (tail-list2 l)) (head-list2 l)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)", env,
				typeEnv);
		TestComplex.testClojureCompileNoCmp(
				"((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
						+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\")))) +)",
				env, typeEnv);
		TestComplex.testClojureCompileNoCmp(
				"((extended-lambda (f) ((((Int:Native Int:Native) #> Int:Native)) (f 21 21))"
						+ "((((Int:String Int:String) #> Int:String)) (f (construct Int String \"21\") (construct Int String \"21\"))))"
						+ "(lambda ((Int:String x) (Int:String y)) (construct Int String (concat (deconstruct x String:Native) (deconstruct y String:Native)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(let-type (A B) ((lambda ((A x) (B y)) (cons x y)) 42 (construct Int String  \"42\")))", env, typeEnv);

		TestComplex.testClojureCompileNoCmp(
				"(let-type (A) (extended-lambda ((List2 l) (A x))" + "((List2:Linked A) (if (can-deconstruct-as l ())"
						+ "(construct List2 Linked x (construct List2 Linked))"
						+ "(construct List2 Linked (head-list l) (append-list (tail-list l) x))))"
						+ "((List2:Functional A) (if (can-deconstruct-as l ())"
						+ "(construct List2 Functional x (construct List2 Functional))"
						+ "(construct List2 Functional (head-list l) (append-list (tail-list l) x))))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp("(extended-lambda ((List2 l))"
				+ "                        ((List2:Linked) (if (can-deconstruct-as l ())"
				+ "                                            (construct List2 Linked)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))"
				+ "                        ((List2:Functional) (if (can-deconstruct-as l ())"
				+ "                                            (construct List2 Functional)"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l)))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp("(let-type (A B) (extended-lambda ((((A) #> B) f) (List2 l))"
				+ "                    ((((A) #> B) List2:Linked) (if (can-deconstruct-as l ())"
				+ "                                                (construct List2 Linked)"
				+ "                                                (construct List2 Linked (f (head-list l)) (map-list f (tail-list l)))))"
				+ "                    ((((A) #> B) List2:Functional) (if (can-deconstruct-as l ())"
				+ "                                                    (construct List2 Functional)"
				+ "                                                    (construct List2 Functional (f (head-list l)) (map-list f (tail-list l)))))))",
				env, typeEnv);

		TestComplex.testClojureCompileNoCmp("(println (cons 42 \"42\"))", env, typeEnv);

		TestComplex.assertIntprtAndCompPrintSameValues(";;List is now already defined internal type, so we omit it\n"
				+ ";;(type List)\n" + "\n" + "(representation Linked List)\n"
				+ "(constructor List Linked (x (List l)) (cons x l))\n" + "(constructor List Linked () ())\n" + "\n"
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" + "\n" + "(define z (fcons 1 2))\n"
				+ "(println (fcar z))\n" + "(println (fcdr z))\n" + "\n" + "(representation Functional List)\n"
				+ "(constructor List Functional (x (List l)) (fcons x l))\n" + "(constructor List Functional () ())\n"
				+ "\n"
				+ "(define x (construct List Linked (construct Int Roman \"XLII\") (construct List Linked (construct Int String \"42\") (construct List Linked 42 (construct List Linked)))))\n"
				+ "(define y (construct List Functional (construct Int Roman \"XLII\") (construct List Functional (construct Int String \"42\") (construct List Functional 42 (construct List Functional)))))\n"
				+ "(println x)\n" + "\n" + "(define is-list-empty (extended-lambda ((List l))\n"
				+ "                        ((List:Linked) (can-deconstruct-as l ()))\n"
				+ "                        ((List:Functional) (can-deconstruct-as l ()))))\n"
				+ "                        \n" + "(println (is-list-empty x))\n" + "(println (is-list-empty y))\n"
				+ "(println (is-list-empty (construct List Linked)))\n"
				+ "(println (is-list-empty (construct List Functional)))\n"
				+ "(println (is-list-empty (let-type (A) (cdr (deconstruct x (A List:Linked))))))\n"
				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct y ((((A List:Functional) #> List:Functional)) #> List:Functional))))))\n"
				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct (construct List Functional 42 (construct List Functional)) ((((A List:Functional) #> List:Functional)) #> List:Functional))))))\n"
				+ "\n" + "(define head-list (let-type (A)\n" + "                    (extended-lambda ((List l))\n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))\n"
				+ "(println (head-list x))\n" + "(println (head-list y))\n" + "\n" + "(define tail-list (let-type (A)\n"
				+ "                    (extended-lambda ((List l)) \n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))\n"
				+ "(println (tail-list x))\n" + "(println (head-list (tail-list y)))\n" + "\n"
				+ "(define build-list-aux (lambda (i n f)\n" + "            						(if (= i n)\n"
				+ "            							(construct List Linked)\n"
				+ "            							(construct List Linked (f i) (build-list-aux (+ i 1) n f)))))\n"
				+ "(println (build-list-aux 0 5 (lambda (x) (+ x 1))))\n" + "\n"
				+ "(define build-list (lambda (n f) (build-list-aux 0 n f)))\n"
				+ "(println (build-list 5 (lambda (x) (+ x 1))))\n" + "\n" + "(define append-list (let-type (A)\n"
				+ "                    (extended-lambda ((List l) (A x))\n"
				+ "                        ((List:Linked A) (if (is-list-empty l)\n"
				+ "                            (construct List Linked x (construct List Linked))\n"
				+ "                            (construct List Linked (head-list l) (append-list (tail-list l) x))))\n"
				+ "						((List:Functional A) (if (is-list-empty l)\n"
				+ "                            (construct List Functional x (construct List Functional))\n"
				+ "                            (construct List Functional (head-list l) (append-list (tail-list l) x)))))))\n"
				+ "                            \n" + "(println (append-list x 21))\n" + "\n"
				+ "(define reverse-list (extended-lambda ((List l))\n"
				+ "                        ((List:Linked) (if (is-list-empty l) \n"
				+ "                                            (construct List Linked) \n"
				+ "                                            (append-list (reverse-list (tail-list l)) (head-list l))))\n"
				+ "                        ((List:Functional) (if (is-list-empty l)\n"
				+ "                                                (construct List Functional)\n"
				+ "                                                (append-list (reverse-list (tail-list l)) (head-list l))))))\n"
				+ "(println (reverse-list x))\n" + "(println (head-list (reverse-list y)))\n" + "");
	}

	@Test
	@DisplayName("Test Clojure TypeSymbol")
	void testClojureTypeSymbol() {

	}

	private static List<Expression> parseString(String s) throws AppendableException {
		CharStream charStream = CharStreams.fromString(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();

		return exprsContext.val.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x)))
				.collect(Collectors.toList());
	}

	private static void testInterpretString(String code, Expression expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression last = null;
		for (Expression e : TestComplex.parseString(code)) {
			@SuppressWarnings("unused")
			Pair<Type, Substitution> p = e.infer(env, typeEnv);
			last = e.interpret(env, typeEnv);
		}

		assertEquals(last, expected);
	}

	private static String compileExpressionsToClojure(List<Expression> l, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		StringBuilder s = new StringBuilder();
		Iterator<Expression> i = l.iterator();
		while (i.hasNext()) {
			s.append(i.next().toClojureCode(env, typeEnv));
			if (i.hasNext()) {
				s.append('\n');
			}
		}
		return s.toString();
	}

	private static String compileToClojure(String code, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		List<Expression> l = TestComplex.parseString(code);
		return TestComplex.compileExpressionsToClojure(l, env, typeEnv);
	}

	private static void testClojureCodeCmp(String generatedCode, String expectedCode) {
		if (generatedCode.compareTo(expectedCode) != 0) {
			fail("Clojure compilation test failed, got " + generatedCode + " expected " + expectedCode);
		}
	}

	private static void testClojureCompileExpression(Expression e, String expected, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		List<Expression> l = new LinkedList<Expression>();
		l.add(e);
		String s = TestComplex.compileExpressionsToClojure(l, env, typeEnv);
		TestComplex.testClojureCodeCmp(s, expected);
	}

	private static void testClojureCompile(String code, String expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		String s = TestComplex.compileToClojure(code, env, typeEnv);

		TestComplex.testClojureCodeCmp(s, expected);
	}

	private static void testClojureCompileRegex(String code, String regex, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		String s = TestComplex.compileToClojure(code, env, typeEnv);
		if (!s.matches(regex)) {
			fail("Clojure compilation test failed, compiling " + code + " do not match " + regex + " got "
					+ s.toString());
		}
	}

	private static void testClojureCompileNoCmp(String code, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		TestComplex.compileToClojure(code, env, typeEnv);
	}

	private static void assertIntprtAndCompPrintSameValues(String code) throws Exception {
		PrintStream stdOut = System.out;
		ByteArrayOutputStream tmp = new ByteArrayOutputStream();
		System.setOut(new PrintStream(tmp));

		Environment intpEnv = Environment.initTopLevelEnvitonment();
		TypeEnvironment intpTypeEnv = TypeEnvironment.initBasicTypes(intpEnv);
		InputStream inStream = new ByteArrayInputStream(code.getBytes());
		velka.lang.interpretation.Compiler.interpret(inStream, intpEnv, intpTypeEnv);

		String intpResult = tmp.toString();

		System.setOut(stdOut);

		TestComplex.testClojureCompileClj(code, intpResult);
	}

	private static String clojureCompilationResult(List<Expression> l, Environment env, TypeEnvironment typeEnv)
			throws Exception {
		File tempFile = File.createTempFile("velka_clojure_test", null);

		String code = velka.lang.interpretation.Compiler.compile(l, env, typeEnv);
		FileOutputStream ofs = new FileOutputStream(tempFile);
		ofs.write(ClojureCodeGenerator.writeHeaders(env, typeEnv).getBytes());
		ofs.write(code.getBytes());
		ofs.close();

		ProcessBuilder pb = new ProcessBuilder("clj", tempFile.getAbsolutePath());
		pb.inheritIO();
		pb.directory(new File("/home/schkabi/Documents/Java/TypeSystem/lib"));

		File tempOut = File.createTempFile("velka_clojure_test_out", null);

		pb.redirectOutput(tempOut);

		Process p = pb.start();
		p.waitFor();

		String result = Files.readString(tempOut.toPath());
		tempFile.delete();
		tempOut.delete();

		return result;
	}

	private static void testClojureCompileClj(String code, String expected) throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		InputStream inStream = new ByteArrayInputStream(code.getBytes());
		String result = TestComplex.clojureCompilationResult(velka.lang.interpretation.Compiler.read(inStream), env,
				typeEnv);

		assertEquals(expected, result);
	}

	private static String escapeBrackets(String s) {
		return s.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)").replaceAll("\\[", "\\\\[")
				.replaceAll("\\]", "\\\\]").replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
				.replaceAll("\\+", "\\\\+");
	}
}
