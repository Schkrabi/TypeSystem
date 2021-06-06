package velka.lang.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
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
import velka.lang.abstraction.ExtendedLambda;
import velka.lang.abstraction.Function;
import velka.lang.abstraction.Lambda;
import velka.lang.abstraction.Operators;
import velka.lang.application.AbstractionApplication;
import velka.lang.application.CanDeconstructAs;
import velka.lang.application.Construct;
import velka.lang.application.IfExpression;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeSymbol;
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
import velka.lang.langbase.JavaArrayList;
import velka.lang.langbase.JavaLinkedList;
import velka.lang.langbase.ListNative;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
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
				"(println ((lambda ((String:Native x) (Int:String y)) x) \"test\" (construct Int String \"1984\")))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(println ((extended-lambda (x y z) ((Bool:Native Int:String Int:String) (if x z y))) #f (construct Int Roman \"XLII\") 66))");
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

		TestComplex.testInterpretString("(define head-list (let-type (A C) (extended-lambda ((List l))\n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> C)) #> C))))))))",
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
	void testSpecialFormsAndApplication() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestComplex.assertIntprtAndCompPrintSameValues("(println ((lambda (x y) x) 42 21))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println ((lambda ((Int:Native x) (Int:Native y)) x) 42 21))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (if #t 42 21))");
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (if #t (construct Int Roman \"XLII\") (construct Int String \"42\")))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (cons 21 21))");
		TestComplex.testClojureCompileNoCmp("(error \"error msg\")", env, typeEnv);
		TestComplex.assertIntprtAndCompPrintSameValues("(println (and #t #f))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (or #t #f))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define answer 42)" + "(println answer)");

		TestComplex.assertIntprtAndCompPrintSameValues("(type Name2)" + "(representation Structured Name2)"
				+ "(constructor Name2 Structured ((String:Native x) (String:Native y)) (cons x y))"
				+ "(representation Unstructured Name2)" + "(constructor Name2 Unstructured ((String:Native x)) x)"
				+ "(conversion Name2:Structured Name2:Unstructured"
				+ "((Name2:Structured x)) (construct Name2 Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
				+ "(println ((lambda ((Name2:Unstructured x)) x) (construct Name2 Structured \"Jan\" \"Novak\")))"
				+ "(println ((extended-lambda ((Int x)) ((Int:Native) \"Native\") ((Int:String) \"String\")) (construct Int String \"42\")))");
	}

	@Test
	@DisplayName("Clojure Operators")
	void testClojureOperators() throws Exception {
		
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (+ 21 21))\n" + "(println (* 1 42))\n" + "(println (/ 84 2))\n" + "(println (- 63 21))\n"
						+ "(println (and #t #f))\n" + "(println (bit-and 42 1))\n" + "(println (bit-or 42 1))\n"
						+ "(println (concat \"Hello \" \"World\"))\n" + "(println (equals? 42 \"42\"))\n"
						+ "(println (< 42 42))\n" + "(println (not #t))\n" + "(println (= 42 42))\n"
						+ "(println (or #t #f))\n" + "(println (cons 42 \"42\"))\n"
						+ "(println (car (cons 42 \"42\")))\n" + "(println (cdr (cons 42 \"42\")))"
						+ "(println (shr 2 1))"
						+ "(println (shl 2 1))"
						+ "(println (bit-not 6))"
						+ "(println (bit-xor 6 5))"
						+ "(println (to-str 42))"
						+ "(println (str-split \"foo bar baz\" \" \"))"
						+ "(println (parse-int \"42\"))");
		
		File tempOut = File.createTempFile("velka_read_test", null);
        String content  = "hello world !!";       
        Files.writeString(tempOut.toPath(), content);
		TestComplex.assertIntprtAndCompPrintSameValues("(println (read-file \"" + tempOut.toPath().toString() + "\"))");
		tempOut.delete();
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
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" + "(define z (fcons 1 2))\n"
				+ "(println (fcar z))\n" + "(println (fcdr z))\n" + "\n" + "(representation Functional List)\n"
				+ "(constructor List Functional (x (List l)) (fcons x l))\n" + "(constructor List Functional () ())\n"
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
				+ "\n" + "(define head-list (let-type (A B)\n" + "                    (extended-lambda ((List l))\n"
				+ "          					((List:Linked) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked)))))\n"
				+ "          					((List:Functional) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))\n"
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
	void testClojureTypeSymbol() throws Exception {
		// (println (let-type (A) (can-unify-representations Int:Native A)))
		TestComplex
				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations,
								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
										new TypeSymbol(new TypeVariable(NameGenerator.next()))))))))));
		// (println (can-unify-representations Int:Native Int:Native))
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
				Operators.PrintlnOperator,
				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations, new Tuple(Arrays
						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative)))))))));
		// (println (can-unify-representations Int:Native Int:Roman))
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
				Operators.PrintlnOperator,
				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations, new Tuple(Arrays
						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman)))))))));
		// (println (can-unify-representations Int:Native String:Native))
		TestComplex
				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations,
								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
										new TypeSymbol(TypeAtom.TypeStringNative)))))))));
		// (println (let-type (A) (can-unify-types Int:Native A)))
		TestComplex
				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes,
								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
										new TypeSymbol(new TypeVariable(NameGenerator.next()))))))))));
		// (println (can-unify-types Int:Native Int:Native))
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
				Operators.PrintlnOperator,
				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes, new Tuple(Arrays
						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative)))))))));
		// (println (can-unify-types Int:Native Int:Roman))
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
				Operators.PrintlnOperator,
				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes, new Tuple(Arrays
						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman)))))))));
		// (println (can-unify-types Int:Native String:Native))
		TestComplex
				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes,
								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
										new TypeSymbol(TypeAtom.TypeStringNative)))))))));
	}

	@Test
	@DisplayName("Test clojure instance-of and instance-of-representation")
	void testClojureInstanceOf() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of 42 Int:Native)");
		TestComplex.assertIntprtAndCompPrintSameValues("(let-type (A) (instance-of 42 A))");
		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of 42 Int:Roman)");
		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of 42 String:Native)");

		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 Int:Native)");
		TestComplex.assertIntprtAndCompPrintSameValues("(let-type (A) (instance-of-representation 42 A))");
		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 Int:Roman)");
		TestComplex.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 String:Native)");
	}

	@Test
	@DisplayName("Test clojure is-same-type and is-same-representation")
	void testClojureIsSameType() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-type 42 42)");
		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-type 42 (construct Int String \"42\"))");
		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-type 42 \"42\")");

		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-representation 42 42)");
		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-representation 42 (construct Int String \"42\"))");
		TestComplex.assertIntprtAndCompPrintSameValues("(is-same-representation 42 \"42\")");
	}

	@Test
	@DisplayName("Test user defined ranking function")
	void testUserDefRanking() throws Exception {
		String ranking = "(lambda ((List:Native formalArgTypes) (List:Native realArgs))" + "(foldr-list-native + 0 (map2-list-native "
				+ "(lambda (x y) (if (is-same-representation x y) 0 1)) " + "formalArgTypes " + "realArgs)))";

		String realArgs = "(construct List Native 42 (construct List Native \"42\" (construct List Native #t (construct List Native))))";

		List<Expression> l = velka.lang.interpretation.Compiler.read(new ByteArrayInputStream(ranking.getBytes()));
		Lambda rankingLambda = (Lambda) l.get(0);
		l = velka.lang.interpretation.Compiler.read(new ByteArrayInputStream(realArgs.getBytes()));
		Expression realArgsExpr = l.get(0);

		// (Int:Native String:Native Bool:Native)
		Expression formalArgs1 = new LitComposite(new Tuple(new TypeSymbol(TypeAtom.TypeIntNative), // TypeSymbol
				new LitComposite(new Tuple(new TypeSymbol(TypeAtom.TypeStringNative), // TypeSymbol
						new LitComposite(new Tuple(new TypeSymbol(TypeAtom.TypeBoolNative), // TypeSymbol
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative)),
								TypeAtom.TypeListNative)),
						TypeAtom.TypeListNative)),
				TypeAtom.TypeListNative);

		Expression formalArgs2 = new LitComposite(new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntString), // TypeSymbol
				new LitComposite(new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeStringNative), // TypeSymbol
						new LitComposite(new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeBoolNative), // TypeSymbol
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
								TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative))),
				TypeAtom.TypeListNative);

		Expression formalArgs3 = new LitComposite(new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntString), // TypeSymbol
				new LitComposite(
						new Tuple(Arrays
								.asList(new TypeSymbol(new TypeAtom(TypeName.STRING, new TypeRepresentation("other"))), // TypeSymbol
										new LitComposite(
												new Tuple(Arrays.asList(
														new TypeSymbol(new TypeAtom(TypeName.BOOL,
																new TypeRepresentation("other"))), // TypeSymbol
														new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
												TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative))),
				TypeAtom.TypeListNative);

		Expression e1 = new AbstractionApplication(rankingLambda, new Tuple(Arrays.asList(formalArgs1, realArgsExpr)));

		Expression e2 = new AbstractionApplication(rankingLambda, new Tuple(Arrays.asList(formalArgs2, realArgsExpr)));

		Expression e3 = new AbstractionApplication(rankingLambda, new Tuple(Arrays.asList(formalArgs3, realArgsExpr)));

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);
		l = velka.lang.interpretation.Compiler.eval(Arrays.asList(e1), env, typeEnv);
		assertEquals(new LitInteger(0), l.get(0));

		l = velka.lang.interpretation.Compiler.eval(Arrays.asList(e2), env, typeEnv);
		assertEquals(new LitInteger(1), l.get(0));

		l = velka.lang.interpretation.Compiler.eval(Arrays.asList(e3), env, typeEnv);
		assertEquals(new LitInteger(3), l.get(0));

		TestComplex.assertIntprtAndCompPrintSameValues(
				Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator, new Tuple(Arrays.asList(e1))),
						new AbstractionApplication(Operators.PrintlnOperator, new Tuple(Arrays.asList(e2))),
						new AbstractionApplication(Operators.PrintlnOperator, new Tuple(Arrays.asList(e3)))));
	}
	
	@Test
	@DisplayName("Test Custom Ranking Function Compilation")
	void testCustomRanking() throws Exception {
		Lambda impl1 = new Lambda(
				new Tuple(new Symbol("a")),
				new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("Int Native")
				);
		Lambda impl2 = new Lambda(
				new Tuple(new Symbol("a")),
				new TypeTuple(TypeAtom.TypeIntString),
				new LitString("Int String")
				);
		Lambda impl3 = new Lambda(
				new Tuple(new Symbol("a")),
				new TypeTuple(TypeAtom.TypeIntRoman),
				new LitString("Int Roman")
				);
		
		ExtendedLambda elambda_defaultRanking = ExtendedLambda.makeExtendedLambda(Arrays.asList(impl1, impl2, impl3));
		
		Lambda ranking = (Lambda)TestComplex.parseString("(let-type (A) (lambda ((List:Native formalArgList) (List:Native realArgList) (A args)) "
																		+ "(if (instance-of-representation (head-list-native formalArgList) Int:Roman) 0 999)))").get(0);
		
		Expression rnkAppl = new AbstractionApplication(
				ranking,
				new Tuple(
						new LitComposite(
								new Tuple(
										new TypeSymbol(TypeAtom.TypeIntRoman), 
										new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative)), 
								TypeAtom.TypeListNative),
						new LitComposite(
								new Tuple(
										new TypeSymbol(TypeAtom.TypeIntNative),
										new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative)
										), 
								TypeAtom.TypeListNative),
						Expression.EMPTY_EXPRESSION
						)
				);
		
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(rnkAppl));
		
		Expression rnkAppl2 = new AbstractionApplication(
				ranking,
				new Tuple(
						new LitComposite(
								new Tuple(
										new TypeSymbol(TypeAtom.TypeIntString), 
										new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative)), 
								TypeAtom.TypeListNative),
						new LitComposite(
								new Tuple(
										new TypeSymbol(TypeAtom.TypeIntNative),
										new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative)
										), 
								TypeAtom.TypeListNative),
						Expression.EMPTY_EXPRESSION
						)
				); 
		
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(rnkAppl2));		 
		
		Lambda ranking2 = (Lambda)TestComplex.parseString("(let-type (A) (lambda ((List:Native formalArgList) (List:Native realArgList) (A args)) "
																		+ "(if (instance-of-representation (head-list-native formalArgList) Int:String) 0 999)))").get(0);
		
		ExtendedLambda elambda_customRanking = ExtendedLambda.makeExtendedLambda(Arrays.asList(impl1, impl2, impl3), ranking);
		
		Tuple args = new Tuple(new LitInteger(42));
		
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);
		
		AbstractionApplication app_defElambda_defRanking = new AbstractionApplication(Operators.PrintlnOperator, new Tuple(new AbstractionApplication(elambda_defaultRanking, args)));
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(app_defElambda_defRanking));
		
		AbstractionApplication app_defElambda_cusRanking = new AbstractionApplication(Operators.PrintlnOperator, new Tuple(new AbstractionApplication(elambda_defaultRanking, args, ranking)));
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(app_defElambda_cusRanking));
		
		AbstractionApplication app_cusElambda_defRanking = new AbstractionApplication(Operators.PrintlnOperator, new Tuple(new AbstractionApplication(elambda_customRanking, args)));
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(app_cusElambda_defRanking));
		
		AbstractionApplication app_cusElambda_cusRanking = new AbstractionApplication(Operators.PrintlnOperator, new Tuple(new AbstractionApplication(elambda_customRanking, args, ranking2)));
		TestComplex.assertIntprtAndCompPrintSameValues(Arrays.asList(app_cusElambda_cusRanking));
	}
	
	@Test
	@DisplayName("Test Clojure Headers")
	void testClojureHeaders() throws Exception {
		StringBuilder definitions = new StringBuilder();
		definitions.append(ClojureCodeGenerator.type2typeSymbolDef + "\n");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (.toString (:lang-type (meta (" + ClojureCodeGenerator.type2typeSymbolSymbol + " "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + ")))))", 
				"Int:Native");
		
		definitions.append(ClojureCodeGenerator.getTypeClojureDef + "\n");
		assertClojureFunction(
				definitions.toString(),
				"(println (.toString (" + ClojureCodeGenerator.getTypeClojureSymbol + " " + LitInteger.clojureIntToClojureLitInteger("1") + ")))",
				TypeAtom.TypeIntNative.toString());
		
		definitions.append(ClojureCodeGenerator.tuple2velkaListDef + "\n");
		
		assertClojureFunction(
				definitions.toString(), 
				"(println (" + ClojureCodeGenerator.tuple2velkaListSymbol + " [1 2 3]))",
				"[[1 [[2 [[3 [[]]]]]]]]");
		
		definitions.append(ClojureCodeGenerator.atomicConversionMapClojureDef + "\n");
		definitions.append(ClojureCodeGenerator.convertAtomClojureDef + "\n");
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntRoman.clojureTypeRepresentation() + 
						LitInteger.clojureIntToClojureLitInteger("1") + "))",
				"[[I]]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntString.clojureTypeRepresentation() + 
						LitInteger.clojureIntToClojureLitInteger("1") + "))",
				"[[1]]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntNative.clojureTypeRepresentation() + 
						LitComposite.clojureValueToClojureLiteral(LitString.clojureStringToClojureLitString("\"1\""), TypeAtom.TypeIntString) + "))",
				"[1]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntRoman.clojureTypeRepresentation() + 
						LitComposite.clojureValueToClojureLiteral(LitString.clojureStringToClojureLitString("\"1\""), TypeAtom.TypeIntString) + "))",
				"[[I]]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntNative.clojureTypeRepresentation() + 
						LitComposite.clojureValueToClojureLiteral(LitString.clojureStringToClojureLitString("\"I\""), TypeAtom.TypeIntRoman) + "))",
				"[1]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertAtomClojureSymbol + " " +  
						TypeAtom.TypeIntString.clojureTypeRepresentation() + 
						LitComposite.clojureValueToClojureLiteral(LitString.clojureStringToClojureLitString("\"I\""), TypeAtom.TypeIntRoman) + "))",
				"[[1]]");
		
		definitions.append("(declare " + ClojureCodeGenerator.convertClojureSymbol + ")\n");
		definitions.append("(declare " + ClojureCodeGenerator.convertTupleClojureSymbol + ")\n");
		definitions.append("(declare " + ClojureCodeGenerator.convertFnClojureSymbol + ")\n");
		definitions.append("(declare " + ClojureCodeGenerator.convertRepOrClojureSymbol + ")\n");
		definitions.append("(declare " + ClojureCodeGenerator.convertToRepOrClojureSymbol + ")\n");
		definitions.append(ClojureCodeGenerator.convertClojureDef + "\n");
		TestComplex.clojureCodeResult(definitions.toString());
		
		definitions.append(ClojureCodeGenerator.convertTupleClojureDef + "\n");
		TestComplex.clojureCodeResult(definitions.toString());
		
		definitions.append(ClojureCodeGenerator.convertFnClojureDef + "\n");
		TestComplex.clojureCodeResult(definitions.toString());
		
		definitions.append(ClojureCodeGenerator.convertRepOrClojureDef + "\n");
		TestComplex.clojureCodeResult(definitions.toString());
		
		definitions.append(ClojureCodeGenerator.convertToRepOrClojureDef + "\n");
		TestComplex.clojureCodeResult(definitions.toString());
		
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		Tuple t = new Tuple(new LitInteger(1), new LitComposite(new LitString("1"), TypeAtom.TypeIntString));
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.convertTupleClojureSymbol + " " +
						new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman).clojureTypeRepresentation() + " " +
						t.toClojureCode(env, typeEnv) + "))",
				"[[1] [[I]]]");
		
		Lambda l = (Lambda)(TestComplex.parseString("(lambda ((Int:String x)) 1)").get(0));
		TypeArrow lambda_to = new TypeArrow(new TypeTuple(TypeAtom.TypeIntRoman), TypeAtom.TypeIntString);
		
		Expression arg = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		assertClojureFunction(
				definitions.toString(),
				"(println (((" + ClojureCodeGenerator.convertFnClojureSymbol + " " +  
				lambda_to.clojureTypeRepresentation() + " " + l.toClojureCode(env, typeEnv) + ") nil) " + arg.toClojureCode(env, typeEnv) + "))",
				"[[1]]");
		
		definitions.append(ClojureCodeGenerator.eapplyClojureDef + "\n");
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.eapplyClojureSymbol + " " + l.toClojureCode(env, typeEnv) + " "
						+ (new Tuple(arg)).toClojureCode(env, typeEnv) + "))",
				"[1]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.eapplyClojureSymbol + " " + l.toClojureCode(env, typeEnv) + " "
						+ (new Tuple(arg)).toClojureCode(env, typeEnv) + "))",
				"[1]");
		
		Tuple typeSymbolTuple = new Tuple(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative));
		Tuple typeSymbolTuple_diff1 = new Tuple(new TypeSymbol(TypeAtom.TypeIntString), new TypeSymbol(TypeAtom.TypeIntNative));
		Tuple typeSymbolTuple_diff2 = new Tuple(new TypeSymbol(TypeAtom.TypeIntString), new TypeSymbol(TypeAtom.TypeIntRoman));
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + AbstractionApplication.defaultRanking.clojureDef() + " nil) ("
						+ ClojureCodeGenerator.tuple2velkaListSymbol + " " + typeSymbolTuple.toClojureCode(env, typeEnv)
						+ ") (" + ClojureCodeGenerator.tuple2velkaListSymbol + " "
						+ typeSymbolTuple.toClojureCode(env, typeEnv) + ") nil))",
				"[0]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + AbstractionApplication.defaultRanking.clojureDef() + " nil) ("
						+ ClojureCodeGenerator.tuple2velkaListSymbol + " " + typeSymbolTuple.toClojureCode(env, typeEnv)
						+ ") (" + ClojureCodeGenerator.tuple2velkaListSymbol + " "
						+ typeSymbolTuple_diff1.toClojureCode(env, typeEnv) + ") nil))",
				"[1]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + AbstractionApplication.defaultRanking.clojureDef() + " nil) ("
						+ ClojureCodeGenerator.tuple2velkaListSymbol + " " + typeSymbolTuple.toClojureCode(env, typeEnv)
						+ ") (" + ClojureCodeGenerator.tuple2velkaListSymbol + " "
						+ typeSymbolTuple_diff2.toClojureCode(env, typeEnv) + ") nil))",
				"[2]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (" + ClojureCodeGenerator.eapplyClojureSymbol + " " + l.toClojureCode(env, typeEnv) + " "
						+ (new Tuple(arg)).toClojureCode(env, typeEnv) + " " + AbstractionApplication.defaultRanking.toClojureCode(env, typeEnv) + "))",
				"[1]");
		
		definitions.append(ClojureCodeGenerator.selectImplementationClojureDef);
		
		Tuple selectArgs1 = new Tuple(new LitInteger(1), new LitInteger(2));
		Expression impl1 = TestComplex.parseString("(lambda ((Int:Native x) (Int:Native y)) \"impl1\")").get(0);
		Tuple selectArgs2 = new Tuple(new LitComposite(new LitString("1"), TypeAtom.TypeIntString), new LitInteger(2));
		Expression impl2 = TestComplex.parseString("(lambda ((Int:String x) (Int:Native y)) \"impl2\")").get(0);
		Tuple selectArgs3 = new Tuple(new LitComposite(new LitString("1"), TypeAtom.TypeIntString), new LitComposite(new LitString("2"), TypeAtom.TypeIntString));
		Expression impl3 = TestComplex.parseString("(lambda ((Int:String x) (Int:String y)) \"impl3\")").get(0);
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + ClojureCodeGenerator.selectImplementationClojureSymbol + " "
						+ AbstractionApplication.defaultRanking.toClojureCode(env, typeEnv) + " "
						+ selectArgs1.toClojureCode(env, typeEnv) + " " + "#{" + "(" + impl1.toClojureCode(env, typeEnv)
						+ " nil)" + " (" + impl2.toClojureCode(env, typeEnv) + " nil)" + " ("
						+ impl3.toClojureCode(env, typeEnv) + " nil)" + "}) nil nil))",
				"[impl1]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + ClojureCodeGenerator.selectImplementationClojureSymbol + " "
						+ AbstractionApplication.defaultRanking.toClojureCode(env, typeEnv) + " "
						+ selectArgs2.toClojureCode(env, typeEnv) + " " + "#{" + "(" + impl1.toClojureCode(env, typeEnv)
						+ " nil)" + " (" + impl2.toClojureCode(env, typeEnv) + " nil)" + " ("
						+ impl3.toClojureCode(env, typeEnv) + " nil)" + "}) nil nil))",
				"[impl2]");
		
		assertClojureFunction(
				definitions.toString(),
				"(println ((" + ClojureCodeGenerator.selectImplementationClojureSymbol + " "
						+ AbstractionApplication.defaultRanking.toClojureCode(env, typeEnv) + " "
						+ selectArgs3.toClojureCode(env, typeEnv) + " " + "#{" + "(" + impl1.toClojureCode(env, typeEnv)
						+ " nil)" + " (" + impl2.toClojureCode(env, typeEnv) + " nil)" + " ("
						+ impl3.toClojureCode(env, typeEnv) + " nil)" + "}) nil nil))",
				"[impl3]");
		
		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(
				new Lambda(
						new Tuple(new Symbol("x")),
						new TypeTuple(TypeAtom.TypeIntNative),
						new LitString("a")),
				new Lambda(
						new Tuple(new Symbol("x")),
						new TypeTuple(TypeAtom.TypeIntString),
						new LitString("b")));
		
		TestComplex.clojureCodeResult(definitions.toString() + 
				"(println (" + ClojureCodeGenerator.convertRepOrClojureSymbol + " "
				+ new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeStringNative).clojureTypeRepresentation() + " "
				+ elambda.toClojureCode(env, typeEnv) + "))");
		
		TestComplex.assertClojureFunction(definitions.toString(),
				 	"(println (" + ClojureCodeGenerator.convertToRepOrClojureSymbol + " " + 
				 			RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntNative, TypeAtom.TypeIntString).clojureTypeRepresentation() + " " +
				 			new LitInteger(42).toClojureCode(env, typeEnv) + "))",
				 	"[42]");
	}
	
	@Test
	@DisplayName("Test List Native Clojure")
	void testListNativeClojure() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(println (construct List Native))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (construct List Native 42 (construct List Native)))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List Native)))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List Native 42 (construct List Native))))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (head-list-native (construct List Native 42 (construct List Native))))");
		//TestComplex.assertIntprtAndCompPrintSameValues("(println (head-list-native (construct List Native)))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (tail-list-native (construct List Native 42 (construct List Native))))");
		//TestComplex.assertIntprtAndCompPrintSameValues("(println (tail-list-native (construct List Native)))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (map-list-native (lambda (x) (+ x 1)) (construct List Native 42 (construct List Native))))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (map2-list-native + (construct List Native 21 (construct List Native 21 (construct List Native))) (construct List Native 21 (construct List Native 21 (construct List Native)))))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (foldl-list-native + 0 (construct List Native 1 (construct List Native 2 (construct List Native)))))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (foldr-list-native + 0 (construct List Native 1 (construct List Native 2 (construct List Native)))))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(println (" + ListNative.headListNativeSymbol + "(" + ListNative.addToEndSymbol + " (construct List Native 21 (construct List Native)) 42)))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l (convert List:Native List:JavaArray (construct List Native 42 (construct List Native 21 (construct List Native)))))"
				+ "(println (" + JavaArrayList.getSymbol + " l 0))");
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l (convert List:Native List:JavaLinked (construct List Native 42 (construct List Native 21 (construct List Native)))))"
				+ "(println (" + JavaLinkedList.getSymbol + " l 0))");
	}
	
	@Test
	@DisplayName("Test Java Array List Clojure")
	void testJavaArrayListClojure() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(construct List JavaArray)");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (" + JavaArrayList.addToEndSymbol.toString() + " (construct List JavaArray) 42))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (" + JavaArrayList.addToIndexSymbol.toString() + " (construct List JavaArray) 0 42))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaArrayList.addAllSymbol + " l1 l2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)"
				+ "(println (" + JavaArrayList.containsSymbol + " l1 42))"
				+ "(println (" + JavaArrayList.containsSymbol + " l1 84))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaArrayList.containsAllSymbol + " l1 l2))"
				+ "(println (" + JavaArrayList.containsAllSymbol + " l2 l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)"
				+ "(println (" + JavaArrayList.getSymbol + " l1 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.indexOfSymbol + " l1 1))"
				+ "(println (" + JavaArrayList.indexOfSymbol + " l1 42))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (" + JavaArrayList.isEmptySymbol + " (construct List JavaArray)))");
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.isEmptySymbol + " l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.lastIndexOfSymbol + " l1 1))"
				+ "(println (" + JavaArrayList.lastIndexOfSymbol + " l1 42))");
		/*
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.removeSymbol + " l1 0))");
				*/
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.removeSymbol + " l1 2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaArrayList.removeAllSymbol + " l1 l2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaArrayList.retainAllSymbol + " l1 l2))"
				+ "(println (" + JavaArrayList.retainAllSymbol + " l2 l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.setSymbol + " l1 0 2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaArrayList.sizeSymbol + " l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 84)" 
				+ "(define l2 (" + JavaArrayList.sublistSymbol + " l1 0 1))"
				+ "(println (" + JavaArrayList.getSymbol + " l2 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 84)" 
				+ "(define l2 (" + JavaArrayList.mapSymbol + " l1 (lambda (x) (* x 2))))"
				+ "(println (" + JavaArrayList.getSymbol + " l2 0))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 3)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 2)"
				+ "(" + JavaArrayList.addToEndSymbol + " l2 1)"
				+ "(define l3 (" + JavaArrayList.map2Symbol + " l1 l2 (lambda (x y) (+ x y))))"
				+ "(println (" + JavaArrayList.getSymbol + " l3 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 21)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 42)" 
				+ "(println (" + JavaArrayList.foldlSymbol + " + 0 l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 4)"
				+ "(println (" + JavaArrayList.foldrSymbol + " / 16 l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l 21)\n"
				+ "(println (" + JavaLinkedList.getSymbol + " (convert List:JavaArray List:JavaLinked l) 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol + " l 21)\n"
				+ "(println (car (let-type (A) (deconstruct (convert List:JavaArray List:Native l) (A List:Native)))))");
	}
	
	@Test
	@DisplayName("Test Java Linked List Clojure")
	void testJavaLinkedListClojure() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(construct List JavaLinked)");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (" + JavaLinkedList.addToEndSymbol.toString() + " (construct List JavaLinked) 42))");
		TestComplex.assertIntprtAndCompPrintSameValues("(println (" + JavaLinkedList.addToIndexSymbol.toString() + " (construct List JavaLinked) 0 42))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaLinkedList.addAllSymbol + " l1 l2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)"
				+ "(println (" + JavaLinkedList.containsSymbol + " l1 42))"
				+ "(println (" + JavaLinkedList.containsSymbol + " l1 84))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaLinkedList.containsAllSymbol + " l1 l2))"
				+ "(println (" + JavaLinkedList.containsAllSymbol + " l2 l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)"
				+ "(println (" + JavaLinkedList.getSymbol + " l1 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.indexOfSymbol + " l1 1))"
				+ "(println (" + JavaLinkedList.indexOfSymbol + " l1 42))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(println (" + JavaLinkedList.isEmptySymbol + " (construct List JavaLinked)))");
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.isEmptySymbol + " l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.lastIndexOfSymbol + " l1 1))"
				+ "(println (" + JavaLinkedList.lastIndexOfSymbol + " l1 42))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.removeSymbol + " l1 2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaLinkedList.removeAllSymbol + " l1 l2))"
				+ "(println (" + JavaLinkedList.removeAllSymbol + " l2 l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 2)"
				+ "(println (" + JavaLinkedList.retainAllSymbol + " l1 l2))"
				+ "(println (" + JavaLinkedList.retainAllSymbol + " l2 l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.setSymbol + " l1 0 2))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(println (" + JavaLinkedList.sizeSymbol + " l1))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 84)" 
				+ "(define l2 (" + JavaLinkedList.sublistSymbol + " l1 0 1))"
				+ "(println (" + JavaLinkedList.getSymbol + " l2 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 84)" 
				+ "(define l2 (" + JavaLinkedList.mapSymbol + " l1 (lambda (x) (* x 2))))"
				+ "(println (" + JavaLinkedList.getSymbol + " l2 0))");
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 3)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 2)"
				+ "(" + JavaLinkedList.addToEndSymbol + " l2 1)"
				+ "(define l3 (" + JavaLinkedList.map2Symbol + " l1 l2 (lambda (x y) (+ x y))))"
				+ "(println (" + JavaLinkedList.getSymbol + " l3 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 21)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 42)" 
				+ "(println (" + JavaLinkedList.foldlSymbol + " + 0 l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 2)" 
				+ "(" + JavaLinkedList.addToEndSymbol + " l1 4)"
				+ "(println (" + JavaLinkedList.foldrSymbol + " / 16 l1))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l 21)\n"
				+ "(println (" + JavaArrayList.getSymbol + " (convert List:JavaLinked List:JavaArray l) 0))");
		
		TestComplex.assertIntprtAndCompPrintSameValues("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol + " l 21)\n"
				+ "(println (car (let-type (A) (deconstruct (convert List:JavaLinked List:Native l) (A List:Native)))))");
	}
	
	@Test
	@DisplayName("Test logging")
	void testLogging() throws Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		TestComplex.testClojureCompileNoCmp("(timestamp)", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(init-logger \"test-clj-log\")", env, typeEnv);
		TestComplex.testClojureCompileNoCmp("(log \"test-clj\")", env, typeEnv);
		TestComplex.assertIntprtAndCompPrintSameValues("(init-logger \"test-clj-log\")\n" + "(log \"test-clj\")");
	}
	
	@Test
	@DisplayName("Test Get")
	void testGet() throws Exception {
		TestComplex.assertIntprtAndCompPrintSameValues("(println (get (cons 42 \"foo\") 0))");
	}
	
	private static void assertClojureFunction(String definitions, String testCase, String expectedResult) throws IOException, InterruptedException {
		//Test that definitions are sound
		TestComplex.clojureCodeResult(definitions);
		//Test the testcase
		String result = TestComplex.clojureCodeResult(definitions + "\n" + testCase);
		assertEquals(expectedResult + "\n", result);
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

	@SuppressWarnings("unused")
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
		InputStream in = new ByteArrayInputStream(code.getBytes());
		List<Expression> exprs = velka.lang.interpretation.Compiler.read(in);

		TestComplex.assertIntprtAndCompPrintSameValues(exprs);
	}

	private static void assertIntprtAndCompPrintSameValues(List<Expression> in) throws Exception {
		Environment intpEnv = Environment.initTopLevelEnvitonment();
		TypeEnvironment intpTypeEnv = TypeEnvironment.initBasicTypes(intpEnv);
		ListNative.initializeInEnvironment(intpEnv, intpTypeEnv);

		String interpretationPrintOut = TestComplex.interpretationPrint(in, intpEnv, intpTypeEnv);

		Environment cmplEnv = Environment.initTopLevelEnvitonment();
		TypeEnvironment cmplTypeEnv = TypeEnvironment.initBasicTypes(cmplEnv);
		ListNative.initializeInEnvironment(cmplEnv, cmplTypeEnv);

		String compilationPrintOut = TestComplex.clojureCompilationResult(in, cmplEnv, cmplTypeEnv);

		assertEquals(interpretationPrintOut, compilationPrintOut);
	}

	private static String interpretationPrint(List<Expression> in, Environment env, TypeEnvironment typeEnv)
			throws Exception {
		PrintStream stdOut = System.out;
		ByteArrayOutputStream tmp = new ByteArrayOutputStream();
		System.setOut(new PrintStream(tmp));

		velka.lang.interpretation.Compiler.eval(in, env, typeEnv);

		String result = tmp.toString();
		System.setOut(stdOut);

		return result;
	}
	
	private static String clojureCodeResult(String code) throws IOException, InterruptedException {
		File tempFile = File.createTempFile("velka_clojure_test", null);
		FileOutputStream ofs = new FileOutputStream(tempFile);
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

	private static String clojureCompilationResult(List<Expression> l, Environment env, TypeEnvironment typeEnv)
			throws Exception {

		StringBuilder code = new StringBuilder();
		code.append(ClojureCodeGenerator.writeHeaders(env, typeEnv));
		code.append(velka.lang.interpretation.Compiler.compile(l, env, typeEnv));
		
		String result = TestComplex.clojureCodeResult(code.toString());
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

	@SuppressWarnings("unused")
	private static String escapeBrackets(String s) {
		return s.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)").replaceAll("\\[", "\\\\[")
				.replaceAll("\\]", "\\\\]").replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
				.replaceAll("\\+", "\\\\+");
	}
}
