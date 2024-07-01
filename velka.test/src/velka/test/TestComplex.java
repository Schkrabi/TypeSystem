package velka.test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.clojure.ClojureCodeGenerator;
import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaBitSet;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.CostAggregation;

class TestComplex extends VelkaTest {
	
	@Test
	@DisplayName("Test Recursion")
	void testRecursion() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretedStringEquals("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(fact 5)",
				new LitInteger(120), env);

		this.assertCompile("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))", env);

		this.assertIntprtAndCompPrintSameValues(
				"(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(println (fact 5))");
	}

	@Test
	@DisplayName("Test Basic Extended lambda")
	void testExtemdedLambda() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretedStringEquals(
				"(constructor Name:Unstructured ((String:Native x)) x)" 
				+ "(constructor Name:Structured ((String:Native x) (String:Native y)) (tuple x y))"
				+ "((extend (extend (extended-lambda (Name)) "
					+ "(lambda ((Name:Unstructured x)) \"unstructured\")) "
					+ "(lambda ((Name:Structured x)) \"structured\")) "
						+ "(construct Name:Unstructured \"Jan Novak\"))",
				new LitString("unstructured"), env);

		this.assertInterpretedStringEquals(
				"((extend (extend (extended-lambda (Name)) "
					+ "(lambda ((Name:Unstructured x)) \"unstructured\")) "
					+ "(lambda ((Name:Structured x)) \"structured\")) "
						+ "(construct Name:Structured \"Jan\" \"Novak\"))",
				new LitString("structured"), env);

		this.assertInterpretedStringEquals(
				"(conversion Name:Structured Name:Unstructured (x) (construct Name:Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
						+ "((lambda ((Name:Unstructured x)) x) (construct Name:Structured \"Jan\" \"Novak\"))",
				new LitComposite(new LitString("JanNovak"),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured"))),
				env);

		this.assertIntprtAndCompPrintSameValues(
				"(println ((lambda ((String:Native x) (Int:String y)) x) \"test\" (construct Int:String \"1984\")))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(println "
				+ "((extend (extended-lambda (Bool Int Int)) "
						+ "(lambda ((Bool:Native x) (Int:String y) (Int:String z)) (if x z y))) "
						+ "#f (construct Int:Roman \"XLII\") 66))");
	}

	@Test
	@DisplayName("Test User List Interpretation")
	void testComplexList() throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		

		TypeName listTypeName = new TypeName("List");

		final TypeAtom linkedList = new TypeAtom(listTypeName, new TypeRepresentation("Linked"));
		final LitComposite emptyList = new LitComposite(Expression.EMPTY_EXPRESSION, linkedList);

		this.assertInterpretedStringEquals(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))"
						+ "(constructor List:Linked () nil)"
						+ "(construct List:Linked 1 (construct List:Linked 2 (construct List:Linked)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env);

		this.assertInterpretedStringEquals("(define fcons (lambda (x y) (lambda (f) (f x y))))"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))" + "(define fcdr (lambda (p) (p (lambda (x y) y))))"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))"
				+ "(constructor List:Functional () nil)", Expression.EMPTY_EXPRESSION, env);

		final LitComposite xlii = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		final LitComposite fortyTwoStr = new LitComposite(new LitString("42"), TypeAtom.TypeIntString);
		final LitInteger fortyTwo = new LitInteger(42);

		this.assertInterpretedStringEquals(
				"(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))",
				Expression.EMPTY_EXPRESSION, env);

		this.assertInterpretedStringEquals(
				"(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))",
				Expression.EMPTY_EXPRESSION, env);
		this.assertInterpretedStringEquals(
					"(define is-list-empty (extended-lambda (List)))"
				+	"(define is-list-empty (extend is-list-empty (lambda ((List:Linked l)) (can-deconstruct-as l ()))))"
				+ 	"(define is-list-empty (extend is-list-empty (lambda ((List:Functional l)) (can-deconstruct-as l ()))))",
				Expression.EMPTY_EXPRESSION, env);
		this.assertInterpretedStringEquals("(is-list-empty x)", LitBoolean.FALSE, env);
		this.assertInterpretedStringEquals("(is-list-empty y)", LitBoolean.FALSE, env);
		this.assertInterpretedStringEquals("(is-list-empty (construct List:Linked))", LitBoolean.TRUE, env);
		this.assertInterpretedStringEquals("(is-list-empty (construct List:Functional))", LitBoolean.TRUE, env);

		this.assertInterpretedStringEquals(
					"(define head-list (extended-lambda (List)))"
				+	"(define head-list (let-type (A C) (extend head-list (lambda ((List:Linked l)) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (car (deconstruct l (A List:Linked))))))))"
				+	"(define head-list (let-type (A C) (extend head-list (lambda ((List:Functional l)) (if (is-list-empty l) (error \"Cannot make head of empty list!\") (fcar (deconstruct l ((((A List:Functional) #> C)) #> C))))))))",
				Expression.EMPTY_EXPRESSION, env);
		this.assertInterpretedStringEquals("(head-list x)", xlii, env);
		this.assertInterpretedStringEquals("(head-list y)", xlii, env);

		this.assertInterpretedStringEquals(
					"(define tail-list (extended-lambda (List)))"
				+	"(define tail-list (let-type (A) (extend tail-list (lambda ((List:Linked l)) (if (is-list-empty l) (error \"Cannot make tail of empty list!\") (cdr (deconstruct l (A List:Linked))))))))"
				+	"(define tail-list (let-type (A) " 
					+ "(extend tail-list " 
						+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
							+ "(error \"Cannot make tail of empty list!\") " 
							+ "(fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))",
				Expression.EMPTY_EXPRESSION, env);

		this.assertInterpretedStringEquals("(tail-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwoStr,
								new LitComposite(new Tuple(Arrays.asList(fortyTwo, emptyList)), linkedList))),
						linkedList),
				env);
		this.assertInterpretedStringEquals("(head-list (tail-list y))", fortyTwoStr, env);

		this.assertInterpretedStringEquals(
				"(define build-list-aux (lambda (i n f) " + "(if (= i n) " + "(construct List:Linked)"
						+ "(construct List:Linked (f i) (build-list-aux (+ i 1) n f)))))"
						+ "(build-list-aux 0 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env);

		this.assertInterpretedStringEquals(
				"(define build-list (lambda (n f) (build-list-aux 0 n f)))" + "(build-list 2 (lambda (x) (+ x 1)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(new Tuple(Arrays.asList(new LitInteger(2), emptyList)), linkedList))),
						linkedList),
				env);

		this.assertInterpretedStringEquals(
					"(define append-list (let-type (A) (extended-lambda (List A))))"
				+	"(define append-list (let-type (A) (extend append-list (lambda ((List:Linked l) (A x)) "
							+	"(if (is-list-empty l) "
								+	"(construct List:Linked x (construct List:Linked)) "
								+	"(construct List:Linked (head-list l) (append-list (tail-list l) x)))))))"
				+	"(define append-list (let-type (A) (extend append-list (lambda ((List:Functional l) (A x)) "
							+	"(if (is-list-empty l) "
								+	"(construct List:Functional x (construct List:Functional)) "
								+	"(construct List:Functional (head-list l) (append-list (tail-list l) x)))))))",
				Expression.EMPTY_EXPRESSION, env);

		this
				.assertInterpretedStringEquals("(append-list x 21)",
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
						env);

//		this.assertInterpretedStringEquals(
//					"(extend (extend (extended-lambda (List)) "
//						+	"(lambda ((List:Linked l)) (if (can-deconstruct-as l ()) "
//							+	"(construct List:Linked) "
//							+	"(append-list (reverse-list (tail-list l)) (head-list l))))) "
//						+	"(lambda ((List:Functional l)) (if (can-deconstruct-as l ()) "
//							+	"(construct List:Functional) "
//							+	"(append-list (reverse-list (tail-list l)) (head-list l)))))",
//				Extend.makeExtendedFunction(List.of(
//						new Function(new TypeTuple(linkedList),
//								new Tuple(new Symbol("l")),
//								new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
//										new Construct(linkedList,
//												Tuple.EMPTY_TUPLE),
//										new AbstractionApplication(new Symbol(
//												"append-list"),
//												new Tuple(
//														new AbstractionApplication(
//																		new Symbol("reverse-list"),
//																		new Tuple(new AbstractionApplication(
//																						new Symbol("tail-list"),
//																						new Tuple(Arrays.asList(
//																								new Symbol("l")))))),
//																new AbstractionApplication(new Symbol("head-list"),
//																		new Tuple(new Symbol("l")))))),
//								env),
//						new Function(new TypeTuple(Arrays.asList(typeListFuntionalAtom)),
//								new Tuple(new Symbol("l")),
//								new IfExpression(new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE),
//										new Construct(typeListFuntionalAtom, Tuple.EMPTY_TUPLE),
//										new AbstractionApplication(
//												new Symbol("append-list"),
//												new Tuple(
//													new AbstractionApplication(
//															new Symbol("reverse-list"),
//															new Tuple(new AbstractionApplication(
//																			new Symbol("tail-list"),
//																			new Tuple(
//																					new Symbol("l"))))),
//													new AbstractionApplication(new Symbol("head-list"),
//															new Tuple(new Symbol("l")))))),
//								env)),
//						env),
//				env);

		this.assertInterpretedStringEquals("(define reverse-list (lambda ((List l)) "
						+ "(if (is-list-empty l) " + "(construct List:Linked) "
						+ "(append-list (reverse-list (tail-list l)) (head-list l)))))" + "(reverse-list x)",
				new LitComposite(
						new Tuple(Arrays.asList(fortyTwo,
								new LitComposite(new Tuple(Arrays.asList(fortyTwoStr,
										new LitComposite(new Tuple(Arrays.asList(xlii, emptyList)), linkedList))),
										linkedList))),
						linkedList),
				env);
	}

	@Test
	@DisplayName("Test Complex Types")
	void testComplexTypes() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretedStringEquals("((lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21)) +)",
				new LitInteger(42), env);
		this.assertInterpretedStringEquals(
				"((extend (extend (extended-lambda (((Int Int) #> Int))) "
					+ "(lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21))) "
					+ "(lambda ((((Int:String Int:String) #> Int:String) f)) (f (construct Int:String \"21\") (construct Int:String \"21\")))) "
					+ "+)",
				new LitInteger(42), env);
		this.assertInterpretedStringEquals(
				"((extend (extend (extended-lambda (((Int Int) #> Int))) "
				+ "(lambda ((((Int:Native Int:Native) #> Int:Native) f)) (f 21 21))) "
				+ "(lambda ((((Int:String Int:String) #> Int:String) f)) (f (construct Int:String \"21\") (construct Int:String \"21\")))) "
				+ "(lambda ((Int:String x) (Int:String y)) (construct Int:String (concat (deconstruct x String:Native) (deconstruct y String:Native)))))",
				new LitComposite(new LitString("2121"), TypeAtom.TypeIntString), env);
		this.assertInterpretedStringEquals(
				"(let-type (A B) ((lambda ((A x) (B y)) (tuple x y)) 42 (construct Int:String  \"42\")))",
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				env);

		this.assertIntprtAndCompPrintSameValues(
				"(constructor Name:Unstructured ((String:Native x)) x) " 
				+ "(constructor Name:Structured ((String:Native x) (String:Native y)) (tuple x y)) "
				+ "(println "
					+ "((extend (extended-lambda (Name)) "
						+ "(lambda ((Name:Unstructured x)) \"unstructured\"))"
					+ "(construct Name:Unstructured \"Jan Novak\")))"
				+ "(println "
					+ "((extend (extended-lambda (Name)) "
						+ "(lambda ((Name:Structured x)) \"structured\")) "
					+ "(construct Name:Structured \"Jan\" \"Novak\")))"
				+ "(conversion Name:Structured Name:Unstructured (x) (construct Name:Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))\n"
				+ "(println ((lambda ((Name:Unstructured x)) x) (construct Name:Structured \"Jan\" \"Novak\")))");
	}

	@Test
	@DisplayName("Clojure Literals")
	void testClojureLiterals() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertIntprtAndCompPrintSameValues("(println 0)");
		this.assertIntprtAndCompPrintSameValues("(println 3.141521)");
		this.assertIntprtAndCompPrintSameValues("(println #t)");
		this.assertIntprtAndCompPrintSameValues("(println #f)");
		this.assertIntprtAndCompPrintSameValues("(println \"Hello World\")");
		this.assertIntprtAndCompPrintSameValues("(println (construct Int:Roman \"XLII\"))");

		this.assertCompiledCodeEquals("variable", "variable", env);
	}

	@Test
	@DisplayName("Clojure Special Forms and Applications")
	void testSpecialFormsAndApplication() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertIntprtAndCompPrintSameValues("(println ((lambda (x y) x) 42 21))");
		this.assertIntprtAndCompPrintSameValues("(println ((lambda ((Int:Native x) (Int:Native y)) x) 42 21))");
		this.assertIntprtAndCompPrintSameValues("(println (if #t 42 21))");
		this.assertIntprtAndCompPrintSameValues(
				"(println (if #t (construct Int:Roman \"XLII\") (construct Int:String \"42\")))");
		this.assertIntprtAndCompPrintSameValues("(println (tuple 21 21))");
		this.assertCompile("(error \"error msg\")", env);
		this.assertIntprtAndCompPrintSameValues("(println (and #t #f))");
		this.assertIntprtAndCompPrintSameValues("(println (or #t #f))");
		this.assertIntprtAndCompPrintSameValues("(define answer 42)" + "(println answer)");

		this.assertIntprtAndCompPrintSameValues(
				"(constructor Name2:Structured ((String:Native x) (String:Native y)) (tuple x y))"
				+ "(constructor Name2:Unstructured ((String:Native x)) x)"
				+ "(conversion Name2:Structured Name2:Unstructured"
				+ "(x) (construct Name2:Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
				+ "(println ((lambda ((Name2:Unstructured x)) x) (construct Name2:Structured \"Jan\" \"Novak\")))"
				+ "(println "
					+"((extend (extend (extended-lambda (Int)) "
						+ "(lambda ((Int:Native x)) \"Native\")) "
						+ "(lambda ((Int:String x)) \"String\")) "
					+ "(construct Int:String \"42\")))");
	}

	@Test
	@DisplayName("Clojure Operators")
	void testClojureOperators() throws Exception {
		
		
		this.assertIntprtAndCompPrintSameValues("(println (+ 21 21))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (* 1 42))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (/ 84 2))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (- 63 21))\n");
		this.assertIntprtAndCompPrintSameValues("(println (and #t #f))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (bit-and 42 1))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (bit-or 42 1))\n");
		this.assertIntprtAndCompPrintSameValues("(println (concat \"Hello \" \"World\"))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (equals? 42 \"42\"))\n");
		this.assertIntprtAndCompPrintSameValues("(println (< 42 42))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (not #t))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (= 42 42))\n");
		this.assertIntprtAndCompPrintSameValues("(println (or #t #f))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (tuple 42 \"42\"))\n");
		this.assertIntprtAndCompPrintSameValues("(println (car (tuple 42 \"42\")))\n"); 
		this.assertIntprtAndCompPrintSameValues("(println (cdr (tuple 42 \"42\")))");
		this.assertIntprtAndCompPrintSameValues("(println (shr 2 1))");
		this.assertIntprtAndCompPrintSameValues("(println (shl 2 1))");
		this.assertIntprtAndCompPrintSameValues("(println (ushr 2 1))");
		this.assertIntprtAndCompPrintSameValues("(println (ushr -1 10))");
		this.assertIntprtAndCompPrintSameValues("(println (bit-not 6))");
		this.assertIntprtAndCompPrintSameValues("(println (bit-xor 6 5))");
		this.assertIntprtAndCompPrintSameValues("(println (to-str 42))");
		this.assertIntprtAndCompPrintSameValues("(println (str-split \"foo bar baz\" \" \"))");
		this.assertIntprtAndCompPrintSameValues("(println (parse-int \"42\"))");
		this.assertIntprtAndCompPrintSameValues("(println (ddiv 1.5 0.5))");
		this.assertIntprtAndCompPrintSameValues("(println (floor 3.141521))");
		this.assertIntprtAndCompPrintSameValues("(println (int-to-double 42))");
		this.assertIntprtAndCompPrintSameValues("(println (dadd 3.14 3.14))");
		this.assertIntprtAndCompPrintSameValues("(println (dlt 3.14 6.28))");
		this.assertIntprtAndCompPrintSameValues("(println (dlt 3.14 3.14))");
		
		File tempOut = File.createTempFile("velka_read_test", null);
        String content  = "hello world !!";       
        Files.writeString(tempOut.toPath(), content);
        this.assertIntprtAndCompPrintSameValues("(println (read-file \"" + this.pathToStr(tempOut.toPath()) + "\"))");
		tempOut.delete();
		
		this.assertIntprtAndCompPrintSameValues("(println (mod 5 3))");
		this.assertIntprtAndCompPrintSameValues("(println (= (timestamp) 0))");
		this.assertIntprtAndCompPrintSameValues(
				"(println (conversion-cost (lambda (x) \"foo\") (tuple (construct Int:Roman \"IV\"))))");
		this.assertIntprtAndCompPrintSameValues(
				"(println (conversion-cost (lambda (x) \"foo\") (tuple 42)))");
	}

	@Test
	@DisplayName("Clojure Conversions")
	void testClojureConversions() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertCompile("(IntNative2IntRoman 42)", env);
		this.assertCompile("(IntNative2IntString 42)", env);
		this.assertCompile("(IntRoman2IntNative (Int:Roman \"XLII\"))", env);
		this.assertCompile("(IntRoman2IntString (Int:Roman \"XLII\"))", env);
		this.assertCompile("(IntString2IntNative (Int:String \"42\"))", env);
		this.assertCompile("(IntString2IntRoman (Int:String \"42\"))", env);

		this.assertCompile(
				"((extend (extended-lambda (Bool Int Int)) "
					+ "(lambda ((Bool:Native x) (Int:String y) (Int:String z)) "
						+ "(if x z y))) "
					+ "#f (Int:Roman \"XLII\") 66)",
				env);

		this.assertIntprtAndCompPrintSameValues(
				"(println (convert Int:Native Int:String 42))\n" + "(println (convert Int:Native Int:Roman 42))\n"
						+ "(println (convert Int:String Int:Native (construct Int:String \"42\")))\n"
						+ "(println (convert Int:String Int:Roman (construct Int:String \"42\")))\n"
						+ "(println (convert Int:Roman Int:Native (construct Int:Roman \"XLII\")))\n"
						+ "(println (convert Int:Roman Int:String (construct Int:Roman \"XLII\")))");
	}

	@Test
	@DisplayName("Clojure List")
	void testListClojure() throws Exception {		
		this.assertIntprtAndCompPrintSameValues(
				"(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(println (fcar z))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(println (fcdr z))" );

		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n" 
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n"
				+ "(println x)\n");
		
		this.assertIntprtAndCompPrintSameValues(
				"(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(let-type (A B C) (println (fcar (deconstruct y ((((A) #> B)) #> C)))))\n");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n" 
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(define is-list-empty "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
					+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
				+ "(println (is-list-empty x))" 
				+ "(println (is-list-empty y))"
				+ "(println (is-list-empty (construct List:Linked)))\n"
				+ "(println (is-list-empty (construct List:Functional)))\n"
				+ "(println (is-list-empty (let-type (A) (cdr (deconstruct x (A List:Linked))))))\n"
				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct y ((((A List:Functional) #> List:Functional)) #> List:Functional))))))\n"
				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct (construct List:Functional 42 (construct List:Functional)) ((((A List:Functional) #> List:Functional)) #> List:Functional))))))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n" 
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(define is-list-empty "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
					+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
				+ "(define head-list (let-type (A B) "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(car (deconstruct l (A List:Linked)))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))"
				+ "(println (head-list x))" 
				+ "(println (head-list y))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n" 
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(define is-list-empty "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
					+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
				+ "(define head-list (let-type (A B) "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(car (deconstruct l (A List:Linked)))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))"				
				+ "(define build-list-aux (lambda (i n f) " 
				+ "(if (= i n) "
					+ "(construct List:Linked) "
					+ "(construct List:Linked (f i) (build-list-aux (+ i 1) n f))))) "
				+ "(println (build-list-aux 0 5 (lambda (x) (+ x 1)))) "
				+ "(define build-list-test (lambda (n f) (build-list-aux 0 n f))) "
				+ "(println (build-list-test 5 (lambda (x) (+ x 1)))) " );
		
		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n" 
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(define is-list-empty "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
					+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
				+ "(define head-list (let-type (A B) "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(car (deconstruct l (A List:Linked)))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))"	
				+ "(define tail-list (let-type (A) "
						+ "(extend (extend (extended-lambda (List)) "
							+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
								+ "(error \"Cannot take tail of an empty list\") "
								+ "(cdr (deconstruct l (A List:Linked)))))) "
							+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
								+ "(error \"Cannot take tail of an empty list\") "
								+ "(fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))"
				+ "(define build-list-aux (lambda (i n f) " 
				+ "(if (= i n) "
					+ "(construct List:Linked) "
					+ "(construct List:Linked (f i) (build-list-aux (+ i 1) n f))))) "				
				+ "(define append-list (let-type (A) "
				+ "(extend (extend (extended-lambda (List A)) "
					+ "(lambda ((List:Linked l) (A x)) (if (is-list-empty l) "
						+ "(construct List:Linked x (construct List:Linked)) "
						+ "(construct List:Linked (head-list l) (append-list (tail-list l) x))))) "
					+ "(lambda ((List:Functional l) (A x)) (if (is-list-empty l) "
						+ "(construct List:Functional x (construct List:Functional)) "
						+ "(construct List:Functional (head-list l) (append-list (tail-list l) x))))))) "
				+ "(println (append-list x 21)) ");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
				+ "(constructor List:Linked () nil)\n" + "\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n" 
				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
				+ "(define z (fcons 1 2))\n"
				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
				+ "(constructor List:Functional () nil)\n"
				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n"
				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
				+ "(define is-list-empty "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
					+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
				+ "(define head-list (let-type (A B) "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(car (deconstruct l (A List:Linked)))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(error \"Cannot make head of empty list\") "
						+ "(fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))"	
				+ "(define tail-list (let-type (A) "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(error \"Cannot take tail of an empty list\") "
						+ "(cdr (deconstruct l (A List:Linked)))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(error \"Cannot take tail of an empty list\") "
						+ "(fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))"
				+ "(define build-list-aux (lambda (i n f) " 
				+ "(if (= i n) "
					+ "(construct List:Linked) "
					+ "(construct List:Linked (f i) (build-list-aux (+ i 1) n f))))) "				
				+ "(define append-list (let-type (A) "
				+ "(extend (extend (extended-lambda (List A)) "
					+ "(lambda ((List:Linked l) (A x)) (if (is-list-empty l) "
						+ "(construct List:Linked x (construct List:Linked)) "
						+ "(construct List:Linked (head-list l) (append-list (tail-list l) x))))) "
					+ "(lambda ((List:Functional l) (A x)) (if (is-list-empty l) "
						+ "(construct List:Functional x (construct List:Functional)) "
						+ "(construct List:Functional (head-list l) (append-list (tail-list l) x))))))) "
				+ "(define reverse-list "
				+ "(extend (extend (extended-lambda (List)) "
					+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
						+ "(construct List:Linked) "
						+ "(append-list (reverse-list (tail-list l)) (head-list l))))) "
					+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
						+ "(construct List:Functional) "
						+ "(append-list (reverse-list (tail-list l)) (head-list l)))))) "
			+ "(println (reverse-list x)) " 
			+ "(println (head-list (reverse-list y)))");
				
//		this.assertIntprtAndCompPrintSameValues(
//				";;List is now already defined internal type, so we omit it\n"
//				+ "(let-type (A) (constructor List:Linked ((A x) (List l)) (tuple x l)))\n" 
//				+ "(constructor List:Linked () nil)\n" + "\n"
//				+ "(define fcons (lambda (x y) (lambda (p) (p x y))))\n"
//				+ "(define fcar (lambda (p) (p (lambda (x y) x))))\n"
//				+ "(define fcdr (lambda (p) (p (lambda (x y) y))))\n" 
//				+ "(define z (fcons 1 2))\n"
//				+ "(println (fcar z))\n" 
//				+ "(println (fcdr z))\n" 
//				+ "(let-type (A) (constructor List:Functional ((A x) (List l)) (fcons x l)))\n" 
//				+ "(constructor List:Functional () nil)\n"
//				+ "(define x (construct List:Linked (construct Int:Roman \"XLII\") (construct List:Linked (construct Int:String \"42\") (construct List:Linked 42 (construct List:Linked)))))\n"
//				+ "(define y (construct List:Functional (construct Int:Roman \"XLII\") (construct List:Functional (construct Int:String \"42\") (construct List:Functional 42 (construct List:Functional)))))\n"
//				+ "(println x)\n" 
//				+ "(define is-list-empty "
//					+ "(extend (extend (extended-lambda (List)) "
//						+ "(lambda ((List:Linked l)) (can-deconstruct-as l ()))) "
//						+ "(lambda ((List:Functional l)) (can-deconstruct-as l ()))))"
//				+ "(println (is-list-empty x))" 
//				+ "(println (is-list-empty y))"
//				+ "(println (is-list-empty (construct List:Linked)))\n"
//				+ "(println (is-list-empty (construct List:Functional)))\n"
//				+ "(println (is-list-empty (let-type (A) (cdr (deconstruct x (A List:Linked))))))\n"
//				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct y ((((A List:Functional) #> List:Functional)) #> List:Functional))))))\n"
//				+ "(println (is-list-empty (let-type (A) (fcdr (deconstruct (construct List:Functional 42 (construct List:Functional)) ((((A List:Functional) #> List:Functional)) #> List:Functional))))))\n"
//				+ "(define head-list (let-type (A B) "
//					+ "(extend (extend (extended-lambda (List)) "
//						+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
//							+ "(error \"Cannot make head of empty list\") "
//							+ "(car (deconstruct l (A List:Linked)))))) "
//						+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
//							+ "(error \"Cannot make head of empty list\") "
//							+ "(fcar (deconstruct l ((((A List:Functional) #> B)) #> B))))))))"
//				+ "(println (head-list x))" 
//				+ "(println (head-list y))" 
//				+ "(define tail-list (let-type (A) "
//					+ "(extend (extend (extended-lambda (List)) "
//						+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
//							+ "(error \"Cannot take tail of an empty list\") "
//							+ "(cdr (deconstruct l (A List:Linked)))))) "
//						+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
//							+ "(error \"Cannot take tail of an empty list\") "
//							+ "(fcdr (deconstruct l ((((A List:Functional) #> List:Functional)) #> List:Functional))))))))"
//				+ "(println (tail-list x)) " 
//				+ "(println (head-list (tail-list y))) "
//				+ "(define build-list-aux (lambda (i n f) " 
//					+ "(if (= i n) "
//						+ "(construct List:Linked) "
//						+ "(construct List:Linked (f i) (build-list-aux (+ i 1) n f))))) "
//				+ "(println (build-list-aux 0 5 (lambda (x) (+ x 1)))) "
//				+ "(define build-list (lambda (n f) (build-list-aux 0 n f))) "
//				+ "(println (build-list 5 (lambda (x) (+ x 1)))) " 
//				+ "(define append-list (let-type (A) "
//					+ "(extend (extend (extended-lambda (List A)) "
//						+ "(lambda ((List:Linked l) (A x)) (if (is-list-empty l) "
//							+ "(construct List:Linked x (construct List:Linked)) "
//							+ "(construct List:Linked (head-list l) (append-list (tail-list l) x))))) "
//						+ "(lambda ((List:Functional l) (A x)) (if (is-list-empty l) "
//							+ "(construct List:Functional x (construct List:Functional)) "
//							+ "(construct List:Functional (head-list l) (append-list (tail-list l) x))))))) "
//				+ "(println (append-list x 21)) "
//				+ "(define reverse-list "
//					+ "(extend (extend (extended-lambda (List)) "
//						+ "(lambda ((List:Linked l)) (if (is-list-empty l) "
//							+ "(construct List:Linked) "
//							+ "(append-list (reverse-list (tail-list l)) (head-list l))))) "
//						+ "(lambda ((List:Functional l)) (if (is-list-empty l) "
//							+ "(construct List:Functional) "
//							+ "(append-list (reverse-list (tail-list l)) (head-list l)))))) "
//				+ "(println (reverse-list x)) " 
//				+ "(println (head-list (reverse-list y)))");
	}

//	@Test
//	@DisplayName("Test Clojure TypeSymbol")
//	void testClojureTypeSymbol() throws Exception {
//		// (println (let-type (A) (can-unify-representations Int:Native A)))
//		this
//				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
//						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations,
//								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//										new TypeSymbol(new TypeVariable(NameGenerator.next()))))))))));
//		// (println (can-unify-representations Int:Native Int:Native))
//		this.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
//				Operators.PrintlnOperator,
//				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations, new Tuple(Arrays
//						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative)))))))));
//		// (println (can-unify-representations Int:Native Int:Roman))
//		this.assertIntprtAndCompPrintSameValues(List.of(
//				new AbstractionApplication(
//						Operators.PrintlnOperator,
//						new Tuple(new AbstractionApplication(Operators.CanUnifyRepresentations, new Tuple(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman)))))));
//		// (println (can-unify-representations Int:Native String:Native))
//		this
//				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
//						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyRepresentations,
//								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//										new TypeSymbol(TypeAtom.TypeStringNative)))))))));
//		// (println (let-type (A) (can-unify-types Int:Native A)))
//		this
//				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
//						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes,
//								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//										new TypeSymbol(new TypeVariable(NameGenerator.next()))))))))));
//		// (println (can-unify-types Int:Native Int:Native))
//		this.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
//				Operators.PrintlnOperator,
//				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes, new Tuple(Arrays
//						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative)))))))));
//		// (println (can-unify-types Int:Native Int:Roman))
//		this.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(
//				Operators.PrintlnOperator,
//				new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes, new Tuple(Arrays
//						.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman)))))))));
//		// (println (can-unify-types Int:Native String:Native))
//		this
//				.assertIntprtAndCompPrintSameValues(Arrays.asList(new AbstractionApplication(Operators.PrintlnOperator,
//						new Tuple(Arrays.asList(new AbstractionApplication(Operators.CanUnifyTypes,
//								new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//										new TypeSymbol(TypeAtom.TypeStringNative)))))))));
//	}

	@Test
	@DisplayName("Test clojure instance-of and instance-of-representation")
	void testClojureInstanceOf() throws Exception {
		this.assertIntprtAndCompPrintSameValues("(instance-of 42 Int:Native)");
		this.assertIntprtAndCompPrintSameValues("(let-type (A) (instance-of 42 A))");
		this.assertIntprtAndCompPrintSameValues("(instance-of 42 Int:Roman)");
		this.assertIntprtAndCompPrintSameValues("(instance-of 42 String:Native)");

		this.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 Int:Native)");
		this.assertIntprtAndCompPrintSameValues("(let-type (A) (instance-of-representation 42 A))");
		this.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 Int:Roman)");
		this.assertIntprtAndCompPrintSameValues("(instance-of-representation 42 String:Native)");
	}

	@Test
	@DisplayName("Test clojure is-same-type and is-same-representation")
	void testClojureIsSameType() throws Exception {
		this.assertIntprtAndCompPrintSameValues("(is-same-type 42 42)");
		this.assertIntprtAndCompPrintSameValues("(is-same-type 42 (construct Int:String \"42\"))");
		this.assertIntprtAndCompPrintSameValues("(is-same-type 42 \"42\")");

		this.assertIntprtAndCompPrintSameValues("(is-same-representation 42 42)");
		this.assertIntprtAndCompPrintSameValues("(is-same-representation 42 (construct Int:String \"42\"))");
		this.assertIntprtAndCompPrintSameValues("(is-same-representation 42 \"42\")");
	}
	
	@Test
	@DisplayName("Test Custom Cost Function Compilation")
	void testCustomRanking() throws Exception {
		Tuple elambda_args = new Tuple(new Symbol("a"));
		
		Lambda impl1 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("Int Native"));
		Lambda impl2 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntString),
				new LitString("Int String"));
		Lambda impl3 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntRoman),
				new LitString("Int Roman"));
		
		//Environment env = TopLevelEnvironment.instantiate();
		//
		Tuple args = new Tuple(new LitInteger(42));
		
		ExtendedLambda elambda_defaultCostFunction = 
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						impl1, 
						impl2, 
						impl3));
		AbstractionApplication app_defCostFunction = 
				new AbstractionApplication(
						elambda_defaultCostFunction, 
						args);
		this.assertIntprtAndCompPrintSameValues(Arrays.asList(app_defCostFunction));
		
		Lambda costFunction = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeInt),
				new LitDouble(Double.MIN_VALUE));

		Map<Lambda, Expression> m = new TreeMap<Lambda, Expression>();
		m.put(impl1, Lambda.constFun(elambda_args.size(), new LitDouble(CostAggregation.instance().defaultImplementationRank())));
		m.put(impl2, costFunction);
		m.put(impl3, Lambda.constFun(elambda_args.size(), new LitDouble(CostAggregation.instance().defaultImplementationRank())));
		
		ExtendedLambda elambda_customCostFunction = 
				ExtendedLambda.makeExtendedLambda(m);

		AbstractionApplication app_customCostFunction = 
				new AbstractionApplication(
						elambda_customCostFunction, 
						args);
		
		this.assertIntprtAndCompPrintSameValues(Arrays.asList(app_customCostFunction));
	}
	
	@Test
	@DisplayName("Test Clojure Headers")
	void testClojureHeaders() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		
		
		StringBuilder definitions = new StringBuilder();
		definitions.append(ClojureHelper.declareNamespace(ClojureCodeGenerator.DEFAULT_NAMESPACE));
		definitions.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
		definitions.append(ClojureHelper.requireNamespace(Operators.NAMESPACE));
		
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction("println", 
						ClojureHelper.applyClojureFunction(ClojureCoreSymbols.listNativeToTuple_full, 
								ListNative.listNativeClojure(LitInteger.clojureLit("1"),
										LitInteger.clojureLit("2")))),
				"(1 2)");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (.toString (:lang-type (meta (" + ClojureCoreSymbols.type2typeSymbolSymbol_full + " "
						+ TypeAtom.TypeIntNative.clojureTypeRepresentation() + ")))))", 
				"Int:Native");
		
		assertClojureFunction(
				definitions.toString(),
				"(println (.toString (" + ClojureCoreSymbols.getTypeClojureSymbol_full + " " + LitInteger.clojureLit("1") + ")))",
				TypeAtom.TypeIntNative.toString());
		
		assertClojureFunction(
				definitions.toString(), 
				"(println (" + ClojureCoreSymbols.tuple2velkaListSymbol_full + " [1 2 3]))",
				"(1 2 3)");
		
		Tuple t = new Tuple(new LitInteger(1), new LitComposite(new LitString("1"), TypeAtom.TypeIntString));
		
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction("println", 
						ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertTupleClojureSymbol_full, 
								new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman).clojureTypeRepresentation(),
								t.toClojureCode(env))),
				"[1 [I]]");
		
		Lambda l = (Lambda)(this.parseString("(lambda ((Int:String x)) 1)").get(0));
		TypeArrow lambda_to = new TypeArrow(new TypeTuple(TypeAtom.TypeIntRoman), TypeAtom.TypeIntString);
		
		Expression arg = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction(
						"println",
						ClojureHelper.applyClojureFunction(
								ClojureHelper.applyClojureFunction(
										ClojureCoreSymbols.convertFnClojureSymbol_full,
										lambda_to.clojureTypeRepresentation(),
										l.toClojureCode(env)),
								arg.toClojureCode(env))),
				"[1]");
		
		Lambda l2 = (Lambda)(this.parseString("(lambda () 1)")).get(0);
		TypeArrow l2_to = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntString);
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction("println", 
						ClojureHelper.applyClojureFunction(
										ClojureHelper.applyClojureFunction(
												ClojureCoreSymbols.convertFnClojureSymbol_full, 
												l2_to.clojureTypeRepresentation(),
												l2.toClojureCode(env)))),
				"[1]");
		
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction(
						"println",
						ClojureHelper.applyVelkaFunction_argsTuple(
								l.toClojureCode(env),
								(new Tuple(arg)).toClojureCode(env))),
				"1");
		
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction(
						"println",
						ClojureHelper.applyVelkaFunction_argsTuple(
								l.toClojureCode(env),
								(new Tuple(arg)).toClojureCode(env))),
				"1"); 
				
				ListNative.of(
				new LitInteger(42),
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString));	
		
//		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(
//				new Lambda(
//						new Tuple(new Symbol("x")),
//						new TypeTuple(TypeAtom.TypeIntNative),
//						new LitString("a")),
//				new Lambda(
//						new Tuple(new Symbol("x")),
//						new TypeTuple(TypeAtom.TypeIntString),
//						new LitString("b")));
//		
	}
	
	@Test
	@DisplayName("Test List Native Clojure")
	void testListNativeClojure() throws Exception {
		this.assertIntprtAndCompPrintSameValues("(println (construct List:Native))");
		this.assertIntprtAndCompPrintSameValues("(println (construct List:Native 42 (construct List:Native)))");
		this.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List:Native)))");
		this.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List:Native 42 (construct List:Native))))");
		this.assertIntprtAndCompPrintSameValues("(println (head-list-native (construct List:Native 42 (construct List:Native))))");
		this.assertIntprtAndCompPrintSameValues("(println (tail-list-native (construct List:Native 42 (construct List:Native))))");
		this.assertIntprtAndCompPrintSameValues("(println (map-list-native (lambda (x) (+ x 1)) (construct List:Native 42 (construct List:Native))))");
		this.assertIntprtAndCompPrintSameValues("(println (map2-list-native + (construct List:Native 21 (construct List:Native 21 (construct List:Native))) (construct List:Native 21 (construct List:Native 21 (construct List:Native)))))");
		this.assertIntprtAndCompPrintSameValues("(println (foldl-list-native + 0 (construct List:Native 1 (construct List:Native 2 (construct List:Native)))))");
		
		this.assertIntprtAndCompPrintSameValues("(println (" + ListNative.headSymbol_out + "(" + ListNative.addToEndSymbol_out + " (construct List:Native 21 (construct List:Native)) 42)))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(define l (convert List:Native List:JavaArray (construct List:Native 42 (construct List:Native 21 (construct List:Native)))))"
				+ "(println (" + JavaArrayList.getSymbol_out + " l 0))");
		this.assertIntprtAndCompPrintSameValues(
				"(define l (convert List:Native List:JavaLinked (construct List:Native 42 (construct List:Native 21 (construct List:Native)))))"
				+ "(println (" + JavaLinkedList.getSymbol_out + " l 0))");
		
		this.assertIntprtAndCompPrintSameValues("(println (contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 42))");
		this.assertIntprtAndCompPrintSameValues("(println (contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 84))");
		this.assertIntprtAndCompPrintSameValues("(println (filter-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x)))");
		this.assertIntprtAndCompPrintSameValues("(println (get-list-native (construct List:Native 42 (construct List:Native)) 0))");
		this.assertIntprtAndCompPrintSameValues("(println (build-list-native 2 (lambda (x) x)))");
		this.assertIntprtAndCompPrintSameValues("(println (remove-list-native (build-list-native 2 (lambda (x) x)) 1))");
		this.assertIntprtAndCompPrintSameValues("(println (size-list-native (build-list-native 42 (lambda (x) x))))");
		this.assertIntprtAndCompPrintSameValues("(println (append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42))))");
		this.assertIntprtAndCompPrintSameValues("(println (reverse-list-native (build-list-native 3 (lambda (x) x))))");
		this.assertIntprtAndCompPrintSameValues("(everyp-list-native (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))");
		this.assertIntprtAndCompPrintSameValues("(everyp-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))");
	}
	
//	@Test
//	@DisplayName("Test Java Bit Set")
//	void testJavaBitSet() throws Exception {
//		this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (construct Set:BitSet)))");
//		this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (construct Set:BitSet 2048)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s (construct Set:BitSet))\n"
//				+ 	"(println (bit-set-str (" + JavaBitSet.setSymbol_out.toString() + " s 3)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s (construct Set:BitSet))\n"
//				+	"(println (bit-set-str (" + JavaBitSet.setValueSymbol_out.toString() + " s 3 #t)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s (construct Set:BitSet))\n"
//				+	"(println (bit-set-str (" + JavaBitSet.setIntervalSymbol_out.toString() + " s 2 5)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s (construct Set:BitSet))\n"
//				+	"(println (bit-set-str (" + JavaBitSet.setIntervalValueSymbol_out.toString() + " s 2 5 #t)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.andSymbol_out.toString() + " s1 s2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.andNotSymbol_out.toString() + " s1 s2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (" + JavaBitSet.cardinalitySymbol_out.toString() + " s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.clearSymbol_out.toString() + " s2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.clearBitIndexSymbol_out.toString() + " s2 5)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.clearIntervalSymbol_out.toString() + " s2 5 7)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.cloneSymbol_out.toString() + " s2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (" + JavaBitSet.equalsSymbol_out.toString() + " s1 s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.flipSymbol_out.toString() + " s2 2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.flipIntervalSymbol_out.toString() + " s2 2 5)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.getSymbol_out.toString() + " s2 5))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.getIntervalSymbol_out.toString() + " s2 5 7)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (" + JavaBitSet.intersectsSymbol_out.toString() + " s1 s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.isEmptySymbol_out.toString() + " s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.lengthSymbol_out.toString() + " s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.nextClearBitSymbol_out.toString() + " s2 5))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.nextSetBitSymbol_out.toString() + " s2 0))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.orSymbol_out.toString() + " s1 s2)))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.previousClearBitSymbol_out.toString() + " s2 5))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.previousSetBitSymbol_out.toString() + " s2 9))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 2 5)\n"
//				+	"(println (" + JavaBitSet.sizeSymbol_out.toString() + " s1))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
//				+	"(println (" + JavaBitSet.strSymbol_out.toString() + " s2))");
//		this.assertIntprtAndCompPrintSameValues(
//					"(define s1 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
//				+	"(define s2 (construct Set:BitSet))\n"
//				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
//				+	"(println (bit-set-str (" + JavaBitSet.xorSymbol_out.toString() + " s1 s2)))");
//	}
	
	@Test
	@DisplayName("Test logging")
	void testLogging() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		
		this.assertCompile("(timestamp)", env);
		this.assertCompile("(init-logger \"test-clj-log\")", env);
		this.assertCompile("(log \"test-clj\")", env);
		this.assertIntprtAndCompPrintSameValues("(init-logger \"test-clj-log\")\n" + "(log \"test-clj\")");
	}
	
	@Test
	@DisplayName("Test Get")
	void testGet() throws Exception {
		this.assertIntprtAndCompPrintSameValues("(println (get (tuple 42 \"foo\") 0))");
	}
	
	@Test
	@DisplayName("Test clojure files")
	void testClojureFiles() throws Exception {		
		Path tmpDir = Files.createTempDirectory("cljTest");
		/*Path depsEdn = ClojureCodeGenerator.createDepsEdn(tmpDir);
		
		Path velkaClojureCore = VelkaClojureCore.generateFile(Files.createTempFile("velka.clojure.core", ""));
		Path velkaClojureOperators = VelkaClojureOperators.generateFile(Files.createTempFile("velka.clojure.operators", ""));
		Path velkaClojureList = VelkaClojureList.generateFile(Files.createTempFile("velka.clojure.list", ""));
		
		Files.delete(velkaClojureCore);
		Files.delete(velkaClojureOperators);
		Files.delete(velkaClojureList);
		Files.delete(depsEdn);*/
		
		ClojureCodeGenerator.generateClojureProject(tmpDir);
		Files.copy(velkaUtilJar, tmpDir.resolve(Paths.get("velka.util.jar")), StandardCopyOption.REPLACE_EXISTING);
		Files.copy(velkaTypesJar, tmpDir.resolve(Paths.get("velka.types.jar")), StandardCopyOption.REPLACE_EXISTING);		
		
		Environment env = TopLevelEnvironment.instantiate();
		
		
		ClojureCodeGenerator.ExpressionListToCljFile(
				tmpDir, 
				this.parseString("(println (+ 21 21))"), 
				env);
		
		//Deletes the tmp dir recursively
		Files.walk(tmpDir)
	      .sorted(Comparator.reverseOrder())
	      .map(Path::toFile)
	      .forEach(File::delete);
	}
	
	@Test
	@DisplayName("Test Loop Recur")
	void testLoopRecur() throws Exception {
		this.assertIntprtAndCompPrintSameValues("(println (loop ((x 1)) (if (= x 2) x (recur (+ x 1)))))");
		this.assertIntprtAndCompPrintSameValues("(println (loop ((x 1) (a (construct List:JavaArray))) (if (= x 2) a (recur (+ x 1) (cdr (tuple (java-array-list-add-to-end a x) a))))))");
		this.assertIntprtAndCompPrintSameValues("(println (loop ((x 0) (s \"\")) (if (= x 3) s (recur (+ x 1) (loop ((y 0) (z s)) (if (= y 2) z (recur (+ y 1) (concat z \"a\"))))))))");
	}
	
	@Test
	@DisplayName("Test extend")
	void testExtend() throws Exception {
		assertIntprtAndCompPrintSameValues(
				"(println ((extend (extend "
				+ "(extended-lambda (Int)) "
					+ "(lambda ((Int:Native x)) \"foo\")) "
					+ "(lambda ((Int:Roman x)) \"bar\")) "
				+ "(construct Int:Roman \"X\")))");
		assertIntprtAndCompPrintSameValues(
				"(println ((extend (extend "
				+ "(extended-lambda (Int)) "
					+ "(lambda ((Int:Native x)) \"foo\")) "
					+ "(lambda ((Int:Roman x)) \"bar\") (lambda ((Int:* x)) -999.0)) "
				+ "42))");
	}
	
	@Test
	@DisplayName("Test Sandbox")
	void testSandbox() throws Exception{	
		var env = TopLevelEnvironment.instantiate();
		
		var tv = TypeVariable.generate();
		var t1 = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative), TypeAtom.TypeIntNative);
		var t2 = new TypeArrow(new TypeTuple(tv, tv), TypeAtom.TypeIntNative);
		
		assertTrue(env.getTypeSystem().canConvert(t1, t2));
		
	}
}
