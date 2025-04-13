package velka.test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Supplier;

import javax.tools.JavaCompiler;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JFormatter;
import com.sun.codemodel.JMethod;

import velka.clojure.ClojureCodeGenerator;
import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.Function;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.core.util.Constants;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.RankAggregation;

class TestComplex extends VelkaTest {
	
	@Test
	void testCmdLineArgs() throws Exception {
		var val = "test";
		this.cljCmdArgs.add(val);
		this.assertCompiledCodePrints(
				"(println (get " + ClojureCoreSymbols.CONSOLE_ARGS_SYMBOL + " 0))", 
				val + System.getProperty("line.separator"));
	}
	
	@Test
	@DisplayName("Test Recursion")
	void testRecursion() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		
		this.assertInterpretedStringEquals("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(fact 5)",
				new LitInteger(120), env);

		this.assertCompile("(define fact (lambda (x) (if (= x 1) x (* x (fact (- x 1))))))", env);

		this.assertIntprtAndCompPrintSameValues(
				"(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(println (to-str (fact 5)))");
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
		
//		this.assertIntprtAndCompPrintSameValues(
//				"(println (to-str"
//				+ "((extend (extended-lambda (Bool Int Int)) "
//						+ "(lambda ((Bool:Native x) (Int:String y) (Int:String z)) (if x z y))) "
//						+ "#f (construct Int:Roman \"XLII\") 66)))");
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

//		this.assertIntprtAndCompPrintSameValues(
//				"(constructor Name:Unstructured ((String:Native x)) x) " 
//				+ "(constructor Name:Structured ((String:Native x) (String:Native y)) (tuple x y)) "
//				+ "(println "
//					+ "((extend (extended-lambda (Name)) "
//						+ "(lambda ((Name:Unstructured x)) \"unstructured\"))"
//					+ "(construct Name:Unstructured \"Jan Novak\")))"
//				+ "(println "
//					+ "((extend (extended-lambda (Name)) "
//						+ "(lambda ((Name:Structured x)) \"structured\")) "
//					+ "(construct Name:Structured \"Jan\" \"Novak\")))"
//				+ "(conversion Name:Structured Name:Unstructured (x) (construct Name:Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))\n"
//				+ "(println (to-str ((lambda ((Name:Unstructured x)) x) (construct Name:Structured \"Jan\" \"Novak\"))))");
	}

	@Test
	@DisplayName("Literals")
	void testLiterals() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		
		this.assertVelkaCode("0", 0);
		this.assertVelkaCode("3.141521", 3.141521);
		this.assertVelkaCode("#t", Boolean.TRUE);
		this.assertVelkaCode("#f", Boolean.FALSE);
		this.assertVelkaCode("\"Hello World\"", "Hello World");
	}

	@Test
	@DisplayName("Clojure Special Forms and Applications")
	void testSpecialFormsAndApplication() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertIntprtAndCompPrintSameValues("(println (to-str ((lambda (x y) x) 42 21)))");
		this.assertIntprtAndCompPrintSameValues("(println (to-str ((lambda ((Int:Native x) (Int:Native y)) x) 42 21)))");
		this.assertIntprtAndCompPrintSameValues("(println (to-str (if #t 42 21)))");
//		this.assertIntprtAndCompPrintSameValues(
//				"(println (to-str (if #t (construct Int:Roman \"XLII\") (construct Int:String \"42\"))))");
		this.assertIntprtAndCompPrintSameValues("(println (to-str (tuple 21 21)))");
		this.assertCompile("(error \"error msg\")", env);
		this.assertIntprtAndCompPrintSameValues("(println (to-str (and #t #f)))");
		this.assertIntprtAndCompPrintSameValues("(println (to-str (or #t #f)))");
		this.assertIntprtAndCompPrintSameValues("(define answer \"42\")" + "(println answer)");

//		this.assertIntprtAndCompPrintSameValues(
//				"(constructor Name2:Structured ((String:Native x) (String:Native y)) (tuple x y))"
//				+ "(constructor Name2:Unstructured ((String:Native x)) x)"
//				+ "(conversion Name2:Structured Name2:Unstructured"
//				+ "(x) (construct Name2:Unstructured (concat (car (deconstruct x (String:Native String:Native))) (cdr (deconstruct x (String:Native String:Native))))))"
//				+ "(println (to-str ((lambda ((Name2:Unstructured x)) x) (construct Name2:Structured \"Jan\" \"Novak\"))))"
//				+ "(println (to-str"
//					+"((extend (extend (extended-lambda (Int)) "
//						+ "(lambda ((Int:Native x)) \"Native\")) "
//						+ "(lambda ((Int:String x)) \"String\")) "
//					+ "(construct Int:String \"42\"))))");
	}

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
		var parm = new Symbol("a");
		
		var impl1 = new Function(this.env,
				new LitString("Int Native"),
				List.of(Pair.of(parm, TypeAtom.TypeIntNative)));
		var impl2 = new Function(this.env,
				new LitString("Int String"),
				List.of(Pair.of(parm, TypeAtom.TypeIntString)));
		var impl3 = new Function(this.env,
				new LitString("Int Roman"),
				List.of(Pair.of(parm, TypeAtom.TypeIntRoman)));
		
		var args = new Tuple(new LitInteger(42));
		
		var efun_default = (new ExtendedFunction(this.env))
				.extend(impl1,
						new Function(this.env, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
								List.of(Pair.of(parm, TypeAtom.TypeInt))))
				.extend(impl2,
						new Function(this.env, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
								List.of(Pair.of(parm, TypeAtom.TypeInt))))
				.extend(impl3,
						new Function(this.env, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
								List.of(Pair.of(parm, TypeAtom.TypeInt))));
		
		var app_defCostFunction = 
				new AbstractionApplication(
						efun_default, 
						args);
		
		this.assertInterpretationEquals(app_defCostFunction, new LitString("Int Native"), this.env);
		
		var efun_custom = (new ExtendedFunction(this.env))
				.extend(impl1,
						new Function(this.env, new LitDouble(.1d),
								List.of(Pair.of(parm, TypeAtom.TypeInt))))
				.extend(impl2,
						new Function(this.env, new LitDouble(.1d),
								List.of(Pair.of(parm, TypeAtom.TypeInt))))
				.extend(impl3,
						new Function(this.env, new LitDouble(.999999999d),
								List.of(Pair.of(parm, TypeAtom.TypeInt))));

		var app_customCostFunction = 
				new AbstractionApplication(
						efun_custom, 
						args);
		
		this.assertInterpretationEquals(app_customCostFunction, new LitString("Int Roman"), this.env);
	}
	
	@Test
	@DisplayName("Test Clojure Headers")
	void testClojureHeaders() throws Exception {
		Environment env = TopLevelEnvironment.instantiate();
		
		
		StringBuilder definitions = new StringBuilder();
		definitions.append(ClojureHelper.declareNamespace(Constants.DEFAULT_NAMESPACE));
		definitions.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
		definitions.append(ClojureHelper.requireNamespace(Operators.singleton().getNamespace()));
		
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
		var lambda_from = new TypeArrow(new TypeTuple(TypeAtom.TypeIntString), TypeAtom.TypeIntNative);
		
		Expression arg = new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman);
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction(
						"println",
						ClojureHelper.applyVelkaFunction(
								ClojureHelper.applyClojureFunction(
										ClojureCoreSymbols.convertFnClojureSymbol_full,
										lambda_from.clojureTypeRepresentation(),
										lambda_to.clojureTypeRepresentation(),
										l.toClojureCode(env)),
								arg.toClojureCode(env))),
				"[1]");
		
		Lambda l2 = (Lambda)(this.parseString("(lambda () 1)")).get(0);
		TypeArrow l2_to = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntString);
		var l2_from = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntNative);
		assertClojureFunction(
				definitions.toString(),
				ClojureHelper.applyClojureFunction("println", 
						ClojureHelper.applyVelkaFunction(
										ClojureHelper.applyClojureFunction(
												ClojureCoreSymbols.convertFnClojureSymbol_full,
												l2_from.clojureTypeRepresentation(),
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
		this.assertVelkaCode(
				"(get (tuple 42 \"foo\") 0)",
				42);
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
		final Object[] aux = new Object[1];
		aux[0] = 1;
		Integer ret = null;
		while(ret == null) {
			ret = (Integer) (new Supplier() {

				@Override
				public Object get() {
					final Integer x = (Integer)aux[0];
					
					return (new Supplier() {

						@Override
						public Object get() {
							if(x == 2) {
								return x;
							}
							else {
								return (new Supplier() {

									@Override
									public Object get() {
										aux[0] = x + 1;
										return null;
									}
									
								}).get();
							}
						}
						
					}).get();
				}
				
			}).get();
		}
		
		
		this.assertVelkaCode(
				"(loop ((x 1)) (if (= x 2) x (recur (+ x 1))))",
				2);
		this.assertVelkaCode(
				"(loop ((x 1) (a (construct List:Native))) "
				+ "(if (= x 2) a (recur (+ x 1) (cdr (tuple (list-native-add-to-end-in-place a x) a)))))",
				List.of(1));
		this.assertVelkaCode(
				"(loop ((x 0) (s \"\")) "
				+ "(if (= x 3) s (recur (+ x 1) (loop ((y 0) (z s)) "
					+ "(if (= y 2) z (recur (+ y 1) (concat z \"a\")))))))",
				"aaaaaa");
	}
	
	@Test
	@DisplayName("Test extend")
	void testExtend() throws Exception {
		this.assertVelkaCode(
				"((extend (extend "
				+ "(extended-lambda (Int)) "
					+ "(lambda ((Int:Native x)) \"foo\")) "
					+ "(lambda ((Int:Roman x)) \"bar\")) "
				+ "(construct Int:Roman \"X\"))",
				"bar");
		this.assertVelkaCode(
				"((extend (extend "
				+ "(extended-lambda (Int)) "
					+ "(lambda ((Int:Native x)) \"foo\")) "
					+ "(lambda ((Int:Roman x)) \"bar\") (lambda ((Int:* x)) 0.0)) "
				+ "42)",
				"foo");
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
	
	@Test
	void testJCodeBase() throws JClassAlreadyExistsException {
		var codeModel = new JCodeModel();
		
		var jp = codeModel._package("com.sookocheff.example");
		var jc = jp._class("DataProcessor");
		
		var processDataMethod = jc.method(JMod.PUBLIC, int.class, "processData");
		
		var resultVar = processDataMethod.body().decl(codeModel.INT, "result", JExpr.lit(0));
		
		processDataMethod.body().assign(resultVar, resultVar.plus(JExpr.lit(42)));
		
		processDataMethod.body().add(
	            codeModel.ref(System.class).staticRef("out").invoke("println").arg(resultVar)
	        );
		
		processDataMethod.body()._return(resultVar);
		 
		var stringWriter = new StringWriter();
        var formatter = new JFormatter(stringWriter);

        // Generate the code for the Person class
        jc.declare(formatter);

        // Convert to String and print
        String generatedCode = stringWriter.toString();
        System.out.println(generatedCode);
	}
	
	@Test
	void testRuntimeClassLoad() throws Exception {
		JCodeModel codeModel = new JCodeModel();
        JDefinedClass exampleClass = codeModel._class("com.example.ExampleClass");
        
        // Create a public method "hello" that prints "Hello from generated code!"
        JMethod helloMethod = exampleClass.method(JMod.PUBLIC, codeModel.VOID, "hello");
        helloMethod.body().add(
        		codeModel.ref(System.class).staticRef("out").invoke("println").arg(JExpr.lit("Hello from generated code!"))
        );
        
        // Save the generated code to the output directory
        File outputDir = new File("./generated-sources");
        outputDir.mkdirs();
        codeModel.build(outputDir);
        
        // Step 2: Compile the generated code
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null);
        
        // Point to the generated source file
        File sourceFile = new File(outputDir, "com/example/ExampleClass.java");
        Iterable<? extends javax.tools.JavaFileObject> compilationUnits = fileManager.getJavaFileObjects(sourceFile);
        
        // Specify the output directory for compiled .class files
        fileManager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.Collections.singletonList(outputDir));
        compiler.getTask(null, fileManager, null, null, null, compilationUnits).call();
        fileManager.close();
        
        // Step 3: Load the compiled class into the runtime
        URLClassLoader classLoader = URLClassLoader.newInstance(new URL[]{outputDir.toURI().toURL()});
        Class<?> loadedClass = Class.forName("com.example.ExampleClass", true, classLoader);
        
        // Step 4: Instantiate the class and invoke the method
        Object instance = loadedClass.getDeclaredConstructor().newInstance();
        Method hello = loadedClass.getMethod("hello");
        hello.invoke(instance);  // Prints: "Hello from generated code!"
	}
	
	@Test
	void sandbox() throws NoSuchMethodException, SecurityException {
		this.assertVelkaCode(
				"((lambda ((((Int:Native) #> Int:Native) f)) (f 42)) (lambda ((Int:* a)) a))",
				Integer.valueOf(42));
	}
}
