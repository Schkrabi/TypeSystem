package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Function;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.application.AndExpression;
import velka.core.application.CanDeconstructAs;
import velka.core.application.Construct;
import velka.core.application.Convert;
import velka.core.application.Deconstruct;
import velka.core.application.DefineConstructor;
import velka.core.application.DefineConversion;
import velka.core.application.DefineRepresentation;
import velka.core.application.DefineSymbol;
import velka.core.application.DefineType;
import velka.core.application.ExceptionExpr;
import velka.core.application.Extend;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.Loop;
import velka.core.application.OrExpression;
import velka.core.application.Recur;
import velka.core.exceptions.ConversionException;
import velka.core.exceptions.DuplicateConversionException;
import velka.core.exceptions.UndefinedTypeException;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.parser.Parser;
import velka.parser.exceptions.InvalidNumberOfArgsException;
import velka.parser.exceptions.UnexpectedExpressionException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;

class TestParser {
	
	private static boolean equalsLambdaUpToTypeVariables(Lambda l1, Lambda l2) throws AppendableException {		
		return Type.unifyTypes(l1.argsType, l2.argsType).isPresent()
				&& l1.args.equals(l2.args)
				&& l1.body.equals(l2.body);
		
	}

	@Test
	@DisplayName("Test Semantic Simple")
	void testSemanticSimple() throws AppendableException {
		this.testParse("()", Expression.EMPTY_EXPRESSION);
		this.testParse("(+ 1 1)", new AbstractionApplication(new Symbol("+"),
				new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(1)))));
		this.testParse("(if #t x y)", new IfExpression(LitBoolean.TRUE, new Symbol("x"), new Symbol("y")));
		// this.testParse("(type Name)", new DefineType(new TypeName("Name")));
		this.testParse("(type Name)", new DefineType(new TypeName("Name")));
		// this.testParse("(representation Unstructured Name)",
		// new DefineRepresentation(new TypeName("Name"), new
		// TypeRepresentation("Unstructured")));
		this.testParse("(representation Unstructured Name)",
				new DefineRepresentation(new TypeName("Name"), new TypeRepresentation("Unstructured")));
		this.testParse("(constructor Name Unstructured ((String:Native x)) x)",
				new DefineConstructor(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Symbol("x"))));
		// this.testParse("(representation Structured Name)",
		// new DefineRepresentation(new TypeName("Name"), new
		// TypeRepresentation("Structured")));
		this.testParse("(representation Structured Name)",
				new DefineRepresentation(new TypeName("Name"), new TypeRepresentation("Structured")));
		this.testParse("(constructor Name Structured ((String:Native x) (String:Native y)) (cons x y))",
				new DefineConstructor(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Structured")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
								new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))))));
		this.testParse(
				"(conversion Name:Structured Name:Unstructured ((Name:Structured name)) "
						+ "(construct Name Unstructured \"Test\"))",
				new DefineConversion(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Structured")),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
						new Tuple(Arrays.asList(new Symbol("name"))),
						new Construct(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
								new Tuple(Arrays.asList(new LitString("Test"))))));

		this.testParse("(define one 1)", new DefineSymbol(new Symbol("one"), new LitInteger(1)));
		this.testParse("(cons 1 2)", new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))));
		this.testParse("(error \"test\")", new ExceptionExpr(new LitString("test")));
		this.testParse("(Int:String \"256\")",
				new AbstractionApplication(new Symbol("Int:String"), new Tuple(Arrays.asList(new LitString("256")))));
		this.testParse("3.141528", new LitDouble(3.141528));
		this.testParse("#f", LitBoolean.FALSE);
		this.testParse("(and #t #f)", new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE))));
		this.testParse("(or #t #f)", new OrExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE))));
		this.testParse("(construct Int String \"42\")",
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("42")))));
		this.testParse("(convert Int:Roman Int:String x)",
				new Convert(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString, new Symbol("x")));

		this.testParse("(lambda (((Int:Native String:Native Bool:Native) t)) t)", new Lambda(
				new Tuple(Arrays.asList(new Symbol("t"))),
				new TypeTuple(Arrays.asList(new TypeTuple(
						Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative, TypeAtom.TypeBoolNative)))),
				new Symbol("t")));

		this.testParse("(lambda (((Int:Native #> Int:Native) f)) f)",
				new Lambda(new Tuple(Arrays.asList(new Symbol("f"))),
						new TypeTuple(Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative))),
						new Symbol("f")));

		this.testParse("(lambda ((((Int:Native String:Native) #> (String:Native Int:Native)) f)) f)",
				new Lambda(new Tuple(Arrays.asList(new Symbol("f"))),
						new TypeTuple(Arrays.asList(new TypeArrow(
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative)),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeIntNative))))),
						new Symbol("f")));
		this.testParse("(instance-of 42 Int:Native)", new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative));
		this.testParse("(instance-of-representation 42 Int:Native)",
				new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative));
		this.testParse("(get (cons 42 \"foo\") 0)",
				Get.makeGet(new Tuple(new LitInteger(42), new LitString("foo")), new LitInteger(0)));
		this.testParse("(tuple 42 \"foo\" #t)", new Tuple(new LitInteger(42), new LitString("foo"), LitBoolean.TRUE));		

		/*
		 * this.testParse("(let-type (A) (lambda ((String:Native x) (A y)) y))", new
		 * Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))), new
		 * TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, new TypeVariable("A"))),
		 * new Symbol("y")));
		 */

		this.testParse("(deconstruct x Int:Native)", new Deconstruct(new Symbol("x"), TypeAtom.TypeIntNative));
		;
		this.testParse("(can-deconstruct-as x Int:Native)",
				new CanDeconstructAs(new Symbol("x"), TypeAtom.TypeIntNative));

		Lambda ranking = new Lambda(new Tuple(new Symbol("x"), new Symbol("y")),
				new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative), new LitInteger(1));

		this.testParse("(eapply + (cons 1 2))",
				new AbstractionApplication(new Symbol("+"), new Tuple(new LitInteger(1), new LitInteger(2))));
		this.testParse("(eapply + (cons 1 2) (lambda ((List:Native x) (List:Native y)) 1))",
				new AbstractionApplication(new Symbol("+"), new Tuple(new LitInteger(1), new LitInteger(2)), ranking));

		Lambda impl = new Lambda(new Tuple(new Symbol("x")), new TypeTuple(TypeAtom.TypeIntNative), new Symbol("x"));
		List<Lambda> impls = new ArrayList<Lambda>();
		impls.add(impl);
		this.testParse(
				"(extended-lambda-selection ((Int x)) (lambda ((List:Native x) (List:Native y)) 1) ((Int:Native) x))",
				ExtendedLambda.makeExtendedLambda(impls, ranking));
		
		this.testParse("(extend foo bar)",
				new Extend(new Symbol("foo"), new Symbol("bar")));
	}

	@Test
	@DisplayName("Test Lambda")
	void testLambda() throws AppendableException {
		String sExpr = "(lambda (x) y)";
		Expression expr = new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
				new TypeTuple(Arrays.asList(new TypeVariable("_x"))), new Symbol("y"));

		Expression parsed = parseString(sExpr);
		assertTrue(parsed instanceof Lambda);

		Lambda parsedLambda = (Lambda) parsed;
		Lambda expectedLambda = (Lambda) expr;

		assertEquals(parsedLambda, expectedLambda);

		assertEquals(parseString("(lambda ((String x) (Int y)) x)"),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeString, TypeAtom.TypeInt)), new Symbol("x")));
	}

	@Test
	@DisplayName("Test Extended Lambda")
	void testElambda() throws AppendableException {
		Expression parsed = parseString("(extended-lambda ((Int x)) ((Int:String) y))");
		assertTrue(parsed instanceof ExtendedLambda);
	}

	@Test
	@DisplayName("Test Validate Extended Lambda")
	public void testValidateElambda() {
		// Too few arguments
		assertThrows(AppendableException.class, () -> parseString("(extended-lambda (x))"));

		// Second argument is not a list
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(extended-lambda x x)"));

		// Badly formed implementation
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(extended-lambda (x) x x)"));
	}

	@Test
	@DisplayName("Vailade If Expression")
	public void testValidateIf() {
		// Too few arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if)"));

		// Too many arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if x y z w)"));
	}

	@Test
	@DisplayName("Test Validate Lambda")
	public void testValidateLambda() {
		// Too few arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda)"));
		// Too many arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda x y z)"));
		// Badly formed argument list
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda x y)"));

//		// non-symbol token instead of lambda
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateLambdaList(Arrays.asList(SemanticNode.make(NodeType.STRING, "lambda"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
//		// Bad symbol in place of lambda
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateLambdaList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	@DisplayName("Test Parse Define Type")
	public void testParseDefType() {
		// Too few arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(type)"));
		// Too many arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(type animal dog)"));
		// Defining non-symbol type
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(type 1234)"));

//		// non-symbol token instead of deftype
//		assertThrows(UnexpectedExpressionException.class, () -> Validations.validateDefTypeList(
//				Arrays.asList(SemanticNode.make(NodeType.STRING, "type"), SemanticNode.make(NodeType.SYMBOL, "x"))));
//		// Bad symbol in place of deftype
//		assertThrows(UnexpectedExpressionException.class, () -> Validations.validateDefTypeList(
//				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	@DisplayName("Test Parse Define Representation")
	public void testParseDefRep() {
		// Too few arguments
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(representation)"));
		// Too many argument
		assertThrows(InvalidNumberOfArgsException.class,
				() -> parseString("(representation list functional random failed)"));
		// Type is not symbol
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(representation 1234 functional)"));
		// Representation is not symbol
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(representation list 1234)"));
	}

	@Test
	@DisplayName("Test Parse Type")
	public void testParseType() {
		// Wrong token in place of type
		assertThrows(AppendableException.class, () -> parseString("(extended-lambda (x) x ((1234) x))"));
	}

	@Test
	@DisplayName("Test Parse Variable Type Pair")
	public void testParseVariableTypePair() {
		// Badly formed variable-type pair
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda ((Integer String x)) x)"));

		// Non-type on type position
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda ((1234 x)) x)"));

		// Not symbol on variable position
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda ((Integer 1234)) x)"));

//		// Pair is not a list
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateVariableTypePair(SemanticNode.make(NodeType.SYMBOL, "x")));
	}

	@Test
	@DisplayName("Test Parse Implementation")
	public void testParseImplementation() {
		// Too many arugments in implementation
		assertThrows(AppendableException.class, () -> parseString("(extended-lambda (x) x ((Integer) x y))"));

		// Badly formed type list
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(extended-lambda (x) x (1234 x))"));

//		assertThrows(AppendableException.class,
//				() -> Validations.validateImplementation(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x")),
//						new TreeMap<TypeVariable, TypeVariable>()));
//		assertThrows(AppendableException.class,
//				() -> Validations.validateImplementation(
//						Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x")),
//						new TreeMap<TypeVariable, TypeVariable>()));
	}

	@Test
	@DisplayName("Test Parse Define Conversion")
	public void testParseDefConversion() {
		// Too few args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(conversion x)"));
		// Too many args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(conversion x y z w q)"));

//		// Bad special form
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateDefconversionList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
//
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations
//						.validateDefconversionList(Arrays.asList(SemanticNode.make(NodeType.INT, Integer.valueOf(1234)),
//								SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"),
//								SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad From Type
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(conversion 1234 Int (x) x)"));
		// Bad To Type
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(conversion Int 1234 (x) x)"));
		// Conversion is not lambda
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(conversion Int Double 1234 x)"));

		// Conversion takes multiple arguments
		assertThrows(AppendableException.class, () -> parseString("(conversion Int Double (x y) x)"));
	}

	@Test
	@DisplayName("Test Parse Define")
	public void testParseDefine() {
		// Too few args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(define x)"));
		// Too many args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(define x y z)"));

		// define token is not a symbol
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateDefineList(Arrays.asList(SemanticNode.make(NodeType.STRING, "define"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
//
//		// Bad symbol in define
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateDefineList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Defined is not a symbol
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(define 1234 1234)"));
	}

	@Test
	@DisplayName("Test Parse Cons")
	public void testParseCons() {
		// Too few args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(cons x)"));
		// Too many args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(cons x y z)"));

//		// cons token is not a symbol
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateConsList(Arrays.asList(SemanticNode.make(NodeType.STRING, "cons"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
//
//		// Bad symbol in cons
//		assertThrows(UnexpectedExpressionException.class,
//				() -> Validations.validateConsList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
//						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	@DisplayName("Test Parse Error")
	public void testParseError() {
		// Too few args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(error)"));
		// Too many args
		assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(error x y )"));

		// error token is not a symbol
//		assertThrows(UnexpectedExpressionException.class, () -> Validations.validateErrorList(
//				Arrays.asList(SemanticNode.make(NodeType.STRING, "error"), SemanticNode.make(NodeType.STRING, "xs"))));
//
//		// Bad symbol in error
//		assertThrows(UnexpectedExpressionException.class, () -> Validations.validateErrorList(
//				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"), SemanticNode.make(NodeType.STRING, "x"))));

		// Error msg is not a string
		assertThrows(UnexpectedExpressionException.class, () -> parseString("(error 1234)"));
	}

//	@Test
//	@DisplayName("Test Parse Node")
//	public void testParseNode() {
//		assertThrows(AppendableException.class,
//				() -> SemanticParser.parseNode(SemanticNode.make(NodeType.UNUSED, new Object())));
//	}
//
//	@Test
//	@DisplayName("Test Parse Special Form")
//	public void testParseSpecialForm() {
//		assertThrows(AppendableException.class, () -> SemanticParser
//				.parseNodelist(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, SemanticParserStatic.UNUSED))));
//	}
//
//	@Test
//	@DisplayName("Test Semantic Parses Static")
//	public void testSemanticParserStatic() throws AppendableException {
//		SemanticNode n = SemanticNode.make(NodeType.SYMBOL, "fail");
//		assertFalse(SemanticParserStatic.isSpecialForm(n));
//
//		List<TypeVariablePair> l = Arrays.asList(new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
//				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
//				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
//		assertTrue(SemanticParserStatic.isArgListFullyTyped(l));
//
//		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
//				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
//		assertFalse(SemanticParserStatic.isArgListFullyTyped(l));
//
//		l = Arrays.asList(new TypeVariablePair(TypeAtom.TypeInt, new Symbol("x")));
//		assertTrue(SemanticParserStatic.isArgListFullyTyped(l));
//		assertFalse(SemanticParserStatic.isArgListUntypped(l));
//
//		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
//		assertFalse(SemanticParserStatic.isArgListFullyTyped(l));
//		assertTrue(SemanticParserStatic.isArgListUntypped(l));
//
//		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
//		assertFalse(SemanticParserStatic.isArgListUntypped(l));
//
//		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
//		assertTrue(SemanticParserStatic.isArgListUntypped(l));
//
//		List<TypeVariablePair> k = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
//				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
//		assertEquals(SemanticParserStatic.listTail(l), k);
//
//		Tuple t = SemanticParserStatic.parseArgsList(
//				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "y")));
//		Tuple exp = new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y")));
//		assertEquals(t, exp);
//
//		assertThrows(UnexpectedExpressionException.class, () -> SemanticParserStatic
//				.parseArgsList(Arrays.asList(SemanticNode.make(NodeType.BOOL, Boolean.valueOf(false)))));
//	}
//
//	@Test
//	@DisplayName("Test Variable Type Pair")
//	public void testVariableTypePair() {
//		TypeVariablePair p = new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x"));
//		assertAll(() -> {
//			p.toString();
//		});
//
//		Integer i = Integer.valueOf(128);
//		assertNotEquals(p, i);
//
//		TypeVariablePair q = new TypeVariablePair(TypeAtom.TypeBool, new Symbol("y"));
//		assertNotEquals(p, q);
//
//		q = new TypeVariablePair(TypeAtom.TypeInt, new Symbol("x"));
//		assertNotEquals(p, q);
//	}

	@Test
	@DisplayName("Test Type Environment")
	public void testTypeEnvironment() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			typeEnv.addType(new TypeName("ListTestTypeEnv"));
			typeEnv.addRepresentation(
					new TypeAtom(new TypeName("ListTestTypeEnv"), new TypeRepresentation("Functional")));
			typeEnv.addType(new TypeName("Test"));
			typeEnv.addRepresentation(new TypeAtom(new TypeName("Test"), new TypeRepresentation("Functional")));
		});

		assertTrue(typeEnv.existsTypeAtom(new TypeAtom(new TypeName("ListTestTypeEnv"), TypeRepresentation.WILDCARD)));
		assertTrue(typeEnv
				.existsTypeAtom(new TypeAtom(new TypeName("ListTestTypeEnv"), new TypeRepresentation("Functional"))));
		assertFalse(typeEnv.existsTypeAtom(new TypeAtom(new TypeName("kawabanga"), TypeRepresentation.WILDCARD)));
		assertFalse(typeEnv
				.existsTypeAtom(new TypeAtom(new TypeName("ListTestTypeEnv"), new TypeRepresentation("kawabanga"))));

		assertAll(() -> typeEnv.getConstructor(TypeAtom.TypeIntRoman,
				new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative))));
		assertThrows(AppendableException.class,
				() -> typeEnv.getConstructor(new TypeAtom(new TypeName("fail"), TypeRepresentation.WILDCARD),
						TypeTuple.EMPTY_TUPLE));

		assertThrows(AppendableException.class,
				() -> typeEnv.addRepresentation(new TypeAtom(new TypeName("Int"), new TypeRepresentation("Roman"))));

		assertThrows(AppendableException.class,
				() -> typeEnv.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative,
						new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION, env)));

		assertThrows(DuplicateConversionException.class,
				() -> typeEnv.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
						new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION, env)));

		typeEnv.convertTo(new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString),
				TypeAtom.TypeIntString, TypeAtom.TypeIntRoman);
		assertThrows(ConversionException.class, () -> typeEnv.convertTo(Expression.EMPTY_EXPRESSION,
				TypeAtom.TypeStringNative, TypeAtom.TypeIntNative));
	}

//	@Test
//	@DisplayName("Test Semantic Node")
//	public void testSemanticNode() {
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
//		assertThrows(AppendableException.class,
//				() -> SemanticNode.make(NodeType.INT, new SemanticPair("fail", "fail")));
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, Integer.valueOf(128)));
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Double.valueOf(3.14)));
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Boolean.valueOf(true)));
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Arrays.asList(128)));
//
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asSymbol());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asPair());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, "fail").asInt());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asDouble());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asString());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asBool());
//		assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Integer.valueOf(128)).asList());
//
//	}

	@Test
	@DisplayName("Test Exceptions")
	void testExceptions() {
		Environment env = Environment.initTopLevelEnvironment();
		assertAll(() -> {
			new UserException("test");
			new DuplicateConversionException(TypeAtom.TypeBool, TypeAtom.TypeBoolNative,
					new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION, env),
					new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION, env));
			new UndefinedTypeException("fail");
		});
	}

	@Test
	@DisplayName("Test Let Type")
	void testLetType() throws AppendableException {
		Expression e = this.parseString("(let-type (A) (lambda ((A x)) (let-type (A) (lambda ((A y)) x))))");

		TypeVariable outer = (TypeVariable) ((Lambda) e).argsType.get(0);
		TypeVariable inner = (TypeVariable) ((Lambda) ((Lambda) e).body).argsType.get(0);

		assertNotEquals(outer, inner);

		Lambda l1 = (Lambda) e;
		Lambda l2 = (Lambda) l1.body;
		// Have to compare names directly as TypeVariables equals if they are not bound
		// by substitution
		assertNotEquals(((TypeVariable) l1.argsType.get(0)).name, ((TypeVariable) l2.argsType.get(0)).name);
	}
	
	@Test
	@DisplayName("Test let")
	void testLet () throws AppendableException {
		String letCode = "(let ((a 21) (b 21)) a)";
		
		Expression e = this.parseString(letCode);
		if(e instanceof AbstractionApplication) {
			AbstractionApplication apl = (AbstractionApplication)e;
			Lambda l = (Lambda)apl.fun;
			if(!TestParser.equalsLambdaUpToTypeVariables(l, new Lambda(new Tuple(new Symbol("a"), new Symbol("b")),
					new TypeTuple(new TypeVariable("A"), new TypeVariable("B")), new Symbol("a")))) {
				Assertions.fail("Fail on " + letCode + " got " + e.toString());
			}
			
			Tuple args = (Tuple)apl.args;
			Assertions.assertEquals(new Tuple(new LitInteger(21), new LitInteger(21)), args);
		}
		else {
			Assertions.fail("Fail on " + letCode + " got " + e.toString());
		}
		
		String letAstCode = "(let* ((a 30) (b 12)) a)";
		
		e = this.parseString(letAstCode);
		if(e instanceof AbstractionApplication) {
			AbstractionApplication apl = (AbstractionApplication)e;
			Lambda l = (Lambda)apl.fun;
			
			if(l.body instanceof AbstractionApplication) {
				AbstractionApplication apl2 = (AbstractionApplication)l.body;
				Lambda l2 = (Lambda)apl2.fun;
				if(!TestParser.equalsLambdaUpToTypeVariables(l2, new Lambda(new Tuple(new Symbol("b")), new TypeTuple(new TypeVariable("B")), new Symbol("a")))) {
					Assertions.fail("Fail on " + letAstCode + " got " + e.toString());
				}
				
				Tuple args2 = (Tuple)apl2.args;
				Assertions.assertEquals(new Tuple(new LitInteger(12)), args2);
				
			} else {
				Assertions.fail("Fail on " + letAstCode + " got " + l.body.toString());
			}
			
			Assertions.assertEquals(new Tuple(new Symbol("a")), l.args);
			Assertions.assertEquals(new Tuple(new LitInteger(30)), apl.args);
		} else {
			Assertions.fail("Fail on " + letAstCode + " got " + e.toString());
		}
	}
	
	@Test
	@DisplayName("Test Loop Recur")
	void testLoopRecur() throws AppendableException {
		Expression e = this.parseString("(loop ((x 10)) 42)");
		
		Assertions.assertTrue(e instanceof Loop);
		
		Loop l = (Loop)e;
		
		Assertions.assertEquals(l,
				new Loop(new Tuple(new Symbol("x")), new LitInteger(42), new Tuple(new LitInteger(10))));
		
		Expression f = this.parseString("(recur 42)");
		
		Assertions.assertTrue(f instanceof Recur);
		
		Recur r = (Recur)f;
		
		Assertions.assertEquals(r, new Recur(new Tuple(new LitInteger(42))));
	}

	private Expression parseString(String s) throws AppendableException {
		List<Expression> l = Parser.read(s); 
		return l.get(l.size() - 1);
	}

	private void testParse(String parsedString, Expression expected) throws AppendableException {
		Expression parsed = this.parseString(parsedString);
		assertEquals(expected, parsed);
	}

}
