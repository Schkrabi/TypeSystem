package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import abstraction.ExtendedLambda;
import abstraction.Function;
import abstraction.Lambda;
import application.AndExpression;
import application.AbstractionApplication;
import application.DefConversionExpression;
import application.DefExpression;
import application.DefRepresentationExpression;
import application.DefTypeExpression;
import application.ExceptionExpr;
import application.IfExpression;
import application.OrExpression;
import expression.Expression;
import expression.Tuple;
import expression.Symbol;
import interpretation.Environment;
import literal.LitBoolean;
import literal.LitComposite;
import literal.LitDouble;
import literal.LitInteger;
import literal.LitString;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import parser.SemanticNode;
import parser.SemanticNode.NodeType;
import parser.SemanticPair;
import semantic.DuplicateConversionException;
import semantic.InvalidNumberOfArgsException;
import semantic.SemanticParser;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import semantic.UnexpectedExpressionException;
import semantic.UserException;
import semantic.Validations;
import semantic.TypeVariablePair;
import semantic.UndefinedTypeException;
import types.ConversionException;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.Pair;

@SuppressWarnings("deprecation")
class TestParser {

	private Expression parsed;
	private Expression expected;

	private static boolean initFlag = false;

	@BeforeEach
	void setUp() throws Exception {
		if (!initFlag) {
			TypeEnvironment.initBasicTypes();
			Environment.initTopLevelEnvitonment();
			initFlag = true;
		}
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testSemanticSimple() throws AppendableException {
		List<Pair<String, Expression>> testcases = Arrays.asList(
				new Pair<String, Expression>("()", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(+ 1 1)",
						new AbstractionApplication(new Symbol("+"),
								new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(1))))),
				new Pair<String, Expression>("(if #t x y)",
						new IfExpression(LitBoolean.TRUE, new Symbol("x"), new Symbol("y"))),
				new Pair<String, Expression>("(deftype Name)", new DefTypeExpression(new TypeName("Name"))),
				new Pair<String, Expression>("(defrep Unstructured Name (String:Native))",
						new DefRepresentationExpression(new TypeName("Name"), new TypeRepresentation("Unstructured"),
								Arrays.asList(SemanticNode.make(SemanticNode.NodeType.PAIR,
										new SemanticPair("String", "Native"))))),
				new Pair<String, Expression>("(defrep Structured Name (String:Native String:Native))",
						new DefRepresentationExpression(new TypeName("Name"), new TypeRepresentation("Structured"),
								Arrays.asList(
										SemanticNode.make(SemanticNode.NodeType.PAIR,
												new SemanticPair("String", "Native")),
										SemanticNode.make(SemanticNode.NodeType.PAIR,
												new SemanticPair("String", "Native"))))),
				new Pair<String, Expression>(
						"(defconversion Name:Structured Name:Unstructured (lambda ((Name:Structured name)) "
								+ "(Name:Unstructured (concat (get 0 name) (concat \" \" (get 1 name))))))",
						new DefConversionExpression(
								new TypeAtom(new TypeName("Name"), new TypeRepresentation("Structured")),
								new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
								new Lambda(new Tuple(Arrays.asList(new Symbol("name"))),
										new TypeTuple(Arrays.asList(new TypeAtom(new TypeName("Name"),
												new TypeRepresentation("Structured")))),
										new AbstractionApplication(new Symbol("Name:Unstructured"), new Tuple(Arrays
												.asList(new AbstractionApplication(new Symbol("concat"), new Tuple(Arrays.asList(
														new AbstractionApplication(new Symbol("get"),
																new Tuple(Arrays.asList(new LitInteger(0),
																		new Symbol("name")))),
														new AbstractionApplication(new Symbol("concat"), new Tuple(Arrays.asList(
																new LitString(" "),
																new AbstractionApplication(new Symbol("get"),
																		new Tuple(Arrays.asList(new LitInteger(1),
																				new Symbol("name")))))))))))))))),
				new Pair<String, Expression>("(define one 1)",
						new DefExpression(new Symbol("one"), new LitInteger(1))),
				new Pair<String, Expression>("(cons 1 2)",
						new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2)))),
				new Pair<String, Expression>("(error \"test\")", new ExceptionExpr(new LitString("test"))),
				new Pair<String, Expression>("(Int:String \"256\")",
						new AbstractionApplication(new Symbol("Int:String"), new Tuple(Arrays.asList(new LitString("256"))))),
				new Pair<String, Expression>("3.141528", new LitDouble(3.141528)),
				new Pair<String, Expression>("#f", LitBoolean.FALSE),
				new Pair<String, Expression>("(and #t #f)",
						new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE)))),
				new Pair<String, Expression>("(and)", new AndExpression(Tuple.EMPTY_TUPLE)),
				new Pair<String, Expression>("(or #t #f)",
						new OrExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE)))),
				new Pair<String, Expression>("(or)", new OrExpression(Tuple.EMPTY_TUPLE))

		);

		for (Pair<String, Expression> p : testcases) {
			Expression parsed = this.parseString(p.first);
			Expression expected = p.second;
			if (!parsed.equals(expected)) {
				fail("Parse error expected " + expected.toString() + " got " + parsed.toString());
			}
		}

		// Dummy object tests
		new Validations();
		new SemanticParserStatic();
	}

	@Test
	void testLambda() throws AppendableException {
		Pair<String, Expression> p = new Pair<String, Expression>("(lambda (x) y)",
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))), new TypeTuple(Arrays.asList(new TypeVariable("_x"))), new Symbol("y")));

		parsed = parseString(p.first);
		if (!(parsed instanceof Lambda)) {
			fail(parsed + " is not a " + Lambda.class.getName());
		}
		Lambda parsedLambda = (Lambda) parsed;
		expected = p.second;
		Lambda expectedLambda = (Lambda) p.second;
		if (!parsedLambda.args.equals(expectedLambda.args) || !parsedLambda.body.equals(expectedLambda.body)
				|| parsedLambda.argsType.size() != expectedLambda.argsType.size()) {
			fail(parsed + " is not equal to " + expected);
		}

		p = new Pair<String, Expression>("(lambda ((String x) (Int y)) x)",
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeString, TypeAtom.TypeInt)), new Symbol("x")));
		parsedLambda = (Lambda) parseString(p.first);
		expectedLambda = (Lambda) p.second;
		if (!parsedLambda.args.equals(expectedLambda.args) || !parsedLambda.body.equals(expectedLambda.body)
				|| parsedLambda.argsType.size() != expectedLambda.argsType.size()) {
			fail(parsed + " is not equal to " + expected);
		}
	}

	@Test
	void testElambda() throws AppendableException {
		// (elambda (x) ((Int:String) y))
		parsed = parseString("(elambda ((Int x)) ((Int:String) y))");
		if (!(parsed instanceof ExtendedLambda)) {
			fail(parsed + " is not a " + ExtendedLambda.class.getName());
		}
	}

	@Test
	public void testValidateElambda() {
		// Too few arguments
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x))"));

		// Second argument is not a list
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(elambda x x)"));

		// Badly formed implementation
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(elambda (x) x x)"));
	}

	@Test
	public void testValidateIf() {
		// Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if)"));

		// Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if x y z w)"));
	}

	@Test
	public void testValidateLambda() {
		// Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda)"));
		// Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda x y z)"));
		// Badly formed argument list
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda x y)"));

		// non-symbol token instead of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateLambdaList(Arrays.asList(SemanticNode.make(NodeType.STRING, "lambda"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
		// Bad symbol in place of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateLambdaList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	public void testParseDefType() {
		// Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(deftype)"));
		// Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(deftype animal dog)"));
		// Defining non-symbol type
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(deftype 1234)"));

		// non-symbol token instead of deftype
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> Validations.validateDefTypeList(
				Arrays.asList(SemanticNode.make(NodeType.STRING, "deftype"), SemanticNode.make(NodeType.SYMBOL, "x"))));
		// Bad symbol in place of deftype
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> Validations.validateDefTypeList(
				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	public void testParseDefRep() {
		// Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defrep)"));
		// Too many argument
		Assertions.assertThrows(InvalidNumberOfArgsException.class,
				() -> parseString("(defrep list functional random failed)"));
		// Type is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defrep 1234 functional constructor)"));
		// Representation is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defrep list 1234 constructor)"));

		// non-symbol token instead of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefRepList(Arrays.asList(SemanticNode.make(NodeType.STRING, "defrep"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"),
						SemanticNode.make(NodeType.SYMBOL, "fail"))));
		// Bad symbol in place of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefRepList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"),
						SemanticNode.make(NodeType.SYMBOL, "fail"))));

		Assertions.assertThrows(AppendableException.class, () -> parseString("(defrep list functional fail)"));
		Assertions.assertThrows(AppendableException.class,
				() -> parseString("(defrep list functional (something (else)))"));
	}

	@Test
	public void testParseType() {
		// Wrong token in place of type
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x) x ((1234) x))"));
	}

	@Test
	public void testParseVariableTypePair() {
		// Badly formed variable-type pair
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(lambda ((Integer String x)) x)"));

		// Non-type on type position
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda ((1234 x)) x)"));

		// Not symbol on variable position
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda ((Integer 1234)) x)"));

		// Pair is not a list
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateVariableTypePair(SemanticNode.make(NodeType.SYMBOL, "x")));
	}

	@Test
	public void testParseImplementation() {
		// Too many arugments in implementation
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x) x ((Integer) x y))"));

		// Badly formed type list
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(elambda (x) x (1234 x))"));

		Assertions.assertThrows(AppendableException.class,
				() -> Validations.validateImplementation(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
		Assertions.assertThrows(AppendableException.class, () -> Validations.validateImplementation(
				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	public void testParseDefConversion() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconversion x)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconversion x y z w)"));

		// Bad special form
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefconversionList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"),
						SemanticNode.make(NodeType.SYMBOL, "x"))));

		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefconversionList(Arrays.asList(
						SemanticNode.make(NodeType.INT, new Integer(1234)), SemanticNode.make(NodeType.SYMBOL, "x"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad From Type
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defconversion 1234 Int (lambda (x) x))"));
		// Bad To Type
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defconversion Int 1234 (lambda (x) x))"));
		// Conversion is not lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defconversion Int Double 1234)"));

		// Conversion takes multiple arguments
		Assertions.assertThrows(AppendableException.class,
				() -> parseString("(defconversion Int Double (lambda (x y) x))"));
	}

	@Test
	public void testParseDefine() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(define x)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(define x y z)"));

		// define token is not a symbol
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefineList(Arrays.asList(SemanticNode.make(NodeType.STRING, "define"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad symbol in define
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefineList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Defined is not a symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(define 1234 1234)"));
	}

	@Test
	public void testParseCons() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(cons x)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(cons x y z)"));

		// cons token is not a symbol
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateConsList(Arrays.asList(SemanticNode.make(NodeType.STRING, "cons"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad symbol in cons
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateConsList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	public void testParseError() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(error)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(error x y )"));

		// error token is not a symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> Validations.validateErrorList(
				Arrays.asList(SemanticNode.make(NodeType.STRING, "error"), SemanticNode.make(NodeType.STRING, "xs"))));

		// Bad symbol in error
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> Validations.validateErrorList(
				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"), SemanticNode.make(NodeType.STRING, "x"))));

		// Error msg is not a string
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(error 1234)"));
	}

	@Test
	public void testParseNode() {
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticParser.parseNode(SemanticNode.make(NodeType.UNUSED, new Object())));
	}

	@Test
	public void testParseSpecialForm() {
		Assertions.assertThrows(AppendableException.class, () -> SemanticParser
				.parseNodelist(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, SemanticParserStatic.UNUSED))));
	}

	@Test
	public void testSemanticParserStatic() throws AppendableException {
		SemanticNode n = SemanticNode.make(NodeType.SYMBOL, "fail");
		if (SemanticParserStatic.isSpecialForm(n)) {
			fail(n.toString() + " is not a special form " + n.getClass().getName());
		}

		List<TypeVariablePair> l = Arrays.asList(new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
		if (!SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is fully typed!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")),
				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
		if (SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is not fully typed!");
		}

		l = Arrays.asList(new TypeVariablePair(TypeAtom.TypeInt, new Symbol("x")));
		if (!SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is fully typed!");
		}
		if (SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is typed!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
		if (SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is not fully typed!");
		}
		if (!SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is untyped!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x")));
		if (SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is not untyped!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
		if (!SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is untyped!");
		}

		List<TypeVariablePair> k = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Symbol("x")),
				new TypeVariablePair(new TypeVariable("x"), new Symbol("x")));
		if (!SemanticParserStatic.listTail(l).equals(k)) {
			fail(k.toString() + " is a tail of " + l.toString());
		}

		Tuple t = SemanticParserStatic.parseArgsList(
				Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "y")));
		Tuple exp = new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y")));
		if (!t.equals(exp)) {
			fail("SemanticParserStatic.parseArgsList not working as expected. Got " + t + " expected " + exp);
		}

		Assertions.assertThrows(UnexpectedExpressionException.class, () -> SemanticParserStatic
				.parseArgsList(Arrays.asList(SemanticNode.make(NodeType.BOOL, new Boolean(false)))));
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	public void testVariableTypePair() {
		TypeVariablePair p = new TypeVariablePair(TypeAtom.TypeBool, new Symbol("x"));
		p.toString();

		Integer i = new Integer(128);
		if (p.equals(i)) {
			fail(p.toString() + " is not equal to " + i.toString());
		}

		TypeVariablePair q = new TypeVariablePair(TypeAtom.TypeBool, new Symbol("y"));
		if (p.equals(q)) {
			fail(p.toString() + " not equals to " + q.toString());
		}

		q = new TypeVariablePair(TypeAtom.TypeInt, new Symbol("x"));
		if (p.equals(q)) {
			fail(p.toString() + " not equals to " + q.toString());
		}
	}

	@Test
	public void testTypeEnvironment() throws AppendableException {
		TypeEnvironment typeEnv = TypeEnvironment.singleton;

		typeEnv.addType(new TypeName("List"));
		typeEnv.addRepresentation(new TypeAtom(new TypeName("List"), new TypeRepresentation("Functional")),
				new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
						Environment.topLevelEnvironment));
		typeEnv.addType(new TypeName("Test"));
		typeEnv.addRepresentation(new TypeAtom(new TypeName("Test"), new TypeRepresentation("Functional")),
				new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
						Environment.topLevelEnvironment));

		typeEnv.getType("List", "Functional");

		if (!typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, "List"))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, \"List\")) == true");
		}
		if (!typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair("List", "Functional")))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair(\"List\", \"Functional\"))) == true");
		}
		if (typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, "kawabanga"))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, \"kawabanga\")) == false");
		}
		if (typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair("List", "kawabanga")))) {
			fail("Exprected typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair(\"List\", \"kawabanga\"))) == false");
		}
		if (typeEnv.isType(SemanticNode.make(NodeType.INT, new Integer(128)))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.INT, new Integer(128))) = false");
		}

		typeEnv.getConstructor(TypeAtom.TypeIntRoman);
		Assertions.assertThrows(NoSuchElementException.class,
				() -> typeEnv.getConstructor(new TypeAtom(new TypeName("fail"), TypeRepresentation.WILDCARD)));

		Assertions.assertThrows(AppendableException.class,
				() -> typeEnv.addRepresentation(new TypeAtom(new TypeName("Int"), new TypeRepresentation("Roman")),
						new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
								Environment.topLevelEnvironment)));

		Assertions.assertThrows(AppendableException.class, () -> typeEnv.addConversion(TypeAtom.TypeIntNative,
				TypeAtom.TypeStringNative,
				new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, expected, Environment.topLevelEnvironment)));

		typeEnv.getType(SemanticNode.make(NodeType.PAIR, new SemanticPair("Int", "Roman")));
		typeEnv.getType(SemanticNode.make(NodeType.SYMBOL, "Int"));

		Assertions.assertThrows(DuplicateConversionException.class,
				() -> TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
						new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
								Environment.topLevelEnvironment)));

		typeEnv.convertTo(new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString),
				TypeAtom.TypeIntString, TypeAtom.TypeIntRoman);
		Assertions.assertThrows(ConversionException.class, () -> TypeEnvironment.singleton
				.convertTo(Expression.EMPTY_EXPRESSION, TypeAtom.TypeStringNative, TypeAtom.TypeIntNative));
	}

	@Test
	public void testSemanticNode() {
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new SemanticPair("fail", "fail")));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, new Integer(128)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Double(3.14)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Boolean(true)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Arrays.asList(128)));

		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asSymbol());
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asPair());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, "fail").asInt());
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asDouble());
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asString());
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asBool());
		Assertions.assertThrows(AppendableException.class,
				() -> SemanticNode.make(NodeType.INT, new Integer(128)).asList());

	}

	@Test
	void testExceptions() {
		new UserException("test");
		new DuplicateConversionException(TypeAtom.TypeBool, TypeAtom.TypeBoolNative,
				new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
						Environment.topLevelEnvironment),
				new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
						Environment.topLevelEnvironment));
		new UndefinedTypeException("fail");
	}

	private Expression parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return SemanticParser.parseNode(exprsContext.val.get(0));
	}

}
