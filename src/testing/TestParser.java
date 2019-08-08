package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import expression.Application;
import expression.DefExpression;
import expression.ExceptionExpr;
import expression.Expression;
import expression.ExtendedLambda;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import expression.TypeConstructionLambda;
import expression.Variable;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import parser.SemanticNode;
import parser.SemanticNode.NodeType;
import parser.SemanticPair;
import semantic.InvalidNumberOfArgsException;
import semantic.SemanticParser;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import semantic.UndefinedTypeException;
import semantic.UnexpectedExpressionException;
import semantic.Validations;
import semantic.TypeVariablePair;
import types.TypeConcrete;
import types.TypeNotRecognizedException;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

@SuppressWarnings("deprecation")
class TestParser {
	SemanticParser semanticParser = new SemanticParser();

	private Expression parsed;
	private Expression expected;

	private static boolean initFlag = false;

	@BeforeEach
	void setUp() throws Exception {
		if (!initFlag) {
			Main.init();
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
						new Application(new Variable("+"),
								new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(1))))),
				new Pair<String, Expression>("(if #t x y)",
						new IfExpression(
								new Tuple(Arrays.asList(LitBoolean.TRUE, new Variable("x"), new Variable("y"))))),
				new Pair<String, Expression>("(deftype List)", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(defrep Functional List)", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(defconversion List:Functional List (lambda ((List:Functional l)) l))",
						Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(define one 1)",
						new DefExpression(new Variable("one"), new LitInteger(1))),
				new Pair<String, Expression>("(cons 1 2)",
						new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2)))),
				new Pair<String, Expression>("(error \"test\")", new ExceptionExpr(new LitString("test"))),
				new Pair<String, Expression>("(Int:String \"256\")",
						new Application(new Variable("Int:String"), new Tuple(Arrays.asList(new LitString("256"))))),
				new Pair<String, Expression>("3.141528", new LitDouble(3.141528)),
				new Pair<String, Expression>("#f", LitBoolean.FALSE)

		);

		for (Pair<String, Expression> p : testcases) {
			parsed = parseString(p.first);
			expected = p.second;
			if (!parsed.equals(expected)) {
				fail(parsed + " is not equal to " + expected);
			}
		}

		// (deftype list)
		if (!semanticParser.typeEnvironment.getType("List").isPresent()) {
			fail("deftype did not defined List type");
		}

		// (deftype Functional List)
		if (!semanticParser.typeEnvironment.getType("List", "Functional").isPresent()) {
			fail("defrep did not defined List:Functional type");
		}

		// (defconversion List:Functional List (lambda ((List:Functional l)) l))
		if (!semanticParser.typeEnvironment.getType("List", "Functional").get()
				.isConvertableTo(semanticParser.typeEnvironment.getType("List").get())) {
			fail("defconversion did not defined conversion List:Functional->List");
		}

		// Dummy object tests
		new Validations();
		new SemanticParserStatic();
	}

	@Test
	void testDefConstructor() throws AppendableException {
		// TODO types are actually created by side effect in parser. Is it viable?
		parseString("(deftype List)");
		parseString("(defrep Functional List)");

		Pair<String, Expression> p = new Pair<String, Expression>("(defconstructor List:Functional (lambda (x) x))",
				new DefExpression(new Variable("List:Functional"),
						new TypeConstructionLambda(semanticParser.typeEnvironment.getType("List", "Functional").get(),
								new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()))),
								new Variable("x"))));
		parsed = parseString(p.first);
		if (!(parsed instanceof DefExpression)) {
			fail(parsed + " is not a " + DefExpression.class.getName());
		}
		DefExpression parsedDefExpression = (DefExpression) parsed;
		expected = p.second;
		DefExpression expectedDefExpression = (DefExpression) p.second;
		if (!(parsedDefExpression.defined instanceof TypeConstructionLambda)) {
			fail(parsedDefExpression.defined + " is not instance of " + TypeConstructionLambda.class.getName());
		}
		TypeConstructionLambda parsedTypeConstructionLambda = (TypeConstructionLambda) parsedDefExpression.defined;
		TypeConstructionLambda expectedTypeConstructionLambda = (TypeConstructionLambda) expectedDefExpression.defined;

		if (!parsedTypeConstructionLambda.constructedType.equals(expectedTypeConstructionLambda.constructedType)
				|| !parsedTypeConstructionLambda.args.equals(expectedTypeConstructionLambda.args)
				|| !parsedTypeConstructionLambda.body.equals(expectedTypeConstructionLambda.body)
				|| parsedTypeConstructionLambda.argsType.size() != expectedTypeConstructionLambda.argsType.size()) {
			fail(parsed + " is not equal to " + expected);
		}
	}

	@Test
	void testLambda() throws AppendableException {
		Pair<String, Expression> p = new Pair<String, Expression>("(lambda (x) y)",
				new Lambda(new Tuple(Arrays.asList(new Variable("x"))), new Variable("y")));

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
	}

	@Test
	void testElambda() throws AppendableException {
		// (elambda (x) x ((Int:String) y))
		parsed = parseString("(elambda (x) x ((Int:String) y))");
		if (!(parsed instanceof ExtendedLambda)) {
			fail(parsed + " is not a " + ExtendedLambda.class.getName());
		}
		ExtendedLambda parsedElambda = (ExtendedLambda) parsed;
		Lambda expectedLambda = new Lambda(new Tuple(Arrays.asList(new Variable("x"))), new Variable("x"));

		Optional<Lambda> foundImplementation = parsedElambda.implementations.stream()
				.filter(x -> x.body.equals(new Variable("x"))).findAny();
		if (!foundImplementation.isPresent()) {
			fail("Implementation " + expectedLambda + " was not found in " + parsedElambda);
		}
		if (!foundImplementation.get().body.equals(expectedLambda.body)
				|| !foundImplementation.get().args.equals(expectedLambda.args)
				|| foundImplementation.get().argsType.size() != expectedLambda.argsType.size()) {
			fail(foundImplementation.get() + " is not equal to " + expectedLambda + " in " + parsedElambda);
		}

		expectedLambda = new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
				new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)), new Variable("y"));
		foundImplementation = parsedElambda.implementations.stream().filter(x -> x.body.equals(new Variable("y")))
				.findAny();
		if (!foundImplementation.isPresent()) {
			fail("Implementation " + expectedLambda + " was not found in " + parsedElambda);
		}
		if (!foundImplementation.get().equals(expectedLambda)) {
			fail(foundImplementation.get() + " is not equal to " + expectedLambda + " in " + parsedElambda);
		}
	}

	@Test
	public void testInvalidTypeConstructor() {
		Assertions.assertThrows(TypeNotRecognizedException.class, () -> parseString("(Int:Arabic 128)"));
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
				() -> parseString("(defrep list functional random)"));
		// Type is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defrep 1234 functional)"));
		// Representation is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defrep list 1234)"));

		// non-symbol token instead of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefRepList(Arrays.asList(SemanticNode.make(NodeType.STRING, "defrep"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
		// Bad symbol in place of lambda
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefRepList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));
	}

	@Test
	public void testParseTypeConstructionLambda() throws AppendableException {
		parseString("(deftype List)");
		parseString("(defrep Functional List)");
		// Invalid lambda
		Assertions.assertThrows(InvalidNumberOfArgsException.class,
				() -> parseString("(defconstructor List:Functional (lambda))"));
		Assertions.assertThrows(InvalidNumberOfArgsException.class,
				() -> parseString("(defconstructor List:Functional (lambda x y z))"));
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defconstructor List:Functional (lambda x y))"));
	}

	@Test
	public void testParseType() {
		// Wrong token in place of type
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x) x ((1234) x))"));

		// Not existing Type
		Assertions.assertThrows(UndefinedTypeException.class, () -> parseString("(defrep Person Cheesy)"));
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
	}

	@Test
	public void testParseDefConversion() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconversion x)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconversion x y z w)"));

		// Bad special form
		Assertions
				.assertThrows(UnexpectedExpressionException.class,
						() -> Validations.validateDefconversionList(Arrays.asList(
								SemanticNode.make(NodeType.SYMBOL, "fail"), SemanticNode.make(NodeType.SYMBOL, "x"),
								SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

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

		// Conversion has bad from type
		Assertions.assertThrows(AppendableException.class,
				() -> parseString("(defconversion Int Double (lambda ((Bool x)) x))"));
	}

	@Test
	public void testParseDefConstructor() {
		// Too few args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconstructor x)"));
		// Too many args
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconstructor x y z)"));

		// defconstructor token is not a symbol
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations
						.validateDefConstructorList(Arrays.asList(SemanticNode.make(NodeType.STRING, "defconstructor"),
								SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad symbol in defconversion
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> Validations.validateDefConstructorList(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, "fail"),
						SemanticNode.make(NodeType.SYMBOL, "x"), SemanticNode.make(NodeType.SYMBOL, "x"))));

		// Bad constructed type
		Assertions.assertThrows(UnexpectedExpressionException.class,
				() -> parseString("(defconstructor 1234 (lambda (x) x))"));

		// Constructor is not lambda
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defconstructor Int x)"));
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
				() -> semanticParser.parseNode(SemanticNode.make(NodeType.UNUSED, new Object())));
	}

	@Test
	public void testParseSpecialForm() {
		Assertions.assertThrows(AppendableException.class, () -> semanticParser
				.parseNodelist(Arrays.asList(SemanticNode.make(NodeType.SYMBOL, SemanticParserStatic.UNUSED))));
	}

	@Test
	public void testSemanticParserStatic() throws AppendableException {
		SemanticNode n = SemanticNode.make(NodeType.SYMBOL, "fail");
		if (SemanticParserStatic.isSpecialForm(n)) {
			fail(n.toString() + " is not a special form " + n.getClass().getName());
		}

		List<TypeVariablePair> l = Arrays.asList(new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")),
				new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")),
				new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")));
		if (!SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is fully typed!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")),
				new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")));
		if (SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is not fully typed!");
		}

		l = Arrays.asList(new TypeVariablePair(TypeConcrete.TypeInt, new Variable("x")));
		if (!SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is fully typed!");
		}
		if (SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is typed!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Variable("x")));
		if (SemanticParserStatic.isArgListFullyTyped(l)) {
			fail(l.toString() + " is not fully typed!");
		}
		if (!SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is untyped!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x")));
		if (SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is not untyped!");
		}

		l = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(new TypeVariable("x"), new Variable("x")));
		if (!SemanticParserStatic.isArgListUntypped(l)) {
			fail(l.toString() + " is untyped!");
		}

		List<TypeVariablePair> k = Arrays.asList(new TypeVariablePair(new TypeVariable("x"), new Variable("x")),
				new TypeVariablePair(new TypeVariable("x"), new Variable("x")));
		if (!SemanticParserStatic.listTail(l)
				.equals(k)) {
			fail(k.toString() + " is a tail of " + l.toString());
		}
		
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> SemanticParserStatic.parseArgsList(Arrays.asList(SemanticNode.make(NodeType.BOOL, new Boolean(false)))));
	}
	
	@Test
	public void testVariableTypePair() {
		TypeVariablePair p = new TypeVariablePair(TypeConcrete.TypeBool, new Variable("x"));
		p.toString();
		
		Integer i = new Integer(128);
		if(p.equals(i)){
			fail(p.toString() + " is not equal to " + i.toString());
		}
		
		TypeVariablePair q = new TypeVariablePair(TypeConcrete.TypeBool, new Variable("y"));
		if(p.equals(q)) {
			fail(p.toString() + " not equals to " + q.toString());
		}
		
		q = new TypeVariablePair(TypeConcrete.TypeInt, new Variable("x"));
		if(p.equals(q)) {
			fail(p.toString() + " not equals to " + q.toString());
		}
	}
	
	@Test
	public void testTypeEnvironment() throws AppendableException {
		TypeEnvironment typeEnv = new TypeEnvironment();
		
		typeEnv.addType("List");
		typeEnv.addRepresentation("List", "Functional");
		typeEnv.addType("Test");
		typeEnv.addRepresentation("Test", "Functional");
		
		typeEnv.getType("List", "Functional");
		
		if(!typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, "List"))){
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, \"List\")) == true");
		}
		if(!typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair("List", "Functional")))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair(\"List\", \"Functional\"))) == true");
		}
		if(typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, "kawabanga"))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.SYMBOL, \"kawabanga\")) == false");
		}
		if(typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair("List", "kawabanga")))) {
			fail("Exprected typeEnv.isType(SemanticNode.make(NodeType.PAIR, new SemanticPair(\"List\", \"kawabanga\"))) == false");
		}
		if(typeEnv.isType(SemanticNode.make(NodeType.INT, new Integer(128)))) {
			fail("Expected typeEnv.isType(SemanticNode.make(NodeType.INT, new Integer(128))) = false");
		}
		
		typeEnv.getConstructor(TypeRepresentation.TypeIntRoman, 1);
		Assertions.assertThrows(NoSuchElementException.class, () -> typeEnv.getConstructor(TypeRepresentation.TypeIntRoman, 3));
		Assertions.assertThrows(NoSuchElementException.class, () -> typeEnv.getConstructor(new TypeConcrete("fail"), 1));
		
		Assertions.assertThrows(AppendableException.class, () -> typeEnv.addType("Int"));
		Assertions.assertThrows(AppendableException.class, () -> typeEnv.addRepresentation("Int", "Roman"));
		
		Assertions.assertThrows(UndefinedTypeException.class, () -> typeEnv.addConversion(new TypeConcrete("fail"), TypeConcrete.TypeInt, new TypeConstructionLambda(null, null, null, expected)));
		Assertions.assertThrows(UndefinedTypeException.class, () -> typeEnv.addConversion(TypeConcrete.TypeInt, new TypeConcrete("fail"), new TypeConstructionLambda(null, null, null, expected)));
		
		typeEnv.addConstructor(typeEnv.getType("List", "Functional").get(), new TypeConstructionLambda(typeEnv.getType("List", "Functional").get(), new Tuple(Arrays.asList(new Variable("x"))), new TypeTuple(Arrays.asList(new TypeVariable("x"))), expected));
		typeEnv.addConstructor(typeEnv.getType("List", "Functional").get(), new TypeConstructionLambda(typeEnv.getType("List", "Functional").get(), Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE, expected));
		Assertions.assertThrows(AppendableException.class, () -> typeEnv.addConstructor(typeEnv.getType("List", "Functional").get(), new TypeConstructionLambda(typeEnv.getType("List", "Functional").get(), Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE, expected)));
	}
	
	@Test
	public void testSemanticNode() {
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new SemanticPair("fail", "fail")));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, new Integer(128)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Double(3.14)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, "fail"));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Boolean(true)));
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, Arrays.asList(128)));
		
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asSymbol());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asPair());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.SYMBOL, "fail").asInt());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asDouble());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asString());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asBool());
		Assertions.assertThrows(AppendableException.class, () -> SemanticNode.make(NodeType.INT, new Integer(128)).asList());
		
	}

	private Expression parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return semanticParser.parseNode(exprsContext.val.get(0));
	}

}
