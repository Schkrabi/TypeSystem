package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

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
import interpretation.Environment;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.InvalidNumberOfArgsException;
import semantic.SemanticParser;
import semantic.UndefinedTypeException;
import semantic.UnexpectedExpressionException;
import types.Type;
import types.TypeNotRecognizedException;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

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
		Pair<String, Expression>[] testcases = new Pair[] {
				new Pair<String, Expression>("()", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(+ 1 1)",
						new Application(new Variable("+"),
								new Tuple(Arrays.asList( new LitInteger(1), new LitInteger(1) )))),
				new Pair<String, Expression>("(if #t x y)",
						new IfExpression(
								new Tuple(Arrays.asList(LitBoolean.TRUE, new Variable("x"), new Variable("y") )))),
				new Pair<String, Expression>("(deftype List)", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(defrep Functional List)", Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(defconversion List:Functional List (lambda ((List:Functional l)) l))",
						Expression.EMPTY_EXPRESSION),
				new Pair<String, Expression>("(define one 1)",
						new DefExpression(new Variable("one"), new LitInteger(1))),
				new Pair<String, Expression>("(cons 1 2)",
						new Tuple(Arrays.asList( new LitInteger(1), new LitInteger(2) ))),
				new Pair<String, Expression>("(error \"test\")", new ExceptionExpr(new LitString("test"))),
				new Pair<String, Expression>("(Int:String \"256\")",
						new Application(new Variable("Int:String"),
								new Tuple(Arrays.asList( new LitString("256") )))),
				new Pair<String, Expression>("3.141528", new LitDouble(3.141528)),
				new Pair<String, Expression>("#f", LitBoolean.FALSE)

		};

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

	}

	@Test
	void testDefConstructor() throws AppendableException {
		// TODO types are actually created by side effect in parser. Is it viable?
		parseString("(deftype List)");
		parseString("(defrep Functional List)");

		Pair<String, Expression> p = new Pair<String, Expression>("(defconstructor List:Functional (lambda (x) x))",
				new DefExpression(new Variable("List:Functional"),
						new TypeConstructionLambda(semanticParser.typeEnvironment.getType("List", "Functional").get(),
								new Tuple(Arrays.asList( new Variable("x") )),
								new TypeTuple(Arrays.asList( new TypeVariable(NameGenerator.next()) )),
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
				new Lambda(new Tuple(Arrays.asList( new Variable("x") )), new Variable("y")));

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
		Lambda expectedLambda = new Lambda(new Tuple(Arrays.asList( new Variable("x") )), new Variable("x"));

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

		expectedLambda = new Lambda(new Tuple(Arrays.asList( new Variable("x") )),
				new TypeTuple(Arrays.asList( TypeRepresentation.TypeIntString )), new Variable("y"));
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
		//Too few arguments
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x))"));
		
		//Second argument is not a list
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(elambda x x)"));
		
		//Badly formed implementation
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(elambda (x) x x)"));
	}
	
	@Test
	public void testValidateIf() {
		//Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if)"));
		
		//Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(if x y z w)"));
	}
	
	@Test
	public void testValidateLambda() {
		//Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda)"));
		//Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(lambda x y z)"));
		//Badly formed argument list
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(lambda x y)"));
	}
	
	@Test
	public void testParseDefType() {
		//Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(deftype)"));
		//Too many arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(deftype animal dog)"));
		//Defining non-symbol type
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(deftype 1234)"));
	}
	
	@Test
	public void testParseDefRep() {
		//Too few arguments
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defrep)"));
		//Too many argument
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defrep list functional random)"));
		//Type is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defrep 1234 functional)"));
		//Representation is not symbol
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defrep list 1234)"));
	}
	
	@Test
	public void testParseTypeConstructionLambda() throws AppendableException {
		parseString("(deftype List)");
		parseString("(defrep Functional List)");
		//Invalid lambda 
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconstructor List:Functional (lambda))"));
		Assertions.assertThrows(InvalidNumberOfArgsException.class, () -> parseString("(defconstructor List:Functional (lambda x y z))"));
		Assertions.assertThrows(UnexpectedExpressionException.class, () -> parseString("(defconstructor List:Functional (lambda x y))"));
	}
	
	@Test
	public void testParseType() {
		//Wrong token in place of type
		Assertions.assertThrows(AppendableException.class, () -> parseString("(elambda (x) x ((1234) x))"));
		
		//Not existing Type
		Assertions.assertThrows(UndefinedTypeException.class, () -> parseString("(defrep Person Cheesy)"));
	}
	
	@Test
	public void testParseVariableTypePair() {
		
	}

	private Expression parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return semanticParser.parseNode(exprsContext.val.get(0));
	}

}
