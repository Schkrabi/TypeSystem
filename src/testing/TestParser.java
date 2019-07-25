package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import expression.Application;
import expression.DefExpression;
import expression.Expression;
import expression.ExtendedLambda;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitInteger;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import types.Type;
import types.TypeRepresentation;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

class TestParser {
	SemanticParser semanticParser = new SemanticParser();

	private Environment env;
	private Expression parsed;
	private Expression expected;

	@BeforeEach
	void setUp() throws Exception {
		Main.init();
		env = Main.initTopLevelEnvironment();
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testSemanticParser() throws AppendableException {
		Pair<String, Expression>[] testcases = new Pair[] {
			new Pair<String, Expression>("()", Expression.EMPTY_EXPRESSION),
			new Pair<String, Expression>("(+ 1 1)", new Application(new Variable("+"),
					new Tuple(new Expression[] { new LitInteger(1), new LitInteger(1) }))),
			new Pair<String, Expression>("lambda (x) y)", new Lambda(new Tuple(new Expression[] { new Variable("x")}), new Variable("y"))),
			new Pair<String, Expression>("(elambda (x) x ((Int:String) y))", new ExtendedLambda((Set<Lambda>) new HashSet<Lambda>(Arrays.asList(
					new Lambda(new Tuple(new Expression[] {new Variable("x")}), new Variable("x")), 
					new Lambda(new Tuple(new Expression[] {new Variable("x")}), new TypeTuple(new Type[] { TypeRepresentation.TypeIntString}), new Variable("y")))))),
			new Pair<String, Expression>("(if #t x y)", new IfExpression(new Tuple(new Expression[] { LitBoolean.TRUE, new Variable("x"), new Variable("y")}))),
			new Pair<String, Expression>("(defconstructor List:Functional (lambda (x) x))",
					new DefExpression(new Variable("List:Functional"), new Lambda(new Tuple(new Expression[] { new Variable("x") }), new Variable("x")))),
			new Pair<String, Expression>("(define one 1)", new DefExpression(new Variable("one"), new LitInteger(1)))
							
		};
		
		for(Pair<String, Expression> p : testcases) {
			parsed = parseString(p.first);
			expected = p.second;
			if (!parsed.equals(expected)) {
				fail(parsed + " is not equal to " + expected);
			}
		}
		
		//(deftype list)
		parsed = parseString("(deftype List)");
		expected = Expression.EMPTY_EXPRESSION;
		if(!parsed.equals(expected)) {
			fail(parsed + " is not equal to " + expected);
		}
		if(!semanticParser.typeEnvironment.getType("List").isPresent()) {
			fail("deftype did not defined List type");
		}
		
		//(deftype Functional List)
		parsed = parseString("(deftype Functional List)");
		expected = Expression.EMPTY_EXPRESSION;
		if(!parsed.equals(expected)) {
			fail(parsed + " is not equal to " + expected);
		}
		if(!semanticParser.typeEnvironment.getType("List", "Functional").isPresent()) {
			fail("defrep did not defined List:Functional type");
		}
		
		//(defconversion List:Functional List (lambda ((List:Functional l)) l))
		parsed = parseString("(defconversion List:Functional List (lambda ((List:Functional l)) l))");
		expected = Expression.EMPTY_EXPRESSION;
		if(!parsed.equals(expected)) {
			fail(parsed + " is not equal to " + expected);
		}
		if(!semanticParser.typeEnvironment.getType("List", "Functional").get().isConvertableTo(semanticParser.typeEnvironment.getType("List").get())) {
			fail("defconversion did not defined conversion List:Functional->List");
		}

	}

	private Expression parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return semanticParser.parseNode(exprsContext.val.get(0));
	}

}
