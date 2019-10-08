package testing;

import static org.junit.jupiter.api.Assertions.*;

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

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import interpretation.Environment;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;
import util.ThrowingFunction;

class TestComplex {
	
	@BeforeAll
	static void setUpBeforeClass() throws Exception {
		Main.init();
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
	void test() throws AppendableException {
		this.testInterpretString(
				"(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" +
				"(fact 5)",
				new LitInteger(120));
		
		this.testInterpretString(
				"(deftype Name)" +
				"(defrep Unstructured Name (lambda ((String:Native x)) x))" +
				"(defrep Structured Name (lambda ((String:Native x) (String:Native y)) (cons x y)))" +
				"((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Unstructured \"Jan Novak\"))",
				new LitString("unstructured"));
		
		this.testInterpretString(
				"((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Structured \"Jan\" \"Novak\"))",
				new LitString("structured"));
		
		this.testInterpretString(
				"(defconversion Name:Structured Name:Unstructured (lambda ((Name:Structured x)) (concat (car x) (cdr x))))" +
				"((lambda ((Name:Unstructured x)) x) (Name:Structured \"Jan\" \"Novak\"))", 
				new LitString("JanNovak"));
				
	}

	private List<Expression> parseString(String s, SemanticParser semanticParser) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();

		return exprsContext.val.stream().map(ThrowingFunction.wrapper(x -> semanticParser.parseNode(x)))
				.collect(Collectors.toList());
	}

	private void testInterpretString(String code, Expression expected) throws AppendableException {
		SemanticParser semanticParser = new SemanticParser();
		Environment topLevel = Environment.create(Environment.topLevelEnvironment, Main.initTopLevelEnvironment());
		Expression last = null;
		for(Expression e : this.parseString(code, semanticParser)) {
			Pair<Type, Substitution> p = e.infer(topLevel);
			last = e.interpret(topLevel);
		}
		
		if(!last.equals(expected)) {
			fail("Interpretation of " + code + " yields " + last + " were expecting " + expected);
		}
	}
}
