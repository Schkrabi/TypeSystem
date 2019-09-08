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
	SemanticParser semanticParser = new SemanticParser();

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
	}

	private List<Expression> parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();

		return exprsContext.val.stream().map(ThrowingFunction.wrapper(x -> semanticParser.parseNode(x)))
				.collect(Collectors.toList());
	}

	private void testInterpretString(String code, Expression expected) throws AppendableException {
		Environment topLevel = new Environment(null, Main.initTopLevelEnvironment());
		Expression last = null;
		for(Expression e : this.parseString(code)) {
			Pair<Type, Substitution> p = e.infer(topLevel);
			last = e.interpret(topLevel);
		}
		
		if(!last.equals(expected)) {
			fail("Interpretation of " + code + " yields " + last + " were expecting " + expected);
		}
	}
}
