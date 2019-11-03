package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Iterator;
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
import expression.LitComposite;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import interpretation.Environment;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import util.AppendableException;
import util.ClojureCodeGenerator;
import util.Pair;
import util.ThrowingFunction;

class TestComplex {

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
		Environment.initTopLevelEnvitonment();
		TypeEnvironment.initBasicTypes();
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
		this.testInterpretString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))" + "(fact 5)",
				new LitInteger(120));

		this.testInterpretString("(deftype Name)" + "(defrep Unstructured Name (String:Native))"
				+ "(defrep Structured Name (String:Native String:Native))"
				+ "((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Unstructured \"Jan Novak\"))",
				new LitString("unstructured"));

		this.testInterpretString(
				"((elambda (x) ((Name:Unstructured) \"unstructured\") ((Name:Structured) \"structured\")) (Name:Structured \"Jan\" \"Novak\"))",
				new LitString("structured"));

		this.testInterpretString(
				"(defconversion Name:Structured Name:Unstructured (lambda ((Name:Structured x)) (Name:Unstructured (concat (NameStructured-0 x) (NameStructured-1 x)))))"
						+ "((lambda ((Name:Unstructured x)) x) (Name:Structured \"Jan\" \"Novak\"))",
				new LitComposite(new Tuple(Arrays.asList(new LitString("JanNovak"))),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured"))));

		this.testInterpretString(
				"(deftype List)" + "(defrep Native List (x List))" + "(defrep Empty List ())"
						+ "(List:Native 1 (List:Native 2 (List:Empty)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(1),
								new LitComposite(
										new Tuple(Arrays.asList(new LitInteger(2),
												new LitComposite(Tuple.EMPTY_TUPLE,
														new TypeAtom(new TypeName("List"),
																new TypeRepresentation("Empty"))))),
										new TypeAtom(new TypeName("List"), TypeRepresentation.NATIVE)))),
						new TypeAtom(new TypeName("List"), TypeRepresentation.NATIVE)));

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
		Environment topLevel = Environment.topLevelEnvironment;
		Expression last = null;
		for (Expression e : this.parseString(code, semanticParser)) {
			Pair<Type, Substitution> p = e.infer(topLevel);
			last = e.interpret(topLevel);
		}

		if (!last.equals(expected)) {
			fail("Interpretation of " + code + " yields " + last + " were expecting " + expected);
		}
	}
	
	private void testClojureCompile(String code, String expected) throws AppendableException{
		SemanticParser semanticParser = new SemanticParser();
		List<Expression> l = this.parseString(code, semanticParser);
		StringBuilder s = new StringBuilder();
		
		Iterator<Expression> i = l.iterator();
		while(i.hasNext()) {
			s.append(i.next().toClojureCode());
			//TODO
		}
	}
}
