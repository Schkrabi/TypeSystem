package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import velka.core.application.DefineSymbol;
import velka.core.application.ExceptionExpr;
import velka.core.application.Extend;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.Let;
import velka.core.application.Loop;
import velka.core.application.OrExpression;
import velka.core.application.Recur;
import velka.core.exceptions.DuplicateConversionException;
import velka.core.exceptions.UndefinedTypeException;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitBoolean;
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
import velka.util.Pair;

class TestParser extends VelkaTest{
	@Test
	@DisplayName("Test Semantic Simple")
	void testSemanticSimple() throws AppendableException {
		this.testParse("nil", Expression.EMPTY_EXPRESSION);
		this.testParse("(+ 1 1)", new AbstractionApplication(new Symbol("+"),
				new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(1)))));
		this.testParse("(if #t x y)", new IfExpression(LitBoolean.TRUE, new Symbol("x"), new Symbol("y")));
		this.testParse("(constructor Name:Unstructured ((String:Native x)) x)",
				new DefineConstructor(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Symbol("x"))));
		this.testParse("(constructor Name:Structured ((String:Native x) (String:Native y)) (tuple x y))",
				new DefineConstructor(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Structured")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
								new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))))));
		this.testParse(
				"(conversion Name:Structured Name:Unstructured (name) "
						+ "(construct Name:Unstructured \"Test\"))",
				new DefineConversion(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Structured")),
						new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
						new Tuple(Arrays.asList(new Symbol("name"))),
						new Construct(new TypeAtom(new TypeName("Name"), new TypeRepresentation("Unstructured")),
								new Tuple(Arrays.asList(new LitString("Test"))))));

		this.testParse("(define one 1)", new DefineSymbol(new Symbol("one"), new LitInteger(1)));
		this.testParse("(tuple 1 2)", new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))));
		this.testParse("(error \"test\")", new ExceptionExpr(new LitString("test")));
		this.testParse("3.141528", new LitDouble(3.141528));
		this.testParse("#f", LitBoolean.FALSE);
		this.testParse("(and #t #f)", new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE))));
		this.testParse("(or #t #f)", new OrExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE))));
		this.testParse("(construct Int:String \"42\")",
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("42")))));
		this.testParse("(convert Int:Roman Int:String x)",
				new Convert(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString, new Symbol("x")));

		this.testParse("(lambda (((Int:Native String:Native Bool:Native) t)) t)", new Lambda(
				new Tuple(Arrays.asList(new Symbol("t"))),
				new TypeTuple(Arrays.asList(new TypeTuple(
						Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative, TypeAtom.TypeBoolNative)))),
				new Symbol("t")));

		this.testParse("(lambda ((((Int:Native) #> Int:Native) f)) f)",
				new Lambda(new Tuple(new Symbol("f")),
						new TypeTuple(new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeIntNative)),
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
		this.testParse("(get (tuple 42 \"foo\") 0)",
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

		this.testParse("(eapply + (tuple 1 2))",
				new AbstractionApplication(new Symbol("+"), new Tuple(new LitInteger(1), new LitInteger(2))));

		Lambda impl = new Lambda(new Tuple(new Symbol("x")), new TypeTuple(TypeAtom.TypeIntNative), new Symbol("x"));
		List<Lambda> impls = new ArrayList<Lambda>();
		impls.add(impl);
		this.testParse("(extend foo bar)",
				new Extend(new Symbol("foo"), new Symbol("bar")));
		this.testParse("(extend foo bar baz)",
				new Extend(new Symbol("foo"), new Symbol("bar"), new Symbol("baz")));
	}

	@Test
	@DisplayName("Test Lambda")
	void testLambda() throws AppendableException {
		String sExpr = "(lambda (x) y)";
		Expression expr = new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
				new TypeTuple(Arrays.asList(new TypeVariable("_x"))), new Symbol("y"));

		Expression parsed = parseString(sExpr).get(0);
		assertTrue(parsed instanceof Lambda);

		Lambda parsedLambda = (Lambda) parsed;
		Lambda expectedLambda = (Lambda) expr;

		assertEquals(parsedLambda, expectedLambda);

		assertEquals(parseString("(lambda ((String x) (Int y)) x)").get(0),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeString, TypeAtom.TypeInt)), new Symbol("x")));
	}

	@Test
	@DisplayName("Test Extended Lambda")
	void testElambda() throws AppendableException {
		Expression parsed = parseString("(extended-lambda (Int))").get(0);
		assertTrue(parsed instanceof ExtendedLambda);
	}

	@Test
	@DisplayName("Test Exceptions")
	void testExceptions() {
		Environment env = TopLevelEnvironment.instantiate();
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
		this.parseString("(let-type (A) (lambda ((A x)) (let-type (A) (lambda ((A y)) x))))")
				.get(0);

//		TypeVariable outer = (TypeVariable) ((Lambda) e).argsType.get(0);
//		TypeVariable inner = (TypeVariable) ((Lambda) ((Lambda) e).body).argsType.get(0);
//
//		assertNotEquals(outer, inner);
//
//		Lambda l1 = (Lambda) e;
//		Lambda l2 = (Lambda) l1.body;
//		// Have to compare names directly as TypeVariables equals if they are not bound
//		// by substitution
//		assertNotEquals(((TypeVariable) l1.argsType.get(0)).name, ((TypeVariable) l2.argsType.get(0)).name);
	}
	
	@Test
	@DisplayName("Test let")
	void testLet () throws AppendableException {
		String letCode = "(let ((a 21) (b 21)) a)";
		
		Expression e = this.parseString(letCode).get(0);
		
		assertEquals(new Let(new Symbol("a"),
				Pair.of(new Symbol("a"), new LitInteger(21)),
				Pair.of(new Symbol("b"), new LitInteger(21))),
				e);
	}
	
	@Test
	@DisplayName("Test Loop Recur")
	void testLoopRecur() throws AppendableException {
		Expression e = this.parseString("(loop ((x 10)) 42)").get(0);
		
		Assertions.assertTrue(e instanceof Loop);
		
		Loop l = (Loop)e;
		
		Assertions.assertEquals(l,
				new Loop(new Tuple(new Symbol("x")), new LitInteger(42), new Tuple(new LitInteger(10))));
		
		Expression f = this.parseString("(recur 42)").get(0);
		
		Assertions.assertTrue(f instanceof Recur);
		
		Recur r = (Recur)f;
		
		Assertions.assertEquals(r, new Recur(new Tuple(new LitInteger(42))));
	}
	
	@Test
	void testConversion() throws Exception {
		var e = this.parseString("(conversion Type:Rep1 Type:Rep2 (x) x (lambda ((Type:Rep1 x)) 42))");
		var c = e.get(0);
		assertTrue(c instanceof DefineConversion);
		var cst = (DefineConversion)c;
		assertEquals(new TypeAtom(new TypeName("Type"), new TypeRepresentation("Rep1")), cst.from);
		assertEquals(new TypeAtom(new TypeName("Type"), new TypeRepresentation("Rep2")), cst.to);
		assertEquals(new Symbol("x"), cst.body);
		assertEquals(new Tuple(new Symbol("x")), cst.args);
		assertTrue(cst.cost instanceof Lambda);
		
		var l = (Lambda)cst.cost;
		assertEquals(new LitInteger(42), l.body);
		assertEquals(new Tuple(new Symbol("x")), l.args);
		assertEquals(new TypeTuple(new TypeAtom(new TypeName("Type"), new TypeRepresentation("Rep1"))), l.argsType);
	}

	private void testParse(String parsedString, Expression expected) throws AppendableException {
		Expression parsed = this.parseString(parsedString).get(0);
		assertEquals(expected, parsed);
	}

}
