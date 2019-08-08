package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import expression.Expression;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitInteger;
import expression.LitString;
import expression.Tuple;
import expression.TypeHolder;
import expression.Variable;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeConcrete;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.Pair;

class TestInterpretation {

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testLitString() {
		LitString litString = new LitString("test");

		Expression e = litString.interpret(new Environment());
		if (!e.equals(litString)) {
			fail(e.toString() + " is not equal to " + litString);
		}
		if (litString.equals(Expression.EMPTY_EXPRESSION)) {
			fail(litString.toString() + " should not be equal to " + Expression.EMPTY_EXPRESSION);
		}

		litString.toString();
		litString.toClojureCode();
		litString.infer(new Environment());

		e = new LitString(" ");
		int cmp = litString.compareTo(e);
		if (cmp == 0) {
			fail(litString.toString() + ".compareTo(" + e.toString() + ") expects 1 got" + cmp);
		}
		cmp = litString.compareTo(litString);
		if (cmp != 0) {
			fail(litString.toString() + ".compareTo(" + litString.toString() + ") expects 0 got " + cmp);
		}
		e = Expression.EMPTY_EXPRESSION;
		cmp = litString.compareTo(e);
		if (cmp == 0) {
			fail(litString.toString() + ".compareTo(" + e.toString() + ") expects not 0 got " + cmp);
		}
	}

	@Test
	public void testLitInteger() {
		LitInteger litInteger = new LitInteger(128);

		Expression e = litInteger.interpret(new Environment());
		if (!e.equals(litInteger)) {
			fail(e.toString() + " is not equal to " + litInteger);
		}
		if (litInteger.equals(Expression.EMPTY_EXPRESSION)) {
			fail(litInteger.toString() + " should not be equal to " + Expression.EMPTY_EXPRESSION);
		}
		e = new LitInteger(0);
		if (e.equals(litInteger)) {
			fail(e.toString() + " is not equal to " + litInteger);
		}

		litInteger.toString();
		litInteger.toClojureCode();
		litInteger.infer(new Environment());

		e = new LitInteger(0);
		int cmp = litInteger.compareTo(e);
		if (cmp == 0) {
			fail(litInteger.toString() + ".compareTo(" + e.toString() + ") expects 1 got" + cmp);
		}
		cmp = litInteger.compareTo(litInteger);
		if (cmp != 0) {
			fail(litInteger.toString() + ".compareTo(" + litInteger.toString() + ") expects 0 got " + cmp);
		}
		e = Expression.EMPTY_EXPRESSION;
		cmp = litInteger.compareTo(e);
		if (cmp == 0) {
			fail(litInteger.toString() + ".compareTo(" + e.toString() + ") expects not 0 got " + cmp);
		}
	}

	@Test
	public void testLitDouble() {
		LitDouble litDouble = new LitDouble(3.14);

		Expression e = litDouble.interpret(new Environment());
		if (!e.equals(litDouble)) {
			fail(e.toString() + " is not equal to " + litDouble);
		}
		if (litDouble.equals(Expression.EMPTY_EXPRESSION)) {
			fail(litDouble.toString() + " should not be equal to " + Expression.EMPTY_EXPRESSION);
		}
		e = new LitDouble(0);
		if (e.equals(litDouble)) {
			fail(e.toString() + " is not equal to " + litDouble);
		}

		litDouble.toString();
		litDouble.toClojureCode();
		litDouble.infer(new Environment());

		e = new LitDouble(0);
		int cmp = litDouble.compareTo(e);
		if (cmp == 0) {
			fail(litDouble.toString() + ".compareTo(" + e.toString() + ") expects 1 got" + cmp);
		}
		cmp = litDouble.compareTo(litDouble);
		if (cmp != 0) {
			fail(litDouble.toString() + ".compareTo(" + litDouble.toString() + ") expects 0 got " + cmp);
		}
		e = Expression.EMPTY_EXPRESSION;
		cmp = litDouble.compareTo(e);
		if (cmp == 0) {
			fail(litDouble.toString() + ".compareTo(" + e.toString() + ") expects not 0 got " + cmp);
		}
	}

	@Test
	public void testLitBoolean() {
		Expression e = LitBoolean.TRUE.interpret(new Environment());
		if (!e.equals(LitBoolean.TRUE)) {
			fail(e.toString() + " is not equal to " + LitBoolean.TRUE);
		}
		if (LitBoolean.TRUE.equals(Expression.EMPTY_EXPRESSION)) {
			fail(LitBoolean.TRUE.toString() + " should not be equal to " + Expression.EMPTY_EXPRESSION);
		}
		if (LitBoolean.TRUE.equals(LitBoolean.FALSE)) {
			fail(LitBoolean.TRUE + " is not equal to " + LitBoolean.FALSE);
		}

		LitBoolean.TRUE.toString();
		LitBoolean.TRUE.toClojureCode();
		LitBoolean.TRUE.infer(new Environment());

		int cmp = LitBoolean.TRUE.compareTo(LitBoolean.FALSE);
		if (cmp == 0) {
			fail(LitBoolean.TRUE.toString() + ".compareTo(" + LitBoolean.FALSE.toString() + ") expects 1 got" + cmp);
		}
		cmp = LitBoolean.FALSE.compareTo(LitBoolean.TRUE);
		if (cmp == 0) {
			fail(LitBoolean.FALSE.toString() + ".compareTo(" + LitBoolean.TRUE.toString() + ") expects 1 got" + cmp);
		}
		cmp = LitBoolean.TRUE.compareTo(LitBoolean.TRUE);
		if (cmp != 0) {
			fail(LitBoolean.TRUE.toString() + ".compareTo(" + LitBoolean.TRUE.toString() + ") expects 0 got " + cmp);
		}
		cmp = LitBoolean.TRUE.compareTo(Expression.EMPTY_EXPRESSION);
		if (cmp == 0) {
			fail(LitBoolean.TRUE.toString() + ".compareTo(" + Expression.EMPTY_EXPRESSION.toString()
					+ ") expects not 0 got " + cmp);
		}
	}

	@Test
	public void testTypeHolder() {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);

		Assertions.assertThrows(AppendableException.class, () -> typeHolder.interpret(new Environment()));
		typeHolder.infer(new Environment());
		Assertions.assertThrows(AppendableException.class, () -> typeHolder.toClojureCode());
		typeHolder.toString();

		Expression e = new TypeHolder(TypeTuple.EMPTY_TUPLE);
		if (!typeHolder.equals(e)) {
			fail(typeHolder + " and " + e + " should be equal");
		}
		int cmp = typeHolder.compareTo(e);
		if (cmp != 0) {
			fail(typeHolder.toString() + ".compareTo(" + e.toString() + ") shoudl be 0 got " + cmp);
		}

		e = new TypeHolder(TypeConcrete.TypeInt);
		if (typeHolder.equals(e)) {
			fail(typeHolder + " and " + e + " should not be equal");
		}
		cmp = typeHolder.compareTo(e);
		if (cmp == 0) {
			fail(typeHolder.toString() + ".compareTo(" + e.toString() + ") shoudl not be 0 got " + cmp);
		}

		e = Expression.EMPTY_EXPRESSION;
		if (typeHolder.equals(e)) {
			fail(typeHolder + " and " + e + " should not be equal");
		}
		cmp = typeHolder.compareTo(e);
		if (cmp == 0) {
			fail(typeHolder.toString() + ".compareTo(" + e.toString() + ") shoudl not be 0 got " + cmp);
		}
	}

	@Test
	public void testVariable() throws AppendableException {
		Variable variable = new Variable("x");

		variable.toString();
		variable.toClojureCode();

		Expression e = new Variable("x");
		if (!variable.equals(e)) {
			fail(variable + " and " + e + " should be equal");
		}
		int cmp = variable.compareTo(e);
		if (cmp != 0) {
			fail(variable.toString() + ".compareTo(" + e.toString() + ") shoudl be 0 got " + cmp);
		}

		e = new Variable("y");
		if (variable.equals(e)) {
			fail(variable + " and " + e + " should not be equal");
		}
		cmp = variable.compareTo(e);
		if (cmp == 0) {
			fail(variable.toString() + ".compareTo(" + e.toString() + ") shoudl not be 0 got " + cmp);
		}

		e = Expression.EMPTY_EXPRESSION;
		if (variable.equals(e)) {
			fail(variable + " and " + e + " should not be equal");
		}
		cmp = variable.compareTo(e);
		if (cmp == 0) {
			fail(variable.toString() + ".compareTo(" + e.toString() + ") shoudl not be 0 got " + cmp);
		}

		LitInteger value = new LitInteger(128);
		Environment bound = new Environment();
		bound.put(variable, value);

		e = variable.interpret(new Environment());
		if (!e.equals(variable)) {
			fail("Variable " + variable + " in empty environment should interpret to itself got " + e);
		}
		Pair<Type, Substitution> p = variable.infer(new Environment());
		if (!(p.first instanceof TypeVariable)) {
			fail("Variable in empty environment shoud infered to typevariable got " + p.first);
		}

		e = variable.interpret(bound);
		if (!e.equals(value)) {
			fail("Variable " + variable + " in bound environment should interpret to " + value + " got " + e);
		}
		p = variable.infer(bound);
		if (!p.first.equals(TypeConcrete.TypeInt)) {
			fail("Variable in bound environment shoud infered to TypeInt got " + p.first);
		}

		final Environment fault = new Environment();
		fault.put(variable, new Expression() {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode() throws AppendableException {
				return null;
			}
		});
		Assertions.assertThrows(AppendableException.class, () -> variable.infer(fault));
	}
	
	@Test
	public void testEmptyExpression () throws AppendableException {
		Expression.EMPTY_EXPRESSION.interpret(new Environment());
		Expression.EMPTY_EXPRESSION.infer(new Environment());
		Expression.EMPTY_EXPRESSION.toClojureCode();		
	}

	@Test
	public void testTuple() throws Exception {
		final Tuple tuple = new Tuple(Arrays.asList(new LitInteger(128), new Variable("x"), LitBoolean.FALSE));

		tuple.get(0);
		Assertions.assertThrows(ArrayIndexOutOfBoundsException.class, () -> tuple.get(4));
		tuple.size();
		tuple.toString();
		tuple.toClojureCode();

		Expression e = new Tuple(tuple.stream().collect(Collectors.toList()));
		if (!tuple.equals(e)) {
			fail(tuple.toString() + " should be equal to " + e.toString());
		}
		int cmp = tuple.compareTo(e);
		if (cmp != 0) {
			fail(tuple.toString() + ".compareTo(" + e.toString() + ") should equal to 0 got " + cmp);
		}

		if (tuple.equals(Tuple.EMPTY_TUPLE)) {
			fail(tuple.toString() + " should not be equal to " + Tuple.EMPTY_TUPLE.toString());
		}
		cmp = tuple.compareTo(Tuple.EMPTY_TUPLE);
		if (cmp == 0) {
			fail(tuple.toString() + ".compareTo(" + Tuple.EMPTY_TUPLE.toString() + ") should equal to 0 got " + cmp);
		}
		
		if(tuple.equals(Expression.EMPTY_EXPRESSION)) {
			fail(tuple.toString() + " should not be equal to " + Expression.EMPTY_EXPRESSION.toString());
		}
		cmp = tuple.compareTo(Expression.EMPTY_EXPRESSION);
		if (cmp == 0) {
			fail(tuple.toString() + ".compareTo(" + Expression.EMPTY_EXPRESSION.toString() + ") should equal to 0 got " + cmp);
		}
		
		e = new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2)));
		if (tuple.equals(e)) {
			fail(tuple.toString() + " should be equal to " + e.toString());
		}
		cmp = tuple.compareTo(e);
		if (cmp == 0) {
			fail(tuple.toString() + ".compareTo(" + e.toString() + ") should equal to 0 got " + cmp);
		}

	}

}
