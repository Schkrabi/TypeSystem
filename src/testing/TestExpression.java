package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import conversions.IntRomanToIntStringWrapper;
import expression.Application;
import expression.DefExpression;
import expression.ExceptionExpr;
import expression.Expression;
import expression.Function;
import expression.IfExpression;
import expression.Lambda;
import expression.LitInteger;
import expression.Literal;
import expression.Literal.ConversionWrapper;
import expression.MetaFunction;
import expression.MetaLambda;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import types.TypeTuple;

class TestExpression {

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testCompareTo() {
		Application application = new Application(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE);
		DefExpression defExpression = new DefExpression(new Variable("x"), Expression.EMPTY_EXPRESSION);
		ExceptionExpr exceptionExpr = new ExceptionExpr(Expression.EMPTY_EXPRESSION);
		IfExpression ifExpression = new IfExpression(new Tuple(new Expression[] { Expression.EMPTY_EXPRESSION,
				Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION }));
		Literal literal = new LitInteger(42);
		MetaFunction metaFunction = new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
				new Environment());
		MetaLambda metaLambda = new Lambda(Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION);
		Variable variable = new Variable("Y");
		ConversionWrapper conversionWrapper = (ConversionWrapper) IntRomanToIntStringWrapper.IntRomanToIntString.body;

		List<Expression> l = Arrays.asList(application, defExpression, exceptionExpr, ifExpression, literal,
				metaFunction, metaLambda, variable, conversionWrapper);

		literal.compareTo(metaFunction);

		for (Expression e : l) {
			for (Expression f : l) {
				if (e != f && (e.compareTo(f) == 0 || f.compareTo(e) == 0)) {
					fail(e.getClass().getName() + " and " + f.getClass().getName()
							+ " compares as same but are not equal.");
				}
			}
		}
	}

}
