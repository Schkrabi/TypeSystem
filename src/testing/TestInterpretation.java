package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import abstraction.ExtendedFunction;
import abstraction.ExtendedLambda;
import abstraction.Function;
import abstraction.Lambda;
import abstraction.Abstraction;
import abstraction.Operator;
import application.AndExpression;
import application.CanDeconstructAs;
import application.Construct;
import application.Convert;
import application.Deconstruct;
import application.DefineConstructor;
import application.AbstractionApplication;
import application.DefineConversion;
import application.DefineSymbol;
import application.DefineType;
import application.ExceptionExpr;
import application.IfExpression;
import application.OrExpression;
import application.DefineRepresentation;
import expression.Expression;
import expression.Tuple;
import expression.TypeHolder;
import expression.Symbol;
import interpretation.Environment;
import langbase.ListNative;
import literal.LitBoolean;
import literal.LitComposite;
import literal.LitDouble;
import literal.LitEnum;
import literal.LitInteger;
import literal.LitString;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import semantic.TypeEnvironment;
import semantic.UserException;
import types.RepresentationOr;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import util.InvalidNumberOfArgumentsException;
import util.Pair;
import util.UnboundVariableException;

@SuppressWarnings("deprecation")
class TestInterpretation {

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
	void testLitString() throws AppendableException {
		LitString litString = new LitString("test");

		TestInterpretation.testReflexivity(litString);
		TestInterpretation.testDifference(litString, new LitString(" "));
		TestInterpretation.testDifference(litString, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litString, litString, Environment.topLevelEnvironment);

		litString.toString();
		litString.hashCode();
		litString.toClojureCode();

		Pair<Type, Substitution> p = litString.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeStringNative, litString, true);
	}

	@Test
	public void testLitInteger() throws AppendableException {
		LitInteger litInteger = new LitInteger(128);

		TestInterpretation.testReflexivity(litInteger);
		TestInterpretation.testDifference(litInteger, new LitInteger(0));
		TestInterpretation.testDifference(litInteger, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litInteger, litInteger, Environment.topLevelEnvironment);

		litInteger.toString();
		litInteger.hashCode();
		litInteger.toClojureCode();

		Pair<Type, Substitution> p = litInteger.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, litInteger, true);
	}

	@Test
	public void testLitDouble() throws AppendableException {
		LitDouble litDouble = new LitDouble(3.14);

		TestInterpretation.testReflexivity(litDouble);
		TestInterpretation.testDifference(litDouble, new LitDouble(0));
		TestInterpretation.testDifference(litDouble, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litDouble, litDouble, Environment.topLevelEnvironment);

		litDouble.toString();
		litDouble.hashCode();
		litDouble.toClojureCode();
		Pair<Type, Substitution> p = litDouble.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeDoubleNative, litDouble, true);
	}

	@Test
	public void testLitBoolean() throws AppendableException {
		TestInterpretation.testReflexivity(LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, LitBoolean.FALSE);
		TestInterpretation.testDifference(LitBoolean.FALSE, LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(LitBoolean.TRUE, LitBoolean.TRUE, Environment.topLevelEnvironment);

		LitBoolean.TRUE.toString();
		LitBoolean.TRUE.toClojureCode();

		Pair<Type, Substitution> p = LitBoolean.TRUE.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, LitBoolean.TRUE, true);
	}

	@Test
	void testLitEnum() throws AppendableException {
		TypeName typeName = new TypeName("TestEnum");
		TypeAtom type = new TypeAtom(typeName, TypeRepresentation.NATIVE);

		LitEnum enumValue1 = new LitEnum("value1", type);
		LitEnum enumValue2 = new LitEnum("value2", type);
		LitEnum differentEnumValue = new LitEnum("value1", TypeAtom.TypeInt);

		TestInterpretation.testReflexivity(enumValue1);
		TestInterpretation.testDifference(enumValue1, enumValue2);
		TestInterpretation.testDifference(enumValue1, differentEnumValue);
		TestInterpretation.testDifference(enumValue1, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(enumValue1, enumValue1, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = enumValue1.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, type, enumValue1, true);

		enumValue1.toString();
		// Not implemented yet
		// enumValue1.toClojureCode();
	}

	@Test
	void testLitComposite() throws AppendableException {
		TypeName typeName = new TypeName("TestComposite");
		TypeAtom type = new TypeAtom(typeName, TypeRepresentation.NATIVE);

		LitComposite composite1 = new LitComposite(
				new Tuple(Arrays.asList(new LitInteger(42), LitBoolean.TRUE, new LitString("test"))), type);
		LitComposite composite2 = new LitComposite(
				new Tuple(Arrays.asList(new LitInteger(84), LitBoolean.FALSE, new LitString("fail"))), type);
		LitComposite composite3 = new LitComposite(
				new Tuple(Arrays.asList(new LitInteger(42), LitBoolean.TRUE, new LitString("test"))), TypeAtom.TypeInt);

		TestInterpretation.testReflexivity(composite1);
		TestInterpretation.testDifference(composite1, composite2);
		TestInterpretation.testDifference(composite1, composite3);
		TestInterpretation.testDifference(composite1, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(composite1, composite1, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = composite1.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, type, composite1);

		composite1.toString();
		composite1.toClojureCode();
	}

	@Test
	public void testTypeHolder() throws AppendableException {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);

		Assertions.assertThrows(AppendableException.class, () -> typeHolder.interpret(Environment.topLevelEnvironment));
		Assertions.assertThrows(AppendableException.class, () -> typeHolder.toClojureCode());
		typeHolder.toString();
		typeHolder.hashCode();

		TestInterpretation.testReflexivity(typeHolder);
		TestInterpretation.testDifference(typeHolder, new TypeHolder(TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(typeHolder, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = typeHolder.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, typeHolder, true);

		TypeHolder placeholder = new TypeHolder(TypeAtom.TypeInt, new Symbol("x"));
		Environment bound = Environment.create(Environment.topLevelEnvironment);
		bound.put(new Symbol("x"), new LitInteger(42));

		TestInterpretation.testInterpretation(placeholder, new LitInteger(42), bound);
		Assertions.assertThrows(AppendableException.class,
				() -> placeholder.interpret(Environment.topLevelEnvironment));

		TypeHolder placeholder2 = new TypeHolder(TypeAtom.TypeInt, new Symbol("__q"));
		Environment.topLevelEnvironment.put(new Symbol("__q"), placeholder2);
		Assertions.assertThrows(AppendableException.class,
				() -> placeholder2.interpret(Environment.topLevelEnvironment));

		Environment bound2 = Environment.create(bound);
		bound2.put(new Symbol("x"), placeholder);
		TestInterpretation.testInterpretation(placeholder, new LitInteger(42), bound2);
	}

	@Test
	public void testVariable() throws AppendableException {
		Symbol variable = new Symbol("x");

		variable.toString();
		variable.hashCode();
		variable.toClojureCode();

		TestInterpretation.testReflexivity(variable);
		TestInterpretation.testDifference(variable, new Symbol("y"));
		TestInterpretation.testDifference(variable, Expression.EMPTY_EXPRESSION);

		LitInteger value = new LitInteger(128);
		Environment bound = Environment.create(Environment.topLevelEnvironment);
		bound.put(variable, value);

		TestInterpretation.testInterpretation(variable, variable, Environment.topLevelEnvironment);
		Pair<Type, Substitution> p = variable.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInferenceOfType(p, TypeVariable.class, variable);

		TestInterpretation.testInterpretation(variable, value, bound);
		p = variable.infer(bound);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, variable);

		final Environment fault = Environment.create(Environment.topLevelEnvironment);
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
			public String toClojureCode(Environment env) throws AppendableException {
				return null;
			}
		});
		Assertions.assertThrows(AppendableException.class, () -> variable.infer(fault));
	}

	@Test
	public void testEmptyExpression() throws AppendableException {
		TestInterpretation.testInference(Expression.EMPTY_EXPRESSION.infer(Environment.topLevelEnvironment),
				TypeTuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION, true);
		TestInterpretation.testInterpretation(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION,
				Environment.topLevelEnvironment);
		Expression.EMPTY_EXPRESSION.toClojureCode();
	}

	@Test
	public void testTuple() throws Exception {
		final Tuple tuple = new Tuple(Arrays.asList(new LitInteger(128), new Symbol("x"), LitBoolean.FALSE));

		tuple.get(0);
		Assertions.assertThrows(ArrayIndexOutOfBoundsException.class, () -> tuple.get(4));
		tuple.size();
		tuple.toString();
		tuple.hashCode();
		tuple.toClojureCode();
		tuple.stream();

		TestInterpretation.testReflexivity(tuple);
		TestInterpretation.testDifference(tuple, Tuple.EMPTY_TUPLE);
		TestInterpretation.testDifference(tuple, Expression.EMPTY_EXPRESSION);
		TestInterpretation.testDifference(tuple,
				new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2))));

		TestInterpretation.testInterpretation(tuple, tuple, Environment.topLevelEnvironment);
		Pair<Type, Substitution> p = tuple.infer(Environment.topLevelEnvironment);
		if (!(p.first instanceof TypeTuple)) {
			fail("Infered type of " + Tuple.class.getName() + " should be " + TypeTuple.class.getName());
		}
		Type t = ((TypeTuple) p.first).get(0);
		if (!t.equals(TypeAtom.TypeIntNative)) {
			fail("First type of " + tuple.toString() + " should be " + TypeAtom.TypeIntNative.toString() + " got " + t);
		}
		t = ((TypeTuple) p.first).get(1);
		if (!(t instanceof TypeVariable)) {
			fail("Second type of " + tuple.toString() + " in empty environment should be "
					+ TypeVariable.class.getName() + " got " + t);
		}
		t = ((TypeTuple) p.first).get(2);
		if (!t.equals(TypeAtom.TypeBoolNative)) {
			fail("Third type of " + tuple.toString() + " should be " + TypeAtom.TypeBoolNative.toString() + " got "
					+ t);
		}

		Environment bound = Environment.create(Environment.topLevelEnvironment);
		bound.put(new Symbol("x"), new LitDouble(3.14));

		TestInterpretation.testInterpretation(tuple,
				new Tuple(Arrays.asList(new LitInteger(128), new LitDouble(3.14), LitBoolean.FALSE)), bound);
		p = tuple.infer(bound);
		TestInterpretation.testInference(p,
				new TypeTuple(
						Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeDoubleNative, TypeAtom.TypeBoolNative)),
				tuple);

		Assertions.assertThrows(AppendableException.class, () -> (new Tuple(Arrays.asList(new Expression() {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env) throws AppendableException {
				return null;
			}
		}))).infer(Environment.topLevelEnvironment));
	}

	@Test
	void testExceptionExpr() throws AppendableException {
		final ExceptionExpr exception = new ExceptionExpr(new LitString("test"));

		Assertions.assertThrows(UserException.class, () -> exception.interpret(Environment.topLevelEnvironment));

		exception.toClojureCode();
		exception.toString();
		exception.hashCode();

		TestInterpretation.testReflexivity(exception);
		TestInterpretation.testDifference(exception, new ExceptionExpr(new LitString("fail")));
		TestInterpretation.testDifference(exception, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = exception.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInferenceOfType(p, TypeVariable.class, exception);

		Assertions.assertThrows(AppendableException.class, () -> new ExceptionExpr(new Expression() {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env) throws AppendableException {
				return null;
			}
		}).infer(Environment.topLevelEnvironment));
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testDefExpression() throws AppendableException {
		DefineSymbol defExpression = new DefineSymbol(new Symbol("pi"), new LitDouble(Math.PI));

		defExpression.toString();
		defExpression.hashCode();
		defExpression.toClojureCode();

		TestInterpretation.testReflexivity(defExpression);
		TestInterpretation.testDifference(defExpression, new DefineSymbol(new Symbol("e"), new LitDouble(Math.E)));
		TestInterpretation.testDifference(defExpression, new DefineSymbol(new Symbol("pi"), new LitDouble(3.141521)));
		TestInterpretation.testDifference(defExpression, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.create(Environment.topLevelEnvironment);
		TestInterpretation.testInterpretation(defExpression, Expression.EMPTY_EXPRESSION, env);
		if (!env.containsVariable(new Symbol("pi"))) {
			fail("Environment after interpretation of " + defExpression.toString() + " should contain "
					+ new Symbol("pi").toString() + " got " + env);
		}

		Pair<Type, Substitution> p = defExpression.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, defExpression);
		if (p.second.equals(Substitution.EMPTY)) {
			fail("Substitution for " + defExpression.toString() + " should not be empty, got " + p.second.toString());
		}
		if (p.second.variableStream().findAny().get().equals(TypeAtom.TypeDoubleNative)) {
			fail("In substitution should be new variable substituted for " + TypeAtom.TypeDoubleNative.toString()
					+ " got " + p.second);
		}

		DefineSymbol recursiveExpression = (DefineSymbol) this
				.parseString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))");
		p = recursiveExpression.infer(Main.initTopLevelEnvironment());
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, recursiveExpression);
		if (p.second.equals(Substitution.EMPTY)) {
			fail("Substitution for " + defExpression.toString() + " should not be empty, got " + p.second.toString());
		}

		Assertions.assertThrows(AppendableException.class, () -> new DefineSymbol(new Symbol("fail"), new Expression() {

			@Override
			public Expression interpret(Environment env) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env) throws AppendableException {
				return null;
			}
		}).infer(Environment.topLevelEnvironment));
	}

	@Test
	void testLambda() throws AppendableException {
		final Lambda lambda = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x"));
		// final Lambda faultArgsLambda = new Lambda(new
		// Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)),
		// TypeTuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION);

		lambda.toString();
		lambda.hashCode();
		// lambda.toClojureCode();
		// Assertions.assertThrows(AppendableException.class, () ->
		// faultArgsLambda.toClojureCode());

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda, new Lambda(new Tuple(Arrays.asList(new Symbol("z"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x")));
		TestInterpretation.testDifference(lambda, new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative, TypeAtom.TypeIntNative)), new Symbol("x")));
		TestInterpretation.testDifference(lambda,
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						Expression.EMPTY_EXPRESSION));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		Expression e = lambda.interpret(Environment.topLevelEnvironment);
		if (!(e instanceof Function)) {
			fail("Interpreted " + lambda.toString() + " should yield " + Function.class.getName() + " got " + e);
		}

		Pair<Type, Substitution> p = lambda.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, lambda);

		// Assertions.assertThrows(AppendableException.class,
		// () -> faultArgsLambda.infer(Environment.topLevelEnvironment));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))").infer(Main.initTopLevelEnvironment()));
	}

	@Test
	void testFunction() throws AppendableException {
		Environment bound = Environment.create(Environment.topLevelEnvironment);
		bound.put(new Symbol("bound"), new LitDouble(3.141521));

		final Function function = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound);

		function.toString();
		function.hashCode();

		TestInterpretation.testReflexivity(function);
		TestInterpretation.testDifference(function,
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
						new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), Expression.EMPTY_EXPRESSION, bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), Environment.topLevelEnvironment));
		TestInterpretation.testDifference(function, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(function, function, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = function.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, function);

		Assertions.assertThrows(AppendableException.class,
				() -> (new Function(new TypeTuple(Arrays.asList(new TypeVariable("x"))),
						new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)), new Symbol("y"),
						Environment.topLevelEnvironment)).infer(Environment.topLevelEnvironment));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))").interpret(Main.initTopLevelEnvironment())
						.infer(Main.initTopLevelEnvironment()));
	}

	@Test
	void testExpendedLambda() throws AppendableException {
		ExtendedLambda lambda = ExtendedLambda.makeExtendedLambda(Arrays.asList(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
						new AbstractionApplication(Operator.IntRomanToIntNative,
								new Tuple(Arrays.asList(new Symbol("x"))))),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new AbstractionApplication(
								Operator.IntStringToIntNative, new Tuple(Arrays.asList(new Symbol("x")))))));

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda,
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new AbstractionApplication(
										Operator.IntRomanToIntNative, new Tuple(Arrays.asList(new Symbol("x"))))))));
		TestInterpretation.testDifference(lambda,
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
								new AbstractionApplication(Operator.IntRomanToIntNative,
										new Tuple(Arrays.asList(new Symbol("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
								new AbstractionApplication(Operator.IntStringToIntNative,
										new Tuple(Arrays.asList(new Symbol("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Symbol("y"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new AbstractionApplication(
										Operator.IntStringToIntNative, new Tuple(Arrays.asList(new Symbol("x"))))))));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		lambda.toString();
		lambda.hashCode();

		lambda.interpret(Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = lambda.infer(Environment.topLevelEnvironment);

		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(Arrays.asList(
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative))),
				lambda);

		Assertions.assertThrows(AppendableException.class,
				() -> (ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), new Symbol("x")))))
										.infer(Environment.topLevelEnvironment));
	}

	@Test
	public void testExtendedFunction() throws AppendableException {
		Environment bound = Environment.create(Environment.topLevelEnvironment);
		bound.put(new Symbol("x"), new LitInteger(42));
		List<Function> implementations = Arrays.asList(
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound),
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound));

		ExtendedFunction function = ExtendedFunction.makeExtendedFunction(implementations, bound);

		TestInterpretation.testReflexivity(function);

		List<Function> tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.remove(0);
		TestInterpretation.testDifference(function, ExtendedFunction.makeExtendedFunction(tmpImpls, bound));

		tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.add(new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), Environment.topLevelEnvironment));
		TestInterpretation.testDifference(function, ExtendedFunction.makeExtendedFunction(tmpImpls, bound));

		TestInterpretation.testDifference(function,
				ExtendedFunction.makeExtendedFunction(implementations, Environment.topLevelEnvironment));

		TestInterpretation.testDifference(function, Expression.EMPTY_EXPRESSION);

		function.toString();
		function.hashCode();

		Pair<Type, Substitution> p = function.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(Arrays.asList(
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntString))),
				function);

		Assertions.assertThrows(AppendableException.class, () -> ExtendedFunction.makeExtendedFunction(Arrays.asList(
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
						new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), Environment.topLevelEnvironment),
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
						new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), Environment.topLevelEnvironment)),
				bound).infer(Environment.topLevelEnvironment));
	}

	@Test
	void testApplication() throws AppendableException {
		AbstractionApplication application = new AbstractionApplication(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Symbol("x")),
				new Tuple(Arrays.asList(new LitInteger(42))));

		TestInterpretation.testReflexivity(application);
		TestInterpretation.testDifference(application,
				new AbstractionApplication(
						new Lambda(new Tuple(Arrays.asList(new Symbol("y"))),
								new TypeTuple(Arrays.asList(new TypeVariable("x"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))));
		TestInterpretation.testDifference(application,
				new AbstractionApplication(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(21)))));
		TestInterpretation.testDifference(application, Expression.EMPTY_EXPRESSION);

		application.toString();
		application.toClojureCode();
		application.hashCode();

		Assertions.assertThrows(InvalidNumberOfArgumentsException.class,
				() -> new AbstractionApplication(new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(new TypeVariable("_x"), new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))).interpret(Environment.topLevelEnvironment));
		Assertions.assertThrows(AppendableException.class,
				() -> new AbstractionApplication(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE)
						.interpret(Environment.topLevelEnvironment));

		TestInterpretation.testInterpretation(application, new LitInteger(42), Environment.topLevelEnvironment);
		Pair<Type, Substitution> p = application.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, application);

		// Test Lexical clojure
		Environment creation = Environment.create(Environment.topLevelEnvironment);
		creation.put(new Symbol("x"), new LitInteger(128));
		Environment evaluation = Environment.create(Environment.topLevelEnvironment);
		evaluation.put(new Symbol("x"), new LitString("foo"));
		AbstractionApplication lexicalClojureTest = new AbstractionApplication(
				new Function(new TypeTuple(Arrays.asList(new TypeVariable("a"))),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("x"), creation),
				new Tuple(Arrays.asList(LitBoolean.TRUE)));

		TestInterpretation.testInterpretation(lexicalClojureTest, new LitInteger(128), evaluation);
		p = lexicalClojureTest.infer(evaluation);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, lexicalClojureTest);

		// Test autoconvert representations
		AbstractionApplication autoConRep = new AbstractionApplication(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				// new Tuple(Arrays.asList(new
				// Application(TypeConstructionLambda.IntRomanConstructor,
				// new Tuple(Arrays.asList(new LitString("V")))))));
				new Tuple(Arrays.asList(new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman))));
		TestInterpretation.testInterpretation(autoConRep, new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				Environment.topLevelEnvironment);
		p = autoConRep.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntString, autoConRep);

		// Test elambda/efunction comparation
		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(Arrays.asList(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new Symbol("x"))));
		AbstractionApplication useString = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString))));
		TestInterpretation.testInterpretation(useString,
				new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString),
				Environment.topLevelEnvironment);
		p = useString.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntString, useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman))));
		TestInterpretation.testInterpretation(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman),
				Environment.topLevelEnvironment);
		p = useRoman.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, useRoman);

		Assertions.assertThrows(AppendableException.class,
				() -> new AbstractionApplication(useString, new Tuple(Arrays.asList(new LitString("fail"))))
						.infer(Environment.topLevelEnvironment));

	}

	@Test
	void testIfExpression() throws AppendableException {
		IfExpression ifExprT = new IfExpression(LitBoolean.TRUE, new LitInteger(42), new LitInteger(21));
		IfExpression ifExprF = new IfExpression(LitBoolean.FALSE, new LitInteger(21), new LitInteger(42));

		TestInterpretation.testReflexivity(ifExprF);
		TestInterpretation.testDifference(ifExprT, ifExprF);
		TestInterpretation.testDifference(ifExprT, Expression.EMPTY_EXPRESSION);

		ifExprT.toString();
		ifExprT.toClojureCode();

		TestInterpretation.testInterpretation(ifExprT, new LitInteger(42), Environment.topLevelEnvironment);
		TestInterpretation.testInterpretation(ifExprF, new LitInteger(42), Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = ifExprT.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, ifExprT);
	}

	@Test
	void testAndExpression() throws AppendableException {
		AndExpression andExpressionT = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.TRUE)));
		AndExpression andExpressionF = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE)));

		TestInterpretation.testReflexivity(andExpressionT);
		TestInterpretation.testDifference(andExpressionT, andExpressionF);
		TestInterpretation.testDifference(andExpressionT, Expression.EMPTY_EXPRESSION);

		andExpressionT.toString();
		andExpressionT.toClojureCode();

		TestInterpretation.testInterpretation(andExpressionT, LitBoolean.TRUE, Environment.topLevelEnvironment);
		TestInterpretation.testInterpretation(andExpressionF, LitBoolean.FALSE, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = andExpressionT.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, andExpressionT);
	}

	@Test
	void testOrExpression() throws AppendableException {
		OrExpression orExpressionT = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.TRUE)));
		OrExpression orExpressionF = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.FALSE)));

		TestInterpretation.testReflexivity(orExpressionT);
		TestInterpretation.testDifference(orExpressionT, orExpressionF);
		TestInterpretation.testDifference(orExpressionT, Expression.EMPTY_EXPRESSION);

		orExpressionT.toString();
		orExpressionF.toClojureCode();

		TestInterpretation.testInterpretation(orExpressionT, LitBoolean.TRUE, Environment.topLevelEnvironment);
		TestInterpretation.testInterpretation(orExpressionF, LitBoolean.FALSE, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = orExpressionT.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, orExpressionT);
	}

	@Test
	void testOperators() throws AppendableException {
		TestInterpretation.testOperator(Operator.Addition,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(21))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.BitAnd, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(0), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.BitOr, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(3), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.Car,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitInteger(42), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.Cdr,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitString("foo"), TypeAtom.TypeStringNative);
		TestInterpretation.testOperator(Operator.Concantenation,
				new Tuple(Arrays.asList(new LitString("foo"), new LitString("bar"))), new LitString("foobar"),
				TypeAtom.TypeStringNative);
		TestInterpretation.testOperator(Operator.Division,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, LitBoolean.FALSE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(43), new LitInteger(42))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.Multiplication,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operator.Not, new Tuple(Arrays.asList(LitBoolean.TRUE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.Not, new Tuple(Arrays.asList(LitBoolean.FALSE)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(42))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.Subtraction,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(42))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		/*
		 * TestInterpretation.testOperator(Operator.Deconstruct, new
		 * Tuple(Arrays.asList(new LitComposite(new LitString("42"),
		 * TypeAtom.TypeIntString))), new LitString("42"), null);
		 */
	}

	@Test
	void testConversions() throws AppendableException {
		TestInterpretation.testConversion(Operator.IntRomanToIntString,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntString));
		TestInterpretation.testConversion(Operator.IntRomanToIntNative,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative));
		TestInterpretation.testConversion(Operator.IntStringToIntRoman,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntRoman));
		TestInterpretation.testConversion(Operator.IntStringToIntNative,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative));
		TestInterpretation.testConversion(Operator.IntNativeToIntString, new LitInteger(5),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntString));
		TestInterpretation.testConversion(Operator.IntNativeToIntRoman, new LitInteger(5),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntRoman));
	}

	@Test
	void testAutoConversion() throws AppendableException {
		Expression e = new AbstractionApplication(Operator.Addition,
				new Tuple(Arrays.asList(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))));

		TestInterpretation.testInterpretation(e, new LitInteger(84), Environment.topLevelEnvironment);
	}

	@Test
	void testEnvironment() throws AppendableException {
		Environment environment = Environment.create(Environment.topLevelEnvironment);
		environment.put(new Symbol("x"), Expression.EMPTY_EXPRESSION);
		environment.put(new Symbol("y"), new LitInteger(42));

		Environment child = Environment.create(environment);
		child.put(new Symbol("z"), Tuple.EMPTY_TUPLE);

		child.isTopLevel();
		environment.isTopLevel();

		child.containsVariable(new Symbol("x"));
		child.containsVariable(new Symbol("z"));
		child.containsVariable(new Symbol("w"));

		child.getVariableValue(new Symbol("x"));
		child.getVariableValue(new Symbol("z"));
		Assertions.assertThrows(UnboundVariableException.class, () -> child.getVariableValue(new Symbol("w")));

		int cmp = environment.compareTo(environment);
		if (cmp != 0) {
			fail("Problem: " + environment + ".compareTo(" + environment + ") != 0 got " + cmp);
		}
		cmp = environment.compareTo(child);
		if (cmp == 0) {
			fail("Problem: " + environment + ".compareTo(" + child + ") == " + cmp);
		}
		cmp = child.compareTo(environment);
		if (cmp == 0) {
			fail("Problem: " + child + ".compareTo(" + environment + ") == " + cmp);
		}

		Environment child2 = Environment.create(environment);
		child2.put(new Symbol("w"), new LitInteger(42));
		cmp = child.compareTo(child2);
		if (cmp == 0) {
			fail("Problem: " + child + ".compareTo(" + child2 + ") == " + cmp);
		}

		Environment child3 = Environment.create(child);
		cmp = child.compareTo(child3);
		if (cmp == 0) {
			fail("Problem: " + child3 + ".compareTo(" + child + ") == " + cmp);
		}

		Environment child4 = Environment.create(environment);
		child4.put(new Symbol("z"), new LitInteger(43));
		cmp = child.compareTo(child4);
		if (cmp == 0) {
			fail("Problem: " + child + ".compareTo(" + child4 + ") == " + cmp);
		}

		Environment child5 = Environment.create(environment);
		child5.put(new Symbol("z"), Tuple.EMPTY_TUPLE);
		child5.put(new Symbol("w"), new LitInteger(42));
		cmp = child.compareTo(child5);
		if (cmp == 0) {
			fail("Problem: " + child + ".compareTo(" + child5 + ") == " + cmp);
		}
	}

	@Test
	void testDefTypeExpression() throws AppendableException {
		DefineType defTypeExpression = new DefineType(new TypeName("Test"));

		defTypeExpression.toString();

		TestInterpretation.testReflexivity(defTypeExpression);
		TestInterpretation.testDifference(defTypeExpression, new DefineType(new TypeName("Test2")));
		TestInterpretation.testDifference(defTypeExpression, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(defTypeExpression, Expression.EMPTY_EXPRESSION,
				Environment.topLevelEnvironment);
		if (!TypeEnvironment.singleton
				.existsTypeAtom(new TypeAtom(new TypeName("Test"), TypeRepresentation.WILDCARD))) {
			fail(defTypeExpression.toString() + " did not created type!");
		}

		Pair<Type, Substitution> p = defTypeExpression.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(Environment.topLevelEnvironment).first,
				defTypeExpression);
	}

	@Test
	void testDefConversionExpression() throws AppendableException {
		TypeName name = new TypeName("__defConversionTest");
		TypeAtom typeAtomNative = new TypeAtom(name, TypeRepresentation.NATIVE);
		TypeAtom typeAtomWildcard = new TypeAtom(name, TypeRepresentation.WILDCARD);
		DefineConversion defCon = DefineConversion.makeDefineConversion(typeAtomNative, typeAtomWildcard,
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))), new TypeTuple(Arrays.asList(typeAtomNative)),
						new LitComposite(new Tuple(Arrays.asList(new Symbol("x"))), typeAtomWildcard)));

		defCon.toString();

		TestInterpretation.testReflexivity(defCon);
		TestInterpretation.testDifference(defCon,
				DefineConversion.makeDefineConversion(new TypeAtom(name, TypeRepresentation.STRING), typeAtomWildcard,
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(new TypeAtom(name, TypeRepresentation.STRING))),
								new LitComposite(new Symbol("y"), typeAtomWildcard))));
		TestInterpretation.testDifference(defCon,
				DefineConversion.makeDefineConversion(typeAtomNative, new TypeAtom(name, TypeRepresentation.STRING),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(typeAtomNative)),
								new LitComposite(new Symbol("x"), new TypeAtom(name, TypeRepresentation.STRING)))));
		TestInterpretation.testDifference(defCon,
				DefineConversion.makeDefineConversion(typeAtomNative, typeAtomWildcard,
						new Lambda(new Tuple(Arrays.asList(new Symbol("y"))),
								new TypeTuple(Arrays.asList(typeAtomNative)),
								new LitComposite(new Symbol("y"), typeAtomWildcard))));
		TestInterpretation.testDifference(defCon, Expression.EMPTY_EXPRESSION);

		TypeEnvironment.singleton.addType(name);
		TypeEnvironment.singleton.addRepresentation(typeAtomNative);
		// TypeEnvironment.singleton.addRepresentation(typeAtomWildcard);

		TestInterpretation.testInterpretation(defCon, Expression.EMPTY_EXPRESSION, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = defCon.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(Environment.topLevelEnvironment).first,
				defCon);
	}

	@Test
	void testDefRepresentationExpression() throws AppendableException {
		TypeName name = new TypeName("__defRepresentationTest");

		DefineRepresentation defRep = new DefineRepresentation(name, TypeRepresentation.NATIVE);

		defRep.toString();

		TestInterpretation.testReflexivity(defRep);
		TestInterpretation.testDifference(defRep, new DefineRepresentation(TypeName.INT, TypeRepresentation.NATIVE));
		TestInterpretation.testDifference(defRep, new DefineRepresentation(name, TypeRepresentation.STRING));
		TestInterpretation.testDifference(defRep, Expression.EMPTY_EXPRESSION);

		TypeEnvironment.singleton.addType(name);
		TestInterpretation.testInterpretation(defRep, Expression.EMPTY_EXPRESSION, Environment.topLevelEnvironment);

		Assertions.assertThrows(AppendableException.class,
				() -> new DefineRepresentation(name, TypeRepresentation.NATIVE)
						.interpret(Environment.topLevelEnvironment));

		Pair<Type, Substitution> p = defRep.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(Environment.topLevelEnvironment).first,
				defRep);
	}

	@Test
	void testDefinceConstructorExpression() throws AppendableException {
		TypeName name = new TypeName("__defConstructorTest");
		TypeAtom type = new TypeAtom(name, TypeRepresentation.NATIVE);
		Lambda constructor = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
				new LitComposite(Expression.EMPTY_EXPRESSION, type));

		DefineConstructor defCon = new DefineConstructor(type, constructor);

		defCon.toString();

		TestInterpretation.testReflexivity(defCon);
		TestInterpretation.testDifference(defCon, new DefineConstructor(TypeAtom.TypeIntNative, constructor));
		TestInterpretation.testDifference(defCon, new DefineConstructor(type, Lambda.identity));

		TypeEnvironment.singleton.addType(name);
		TypeEnvironment.singleton.addRepresentation(type);
		TestInterpretation.testInterpretation(defCon, Expression.EMPTY_EXPRESSION, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = defCon.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(Environment.topLevelEnvironment).first,
				defCon);
	}

	@Test
	void testConstruct() throws AppendableException {
		Construct construct = new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XLII"))));

		construct.toString();

		TestInterpretation.testReflexivity(construct);
		TestInterpretation.testDifference(construct,
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("XLII")))));
		TestInterpretation.testDifference(construct,
				new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XXI")))));
		TestInterpretation.testDifference(construct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(construct, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = construct.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, construct);
	}

	@Test
	void testDeconstruct() throws AppendableException {
		Deconstruct deconstruct = new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
				TypeAtom.TypeStringNative);

		deconstruct.toString();

		TestInterpretation.testReflexivity(deconstruct);
		TestInterpretation.testDifference(deconstruct, new Deconstruct(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeStringNative));
		TestInterpretation.testDifference(deconstruct,
				new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(deconstruct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(deconstruct, new LitString("42"), Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = deconstruct.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeStringNative, deconstruct);
	}

	@Test
	void testCanDeconstructAs() throws AppendableException {
		CanDeconstructAs canDeconstruct = new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeStringNative);
		CanDeconstructAs cannotDeconstrut = new CanDeconstructAs(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeIntNative);

		canDeconstruct.toString();

		TestInterpretation.testReflexivity(canDeconstruct);
		TestInterpretation.testDifference(canDeconstruct, new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(canDeconstruct,
				new CanDeconstructAs(new LitDouble(3.14), TypeAtom.TypeStringNative));
		TestInterpretation.testDifference(canDeconstruct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(canDeconstruct, LitBoolean.TRUE, Environment.topLevelEnvironment);
		TestInterpretation.testInterpretation(cannotDeconstrut, LitBoolean.FALSE, Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = canDeconstruct.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, canDeconstruct);
	}

	@Test
	void testConvert() throws AppendableException {
		Convert convert = new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(42));

		convert.toString();

		TestInterpretation.testReflexivity(convert);
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, new LitInteger(42)));
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, new LitInteger(42)));
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(21)));
		TestInterpretation.testDifference(convert, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(convert, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				Environment.topLevelEnvironment);

		Pair<Type, Substitution> p = convert.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, convert);
	}

	@Test
	void testListNative() throws AppendableException {		
		this.testInterpretString("(construct List Native)",
				new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative));
		this.testInterpretString("(construct List Native 42 (construct List Native))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(42),
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative));

		this.testInterpretString("(is-list-native-empty (construct List Native))", LitBoolean.TRUE);
		this.testInterpretString("(is-list-native-empty (construct List Native 42 (construct List Native)))",
				LitBoolean.FALSE);

		this.testInterpretString("(head-list-native (construct List Native 42 (construct List Native)))",
				new LitInteger(42));
		Assertions.assertThrows(UserException.class, () -> this
				.testInterpretString("(head-list-native (construct List Native))", Expression.EMPTY_EXPRESSION));

		this.testInterpretString("(tail-list-native (construct List Native 42 (construct List Native)))",
				new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative));
		Assertions.assertThrows(UserException.class, () -> this
				.testInterpretString("(tail-list-native (construct List Native))", Expression.EMPTY_EXPRESSION));

		this.testInterpretString(
				"(map-list-native (lambda (x) (+ x 1)) (construct List Native 42 (construct List Native)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(43),
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative));
	}

	private Expression parseString(String s) throws AppendableException {
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return SemanticParser.parseNode(exprsContext.val.get(0));
	}

	private static void testReflexivity(Expression original) {
		Expression e = original;
		if (!original.equals(e)) {
			fail(original.toString() + " and " + e.toString() + " should be equal");
		}
		int hash1 = original.hashCode();
		int hash2 = original.hashCode();
		if (hash1 != hash2) {
			fail("Hash of " + original + " is incosnsistent got " + hash1 + " and " + hash2);
		}
		int cmp = original.compareTo(e);
		if (cmp != 0) {
			fail("Expecting " + original.toString() + ".compareTo(" + e.toString() + ") == 0 got " + cmp);
		}
	}

	private static void testDifference(Expression original, Expression e) {
		if (original.equals(e)) {
			fail(original.toString() + " and " + e.toString() + " should not equal");
		}
		int cmp = original.compareTo(e);
		if (cmp == 0) {
			fail("Expecting " + original.toString() + ".compareTo(" + e.toString() + ") != 0 got " + cmp);
		}
	}

	private static void testInference(Pair<Type, Substitution> result, Type expected, Expression infered) {
		TestInterpretation.testInference(result, expected, infered, false);
	}

	private static void testInference(Pair<Type, Substitution> p, Type expected, Expression infered,
			boolean shouldeBeSUbstEmpty) {
		if (!p.first.equals(expected)) {
			fail("Type of " + infered + " should be " + expected + " got " + p.first);
		}
		if (shouldeBeSUbstEmpty && !p.second.equals(Substitution.EMPTY)) {
			fail("Substitution for inference of " + infered + " should be empty got " + p.second);
		}
	}

	private static void testInferenceOfType(Pair<Type, Substitution> p, Class<? extends Type> expected,
			Expression infered) {
		TestInterpretation.testInferenceOfType(p, expected, infered, false);
	}

	private static void testInferenceOfType(Pair<Type, Substitution> p, Class<? extends Type> expected,
			Expression infered, boolean shouldBeSubstEmpty) {
		if (!(expected.isInstance(p.first))) {
			fail("Type of " + infered + " should be of class " + expected.getName() + " got " + p.first + " of class "
					+ p.first.getClass().getName());
		}
		if (shouldBeSubstEmpty && !p.second.equals(Substitution.EMPTY)) {
			fail("Substitution for inference of " + infered + " should be empty got " + p.second);
		}
	}

	private static void testInterpretation(Expression interpreted, Expression expected, Environment env)
			throws AppendableException {
		Expression e = interpreted.interpret(env);
		if (!e.equals(expected)) {
			fail("Interpretation of " + interpreted + " in " + env + " expected " + expected + " got " + e);
		}
	}

	private static void testOperator(final Operator operator, Tuple args, Expression expectedInterpret,
			Type expectedInference) throws AppendableException {
		AbstractionApplication application = new AbstractionApplication(operator, args);

		TestInterpretation.testInterpretation(application, expectedInterpret, Environment.topLevelEnvironment);
		Pair<Type, Substitution> p = application.infer(Environment.topLevelEnvironment);
		if (expectedInference != null) {
			TestInterpretation.testInference(p, expectedInference, application);
		}

		operator.toString();
		operator.toClojureCode();
	}

	private static void testConversion(Abstraction conversion, Expression argument, Expression expectedInterpret,
			Type expectedInfer) throws AppendableException {
		conversion.toString();

		AbstractionApplication appl = new AbstractionApplication(conversion, new Tuple(Arrays.asList(argument)));
		TestInterpretation.testInterpretation(appl, expectedInterpret, Environment.topLevelEnvironment);
		Pair<Type, Substitution> p = conversion.infer(Environment.topLevelEnvironment);
		TestInterpretation.testInference(p, expectedInfer, conversion);
	}

	private void testInterpretString(String interpreted, Expression expected) throws AppendableException {
		Expression e = this.parseString(interpreted);
		TestInterpretation.testInterpretation(e, expected, Environment.topLevelEnvironment);
	}
}
