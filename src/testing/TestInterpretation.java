package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import conversions.IntRomanToIntWrapper;
import conversions.IntStringToIntWrapper;
import expression.Application;
import expression.Constructor;
import expression.DefExpression;
import expression.ExceptionExpr;
import expression.Expression;
import expression.ExtendedLambda;
import expression.Function;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitInteger;
import expression.LitString;
import expression.MetaFunction;
import expression.MetaLambda;
import expression.Tuple;
import expression.TypeConstructionLambda;
import expression.TypeHolder;
import expression.Variable;
import interpretation.Environment;
import main.Main;
import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import semantic.SemanticParser;
import semantic.UserException;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import util.Pair;

class TestInterpretation {

	SemanticParser semanticParser = new SemanticParser();

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
	void testLitString() throws AppendableException {
		LitString litString = new LitString("test");

		TestInterpretation.testReflexivity(litString);
		TestInterpretation.testDifference(litString, new LitString(" "));
		TestInterpretation.testDifference(litString, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litString, litString, new Environment());

		litString.toString();
		litString.toClojureCode();

		Pair<Type, Substitution> p = litString.infer(new Environment());
		TestInterpretation.testInference(p, TypeConcrete.TypeString, litString, true);
	}

	@Test
	public void testLitInteger() throws AppendableException {
		LitInteger litInteger = new LitInteger(128);

		TestInterpretation.testReflexivity(litInteger);
		TestInterpretation.testDifference(litInteger, new LitInteger(0));
		TestInterpretation.testDifference(litInteger, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litInteger, litInteger, new Environment());

		litInteger.toString();
		litInteger.toClojureCode();

		Pair<Type, Substitution> p = litInteger.infer(new Environment());
		TestInterpretation.testInference(p, TypeConcrete.TypeInt, litInteger, true);
	}

	@Test
	public void testLitDouble() throws AppendableException {
		LitDouble litDouble = new LitDouble(3.14);

		TestInterpretation.testReflexivity(litDouble);
		TestInterpretation.testDifference(litDouble, new LitDouble(0));
		TestInterpretation.testDifference(litDouble, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litDouble, litDouble, new Environment());

		litDouble.toString();
		litDouble.toClojureCode();
		Pair<Type, Substitution> p = litDouble.infer(new Environment());
		TestInterpretation.testInference(p, TypeConcrete.TypeDouble, litDouble, true);
	}

	@Test
	public void testLitBoolean() throws AppendableException {
		TestInterpretation.testReflexivity(LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, LitBoolean.FALSE);
		TestInterpretation.testDifference(LitBoolean.FALSE, LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(LitBoolean.TRUE, LitBoolean.TRUE, new Environment());

		LitBoolean.TRUE.toString();
		LitBoolean.TRUE.toClojureCode();

		Pair<Type, Substitution> p = LitBoolean.TRUE.infer(new Environment());
		TestInterpretation.testInference(p, TypeConcrete.TypeBool, LitBoolean.TRUE, true);
	}

	@Test
	public void testTypeHolder() {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);

		Assertions.assertThrows(AppendableException.class, () -> typeHolder.interpret(new Environment()));
		Assertions.assertThrows(AppendableException.class, () -> typeHolder.toClojureCode());
		typeHolder.toString();

		TestInterpretation.testReflexivity(typeHolder);
		TestInterpretation.testDifference(typeHolder, new TypeHolder(TypeConcrete.TypeInt));
		TestInterpretation.testDifference(typeHolder, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = typeHolder.infer(new Environment());
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, typeHolder, true);
	}

	@Test
	public void testVariable() throws AppendableException {
		Variable variable = new Variable("x");

		variable.toString();
		variable.toClojureCode();

		TestInterpretation.testReflexivity(variable);
		TestInterpretation.testDifference(variable, new Variable("y"));
		TestInterpretation.testDifference(variable, Expression.EMPTY_EXPRESSION);

		LitInteger value = new LitInteger(128);
		Environment bound = new Environment();
		bound.put(variable, value);

		TestInterpretation.testInterpretation(variable, variable, new Environment());
		Pair<Type, Substitution> p = variable.infer(new Environment());
		TestInterpretation.testInferenceOfType(p, TypeVariable.class, variable);

		TestInterpretation.testInterpretation(variable, value, bound);
		p = variable.infer(bound);
		TestInterpretation.testInference(p, TypeConcrete.TypeInt, variable);

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
	public void testEmptyExpression() throws AppendableException {
		TestInterpretation.testInference(Expression.EMPTY_EXPRESSION.infer(new Environment()), TypeTuple.EMPTY_TUPLE,
				Expression.EMPTY_EXPRESSION, true);
		TestInterpretation.testInterpretation(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION,
				new Environment());
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
		tuple.stream();

		TestInterpretation.testReflexivity(tuple);
		TestInterpretation.testDifference(tuple, Tuple.EMPTY_TUPLE);
		TestInterpretation.testDifference(tuple, Expression.EMPTY_EXPRESSION);
		TestInterpretation.testDifference(tuple,
				new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2))));

		TestInterpretation.testInterpretation(tuple, tuple, new Environment());
		Pair<Type, Substitution> p = tuple.infer(new Environment());
		if (!(p.first instanceof TypeTuple)) {
			fail("Infered type of " + Tuple.class.getName() + " should be " + TypeTuple.class.getName());
		}
		Type t = ((TypeTuple) p.first).get(0);
		if (!t.equals(TypeConcrete.TypeInt)) {
			fail("First type of " + tuple.toString() + " should be " + TypeConcrete.TypeInt.toString() + " got " + t);
		}
		t = ((TypeTuple) p.first).get(1);
		if (!(t instanceof TypeVariable)) {
			fail("Second type of " + tuple.toString() + " in empty environment should be "
					+ TypeVariable.class.getName() + " got " + t);
		}
		t = ((TypeTuple) p.first).get(2);
		if (!t.equals(TypeConcrete.TypeBool)) {
			fail("Third type of " + tuple.toString() + " should be " + TypeConcrete.TypeBool.toString() + " got " + t);
		}

		Environment bound = new Environment();
		bound.put(new Variable("x"), new LitDouble(3.14));

		TestInterpretation.testInterpretation(tuple,
				new Tuple(Arrays.asList(new LitInteger(128), new LitDouble(3.14), LitBoolean.FALSE)), bound);
		p = tuple.infer(bound);
		TestInterpretation.testInference(p,
				new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeDouble, TypeConcrete.TypeBool)),
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
			public String toClojureCode() throws AppendableException {
				return null;
			}
		}))).infer(new Environment()));
	}

	@Test
	void testExceptionExpr() throws AppendableException {
		final ExceptionExpr exception = new ExceptionExpr(new LitString("test"));

		Assertions.assertThrows(UserException.class, () -> exception.interpret(new Environment()));

		exception.toClojureCode();
		exception.toString();

		TestInterpretation.testReflexivity(exception);
		TestInterpretation.testDifference(exception, new ExceptionExpr(new LitString("fail")));
		TestInterpretation.testDifference(exception, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = exception.infer(new Environment());
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
			public String toClojureCode() throws AppendableException {
				return null;
			}
		}).infer(new Environment()));
	}

	@Test
	void testDefExpression() throws AppendableException {
		DefExpression defExpression = new DefExpression(new Variable("pi"), new LitDouble(Math.PI));

		defExpression.toString();
		defExpression.toClojureCode();

		TestInterpretation.testReflexivity(defExpression);
		TestInterpretation.testDifference(defExpression, new DefExpression(new Variable("e"), new LitDouble(Math.E)));
		TestInterpretation.testDifference(defExpression,
				new DefExpression(new Variable("pi"), new LitDouble(3.141521)));
		TestInterpretation.testDifference(defExpression, Expression.EMPTY_EXPRESSION);

		Environment env = new Environment();
		TestInterpretation.testInterpretation(defExpression, Expression.EMPTY_EXPRESSION, env);
		if (!env.containsVariable(new Variable("pi"))) {
			fail("Environment after interpretation of " + defExpression.toString() + " should contain "
					+ new Variable("pi").toString() + " got " + env);
		}

		Pair<Type, Substitution> p = defExpression.infer(new Environment());
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, defExpression);
		if (p.second.isEmpty()) {
			fail("Substitution for " + defExpression.toString() + " should not be empty, got " + p.second.toString());
		}
		if (p.second.keySet().stream().findAny().get().equals(TypeConcrete.TypeDouble)) {
			fail("In substitution should be new variable substituted for " + TypeConcrete.TypeDouble.toString()
					+ " got " + p.second);
		}

		DefExpression recursiveExpression = (DefExpression) this
				.parseString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))");
		p = recursiveExpression.infer(Main.initTopLevelEnvironment());
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, recursiveExpression);
		if (p.second.isEmpty()) {
			fail("Substitution for " + defExpression.toString() + " should not be empty, got " + p.second.toString());
		}

		Assertions.assertThrows(AppendableException.class,
				() -> new DefExpression(new Variable("fail"), new Expression() {

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
				}).infer(new Environment()));
	}

	@Test
	void testLambda() throws AppendableException {
		new Lambda(new Tuple(Arrays.asList(new Variable("x"))), Expression.EMPTY_EXPRESSION);
		new Lambda(new Variable("x"), Expression.EMPTY_EXPRESSION);
		final Lambda lambda = new Lambda(new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
				new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)), new Variable("x"));
		final Lambda faultArgsLambda = new Lambda(new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)),
				Expression.EMPTY_EXPRESSION);

		lambda.toString();
		lambda.toClojureCode();
		Assertions.assertThrows(AppendableException.class, () -> faultArgsLambda.toClojureCode());

		lambda.getLambda();
		lambda.getLambda(new Comparator<Lambda>() {

			@Override
			public int compare(Lambda arg0, Lambda arg1) {
				return 0;
			}
		});

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda,
				new Lambda(new Tuple(Arrays.asList(new Variable("z"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)), new Variable("x")));
		TestInterpretation.testDifference(lambda,
				new Lambda(new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeDouble, TypeConcrete.TypeInt)),
						new Variable("x")));
		TestInterpretation.testDifference(lambda,
				new Lambda(new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						Expression.EMPTY_EXPRESSION));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		Expression e = lambda.interpret(new Environment());
		if (!(e instanceof Function)) {
			fail("Interpreted " + lambda.toString() + " should yield " + Function.class.getName() + " got " + e);
		}

		Pair<Type, Substitution> p = lambda.infer(new Environment());
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, lambda);

		Assertions.assertThrows(AppendableException.class, () -> faultArgsLambda.infer(new Environment()));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))").infer(Main.initTopLevelEnvironment()));
	}

	@Test
	void testFunction() throws AppendableException {
		Environment bound = new Environment();
		bound.put(new Variable("bound"), new LitDouble(3.141521));

		final Function function = new Function(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("x"))), new Variable("bound"), bound);

		function.toString();
		function.getFunction();
		function.getFunction(new Comparator<Function>() {

			@Override
			public int compare(Function arg0, Function arg1) {
				return 0;
			}
		});

		TestInterpretation.testReflexivity(function);
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeConcrete.TypeDouble)),
				new Tuple(Arrays.asList(new Variable("x"))), new Variable("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("y"))), new Variable("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("x"))), Expression.EMPTY_EXPRESSION, bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("x"))), new Variable("bound"), new Environment()));
		TestInterpretation.testDifference(function, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(function, function, new Environment());

		Pair<Type, Substitution> p = function.infer(new Environment());
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, function);

		Assertions.assertThrows(AppendableException.class,
				() -> (new Function(new TypeTuple(Arrays.asList(new TypeVariable("x"))),
						new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)), new Variable("y"), new Environment()))
								.infer(new Environment()));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))").interpret(Main.initTopLevelEnvironment())
						.infer(Main.initTopLevelEnvironment()));
	}

	@Test
	void testMetaLambda() {
		Expression e = new Lambda(new Variable(""), Expression.EMPTY_EXPRESSION);
		if (!MetaLambda.isApplicableExpression(e)) {
			fail(e.toString() + " is applicable expression!");
		}
		e = Expression.EMPTY_EXPRESSION;
		if (MetaLambda.isApplicableExpression(e)) {
			fail(e.toString() + " is not an applicable expression!");
		}
	}

	@Test
	void testMetaFunction() {
		TestInterpretation.testDifference(new MetaFunction(new Environment()) {

			@Override
			public Function getFunction() {
				return null;
			}

			@Override
			public Function getFunction(Comparator<? super Function> c) {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				return null;
			}
		}, Expression.EMPTY_EXPRESSION);

		Function function = new Function(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION,
				new Environment());
		Assertions.assertThrows(AppendableException.class, () -> function.toClojureCode());

		Expression e = function;
		if (!MetaFunction.isFunction(e)) {
			fail(e.toString() + " is function!");
		}
		e = Expression.EMPTY_EXPRESSION;
		if (MetaFunction.isFunction(e)) {
			fail(e.toString() + " is not function!");
		}
	}

	@Test
	void testTypeConstructionLambda() throws AppendableException {
		TypeConstructionLambda lambda = new TypeConstructionLambda(TypeConcrete.TypeDouble,
				new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
				new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))));

		lambda.toString();

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda,
				new TypeConstructionLambda(TypeConcrete.TypeInt,
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y")))));
		TestInterpretation.testDifference(lambda,
				new TypeConstructionLambda(TypeConcrete.TypeDouble,
						new Tuple(Arrays.asList(new Variable("z"), new Variable("w"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y")))));
		TestInterpretation.testDifference(lambda,
				new TypeConstructionLambda(TypeConcrete.TypeDouble,
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeDouble, TypeConcrete.TypeDouble)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y")))));
		TestInterpretation.testDifference(lambda,
				new TypeConstructionLambda(TypeConcrete.TypeDouble,
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("z"), new Variable("w")))));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		Expression e = lambda.interpret(new Environment());
		if (!(e instanceof Constructor)) {
			fail(lambda.toString() + " should interpret to " + Constructor.class.getName() + " got " + e.toString());
		}

		Pair<Type, Substitution> p = lambda.infer(new Environment());
		TestInterpretation.testInference(p,
				new TypeArrow(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						TypeConcrete.TypeDouble),
				lambda);

		Assertions.assertThrows(AppendableException.class, () -> new TypeConstructionLambda(TypeTuple.EMPTY_TUPLE,
				Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE, new Expression() {

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
				}).infer(new Environment()));
	}

	@Test
	void testConstructor() throws AppendableException {
		Environment env = new Environment();
		env.put(new Variable("z"), LitBoolean.TRUE);

		Constructor constructor = new Constructor(
				new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
				new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
				new Tuple(Arrays.asList(new Variable("x"), new Variable("y"), new Variable("z"))),
				TypeConcrete.TypeDouble, env);

		constructor.toString();

		TestInterpretation.testReflexivity(constructor);
		TestInterpretation.testDifference(constructor,
				new Constructor(new TypeTuple(Arrays.asList(TypeConcrete.TypeBool, TypeConcrete.TypeBool)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"), new Variable("z"))),
						TypeConcrete.TypeDouble, env));
		TestInterpretation.testDifference(constructor,
				new Constructor(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("w"), new Variable("q"))),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"), new Variable("z"))),
						TypeConcrete.TypeDouble, env));
		TestInterpretation.testDifference(constructor,
				new Constructor(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new Tuple(Arrays.asList(new Variable("w"), new Variable("q"), new Variable("r"))),
						TypeConcrete.TypeDouble, env));
		TestInterpretation.testDifference(constructor,
				new Constructor(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"), new Variable("z"))),
						TypeConcrete.TypeBool, env));
		TestInterpretation.testDifference(constructor,
				new Constructor(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"))),
						new Tuple(Arrays.asList(new Variable("x"), new Variable("y"), new Variable("z"))),
						TypeConcrete.TypeDouble, new Environment()));
		TestInterpretation.testDifference(constructor, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = constructor.infer(new Environment());
		TestInterpretation.testInference(p,
				new TypeArrow(new TypeTuple(Arrays.asList(TypeConcrete.TypeInt, TypeConcrete.TypeInt)),
						TypeConcrete.TypeDouble),
				constructor);
		Assertions.assertThrows(AppendableException.class,
				() -> (new Constructor(TypeTuple.EMPTY_TUPLE, Tuple.EMPTY_TUPLE, new Expression() {

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
				}, TypeTuple.EMPTY_TUPLE, new Environment())).infer(new Environment()));
	}

	@Test
	void testExpendedLambda() throws AppendableException {
		ExtendedLambda lambda = new ExtendedLambda(Arrays.asList(
				new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
						new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Variable("x")),
				new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
						new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntRoman)),
						new Application(IntRomanToIntWrapper.IntRomanToInt,
								new Tuple(Arrays.asList(new Variable("x"))))),
				new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
						new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)), new Application(
								IntStringToIntWrapper.IntStringToInt, new Tuple(Arrays.asList(new Variable("x")))))));

		ExtendedLambda noDefaultImplementation = new ExtendedLambda(
				Arrays.asList(new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)), new Variable("x"))));

		new ExtendedLambda(new Tuple(Arrays.asList(new Variable("x"))), new Variable("x"), new LinkedList<Lambda>());

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda,
				new ExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Variable("y"))),
								new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Variable("x")),
						new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntRoman)),
								new Application(IntRomanToIntWrapper.IntRomanToInt,
										new Tuple(Arrays.asList(new Variable("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)),
								new Application(IntStringToIntWrapper.IntStringToInt,
										new Tuple(Arrays.asList(new Variable("x"))))))));
		TestInterpretation.testDifference(lambda,
				new ExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Variable("x")),
						new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntRoman)),
								new Application(IntRomanToIntWrapper.IntRomanToInt,
										new Tuple(Arrays.asList(new Variable("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Variable("x"))),
								new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)),
								new Application(IntStringToIntWrapper.IntStringToInt,
										new Tuple(Arrays.asList(new Variable("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Variable("y"))),
								new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)),
								new Application(IntStringToIntWrapper.IntStringToInt,
										new Tuple(Arrays.asList(new Variable("x"))))))));
		TestInterpretation.testDifference(lambda,
				new ExtendedLambda(Arrays.asList(new Lambda(new Tuple(Arrays.asList(new Variable("y"))),
						new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)), new Variable("x")))));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		lambda.getLambda();
		lambda.getLambda(new Comparator<Lambda>() {

			@Override
			public int compare(Lambda arg0, Lambda arg1) {
				return 0;
			}
		});
		lambda.toClojureCode();
		lambda.toString();
		Assertions.assertThrows(AppendableException.class, () -> noDefaultImplementation.defaultImplementation());
		noDefaultImplementation.toString();
		lambda.defaultImplementation();
		lambda.getSortedImplementations(new Comparator<Lambda>() {

			@Override
			public int compare(Lambda arg0, Lambda arg1) {
				return 0;
			}
		});
		lambda.getSortedImplementations();
		
		lambda.interpret(new Environment());
		
		Pair<Type, Substitution> p = lambda.infer(new Environment());
		new Variable("y");
	}

	private Expression parseString(String s) throws AppendableException {
		@SuppressWarnings("deprecation")
		CharStream charStream = new ANTLRInputStream(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return semanticParser.parseNode(exprsContext.val.get(0));
	}

	private static void testReflexivity(Expression original) {
		Expression e = original;
		if (!original.equals(e)) {
			fail(original.toString() + " and " + e.toString() + " should be equal");
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
		if (shouldeBeSUbstEmpty && !p.second.isEmpty()) {
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
		if (shouldBeSubstEmpty && !p.second.isEmpty()) {
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
}
