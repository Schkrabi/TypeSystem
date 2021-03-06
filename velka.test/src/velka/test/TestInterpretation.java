package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.ConversionOperators;
import velka.core.abstraction.ExtendedFunction;
import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Function;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.abstraction.Operators;
import velka.core.application.AbstractionApplication;
import velka.core.application.AndExpression;
import velka.core.application.CanDeconstructAs;
import velka.core.application.Construct;
import velka.core.application.Convert;
import velka.core.application.Deconstruct;
import velka.core.application.DefineConstructor;
import velka.core.application.DefineConversion;
import velka.core.application.DefineRepresentation;
import velka.core.application.DefineSymbol;
import velka.core.application.DefineType;
import velka.core.application.ExceptionExpr;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.OrExpression;
import velka.core.exceptions.InvalidNumberOfArgumentsException;
import velka.core.exceptions.UnboundVariableException;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.expression.TypeSymbol;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitEnum;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.parser.Parser;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

class TestInterpretation {

	@Test
	@DisplayName("Test String Literal")
	void testLitString() throws AppendableException {
		LitString litString = new LitString("test");
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litString.toString();
			litString.hashCode();
			litString.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(litString);
		TestInterpretation.testDifference(litString, new LitString(" "));
		TestInterpretation.testDifference(litString, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litString, litString, env, typeEnv);

		Pair<Type, Substitution> p = litString.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeStringNative, litString, true);
	}

	@Test
	@DisplayName("Test Integer Literal")
	public void testLitInteger() throws AppendableException {
		LitInteger litInteger = new LitInteger(128);
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litInteger.toString();
			litInteger.hashCode();
			litInteger.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(litInteger);
		TestInterpretation.testDifference(litInteger, new LitInteger(0));
		TestInterpretation.testDifference(litInteger, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litInteger, litInteger, env, typeEnv);

		Pair<Type, Substitution> p = litInteger.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, litInteger, true);
	}

	@Test
	@DisplayName("Test Double Literal")
	public void testLitDouble() throws AppendableException {
		LitDouble litDouble = new LitDouble(3.14);
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litDouble.toString();
			litDouble.hashCode();
			litDouble.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(litDouble);
		TestInterpretation.testDifference(litDouble, new LitDouble(0));
		TestInterpretation.testDifference(litDouble, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(litDouble, litDouble, env, typeEnv);

		Pair<Type, Substitution> p = litDouble.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeDoubleNative, litDouble, true);
	}

	@Test
	@DisplayName("Test Boolean Literal")
	public void testLitBoolean() throws AppendableException {
		TestInterpretation.testReflexivity(LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, LitBoolean.FALSE);
		TestInterpretation.testDifference(LitBoolean.FALSE, LitBoolean.TRUE);
		TestInterpretation.testDifference(LitBoolean.TRUE, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInterpretation(LitBoolean.TRUE, LitBoolean.TRUE, env, typeEnv);

		assertAll(() -> {
			LitBoolean.TRUE.toString();
			LitBoolean.TRUE.toClojureCode(env, typeEnv);
		});

		Pair<Type, Substitution> p = LitBoolean.TRUE.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, LitBoolean.TRUE, true);
	}

	@Test
	@DisplayName("Test Enum Literal")
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

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInterpretation(enumValue1, enumValue1, env, typeEnv);

		Pair<Type, Substitution> p = enumValue1.infer(env, typeEnv);
		TestInterpretation.testInference(p, type, enumValue1, true);

		assertAll(() -> {
			enumValue1.toString();
			// Not implemented yet
			// enumValue1.toClojureCode(env, typeEnv);
		});
	}

	@Test
	@DisplayName("Test Composite Literal")
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

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInterpretation(composite1, composite1, env, typeEnv);

		Pair<Type, Substitution> p = composite1.infer(env, typeEnv);
		TestInterpretation.testInference(p, type, composite1);

		assertAll(() -> {
			composite1.toString();
			composite1.toClojureCode(env, typeEnv);
		});
	}

	@Test
	@DisplayName("Test Type Holder")
	public void testTypeHolder() throws AppendableException {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(AppendableException.class, () -> typeHolder.interpret(env, typeEnv));
		assertThrows(AppendableException.class, () -> typeHolder.toClojureCode(env, typeEnv));
		assertAll(() -> {
			typeHolder.toString();
			typeHolder.hashCode();
		});

		TestInterpretation.testReflexivity(typeHolder);
		TestInterpretation.testDifference(typeHolder, new TypeHolder(TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(typeHolder, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = typeHolder.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, typeHolder, true);

		TypeHolder placeholder = new TypeHolder(TypeAtom.TypeInt, new Symbol("x"));
		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitInteger(42));

		TestInterpretation.testInterpretation(placeholder, new LitInteger(42), bound, typeEnv);
		assertThrows(AppendableException.class, () -> placeholder.interpret(env, typeEnv));

		TypeHolder placeholder2 = new TypeHolder(TypeAtom.TypeInt, new Symbol("__q"));
		env.put(new Symbol("__q"), placeholder2);
		assertThrows(AppendableException.class, () -> placeholder2.interpret(env, typeEnv));

		Environment bound2 = Environment.create(bound);
		bound2.put(new Symbol("x"), placeholder);
		TestInterpretation.testInterpretation(placeholder, new LitInteger(42), bound2, typeEnv);
	}

	@Test
	@DisplayName("Test Symbol")
	public void testVariable() throws AppendableException {
		Symbol variable = new Symbol("x");
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			variable.toString();
			variable.hashCode();
			variable.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(variable);
		TestInterpretation.testDifference(variable, new Symbol("y"));
		TestInterpretation.testDifference(variable, Expression.EMPTY_EXPRESSION);

		LitInteger value = new LitInteger(128);
		Environment bound = Environment.create(env);
		bound.put(variable, value);

		TestInterpretation.testInterpretation(variable, variable, env, typeEnv);
		Pair<Type, Substitution> p = variable.infer(env, typeEnv);
		TestInterpretation.testInferenceOfType(p, TypeVariable.class, variable);

		TestInterpretation.testInterpretation(variable, value, bound, typeEnv);
		p = variable.infer(bound, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, variable);

		final Environment fault = Environment.create(env);
		fault.put(variable, new Expression() {

			@Override
			public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}
		});
		assertThrows(AppendableException.class, () -> variable.infer(fault, typeEnv));
	}

	@Test
	@DisplayName("Test Empty Expression")
	public void testEmptyExpression() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInference(Expression.EMPTY_EXPRESSION.infer(env, typeEnv), TypeTuple.EMPTY_TUPLE,
				Expression.EMPTY_EXPRESSION, true);
		TestInterpretation.testInterpretation(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION, env, typeEnv);

		assertAll(() -> {
			Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv);
		});
	}

	@Test
	@DisplayName("Test Tuple")
	public void testTuple() throws Exception {
		final Tuple tuple = new Tuple(Arrays.asList(new LitInteger(128), new Symbol("x"), LitBoolean.FALSE));
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			tuple.get(0);
			tuple.size();
			tuple.toString();
			tuple.hashCode();
			tuple.toClojureCode(env, typeEnv);
			tuple.stream();
		});

		assertThrows(ArrayIndexOutOfBoundsException.class, () -> tuple.get(4));

		TestInterpretation.testReflexivity(tuple);
		TestInterpretation.testDifference(tuple, Tuple.EMPTY_TUPLE);
		TestInterpretation.testDifference(tuple, Expression.EMPTY_EXPRESSION);
		TestInterpretation.testDifference(tuple,
				new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2))));

		TestInterpretation.testInterpretation(tuple, tuple, env, typeEnv);
		Pair<Type, Substitution> p = tuple.infer(env, typeEnv);
		assertTrue(p.first instanceof TypeTuple);
		assertEquals(((TypeTuple) p.first).get(0), TypeAtom.TypeIntNative);
		assertTrue(((TypeTuple) p.first).get(1) instanceof TypeVariable);
		assertEquals(((TypeTuple) p.first).get(2), TypeAtom.TypeBoolNative);

		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitDouble(3.14));

		TestInterpretation.testInterpretation(tuple,
				new Tuple(Arrays.asList(new LitInteger(128), new LitDouble(3.14), LitBoolean.FALSE)), bound, typeEnv);
		p = tuple.infer(bound, typeEnv);
		TestInterpretation.testInference(p,
				new TypeTuple(
						Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeDoubleNative, TypeAtom.TypeBoolNative)),
				tuple);

		assertThrows(AppendableException.class, () -> (new Tuple(Arrays.asList(new Expression() {

			@Override
			public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}
		}))).infer(env, typeEnv));
	}

	@Test
	@DisplayName("Test User Exception")
	void testExceptionExpr() throws AppendableException {
		final ExceptionExpr exception = new ExceptionExpr(new LitString("test"));
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(UserException.class, () -> exception.interpret(env, typeEnv));

		assertAll(() -> {
			exception.toClojureCode(env, typeEnv);
			exception.toString();
			exception.hashCode();
		});

		TestInterpretation.testReflexivity(exception);
		TestInterpretation.testDifference(exception, new ExceptionExpr(new LitString("fail")));
		TestInterpretation.testDifference(exception, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = exception.infer(env, typeEnv);
		TestInterpretation.testInferenceOfType(p, TypeVariable.class, exception);

		assertThrows(AppendableException.class, () -> new ExceptionExpr(new Expression() {

			@Override
			public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}
		}).infer(env, typeEnv));
	}

	@Test
	@DisplayName("Test Define Expression")
	void testDefExpression() throws AppendableException {
		DefineSymbol defExpression = new DefineSymbol(new Symbol("pi"), new LitDouble(Math.PI));
		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defExpression.toString();
			defExpression.hashCode();
			defExpression.toClojureCode(top, typeEnv);
		});

		TestInterpretation.testReflexivity(defExpression);
		TestInterpretation.testDifference(defExpression, new DefineSymbol(new Symbol("e"), new LitDouble(Math.E)));
		TestInterpretation.testDifference(defExpression, new DefineSymbol(new Symbol("pi"), new LitDouble(3.141521)));
		TestInterpretation.testDifference(defExpression, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.create(top);
		TestInterpretation.testInterpretation(defExpression, Expression.EMPTY_EXPRESSION, env, typeEnv);
		assertTrue(env.containsVariable(new Symbol("pi")));

		Pair<Type, Substitution> p = defExpression.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, defExpression);
		assertNotEquals(p.second, Substitution.EMPTY);
		assertNotEquals(p.second.variableStream().findAny().get(), TypeAtom.TypeDoubleNative);

		DefineSymbol recursiveExpression = (DefineSymbol) TestInterpretation
				.parseString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))");
		p = recursiveExpression.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeTuple.EMPTY_TUPLE, recursiveExpression);
		assertNotEquals(p.second, Substitution.EMPTY);

		assertThrows(AppendableException.class, () -> new DefineSymbol(new Symbol("fail"), new Expression() {

			@Override
			public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				throw new AppendableException("test");
			}

			@Override
			public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return null;
			}
		}).infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Lambda")
	void testLambda() throws AppendableException {
		final Lambda lambda = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x"));
		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top, typeEnv);
		});

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

		Expression e = lambda.interpret(top, typeEnv);
		assertTrue(e instanceof Function);

		Pair<Type, Substitution> p = lambda.infer(top, typeEnv);
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, lambda);

		assertThrows(TypesDoesNotUnifyException.class,
				() -> TestInterpretation.parseString("(lambda ((String x)) (+ x x))").infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Function")
	void testFunction() throws AppendableException {
		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		Environment bound = Environment.create(top);
		bound.put(new Symbol("bound"), new LitDouble(3.141521));

		final Function function = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		TestInterpretation.testReflexivity(function);
		TestInterpretation.testDifference(function,
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
						new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("bound"), bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), Expression.EMPTY_EXPRESSION, bound));
		TestInterpretation.testDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), top));
		TestInterpretation.testDifference(function, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(function, function, top, typeEnv);

		Pair<Type, Substitution> p = function.infer(top, typeEnv);
		TestInterpretation.testInferenceOfType(p, TypeArrow.class, function);

		assertThrows(AppendableException.class,
				() -> (new Function(new TypeTuple(Arrays.asList(new TypeVariable("x"))),
						new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)), new Symbol("y"), top)).infer(top,
								typeEnv));
		assertThrows(TypesDoesNotUnifyException.class, () -> TestInterpretation
				.parseString("(lambda ((String x)) (+ x x))").interpret(top, typeEnv).infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Extended Lambda")
	void testExpendedLambda() throws AppendableException {
		ExtendedLambda lambda = ExtendedLambda.makeExtendedLambda(Arrays.asList(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
						new AbstractionApplication(ConversionOperators.IntRomanToIntNative,
								new Tuple(Arrays.asList(new Symbol("x"))))),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new AbstractionApplication(
								ConversionOperators.IntStringToIntNative, new Tuple(Arrays.asList(new Symbol("x")))))));

		TestInterpretation.testReflexivity(lambda);
		TestInterpretation.testDifference(lambda,
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new AbstractionApplication(
										ConversionOperators.IntRomanToIntNative, new Tuple(Arrays.asList(new Symbol("x"))))))));
		TestInterpretation.testDifference(lambda,
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
								new AbstractionApplication(ConversionOperators.IntRomanToIntNative,
										new Tuple(Arrays.asList(new Symbol("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
								new AbstractionApplication(ConversionOperators.IntStringToIntNative,
										new Tuple(Arrays.asList(new Symbol("x"))))),
						new Lambda(new Tuple(Arrays.asList(new Symbol("y"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new AbstractionApplication(
										ConversionOperators.IntStringToIntNative, new Tuple(Arrays.asList(new Symbol("x"))))))));
		TestInterpretation.testDifference(lambda, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top, typeEnv);
			lambda.interpret(top, typeEnv);
		});

		Pair<Type, Substitution> p = lambda.infer(top, typeEnv);

		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(Arrays.asList(
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative))),
				lambda);

		assertThrows(AppendableException.class,
				() -> (ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), new Symbol("x"))))).infer(top,
										typeEnv));
	}

	@Test
	@DisplayName("Test Extended Function")
	public void testExtendedFunction() throws AppendableException {
		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		Environment bound = Environment.create(top);
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
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), top));
		TestInterpretation.testDifference(function, ExtendedFunction.makeExtendedFunction(tmpImpls, bound));

		TestInterpretation.testDifference(function, ExtendedFunction.makeExtendedFunction(implementations, top));

		TestInterpretation.testDifference(function, Expression.EMPTY_EXPRESSION);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		Pair<Type, Substitution> p = function.infer(top, typeEnv);
		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(Arrays.asList(
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntString))),
				function);

		assertThrows(AppendableException.class, () -> ExtendedFunction
				.makeExtendedFunction(Arrays.asList(
						new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
								new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), top),
						new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
								new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), top)),
						bound)
				.infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Application")
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

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			application.toString();
			application.toClojureCode(top, typeEnv);
			application.hashCode();
		});

		assertThrows(InvalidNumberOfArgumentsException.class,
				() -> new AbstractionApplication(new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(new TypeVariable("_x"), new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))).interpret(top, typeEnv));
		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE).interpret(top,
						typeEnv));

		TestInterpretation.testInterpretation(application, new LitInteger(42), top, typeEnv);
		Pair<Type, Substitution> p = application.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, application);

		// Test Lexical clojure
		Environment creation = Environment.create(top);
		creation.put(new Symbol("x"), new LitInteger(128));
		Environment evaluation = Environment.create(top);
		evaluation.put(new Symbol("x"), new LitString("foo"));
		AbstractionApplication lexicalClojureTest = new AbstractionApplication(
				new Function(new TypeTuple(Arrays.asList(new TypeVariable("a"))),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("x"), creation),
				new Tuple(Arrays.asList(LitBoolean.TRUE)));

		TestInterpretation.testInterpretation(lexicalClojureTest, new LitInteger(128), evaluation, typeEnv);
		p = lexicalClojureTest.infer(top, typeEnv);
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
				top, typeEnv);
		p = autoConRep.infer(top, typeEnv);
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
				new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString), top, typeEnv);
		p = useString.infer(top, typeEnv);
		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman))));
		TestInterpretation.testInterpretation(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman), top, typeEnv);
		p = useRoman.infer(top, typeEnv);
		TestInterpretation.testInference(p,
				RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useRoman);

		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(elambda, new Tuple(Arrays.asList(new LitString("fail")))).infer(top,
						typeEnv));

	}

	@Test
	@DisplayName("Test If")
	void testIfExpression() throws AppendableException {
		IfExpression ifExprT = new IfExpression(LitBoolean.TRUE, new LitInteger(42), new LitInteger(21));
		IfExpression ifExprF = new IfExpression(LitBoolean.FALSE, new LitInteger(21), new LitInteger(42));

		TestInterpretation.testReflexivity(ifExprF);
		TestInterpretation.testDifference(ifExprT, ifExprF);
		TestInterpretation.testDifference(ifExprT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			ifExprT.toString();
			ifExprT.toClojureCode(top, typeEnv);
			ifExprT.hashCode();
		});

		TestInterpretation.testInterpretation(ifExprT, new LitInteger(42), top, typeEnv);
		TestInterpretation.testInterpretation(ifExprF, new LitInteger(42), top, typeEnv);

		Pair<Type, Substitution> p = ifExprT.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, ifExprT);
	}

	@Test
	@DisplayName("Test AND")
	void testAndExpression() throws AppendableException {
		AndExpression andExpressionT = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.TRUE)));
		AndExpression andExpressionF = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE)));

		TestInterpretation.testReflexivity(andExpressionT);
		TestInterpretation.testDifference(andExpressionT, andExpressionF);
		TestInterpretation.testDifference(andExpressionT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			andExpressionT.toString();
			andExpressionT.toClojureCode(top, typeEnv);
			andExpressionT.hashCode();
		});

		TestInterpretation.testInterpretation(andExpressionT, LitBoolean.TRUE, top, typeEnv);
		TestInterpretation.testInterpretation(andExpressionF, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = andExpressionT.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, andExpressionT);
	}

	@Test
	void testOrExpression() throws AppendableException {
		OrExpression orExpressionT = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.TRUE)));
		OrExpression orExpressionF = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.FALSE)));

		TestInterpretation.testReflexivity(orExpressionT);
		TestInterpretation.testDifference(orExpressionT, orExpressionF);
		TestInterpretation.testDifference(orExpressionT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			orExpressionT.toString();
			orExpressionF.toClojureCode(top, typeEnv);
			orExpressionT.hashCode();
		});

		TestInterpretation.testInterpretation(orExpressionT, LitBoolean.TRUE, top, typeEnv);
		TestInterpretation.testInterpretation(orExpressionF, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = orExpressionT.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, orExpressionT);
	}

	@Test
	@DisplayName("Test Operators")
	void testOperators() throws AppendableException, IOException {
		TestInterpretation.testOperator(Operators.Addition,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(21))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.BitAnd, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(0), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.BitOr, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(3), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.Car,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitInteger(42), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.Cdr,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitString("foo"), TypeAtom.TypeStringNative);
		TestInterpretation.testOperator(Operators.Concantenation,
				new Tuple(Arrays.asList(new LitString("foo"), new LitString("bar"))), new LitString("foobar"),
				TypeAtom.TypeStringNative);
		TestInterpretation.testOperator(Operators.Division,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, LitBoolean.FALSE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(43), new LitInteger(42))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.Multiplication,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.Not, new Tuple(Arrays.asList(LitBoolean.TRUE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.Not, new Tuple(Arrays.asList(LitBoolean.FALSE)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(42))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.Subtraction,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(42))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyRepresentations,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyRepresentations, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyTypes,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.CanUnifyTypes, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);

		TestInterpretation.testOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);

		TestInterpretation.testOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		
		TestInterpretation.testOperator(Operators.BitShiftRight, new Tuple(new LitInteger(2), new LitInteger(1)),
				new LitInteger(1), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.BitShiftLeft, new Tuple(new LitInteger(2), new LitInteger(1)),
				new LitInteger(4), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.BitNot, new Tuple(new LitInteger(6)), new LitInteger(-7), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.BitXor, new Tuple(new LitInteger(5), new LitInteger(6)), new LitInteger(3), TypeAtom.TypeIntNative);
		TestInterpretation.testOperator(Operators.ToStr, new Tuple(new LitInteger(42)), new LitString("42"), TypeAtom.TypeStringNative);
		
		File tempOut = File.createTempFile("velka_read_test", null);
        String content  = "hello world !!";       
        Files.writeString(tempOut.toPath(), content);
        
        TestInterpretation.testOperator(
        		Operators.ReadFile, 
        		new Tuple(new LitString(tempOut.toPath().toString())), 
        		new LitString(content), 
        		TypeAtom.TypeStringNative);
        
        tempOut.delete();
        
        TestInterpretation.testOperator(Operators.StrSplit, 
        		new Tuple(new LitString("foo bar baz"), 
        		new LitString(" ")), 
        		new LitComposite(
        				new Tuple(
        						new LitString("foo"), 
        						new LitComposite(
        								new Tuple(
        									new LitString("bar"), 
        									new LitComposite(
        											new Tuple(
        													new LitString("baz"), 
        													ListNative.EMPTY_LIST_NATIVE), 
        											TypeAtom.TypeListNative)), 
        								TypeAtom.TypeListNative)),
        				TypeAtom.TypeListNative), 
        		TypeAtom.TypeListNative);
        
        TestInterpretation.testOperator(Operators.parseInt, 
        		new Tuple(new LitString("42")), new LitInteger(42), TypeAtom.TypeIntNative);
	}

	@Test
	@DisplayName("Test Conversions")
	void testConversions() throws AppendableException {
		TestInterpretation.testConversion(ConversionOperators.IntRomanToIntString,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntString));
		TestInterpretation.testConversion(ConversionOperators.IntRomanToIntNative,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative));
		TestInterpretation.testConversion(ConversionOperators.IntStringToIntRoman,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntRoman));
		TestInterpretation.testConversion(ConversionOperators.IntStringToIntNative,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative));
		TestInterpretation.testConversion(ConversionOperators.IntNativeToIntString, new LitInteger(5),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntString));
		TestInterpretation.testConversion(ConversionOperators.IntNativeToIntRoman, new LitInteger(5),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntRoman));
	}

	@Test
	@DisplayName("Test Automatic conversion")
	void testAutoConversion() throws AppendableException {
		Expression e = new AbstractionApplication(Operators.Addition,
				new Tuple(Arrays.asList(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))));

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		TestInterpretation.testInterpretation(e, new LitInteger(84), top, typeEnv);
	}

	@Test
	@DisplayName("Test Environment")
	void testEnvironment() throws AppendableException {
		Environment top = Environment.initTopLevelEnvitonment();

		Environment environment = Environment.create(top);
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
		assertThrows(UnboundVariableException.class, () -> child.getVariableValue(new Symbol("w")));

		assertEquals(environment.compareTo(environment), 0);
		assertNotEquals(environment.compareTo(child), 0);
		assertNotEquals(child.compareTo(environment), 0);

		Environment child2 = Environment.create(environment);
		child2.put(new Symbol("w"), new LitInteger(42));
		assertNotEquals(child.compareTo(child2), 0);

		Environment child3 = Environment.create(child);
		assertNotEquals(child.compareTo(child3), 0);

		Environment child4 = Environment.create(environment);
		child4.put(new Symbol("z"), new LitInteger(43));
		assertNotEquals(child.compareTo(child4), 0);

		Environment child5 = Environment.create(environment);
		child5.put(new Symbol("z"), Tuple.EMPTY_TUPLE);
		child5.put(new Symbol("w"), new LitInteger(42));
		assertNotEquals(child.compareTo(child5), 0);
	}

	@Test
	@DisplayName("Test Define Type")
	void testDefTypeExpression() throws AppendableException {
		DefineType defTypeExpression = new DefineType(new TypeName("Test"));

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defTypeExpression.toString();
			defTypeExpression.hashCode();
			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			defTypeExpression.toClojureCode(top, cljTypeEnv);
		});

		TestInterpretation.testReflexivity(defTypeExpression);
		TestInterpretation.testDifference(defTypeExpression, new DefineType(new TypeName("Test2")));
		TestInterpretation.testDifference(defTypeExpression, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(defTypeExpression, Expression.EMPTY_EXPRESSION, top, typeEnv);
		assertTrue(typeEnv.existsTypeAtom(new TypeAtom(new TypeName("Test"), TypeRepresentation.WILDCARD)));

		Pair<Type, Substitution> p = defTypeExpression.infer(top, typeEnv);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defTypeExpression);
	}

	@Test
	@DisplayName("Test Define Conversion")
	void testDefConversionExpression() throws AppendableException {
		TypeName name = new TypeName("__defConversionTest");
		TypeAtom typeAtomNative = new TypeAtom(name, TypeRepresentation.NATIVE);
		TypeAtom typeAtomWildcard = new TypeAtom(name, TypeRepresentation.WILDCARD);
		DefineConversion defCon = new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("x"))),
				new LitComposite(new Tuple(Arrays.asList(new Symbol("x"))), typeAtomWildcard));

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);
		typeEnv.addType(name);
		typeEnv.addRepresentation(typeAtomNative);

		assertAll(() -> {
			defCon.toString();
			defCon.hashCode();

			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			cljTypeEnv.addType(name);
			cljTypeEnv.addRepresentation(typeAtomNative);
			defCon.toClojureCode(top, cljTypeEnv);
		});

		TestInterpretation.testReflexivity(defCon);
		TestInterpretation.testDifference(defCon,
				new DefineConversion(new TypeAtom(name, TypeRepresentation.STRING), typeAtomWildcard,
						new Tuple(Arrays.asList(new Symbol("x"))),
						new LitComposite(new Symbol("y"), typeAtomWildcard)));
		TestInterpretation.testDifference(defCon,
				new DefineConversion(typeAtomNative, new TypeAtom(name, TypeRepresentation.STRING),
						new Tuple(Arrays.asList(new Symbol("x"))),
						new LitComposite(new Symbol("y"), typeAtomWildcard)));
		TestInterpretation.testDifference(defCon, new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("y"))), new LitComposite(new Symbol("y"), typeAtomWildcard)));
		TestInterpretation.testDifference(defCon, new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("x"))), Expression.EMPTY_EXPRESSION));

		TestInterpretation.testDifference(defCon, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(defCon, Expression.EMPTY_EXPRESSION, top, typeEnv);

		Pair<Type, Substitution> p = defCon.infer(top, typeEnv);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defCon);
	}

	@Test
	@DisplayName("Test Define Representation")
	void testDefRepresentationExpression() throws AppendableException {
		TypeName name = new TypeName("__defRepresentationTest");

		DefineRepresentation defRep = new DefineRepresentation(name, TypeRepresentation.NATIVE);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defRep.toString();
			defRep.hashCode();

			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			cljTypeEnv.addType(name);
			defRep.toClojureCode(top, cljTypeEnv);
		});

		TestInterpretation.testReflexivity(defRep);
		TestInterpretation.testDifference(defRep, new DefineRepresentation(TypeName.INT, TypeRepresentation.NATIVE));
		TestInterpretation.testDifference(defRep, new DefineRepresentation(name, TypeRepresentation.STRING));
		TestInterpretation.testDifference(defRep, Expression.EMPTY_EXPRESSION);

		typeEnv.addType(name);
		TestInterpretation.testInterpretation(defRep, Expression.EMPTY_EXPRESSION, top, typeEnv);

		assertThrows(AppendableException.class,
				() -> new DefineRepresentation(name, TypeRepresentation.NATIVE).interpret(top, typeEnv));

		Pair<Type, Substitution> p = defRep.infer(top, typeEnv);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defRep);
	}

	@Test
	@DisplayName("Test Define Constructor")
	void testDefinceConstructorExpression() throws AppendableException {
		TypeName name = new TypeName("__defConstructorTest");
		TypeAtom type = new TypeAtom(name, TypeRepresentation.NATIVE);
		Lambda constructor = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
				new LitComposite(Expression.EMPTY_EXPRESSION, type));

		DefineConstructor defCon = new DefineConstructor(type, constructor);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);
		typeEnv.addType(name);
		typeEnv.addRepresentation(type);

		assertAll(() -> {
			defCon.toString();
			defCon.hashCode();
			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			cljTypeEnv.addType(name);
			cljTypeEnv.addRepresentation(type);

			defCon.toClojureCode(top, cljTypeEnv);
		});

		TestInterpretation.testReflexivity(defCon);
		TestInterpretation.testDifference(defCon, new DefineConstructor(TypeAtom.TypeIntNative, constructor));
		TestInterpretation.testDifference(defCon, new DefineConstructor(type, Lambda.identity));

		TestInterpretation.testInterpretation(defCon, Expression.EMPTY_EXPRESSION, top, typeEnv);

		Pair<Type, Substitution> p = defCon.infer(top, typeEnv);
		TestInterpretation.testInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defCon);
	}

	@Test
	@DisplayName("Test Construct")
	void testConstruct() throws AppendableException {
		Construct construct = new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XLII"))));

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			construct.toString();
			construct.hashCode();
			construct.toClojureCode(top, typeEnv);
		});

		TestInterpretation.testReflexivity(construct);
		TestInterpretation.testDifference(construct,
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("XLII")))));
		TestInterpretation.testDifference(construct,
				new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XXI")))));
		TestInterpretation.testDifference(construct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(construct, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				top, typeEnv);

		Pair<Type, Substitution> p = construct.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, construct);
	}

	@Test
	@DisplayName("Test Deconstruct")
	void testDeconstruct() throws AppendableException {
		Deconstruct deconstruct = new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
				TypeAtom.TypeStringNative);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			deconstruct.toString();
			deconstruct.hashCode();
			deconstruct.toClojureCode(top, typeEnv);
		});

		TestInterpretation.testReflexivity(deconstruct);
		TestInterpretation.testDifference(deconstruct, new Deconstruct(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeStringNative));
		TestInterpretation.testDifference(deconstruct,
				new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(deconstruct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(deconstruct, new LitString("42"), top, typeEnv);

		Pair<Type, Substitution> p = deconstruct.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeStringNative, deconstruct);
	}

	@Test
	@DisplayName("Test Can Deconstruct As")
	void testCanDeconstructAs() throws AppendableException {
		CanDeconstructAs canDeconstruct = new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeStringNative);
		CanDeconstructAs cannotDeconstrut = new CanDeconstructAs(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeIntNative);

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			canDeconstruct.toString();
			canDeconstruct.hashCode();
			canDeconstruct.toClojureCode(top, typeEnv);
		});

		TestInterpretation.testReflexivity(canDeconstruct);
		TestInterpretation.testDifference(canDeconstruct, new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(canDeconstruct,
				new CanDeconstructAs(new LitDouble(3.14), TypeAtom.TypeStringNative));
		TestInterpretation.testDifference(canDeconstruct, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(canDeconstruct, LitBoolean.TRUE, top, typeEnv);
		TestInterpretation.testInterpretation(cannotDeconstrut, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = canDeconstruct.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, canDeconstruct);
	}

	@Test
	@DisplayName("Test Convert")
	void testConvert() throws AppendableException {
		Convert convert = new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(42));

		Environment top = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			convert.hashCode();
			convert.toString();
			convert.toClojureCode(top, typeEnv);
		});

		TestInterpretation.testReflexivity(convert);
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, new LitInteger(42)));
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, new LitInteger(42)));
		TestInterpretation.testDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(21)));
		TestInterpretation.testDifference(convert, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(convert, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				top, typeEnv);

		Pair<Type, Substitution> p = convert.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, convert);
	}

	@Test
	@DisplayName("Test List Native")
	void testListNative() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);

		TestInterpretation.testInterpretString("(construct List Native)",
				new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative));
		TestInterpretation.testInterpretString("(construct List Native 42 (construct List Native))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(42),
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative));

		TestInterpretation.testInterpretString("(is-list-native-empty (construct List Native))", LitBoolean.TRUE, env,
				typeEnv);
		TestInterpretation.testInterpretString(
				"(is-list-native-empty (construct List Native 42 (construct List Native)))", LitBoolean.FALSE, env,
				typeEnv);

		TestInterpretation.testInterpretString("(head-list-native (construct List Native 42 (construct List Native)))",
				new LitInteger(42), env, typeEnv);
		assertThrows(UserException.class,
				() -> TestInterpretation.testInterpretString("(head-list-native (construct List Native))",
						Expression.EMPTY_EXPRESSION, env, typeEnv));

		TestInterpretation.testInterpretString("(tail-list-native (construct List Native 42 (construct List Native)))",
				new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative), env, typeEnv);
		assertThrows(UserException.class,
				() -> TestInterpretation.testInterpretString("(tail-list-native (construct List Native))",
						Expression.EMPTY_EXPRESSION, env, typeEnv));

		TestInterpretation.testInterpretString(
				"(map-list-native (lambda (x) (+ x 1)) (construct List Native 42 (construct List Native)))",
				new LitComposite(
						new Tuple(Arrays.asList(new LitInteger(43),
								new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative),
				env, typeEnv);

		TestInterpretation.testInterpretString(
				"(map2-list-native + (construct List Native 21 (construct List Native 21 (construct List Native))) (construct List Native 21 (construct List Native 21 (construct List Native))))",
				new LitComposite(new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(
								new Tuple(Arrays.asList(new LitInteger(42),
										new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative))),
								TypeAtom.TypeListNative))),
						TypeAtom.TypeListNative),
				env, typeEnv);

		TestInterpretation.testInterpretString(
				"(foldl-list-native + 0 (construct List Native 1 (construct List Native 2 (construct List Native))))",
				new LitInteger(3), env, typeEnv);
		TestInterpretation.testInterpretString(
				"(foldr-list-native + 0 (construct List Native 1 (construct List Native 2 (construct List Native))))",
				new LitInteger(3), env, typeEnv);
		
		TestInterpretation.testInterpretString(
				"(" + ListNative.addToEndSymbol_out + " (construct List Native 21 (construct List Native)) 42)",
				new LitComposite(
						new Tuple(
								new LitInteger(21),
								new LitComposite(
										new Tuple(
												new LitInteger(42),
												ListNative.EMPTY_LIST_NATIVE
												),
										TypeAtom.TypeListNative
										)
								),
						TypeAtom.TypeListNative
						), env, typeEnv);
		
		ArrayList<Expression> al = new ArrayList<Expression>();
		al.add(new LitInteger(42));
		al.add(new LitInteger(21));
		TestInterpretation.testInterpretString(
				"(convert List:Native List:JavaArray (construct List Native 42 (construct List Native 21 (construct List Native))))",
				new LitComposite(new LitInteropObject(al), JavaArrayList.TypeListJavaArray), env, typeEnv);
		
		LinkedList<Expression> ll = new LinkedList<Expression>();
		ll.add(new LitInteger(42));
		ll.add(new LitInteger(21));
		TestInterpretation.testInterpretString(
				"(convert List:Native List:JavaLinked (construct List Native 42 (construct List Native 21 (construct List Native))))",
				new LitComposite(new LitInteropObject(ll), JavaLinkedList.TypeListJavaLinked), env, typeEnv);
				
		TestInterpretation.testInterpretString("(contains-list-native (construct List Native 42 (construct List Native 21 (construct List Native))) 42)", LitBoolean.TRUE, env, typeEnv);
		TestInterpretation.testInterpretString("(contains-list-native (construct List Native 42 (construct List Native 21 (construct List Native))) 84)", LitBoolean.FALSE, env, typeEnv);
		
		TestInterpretation.testInterpretString("(filter-list-native (construct List Native #t (construct List Native #f (construct List Native))) (lambda (x) x))", 
				new LitComposite(new Tuple(LitBoolean.TRUE, ListNative.EMPTY_LIST_NATIVE), TypeAtom.TypeListNative), 
				env, typeEnv);
		TestInterpretation.testInterpretString("(get-list-native (construct List Native 42 (construct List Native)) 0)", new LitInteger(42), env, typeEnv);
		TestInterpretation.testInterpretString("(build-list-native 2 (lambda (x) x))", new LitComposite(new Tuple(new LitInteger(0), new LitComposite(new Tuple(new LitInteger(1), ListNative.EMPTY_LIST_NATIVE), TypeAtom.TypeListNative)), TypeAtom.TypeListNative), env, typeEnv);
		
		TestInterpretation.testInterpretString("(remove-list-native (build-list-native 2 (lambda (x) x)) 1)", new LitComposite(new Tuple(new LitInteger(0), ListNative.EMPTY_LIST_NATIVE), TypeAtom.TypeListNative), env, typeEnv);
		TestInterpretation.testInterpretString("(size-list-native (build-list-native 42 (lambda (x) x)))", new LitInteger(42), env, typeEnv);
		TestInterpretation.testInterpretString("(append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42)))", new LitComposite(new Tuple(new LitInteger(21), new LitComposite(new Tuple(new LitInteger(42), ListNative.EMPTY_LIST_NATIVE), TypeAtom.TypeListNative)), TypeAtom.TypeListNative), env, typeEnv);
	}

	@Test
	@DisplayName("Test Type Symbol")
	void testTypeSymbol() throws AppendableException {
		TypeSymbol typeSymbol = new TypeSymbol(TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			typeSymbol.toString();
			typeSymbol.hashCode();
			typeSymbol.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(typeSymbol);
		TestInterpretation.testDifference(typeSymbol, new TypeSymbol(TypeAtom.TypeBoolNative));
		TestInterpretation.testDifference(typeSymbol, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(typeSymbol, typeSymbol, env, typeEnv);

		Pair<Type, Substitution> p = typeSymbol.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntNative, typeSymbol);
	}

	@Test
	@DisplayName("Test instance-of")
	void testInstanceOf() throws AppendableException {
		InstanceOf iof = new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			iof.toString();
			iof.hashCode();
			iof.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(iof);
		InstanceOf iof_isStrInt = new InstanceOf(new LitString("foo"), TypeAtom.TypeIntNative);
		TestInterpretation.testDifference(iof, iof_isStrInt);
		InstanceOf iof_typevar = new InstanceOf(new LitInteger(42), new TypeVariable(NameGenerator.next()));
		TestInterpretation.testDifference(iof, iof_typevar);
		InstanceOf iof_otherRepre = new InstanceOf(new LitInteger(42), TypeAtom.TypeIntRoman);
		TestInterpretation.testDifference(iof, iof_otherRepre);
		TestInterpretation.testDifference(iof,
				new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(iof, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(iof, LitBoolean.TRUE, env, typeEnv);
		TestInterpretation.testInterpretation(iof_isStrInt, LitBoolean.FALSE, env, typeEnv);
		TestInterpretation.testInterpretation(iof_typevar, LitBoolean.TRUE, env, typeEnv);
		TestInterpretation.testInterpretation(iof_otherRepre, LitBoolean.TRUE, env, typeEnv);

		Pair<Type, Substitution> p = iof.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, iof);
	}

	@Test
	@DisplayName("Test instance-of-representation")
	void testInstanceOfRepresentation() throws AppendableException {
		InstanceOfRepresentation iofr = new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			iofr.toString();
			iofr.hashCode();
			iofr.toClojureCode(env, typeEnv);
		});

		TestInterpretation.testReflexivity(iofr);
		InstanceOfRepresentation iofr_isStrInt = new InstanceOfRepresentation(new LitString("foo"),
				TypeAtom.TypeIntNative);
		TestInterpretation.testDifference(iofr, iofr_isStrInt);
		InstanceOfRepresentation iofr_typevar = new InstanceOfRepresentation(new LitInteger(42),
				new TypeVariable(NameGenerator.next()));
		TestInterpretation.testDifference(iofr, iofr_typevar);
		InstanceOfRepresentation iofr_otherRepre = new InstanceOfRepresentation(new LitInteger(42),
				TypeAtom.TypeIntRoman);
		TestInterpretation.testDifference(iofr, iofr_otherRepre);
		TestInterpretation.testDifference(iofr, new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative));
		TestInterpretation.testDifference(iofr, Expression.EMPTY_EXPRESSION);

		TestInterpretation.testInterpretation(iofr, LitBoolean.TRUE, env, typeEnv);
		TestInterpretation.testInterpretation(iofr_isStrInt, LitBoolean.FALSE, env, typeEnv);
		TestInterpretation.testInterpretation(iofr_typevar, LitBoolean.TRUE, env, typeEnv);
		TestInterpretation.testInterpretation(iofr_otherRepre, LitBoolean.FALSE, env, typeEnv);

		Pair<Type, Substitution> p = iofr.infer(env, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeBoolNative, iofr);
	}

	@Test
	@DisplayName("Test custom ranking function interpretation")
	void testCustomRanking() throws AppendableException {
		Lambda impl1 = new Lambda(new Tuple(new Symbol("a")), new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("Int Native"));
		Lambda impl2 = new Lambda(new Tuple(new Symbol("a")), new TypeTuple(TypeAtom.TypeIntString),
				new LitString("Int String"));
		Lambda impl3 = new Lambda(new Tuple(new Symbol("a")), new TypeTuple(TypeAtom.TypeIntRoman),
				new LitString("Int Roman"));

		ExtendedLambda elambda_defaultRanking = ExtendedLambda.makeExtendedLambda(Arrays.asList(impl1, impl2, impl3));

		Lambda ranking = (Lambda) TestInterpretation.parseString("(lambda (formalArgList realArgList args) "
				+ "(if (instance-of-representation (head-list-native formalArgList) Int:Roman) 0 999))");

		Lambda ranking2 = (Lambda) TestInterpretation.parseString("(lambda (formalArgList realArgList args) "
				+ "(if (instance-of-representation (head-list-native formalArgList) Int:String) 0 999))");

		ExtendedLambda elambda_customRanking = ExtendedLambda.makeExtendedLambda(Arrays.asList(impl1, impl2, impl3),
				ranking);

		Tuple args = new Tuple(new LitInteger(42));

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);

		AbstractionApplication app_defElambda_defRanking = new AbstractionApplication(elambda_defaultRanking, args);
		TestInterpretation.testInterpretation(app_defElambda_defRanking, new LitString("Int Native"), env, typeEnv);

		AbstractionApplication app_defElambda_cusRanking = new AbstractionApplication(elambda_defaultRanking, args,
				ranking);
		TestInterpretation.testInterpretation(app_defElambda_cusRanking, new LitString("Int Roman"), env, typeEnv);

		AbstractionApplication app_cusElambda_defRanking = new AbstractionApplication(elambda_customRanking, args);
		TestInterpretation.testInterpretation(app_cusElambda_defRanking, new LitString("Int Roman"), env, typeEnv);

		AbstractionApplication app_cusElambda_cusRanking = new AbstractionApplication(elambda_customRanking, args,
				ranking2);
		TestInterpretation.testInterpretation(app_cusElambda_cusRanking, new LitString("Int String"), env, typeEnv);
	}

	@Test
	@DisplayName("Test Java Array List")
	void testJavaArrayList() throws Exception {
		TestInterpretation.testInterpretString("(construct List JavaArray)",
				new LitComposite(new LitInteropObject(new ArrayList<Object>()), JavaArrayList.TypeListJavaArray));

		ArrayList<Object> l = new ArrayList<Object>();
		l.add(new LitInteger(42));

		TestInterpretation.testInterpretString(
				"(" + JavaArrayList.addToEndSymbol_out.toString() + " (construct List JavaArray) 42)", LitBoolean.TRUE);
		TestInterpretation.testInterpretString(
				"(" + JavaArrayList.addToIndexSymbol_out.toString() + " (construct List JavaArray) 0 42)",
				Expression.EMPTY_EXPRESSION);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.addAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.containsSymbol_out + " l1 42)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.containsSymbol_out + " l1 84)",
				LitBoolean.FALSE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 42)"
				+ "(" + JavaArrayList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.getSymbol_out + " l1 0)",
				new LitInteger(1));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.indexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.indexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		
		TestInterpretation.testInterpretString("(" + JavaArrayList.isEmptySymbol_out + " (construct List JavaArray))", 
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.isEmptySymbol_out + " l1)", 
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.lastIndexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.lastIndexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		/*TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(" + JavaArrayList.removeSymbol + " l1 0)", 
				new LitInteger(1));*/
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.removeSymbol_out + " l1 2)", 
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.removeAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 4)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 5)"
				+ "(" + JavaArrayList.removeAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.retainAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.retainAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.setSymbol_out + " l1 0 2)", 
				new LitInteger(1));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.sizeSymbol_out + " l1)", 
				new LitInteger(2));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 84)" 
				+ "(" + JavaArrayList.sublistSymbol_out + " l1 0 1)", 
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)"  
				+ "(" + JavaArrayList.mapSymbol_out + " l1 (lambda (x) (* 2 x)))", 
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 21)"
				+ "(" + JavaArrayList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y)))",
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaArrayList.foldlSymbol_out + " + 0 l1)", 
				new LitInteger(63));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 4)"
				+ "(" + JavaArrayList.foldrSymbol_out + " / 16 l1)", 
				new LitInteger(2));
		
		LinkedList<Expression> converted = new LinkedList<Expression>();
		converted.add(new LitInteger(42));
		converted.add(new LitInteger(21));
		TestInterpretation.testInterpretString("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaArray List:JavaLinked l)", 
				new LitComposite(new LitInteropObject(converted), JavaLinkedList.TypeListJavaLinked));
		
		TestInterpretation.testInterpretString("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaArray List:Native l)", 
				new LitComposite(
						new Tuple(
								new LitInteger(42),
								new LitComposite(
										new Tuple(
												new LitInteger(21), 
												ListNative.EMPTY_LIST_NATIVE), 
										TypeAtom.TypeListNative)), 
						TypeAtom.TypeListNative));
	}
	
	@Test
	@DisplayName("Test Java Linked List")
	void testJavaLinkedList() throws Exception {
		TestInterpretation.testInterpretString("(construct List JavaLinked)",
				new LitComposite(new LitInteropObject(new LinkedList<Object>()), JavaLinkedList.TypeListJavaLinked));

		LinkedList<Object> l = new LinkedList<Object>();
		l.add(new LitInteger(42));

		TestInterpretation.testInterpretString(
				"(" + JavaLinkedList.addToEndSymbol_out.toString() + " (construct List JavaLinked) 42)", LitBoolean.TRUE);
		TestInterpretation.testInterpretString(
				"(" + JavaLinkedList.addToIndexSymbol_out.toString() + " (construct List JavaLinked) 0 42)",
				Expression.EMPTY_EXPRESSION);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.addAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.containsSymbol_out + " l1 42)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.containsSymbol_out + " l1 84)",
				LitBoolean.FALSE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 42)"
				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.getSymbol_out + " l1 0)",
				new LitInteger(1));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		
		TestInterpretation.testInterpretString(
				"(" + JavaLinkedList.isEmptySymbol_out + " (construct List JavaLinked))", 
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.isEmptySymbol_out + " l1)", 
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 42)",
				new LitInteger(-1));

		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.removeSymbol_out + " l1 2)", 
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.removeAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 4)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 5)"
				+ "(" + JavaLinkedList.removeAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.retainAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.retainAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.setSymbol_out + " l1 0 2)", 
				new LitInteger(1));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.sizeSymbol_out + " l1)", 
				new LitInteger(2));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 84)" 
				+ "(" + JavaLinkedList.sublistSymbol_out + " l1 0 1)", 
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"  
				+ "(" + JavaLinkedList.mapSymbol_out + " l1 (lambda (x) (* 2 x)))", 
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		TestInterpretation.testInterpretString("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 21)"
				+ "(" + JavaLinkedList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y)))",
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaLinkedList.foldlSymbol_out + " + 0 l1)", 
				new LitInteger(63));
		
		TestInterpretation.testInterpretString(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 4)"
				+ "(" + JavaLinkedList.foldrSymbol_out + " / 16 l1)", 
				new LitInteger(2));
		
		ArrayList<Expression> converted = new ArrayList<Expression>();
		converted.add(new LitInteger(42));
		converted.add(new LitInteger(21));
		TestInterpretation.testInterpretString("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaLinked List:JavaArray l)", 
				new LitComposite(new LitInteropObject(converted), JavaArrayList.TypeListJavaArray));
		
		TestInterpretation.testInterpretString("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaLinked List:Native l)", 
				new LitComposite(
						new Tuple(
								new LitInteger(42),
								new LitComposite(
										new Tuple(
												new LitInteger(21), 
												ListNative.EMPTY_LIST_NATIVE), 
										TypeAtom.TypeListNative)), 
						TypeAtom.TypeListNative));
	}
	
	@Test
	@DisplayName("Test logging")
	void testLogging() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		assertNotEquals(new LitInteger(0),
				TestInterpretation.parseString("(timestamp)").interpret(env, typeEnv));
		
		TestInterpretation.testInterpretString("(init-logger \"test-log\")", Expression.EMPTY_EXPRESSION);
		Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		logger.info("test");
		
		TestInterpretation.testInterpretString("(log \"test-2\")", Expression.EMPTY_EXPRESSION);
	}
	
	@Test
	@DisplayName("Test deep inference")
	void testDeepInference() throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		ListNative.initializeInEnvironment(env, typeEnv);
		
		String code = "(define build-list-native-aux\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f) (Int:Native i))\n" + 
				"                    (if (< i n)\n" + 
				"                        (construct List Native (f i) (build-list-native-aux n f (+ i 1)))\n" + 
				"                        (construct List Native)))))\n" + 
				"        (define build-list-native\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f))\n" + 
				"                    (build-list-native-aux n f 0))))\n" + 
				"        (build-list-native\n" + 
				"            1 (lambda ((Int:Native x)) (build-list-native x (lambda ((Int:Native y)) y))))";
		
		List<Expression> exprs = TestInterpretation.parseString_multipleExpression(code);
		for(Expression e : exprs) {
			e.infer(env, typeEnv);
			e.interpret(env, typeEnv);
		}
	}
	
	@Test
	@DisplayName("Test Get")
	void testGet() throws AppendableException {
		Tuple t = new Tuple(new LitInteger(42), new LitString("foo"));
		Get get = Get.makeGet(t, new LitInteger(0));
		Get get2 = Get.makeGet(t, new LitInteger(1));
		Get get3 = Get.makeGet(t, new LitComposite(new LitString("0"), TypeAtom.TypeIntString));
		
		TestInterpretation.testReflexivity(get);
		TestInterpretation.testDifference(get, get2);
		TestInterpretation.testDifference(get, Expression.EMPTY_EXPRESSION);
		
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		assertAll(() -> {
			get.toString();
			get.toClojureCode(env, typeEnv);
			get.hashCode();
			get.infer(env, typeEnv);
		});
		
		TestInterpretation.testInterpretation(get, new LitInteger(42), env, typeEnv);
		TestInterpretation.testInterpretation(get2, new LitString("foo"), env, typeEnv);
		TestInterpretation.testInterpretation(get3, new LitInteger(42), env, typeEnv);
	}

	private static Expression parseString(String s) throws AppendableException {
		List<Expression> l = Parser.read(s); 
		return l.get(l.size() - 1);
	}
	
	private static List<Expression> parseString_multipleExpression(String s) throws AppendableException{
		return Parser.read(s);
	}

	private static void testReflexivity(Expression original) {
		Expression e = original;
		assertEquals(original, e);
		assertEquals(original.hashCode(), original.hashCode());
		assertEquals(original.compareTo(e), 0);
	}

	private static void testDifference(Expression original, Expression e) {
		assertNotEquals(original, e);
		assertNotEquals(original.compareTo(e), 0);
	}

	private static void testInference(Pair<Type, Substitution> result, Type expected, Expression infered) {
		TestInterpretation.testInference(result, expected, infered, false);
	}

	private static void testInference(Pair<Type, Substitution> p, Type expected, Expression infered,
			boolean shouldeBeSUbstEmpty) {
		assertEquals(expected, p.first);
		if (shouldeBeSUbstEmpty) {
			assertEquals(Substitution.EMPTY, p.second);
		}
	}

	private static void testInferenceOfType(Pair<Type, Substitution> p, Class<? extends Type> expected,
			Expression infered) {
		TestInterpretation.testInferenceOfType(p, expected, infered, false);
	}

	private static void testInferenceOfType(Pair<Type, Substitution> p, Class<? extends Type> expected,
			Expression infered, boolean shouldBeSubstEmpty) {
		assertTrue(expected.isInstance(p.first));
		if (shouldBeSubstEmpty) {
			assertEquals(p.second, Substitution.EMPTY);
		}
	}

	private static void testInterpretation(Expression interpreted, Expression expected, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		Expression e = interpreted.interpret(env, typeEnv);
		assertEquals(expected, e);
	}
	
	private static void testInterpretation(Collection<Expression> interpreted, Expression expected, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression f = null;
		for(Expression e : interpreted) {
			f = e.interpret(env, typeEnv);
		}
		assertEquals(expected, f);
	}

	private static void testOperator(final Operator operator, Tuple args, Expression expectedInterpret,
			Type expectedInference) throws AppendableException {
		AbstractionApplication application = new AbstractionApplication(operator, args);
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInterpretation(application, expectedInterpret, env, typeEnv);
		Pair<Type, Substitution> p = application.infer(env, typeEnv);
		if (expectedInference != null) {
			TestInterpretation.testInference(p, expectedInference, application);
		}

		assertAll(() -> {
			operator.toString();
			operator.toClojureCode(env, typeEnv);
		});
	}

	private static void testConversion(Abstraction conversion, Expression argument, Expression expectedInterpret,
			Type expectedInfer) throws AppendableException {
		assertAll(() -> {
			conversion.toString();
		});

		AbstractionApplication appl = new AbstractionApplication(conversion, new Tuple(Arrays.asList(argument)));
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		TestInterpretation.testInterpretation(appl, expectedInterpret, env, typeEnv);
		Pair<Type, Substitution> p = conversion.infer(env, typeEnv);
		TestInterpretation.testInference(p, expectedInfer, conversion);
	}

	private static void testInterpretString(String interpreted, Expression expected) throws AppendableException {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		TestInterpretation.testInterpretString(interpreted, expected, env, typeEnv);
	}

	private static void testInterpretString(String interpreted, Expression expected, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		List<Expression> l = TestInterpretation.parseString_multipleExpression(interpreted);
		TestInterpretation.testInterpretation(l, expected, env, typeEnv);
	}
}
