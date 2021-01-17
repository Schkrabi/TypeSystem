package velka.lang.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.lang.abstraction.ExtendedFunction;
import velka.lang.abstraction.ExtendedLambda;
import velka.lang.abstraction.Function;
import velka.lang.abstraction.Lambda;
import velka.lang.abstraction.Abstraction;
import velka.lang.abstraction.Operator;
import velka.lang.application.AndExpression;
import velka.lang.application.CanDeconstructAs;
import velka.lang.application.Construct;
import velka.lang.application.Convert;
import velka.lang.application.Deconstruct;
import velka.lang.application.DefineConstructor;
import velka.lang.application.AbstractionApplication;
import velka.lang.application.DefineConversion;
import velka.lang.application.DefineSymbol;
import velka.lang.application.DefineType;
import velka.lang.application.ExceptionExpr;
import velka.lang.application.IfExpression;
import velka.lang.application.InstanceOf;
import velka.lang.application.InstanceOfRepresentation;
import velka.lang.application.OrExpression;
import velka.lang.application.DefineRepresentation;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeHolder;
import velka.lang.expression.TypeSymbol;
import velka.lang.expression.Symbol;
import velka.lang.interpretation.Environment;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitDouble;
import velka.lang.literal.LitEnum;
import velka.lang.literal.LitInteger;
import velka.lang.literal.LitString;
import velka.lang.parser.SchemeLexer;
import velka.lang.parser.SchemeParser;
import velka.lang.parser.SchemeParser.ExprsContext;
import velka.lang.semantic.SemanticParser;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.exceptions.UserException;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.exceptions.InvalidNumberOfArgumentsException;
import velka.lang.util.Pair;
import velka.lang.exceptions.UnboundVariableException;

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
		TestInterpretation.testInference(p, TypeAtom.TypeIntString, useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman))));
		TestInterpretation.testInterpretation(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman), top, typeEnv);
		p = useRoman.infer(top, typeEnv);
		TestInterpretation.testInference(p, TypeAtom.TypeIntRoman, useRoman);

		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(useString, new Tuple(Arrays.asList(new LitString("fail")))).infer(top,
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
		TestInterpretation.testOperator(Operator.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyRepresentations,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyRepresentations, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyTypes,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.CanUnifyTypes, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);

		TestInterpretation.testOperator(Operator.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);

		TestInterpretation.testOperator(Operator.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		TestInterpretation.testOperator(Operator.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
	}

	@Test
	@DisplayName("Test Conversions")
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
	@DisplayName("Test Automatic conversion")
	void testAutoConversion() throws AppendableException {
		Expression e = new AbstractionApplication(Operator.Addition,
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

	private static Expression parseString(String s) throws AppendableException {
		CharStream charStream = CharStreams.fromString(s);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);

		ExprsContext exprsContext = parser.exprs();
		return SemanticParser.parseNode(exprsContext.val.get(0));
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
		assertEquals(p.first, expected);
		if (shouldeBeSUbstEmpty) {
			assertEquals(p.second, Substitution.EMPTY);
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
		assertEquals(e, expected);
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
		Expression e = TestInterpretation.parseString(interpreted);
		TestInterpretation.testInterpretation(e, expected, env, typeEnv);
	}
}
