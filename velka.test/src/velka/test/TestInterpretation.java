package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.compiler.LangbaseDocumentationGenerator;
import velka.core.abstraction.ExtendedFunction;
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
import velka.core.application.DefineRepresentation;
import velka.core.application.DefineSymbol;
import velka.core.application.DefineType;
import velka.core.application.ExceptionExpr;
import velka.core.application.Extend;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.OrExpression;
import velka.core.exceptions.InvalidArgumentsException;
import velka.core.exceptions.UnboundVariableException;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.expression.TypeSymbol;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.ConstructorOperators;
import velka.core.langbase.ConversionOperators;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaBitSet;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitEnum;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeSetDoesNotUnifyException;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

class TestInterpretation extends VelkaTest{

	@Test
	@DisplayName("Test String Literal")
	void testLitString() throws AppendableException {
		LitString litString = new LitString("test");
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litString.toString();
			litString.hashCode();
			litString.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(litString);
		this.assertDifference(litString, new LitString(" "));
		this.assertDifference(litString, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litString, litString, env, typeEnv);

		Pair<Type, Substitution> p = litString.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeStringNative, litString, true);
	}

	@Test
	@DisplayName("Test Integer Literal")
	public void testLitInteger() throws AppendableException {
		LitInteger litInteger = new LitInteger(128);
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litInteger.toString();
			litInteger.hashCode();
			litInteger.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(litInteger);
		this.assertDifference(litInteger, new LitInteger(0));
		this.assertDifference(litInteger, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litInteger, litInteger, env, typeEnv);

		Pair<Type, Substitution> p = litInteger.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, litInteger, true);
	}

	@Test
	@DisplayName("Test Double Literal")
	public void testLitDouble() throws AppendableException {
		LitDouble litDouble = new LitDouble(3.14);
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			litDouble.toString();
			litDouble.hashCode();
			litDouble.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(litDouble);
		this.assertDifference(litDouble, new LitDouble(0));
		this.assertDifference(litDouble, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litDouble, litDouble, env, typeEnv);

		Pair<Type, Substitution> p = litDouble.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeDoubleNative, litDouble, true);
	}

	@Test
	@DisplayName("Test Boolean Literal")
	public void testLitBoolean() throws AppendableException {
		this.assertReflexivity(LitBoolean.TRUE);
		this.assertDifference(LitBoolean.TRUE, LitBoolean.FALSE);
		this.assertDifference(LitBoolean.FALSE, LitBoolean.TRUE);
		this.assertDifference(LitBoolean.TRUE, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		this.assertInterpretationEquals(LitBoolean.TRUE, LitBoolean.TRUE, env, typeEnv);

		assertAll(() -> {
			LitBoolean.TRUE.toString();
			LitBoolean.TRUE.toClojureCode(env, typeEnv);
		});

		Pair<Type, Substitution> p = LitBoolean.TRUE.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, LitBoolean.TRUE, true);
	}

	@Test
	@DisplayName("Test Enum Literal")
	void testLitEnum() throws AppendableException {
		TypeName typeName = new TypeName("TestEnum");
		TypeAtom type = new TypeAtom(typeName, TypeRepresentation.NATIVE);

		LitEnum enumValue1 = new LitEnum("value1", type);
		LitEnum enumValue2 = new LitEnum("value2", type);
		LitEnum differentEnumValue = new LitEnum("value1", TypeAtom.TypeInt);

		this.assertReflexivity(enumValue1);
		this.assertDifference(enumValue1, enumValue2);
		this.assertDifference(enumValue1, differentEnumValue);
		this.assertDifference(enumValue1, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		this.assertInterpretationEquals(enumValue1, enumValue1, env, typeEnv);

		Pair<Type, Substitution> p = enumValue1.infer(env, typeEnv);
		this.assertInference(p, type, enumValue1, true);

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

		this.assertReflexivity(composite1);
		this.assertDifference(composite1, composite2);
		this.assertDifference(composite1, composite3);
		this.assertDifference(composite1, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		this.assertInterpretationEquals(composite1, composite1, env, typeEnv);

		Pair<Type, Substitution> p = composite1.infer(env, typeEnv);
		this.assertInference(p, type, composite1);

		assertAll(() -> {
			composite1.toString();
			composite1.toClojureCode(env, typeEnv);
		});
	}

	@Test
	@DisplayName("Test Type Holder")
	public void testTypeHolder() throws AppendableException {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(AppendableException.class, () -> typeHolder.interpret(env, typeEnv));
		assertThrows(AppendableException.class, () -> typeHolder.toClojureCode(env, typeEnv));
		assertAll(() -> {
			typeHolder.toString();
			typeHolder.hashCode();
		});

		this.assertReflexivity(typeHolder);
		this.assertDifference(typeHolder, new TypeHolder(TypeAtom.TypeIntNative));
		this.assertDifference(typeHolder, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = typeHolder.infer(env, typeEnv);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, typeHolder, true);

		TypeHolder placeholder = new TypeHolder(TypeAtom.TypeInt, new Symbol("x"));
		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitInteger(42));

		this.assertInterpretationEquals(placeholder, new LitInteger(42), bound, typeEnv);
		assertThrows(AppendableException.class, () -> placeholder.interpret(env, typeEnv));

		TypeHolder placeholder2 = new TypeHolder(TypeAtom.TypeInt, new Symbol("__q"));
		env.put(new Symbol("__q"), placeholder2);
		assertThrows(AppendableException.class, () -> placeholder2.interpret(env, typeEnv));

		Environment bound2 = Environment.create(bound);
		bound2.put(new Symbol("x"), placeholder);
		this.assertInterpretationEquals(placeholder, new LitInteger(42), bound2, typeEnv);
	}

	@Test
	@DisplayName("Test Symbol")
	public void testVariable() throws AppendableException {
		Symbol variable = new Symbol("x");
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			variable.toString();
			variable.hashCode();
			variable.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(variable);
		this.assertDifference(variable, new Symbol("y"));
		this.assertDifference(variable, Expression.EMPTY_EXPRESSION);

		LitInteger value = new LitInteger(128);
		Environment bound = Environment.create(env);
		bound.put(variable, value);

		this.assertInterpretationEquals(variable, variable, env, typeEnv);
		Pair<Type, Substitution> p = variable.infer(env, typeEnv);
		this.assertInferenceClass(p, TypeVariable.class, variable);

		this.assertInterpretationEquals(variable, value, bound, typeEnv);
		p = variable.infer(bound, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, variable);

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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
					throws AppendableException {
				return null;
			}
		});
		assertThrows(AppendableException.class, () -> variable.infer(fault, typeEnv));
	}

	@Test
	@DisplayName("Test Empty Expression")
	public void testEmptyExpression() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		this.assertInference(Expression.EMPTY_EXPRESSION.infer(env, typeEnv), TypeTuple.EMPTY_TUPLE,
				Expression.EMPTY_EXPRESSION, true);
		this.assertInterpretationEquals(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION, env, typeEnv);

		assertAll(() -> {
			Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv);
		});
	}

	@Test
	@DisplayName("Test Tuple")
	public void testTuple() throws Exception {
		final Tuple tuple = new Tuple(Arrays.asList(new LitInteger(128), new Symbol("x"), LitBoolean.FALSE));
		Environment env = Environment.initTopLevelEnvironment();
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

		this.assertReflexivity(tuple);
		this.assertDifference(tuple, Tuple.EMPTY_TUPLE);
		this.assertDifference(tuple, Expression.EMPTY_EXPRESSION);
		this.assertDifference(tuple,
				new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2))));

		this.assertInterpretationEquals(tuple, tuple, env, typeEnv);
		Pair<Type, Substitution> p = tuple.infer(env, typeEnv);
		assertTrue(p.first instanceof TypeTuple);
		assertEquals(((TypeTuple) p.first).get(0), TypeAtom.TypeIntNative);
		assertTrue(((TypeTuple) p.first).get(1) instanceof TypeVariable);
		assertEquals(((TypeTuple) p.first).get(2), TypeAtom.TypeBoolNative);

		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitDouble(3.14));

		this.assertInterpretationEquals(tuple,
				new Tuple(Arrays.asList(new LitInteger(128), new LitDouble(3.14), LitBoolean.FALSE)), bound, typeEnv);
		p = tuple.infer(bound, typeEnv);
		this.assertInference(p,
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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
					throws AppendableException {
				return null;
			}
		}))).infer(env, typeEnv));
		
		//Test if cross-used bound variables in tuples are infered correctly
		Expression e = parseString("(let-type (A) (lambda ((A x)) (tuple (+ x 0) (floor x))))")
				.get(0);
		
		assertThrows(SubstitutionsCannotBeMergedException.class, () -> e.infer(env, typeEnv));
		
		assertEquals(
				(new Tuple(Arrays.asList(Tuple.EMPTY_TUPLE))),
				(new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE)))
				.stream()
				.filter(x -> !x.equals(Expression.EMPTY_EXPRESSION))
				.collect(Tuple.toTuple));
	}

	@Test
	@DisplayName("Test User Exception")
	void testExceptionExpr() throws AppendableException {
		final ExceptionExpr exception = new ExceptionExpr(new LitString("test"));
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(UserException.class, () -> exception.interpret(env, typeEnv));

		assertAll(() -> {
			exception.toClojureCode(env, typeEnv);
			exception.toString();
			exception.hashCode();
		});

		this.assertReflexivity(exception);
		this.assertDifference(exception, new ExceptionExpr(new LitString("fail")));
		this.assertDifference(exception, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = exception.infer(env, typeEnv);
		this.assertInferenceClass(p, TypeVariable.class, exception);

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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
					throws AppendableException {
				// TODO Auto-generated method stub
				return null;
			}
		}).infer(env, typeEnv));
	}

	@Test
	@DisplayName("Test Define Expression")
	void testDefExpression() throws AppendableException {
		DefineSymbol defExpression = new DefineSymbol(new Symbol("pi"), new LitDouble(Math.PI));
		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defExpression.toString();
			defExpression.hashCode();
			defExpression.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(defExpression);
		this.assertDifference(defExpression, new DefineSymbol(new Symbol("e"), new LitDouble(Math.E)));
		this.assertDifference(defExpression, new DefineSymbol(new Symbol("pi"), new LitDouble(3.141521)));
		this.assertDifference(defExpression, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.create(top);
		this.assertInterpretationEquals(defExpression, Expression.EMPTY_EXPRESSION, env, typeEnv);
		assertTrue(env.containsVariable(new Symbol("pi")));

		Pair<Type, Substitution> p = defExpression.infer(top, typeEnv);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, defExpression);
		//assertNotEquals(p.second, Substitution.EMPTY);
		//assertNotEquals(p.second.variableStream().findAny().get(), TypeAtom.TypeDoubleNative);

		DefineSymbol recursiveExpression = (DefineSymbol) this
				.parseString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))").get(0);
		p = recursiveExpression.infer(top, typeEnv);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, recursiveExpression);
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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
					throws AppendableException {
				// TODO Auto-generated method stub
				return null;
			}
		}).infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Lambda")
	void testLambda() throws AppendableException {
		final Lambda lambda = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x"));
		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(lambda);
		this.assertDifference(lambda, new Lambda(new Tuple(Arrays.asList(new Symbol("z"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x")));
		this.assertDifference(lambda, new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative, TypeAtom.TypeIntNative)), new Symbol("x")));
		this.assertDifference(lambda,
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
						Expression.EMPTY_EXPRESSION));
		this.assertDifference(lambda, Expression.EMPTY_EXPRESSION);

		Expression e = lambda.interpret(top, typeEnv);
		assertTrue(e instanceof Function);

		Pair<Type, Substitution> p = lambda.infer(top, typeEnv);
		this.assertInferenceClass(p, TypeArrow.class, lambda);

		assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))")
						.get(0).infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Function")
	void testFunction() throws AppendableException {
		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		Environment bound = Environment.create(top);
		bound.put(new Symbol("bound"), new LitDouble(3.141521));

		final Function function = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		this.assertReflexivity(function);
		this.assertDifference(function,
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
						new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), bound));
		this.assertDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("bound"), bound));
		this.assertDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), Expression.EMPTY_EXPRESSION, bound));
		this.assertDifference(function, new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("bound"), top));
		this.assertDifference(function, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(function, function, top, typeEnv);

		Pair<Type, Substitution> p = function.infer(top, typeEnv);
		this.assertInferenceClass(p, TypeArrow.class, function);

		assertThrows(AppendableException.class,
				() -> (new Function(new TypeTuple(Arrays.asList(new TypeVariable("x"))),
						new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)), new Symbol("y"), top)).infer(top,
								typeEnv));
		assertThrows(TypesDoesNotUnifyException.class, () -> this
				.parseString("(lambda ((String x)) (+ x x))")
					.get(0).interpret(top, typeEnv).infer(top, typeEnv));
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

		this.assertReflexivity(lambda);
		this.assertDifference(lambda,
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new Symbol("x")),
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new AbstractionApplication(
										ConversionOperators.IntRomanToIntNative, new Tuple(Arrays.asList(new Symbol("x"))))))));
		this.assertDifference(lambda,
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
		this.assertDifference(lambda, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top, typeEnv);
			lambda.interpret(top, typeEnv);
		});

		Pair<Type, Substitution> p = lambda.infer(top, typeEnv);

		this.assertInference(p,
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
		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		Environment bound = Environment.create(top);
		bound.put(new Symbol("x"), new LitInteger(42));
		List<Function> implementations = Arrays.asList(
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound),
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound));

		ExtendedFunction function = ExtendedFunction.makeExtendedFunction(implementations, bound, typeEnv);

		this.assertReflexivity(function);

		List<Function> tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.remove(0);
		this.assertDifference(function, ExtendedFunction.makeExtendedFunction(tmpImpls, bound, typeEnv));

		tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.add(new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), top));
		this.assertDifference(function, ExtendedFunction.makeExtendedFunction(tmpImpls, bound, typeEnv));

		this.assertDifference(function, ExtendedFunction.makeExtendedFunction(implementations, top, typeEnv));

		this.assertDifference(function, Expression.EMPTY_EXPRESSION);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		Pair<Type, Substitution> p = function.infer(top, typeEnv);
		this.assertInference(p,
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
						bound, typeEnv)
				.infer(top, typeEnv));
	}

	@Test
	@DisplayName("Test Application")
	void testApplication() throws AppendableException {
		AbstractionApplication application = new AbstractionApplication(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Symbol("x")),
				new Tuple(Arrays.asList(new LitInteger(42))));

		this.assertReflexivity(application);
		this.assertDifference(application,
				new AbstractionApplication(
						new Lambda(new Tuple(Arrays.asList(new Symbol("y"))),
								new TypeTuple(Arrays.asList(new TypeVariable("x"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))));
		this.assertDifference(application,
				new AbstractionApplication(
						new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
								new TypeTuple(Arrays.asList(new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(21)))));
		this.assertDifference(application, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			application.toString();
			application.toClojureCode(top, typeEnv);
			application.hashCode();
		});

		assertThrows(InvalidArgumentsException.class,
				() -> new AbstractionApplication(new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(new TypeVariable("_x"), new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))).interpret(top, typeEnv));
		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE).interpret(top,
						typeEnv));

		this.assertInterpretationEquals(application, new LitInteger(42), top, typeEnv);
		Pair<Type, Substitution> p = application.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, application);

		// Test Lexical clojure
		Environment creation = Environment.create(top);
		creation.put(new Symbol("x"), new LitInteger(128));
		Environment evaluation = Environment.create(top);
		evaluation.put(new Symbol("x"), new LitString("foo"));
		AbstractionApplication lexicalClojureTest = new AbstractionApplication(
				new Function(new TypeTuple(Arrays.asList(new TypeVariable("a"))),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("x"), creation),
				new Tuple(Arrays.asList(LitBoolean.TRUE)));

		this.assertInterpretationEquals(lexicalClojureTest, new LitInteger(128), evaluation, typeEnv);
		p = lexicalClojureTest.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, lexicalClojureTest);

		// Test autoconvert representations
		AbstractionApplication autoConRep = new AbstractionApplication(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				// new Tuple(Arrays.asList(new
				// Application(TypeConstructionLambda.IntRomanConstructor,
				// new Tuple(Arrays.asList(new LitString("V")))))));
				new Tuple(Arrays.asList(new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman))));
		this.assertInterpretationEquals(autoConRep, new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				top, typeEnv);
		p = autoConRep.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntString, autoConRep);

		// Test elambda/efunction comparation
		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(Arrays.asList(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new Symbol("x"))));
		AbstractionApplication useString = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString))));
		this.assertInterpretationEquals(useString,
				new LitComposite(new Tuple(Arrays.asList(new LitString("5"))), TypeAtom.TypeIntString), top, typeEnv);
		p = useString.infer(top, typeEnv);
		this.assertInference(p,
				RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman))));
		this.assertInterpretationEquals(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman), top, typeEnv);
		p = useRoman.infer(top, typeEnv);
		this.assertInference(p,
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

		this.assertReflexivity(ifExprF);
		this.assertDifference(ifExprT, ifExprF);
		this.assertDifference(ifExprT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			ifExprT.toString();
			ifExprT.toClojureCode(top, typeEnv);
			ifExprT.hashCode();
		});

		this.assertInterpretationEquals(ifExprT, new LitInteger(42), top, typeEnv);
		this.assertInterpretationEquals(ifExprF, new LitInteger(42), top, typeEnv);

		Pair<Type, Substitution> p = ifExprT.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, ifExprT);
	}

	@Test
	@DisplayName("Test AND")
	void testAndExpression() throws AppendableException {
		AndExpression andExpressionT = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.TRUE)));
		AndExpression andExpressionF = new AndExpression(new Tuple(Arrays.asList(LitBoolean.TRUE, LitBoolean.FALSE)));

		this.assertReflexivity(andExpressionT);
		this.assertDifference(andExpressionT, andExpressionF);
		this.assertDifference(andExpressionT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			andExpressionT.toString();
			andExpressionT.toClojureCode(top, typeEnv);
			andExpressionT.hashCode();
		});

		this.assertInterpretationEquals(andExpressionT, LitBoolean.TRUE, top, typeEnv);
		this.assertInterpretationEquals(andExpressionF, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = andExpressionT.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, andExpressionT);
	}

	@Test
	void testOrExpression() throws AppendableException {
		OrExpression orExpressionT = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.TRUE)));
		OrExpression orExpressionF = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.FALSE)));

		this.assertReflexivity(orExpressionT);
		this.assertDifference(orExpressionT, orExpressionF);
		this.assertDifference(orExpressionT, Expression.EMPTY_EXPRESSION);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			orExpressionT.toString();
			orExpressionF.toClojureCode(top, typeEnv);
			orExpressionT.hashCode();
		});

		this.assertInterpretationEquals(orExpressionT, LitBoolean.TRUE, top, typeEnv);
		this.assertInterpretationEquals(orExpressionF, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = orExpressionT.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, orExpressionT);
	}

	@Test
	@DisplayName("Test Operators")
	void testOperators() throws AppendableException, IOException {
		this.assertOperator(Operators.Addition,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(21))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		this.assertOperator(Operators.BitAnd, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(0), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.BitOr, new Tuple(Arrays.asList(new LitInteger(1), new LitInteger(2))),
				new LitInteger(3), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.Car,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitInteger(42), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.Cdr,
				new Tuple(Arrays.asList(new Tuple(Arrays.asList(new LitInteger(42), new LitString("foo"))))),
				new LitString("foo"), TypeAtom.TypeStringNative);
		this.assertOperator(Operators.Concantenation,
				new Tuple(Arrays.asList(new LitString("foo"), new LitString("bar"))), new LitString("foobar"),
				TypeAtom.TypeStringNative);
		this.assertOperator(Operators.Division,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		this.assertOperator(Operators.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, LitBoolean.FALSE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.Equals,
				new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.LesserThan,
				new Tuple(Arrays.asList(new LitInteger(43), new LitInteger(42))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.Multiplication,
				new Tuple(Arrays.asList(new LitInteger(21), new LitInteger(2))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		this.assertOperator(Operators.Not, new Tuple(Arrays.asList(LitBoolean.TRUE)), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.Not, new Tuple(Arrays.asList(LitBoolean.FALSE)), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(42))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.NumericEqual,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(43))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.Subtraction,
				new Tuple(Arrays.asList(new LitInteger(84), new LitInteger(42))), new LitInteger(42),
				TypeAtom.TypeIntNative);
		this.assertOperator(Operators.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyRepresentations,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyRepresentations,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyRepresentations, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyTypes,
				new Tuple(
						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyTypes,
				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.CanUnifyTypes, new Tuple(
				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);

		this.assertOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.IsSameType,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);

		this.assertOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitInteger(21))), LitBoolean.TRUE,
				TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.IsSameRepresentation,
				new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), LitBoolean.FALSE,
				TypeAtom.TypeBoolNative);
		
		this.assertOperator(Operators.BitShiftRight, new Tuple(new LitInteger(2), new LitInteger(1)),
				new LitInteger(1), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.BitShiftLeft, new Tuple(new LitInteger(2), new LitInteger(1)),
				new LitInteger(4), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.UnsignedBitShiftRight, new Tuple(new LitInteger(2), new LitInteger(1)),
				new LitInteger(1), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.UnsignedBitShiftRight, new Tuple(new LitInteger(-1), new LitInteger(10)), 
				new LitInteger(18014398509481983l), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.BitNot, new Tuple(new LitInteger(6)), new LitInteger(-7), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.BitXor, new Tuple(new LitInteger(5), new LitInteger(6)), new LitInteger(3), TypeAtom.TypeIntNative);
		this.assertOperator(Operators.ToStr, new Tuple(new LitInteger(42)), new LitString("[42]"), TypeAtom.TypeStringNative);
		
		File tempOut = File.createTempFile("velka_read_test", null);
        String content  = "hello world !!";       
        Files.writeString(tempOut.toPath(), content);
        
        this.assertOperator(
        		Operators.ReadFile, 
        		new Tuple(new LitString(tempOut.toPath().toString())), 
        		new LitString(content), 
        		TypeAtom.TypeStringNative);
        
        tempOut.delete();
        
        Environment env = Environment.initTopLevelEnvironment();
        TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
        
        this.assertOperator(Operators.StrSplit, 
        		new Tuple(new LitString("foo bar baz"), 
        		new LitString(" ")),
        		ListNative.makeListNativeExpression(new LitString("foo"), new LitString("bar"), new LitString("baz")).interpret(env, typeEnv), 
        		TypeAtom.TypeListNative);
        
        this.assertOperator(Operators.ParseInt, 
        		new Tuple(new LitString("42")), new LitInteger(42), TypeAtom.TypeIntNative);
        
		this.assertOperator(Operators.IntToDouble, new Tuple(new LitInteger(42)), new LitDouble(42.0f),
				TypeAtom.TypeDoubleNative);
		
		this.assertOperator(Operators.Floor, new Tuple(new LitDouble(3.14f)), new LitInteger(3),
				TypeAtom.TypeIntNative);
		
		this.assertOperator(Operators.DoubleAddition, new Tuple(new LitDouble(3.14), new LitDouble(3.14)), new LitDouble(6.28), TypeAtom.TypeDoubleNative);
		this.assertOperator(Operators.DoubleLesserThan, new Tuple(new LitDouble(3.14), new LitDouble(6.28)), LitBoolean.TRUE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.DoubleLesserThan, new Tuple(new LitDouble(3.14), new LitDouble(3.14)), LitBoolean.FALSE, TypeAtom.TypeBoolNative);
		this.assertOperator(Operators.Modulo, new Tuple(new LitInteger(5), new LitInteger(3)), new LitInteger(2), TypeAtom.TypeIntNative);
		
		this.assertOperator(Operators.ConversionCost,
				new Tuple(new Lambda(new Tuple(new Symbol("x")), new TypeTuple(TypeAtom.TypeIntNative), new LitString("foo")),
						new Tuple(new LitComposite(new LitString("IV"), TypeAtom.TypeIntRoman))),
				new LitInteger(1),
				TypeAtom.TypeIntNative);
		this.assertOperator(Operators.ConversionCost,
				new Tuple(new Lambda(new Tuple(new Symbol("x")), new TypeTuple(TypeAtom.TypeIntNative), new LitString("foo")),
						new Tuple(new LitInteger(42))),
				new LitInteger(0),
				TypeAtom.TypeIntNative);
	}

	@Test
	@DisplayName("Test Conversions")
	void testConversions() throws AppendableException {
		this.assertConversion(ConversionOperators.IntRomanToIntString,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntString));
		this.assertConversion(ConversionOperators.IntRomanToIntNative,
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntNative));
		this.assertConversion(ConversionOperators.IntStringToIntRoman,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntRoman));
		this.assertConversion(ConversionOperators.IntStringToIntNative,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString), new LitInteger(5),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntNative));
		this.assertConversion(ConversionOperators.IntNativeToIntString, new LitInteger(5),
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntString));
		this.assertConversion(ConversionOperators.IntNativeToIntRoman, new LitInteger(5),
				new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntRoman));
	}

	@Test
	@DisplayName("Test Automatic conversion")
	void testAutoConversion() throws AppendableException {
		Expression e = new AbstractionApplication(Operators.Addition,
				new Tuple(Arrays.asList(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
						new LitComposite(new LitString("42"), TypeAtom.TypeIntString))));

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		this.assertInterpretationEquals(e, new LitInteger(84), top, typeEnv);
	}

	@Test
	@DisplayName("Test Environment")
	void testEnvironment() throws AppendableException {
		Environment top = Environment.initTopLevelEnvironment();

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

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defTypeExpression.toString();
			defTypeExpression.hashCode();
			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			defTypeExpression.toClojureCode(top, cljTypeEnv);
		});

		this.assertReflexivity(defTypeExpression);
		this.assertDifference(defTypeExpression, new DefineType(new TypeName("Test2")));
		this.assertDifference(defTypeExpression, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(defTypeExpression, Expression.EMPTY_EXPRESSION, top, typeEnv);
		assertTrue(typeEnv.existsTypeAtom(new TypeAtom(new TypeName("Test"), TypeRepresentation.WILDCARD)));

		Pair<Type, Substitution> p = defTypeExpression.infer(top, typeEnv);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defTypeExpression);
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

		Environment top = Environment.initTopLevelEnvironment();
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

		this.assertReflexivity(defCon);
		this.assertDifference(defCon,
				new DefineConversion(new TypeAtom(name, TypeRepresentation.STRING), typeAtomWildcard,
						new Tuple(Arrays.asList(new Symbol("x"))),
						new LitComposite(new Symbol("y"), typeAtomWildcard)));
		this.assertDifference(defCon,
				new DefineConversion(typeAtomNative, new TypeAtom(name, TypeRepresentation.STRING),
						new Tuple(Arrays.asList(new Symbol("x"))),
						new LitComposite(new Symbol("y"), typeAtomWildcard)));
		this.assertDifference(defCon, new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("y"))), new LitComposite(new Symbol("y"), typeAtomWildcard)));
		this.assertDifference(defCon, new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("x"))), Expression.EMPTY_EXPRESSION));

		this.assertDifference(defCon, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(defCon, Expression.EMPTY_EXPRESSION, top, typeEnv);

		Pair<Type, Substitution> p = defCon.infer(top, typeEnv);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defCon);
	}

	@Test
	@DisplayName("Test Define Representation")
	void testDefRepresentationExpression() throws AppendableException {
		TypeName name = new TypeName("__defRepresentationTest");

		DefineRepresentation defRep = new DefineRepresentation(name, TypeRepresentation.NATIVE);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			defRep.toString();
			defRep.hashCode();

			TypeEnvironment cljTypeEnv = TypeEnvironment.initBasicTypes(top);
			cljTypeEnv.addType(name);
			defRep.toClojureCode(top, cljTypeEnv);
		});

		this.assertReflexivity(defRep);
		this.assertDifference(defRep, new DefineRepresentation(TypeName.INT, TypeRepresentation.NATIVE));
		this.assertDifference(defRep, new DefineRepresentation(name, TypeRepresentation.STRING));
		this.assertDifference(defRep, Expression.EMPTY_EXPRESSION);

		typeEnv.addType(name);
		this.assertInterpretationEquals(defRep, Expression.EMPTY_EXPRESSION, top, typeEnv);

		assertThrows(AppendableException.class,
				() -> new DefineRepresentation(name, TypeRepresentation.NATIVE).interpret(top, typeEnv));

		Pair<Type, Substitution> p = defRep.infer(top, typeEnv);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defRep);
	}

	@Test
	@DisplayName("Test Define Constructor")
	void testDefinceConstructorExpression() throws AppendableException {
		TypeName name = new TypeName("__defConstructorTest");
		TypeAtom type = new TypeAtom(name, TypeRepresentation.NATIVE);
		Lambda constructor = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
				new LitComposite(Expression.EMPTY_EXPRESSION, type));

		DefineConstructor defCon = new DefineConstructor(type, constructor);

		Environment top = Environment.initTopLevelEnvironment();
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

		this.assertReflexivity(defCon);
		this.assertDifference(defCon, new DefineConstructor(TypeAtom.TypeIntNative, constructor));
		this.assertDifference(defCon, new DefineConstructor(type, Lambda.identity));

		this.assertInterpretationEquals(defCon, Expression.EMPTY_EXPRESSION, top, typeEnv);

		Pair<Type, Substitution> p = defCon.infer(top, typeEnv);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top, typeEnv).first, defCon);
	}

	@Test
	@DisplayName("Test Construct")
	void testConstruct() throws AppendableException {
		Construct construct = new Construct(TypeAtom.TypeIntRoman, new Tuple(new LitString("XLII")));

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			construct.toString();
			construct.hashCode();
			construct.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(construct);
		this.assertDifference(construct,
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("XLII")))));
		this.assertDifference(construct,
				new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XXI")))));
		this.assertDifference(construct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(construct, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				top, typeEnv);

		Pair<Type, Substitution> p = construct.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntRoman, construct);
	}

	@Test
	@DisplayName("Test Deconstruct")
	void testDeconstruct() throws AppendableException {
		Deconstruct deconstruct = new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
				TypeAtom.TypeStringNative);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			deconstruct.toString();
			deconstruct.hashCode();
			deconstruct.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(deconstruct);
		this.assertDifference(deconstruct, new Deconstruct(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeStringNative));
		this.assertDifference(deconstruct,
				new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		this.assertDifference(deconstruct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(deconstruct, new LitString("42"), top, typeEnv);

		Pair<Type, Substitution> p = deconstruct.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeStringNative, deconstruct);
	}

	@Test
	@DisplayName("Test Can Deconstruct As")
	void testCanDeconstructAs() throws AppendableException {
		CanDeconstructAs canDeconstruct = new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeStringNative);
		CanDeconstructAs cannotDeconstrut = new CanDeconstructAs(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeIntNative);

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			canDeconstruct.toString();
			canDeconstruct.hashCode();
			canDeconstruct.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(canDeconstruct);
		this.assertDifference(canDeconstruct, new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		this.assertDifference(canDeconstruct,
				new CanDeconstructAs(new LitDouble(3.14), TypeAtom.TypeStringNative));
		this.assertDifference(canDeconstruct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(canDeconstruct, LitBoolean.TRUE, top, typeEnv);
		this.assertInterpretationEquals(cannotDeconstrut, LitBoolean.FALSE, top, typeEnv);

		Pair<Type, Substitution> p = canDeconstruct.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, canDeconstruct);
	}

	@Test
	@DisplayName("Test Convert")
	void testConvert() throws AppendableException {
		Convert convert = new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(42));

		Environment top = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(top);

		assertAll(() -> {
			convert.hashCode();
			convert.toString();
			convert.toClojureCode(top, typeEnv);
		});

		this.assertReflexivity(convert);
		this.assertDifference(convert,
				new Convert(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, new LitInteger(42)));
		this.assertDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, new LitInteger(42)));
		this.assertDifference(convert,
				new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(21)));
		this.assertDifference(convert, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(convert, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				top, typeEnv);

		Pair<Type, Substitution> p = convert.infer(top, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntRoman, convert);
	}

	@Test
	@DisplayName("Test List Native")
	void testListNative() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		assertEquals(
				new LitComposite(
						new LitInteropObject(new LinkedList<Expression>(
								Arrays.asList(new LitInteger(42), new LitInteger(21), new LitInteger(2)))),
						TypeAtom.TypeListNative),
				ListNative.makeListNativeExpression(new LitInteger(42), new LitInteger(21), new LitInteger(2)).interpret(env, typeEnv));

		this.assertInterpretedStringEquals("(construct List Native)",
				ListNative.EMPTY_LIST_NATIVE, env, typeEnv);
		this.assertInterpretedStringEquals("(construct List Native 42 (construct List Native))",
				ListNative.makeListNativeExpression(new LitInteger(42)).interpret(env, typeEnv), env, typeEnv);

		this.assertInterpretedStringEquals("(is-list-native-empty (construct List Native))", LitBoolean.TRUE, env,
				typeEnv);
		this.assertInterpretedStringEquals(
				"(is-list-native-empty (construct List Native 42 (construct List Native)))", LitBoolean.FALSE, env,
				typeEnv);

		this.assertInterpretedStringEquals("(head-list-native (construct List Native 42 (construct List Native)))",
				new LitInteger(42), env, typeEnv);
		assertThrows(UserException.class,
				() -> this.assertInterpretedStringEquals("(head-list-native (construct List Native))",
						Expression.EMPTY_EXPRESSION, env, typeEnv));

		this.assertInterpretedStringEquals("(tail-list-native (construct List Native 42 (construct List Native)))",
				ListNative.EMPTY_LIST_NATIVE, env, typeEnv);
		assertThrows(UserException.class,
				() -> this.assertInterpretedStringEquals("(tail-list-native (construct List Native))",
						Expression.EMPTY_EXPRESSION, env, typeEnv));

		this.assertInterpretedStringEquals(
				"(map-list-native (lambda (x) (+ x 1)) (construct List Native 42 (construct List Native)))",
				ListNative.makeListNativeExpression(new LitInteger(43)).interpret(env, typeEnv), env, typeEnv);

		this.assertInterpretedStringEquals(
				"(map2-list-native + (construct List Native 21 (construct List Native 21 (construct List Native))) (construct List Native 21 (construct List Native 21 (construct List Native))))",
				ListNative.makeListNativeExpression(new LitInteger(42), new LitInteger(42)).interpret(env, typeEnv),
				env, typeEnv);

		this.assertInterpretedStringEquals(
				"(foldl-list-native + 0 (construct List Native 1 (construct List Native 2 (construct List Native))))",
				new LitInteger(3), env, typeEnv);
		
		this.assertInterpretedStringEquals(
				"(" + ListNative.addToEndSymbol_out + " (construct List Native 21 (construct List Native)) 42)",
				ListNative.makeListNativeExpression(new LitInteger(21), new LitInteger(42)).interpret(env, typeEnv), env, typeEnv);
		
		ArrayList<Expression> al = new ArrayList<Expression>();
		al.add(new LitInteger(42));
		al.add(new LitInteger(21));
		this.assertInterpretedStringEquals(
				"(convert List:Native List:JavaArray (construct List Native 42 (construct List Native 21 (construct List Native))))",
				new LitComposite(new LitInteropObject(al), JavaArrayList.TypeListJavaArray), env, typeEnv);
		
		LinkedList<Expression> ll = new LinkedList<Expression>();
		ll.add(new LitInteger(42));
		ll.add(new LitInteger(21));
		this.assertInterpretedStringEquals(
				"(convert List:Native List:JavaLinked (construct List Native 42 (construct List Native 21 (construct List Native))))",
				new LitComposite(new LitInteropObject(ll), JavaLinkedList.TypeListJavaLinked), env, typeEnv);
				
		this.assertInterpretedStringEquals("(contains-list-native (construct List Native 42 (construct List Native 21 (construct List Native))) 42)", LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretedStringEquals("(contains-list-native (construct List Native 42 (construct List Native 21 (construct List Native))) 84)", LitBoolean.FALSE, env, typeEnv);
		
		this.assertInterpretedStringEquals("(filter-list-native (construct List Native #t (construct List Native #f (construct List Native))) (lambda (x) x))",
				ListNative.makeListNativeExpression(LitBoolean.TRUE).interpret(env, typeEnv), env, typeEnv);
		this.assertInterpretedStringEquals("(get-list-native (construct List Native 42 (construct List Native)) 0)", new LitInteger(42), env, typeEnv);
		this.assertInterpretedStringEquals("(build-list-native 2 (lambda (x) x))", ListNative.makeListNativeExpression(new LitInteger(0), new LitInteger(1)).interpret(env, typeEnv), env, typeEnv);
		
		this.assertInterpretedStringEquals("(remove-list-native (build-list-native 2 (lambda (x) x)) 1)", ListNative.makeListNativeExpression(new LitInteger(0)).interpret(env, typeEnv), env, typeEnv);
		this.assertInterpretedStringEquals("(size-list-native (build-list-native 42 (lambda (x) x)))", new LitInteger(42), env, typeEnv);
		this.assertInterpretedStringEquals("(append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42)))", 
				ListNative.makeListNativeExpression(new LitInteger(21), new LitInteger(42)).interpret(env, typeEnv), env, typeEnv);
		
		this.assertInterpretedStringEquals("(reverse-list-native (build-list-native 3 (lambda (x) x)))",
				ListNative.makeListNativeExpression(new LitInteger(2), new LitInteger(1), new LitInteger(0))
						.interpret(env, typeEnv),
				env, typeEnv);
		
		this.assertInterpretedStringEquals("(everyp-list-native (construct List Native #t (construct List Native #t (construct List Native))) (lambda (x) x))",
				LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretedStringEquals("(everyp-list-native (construct List Native #t (construct List Native #f (construct List Native))) (lambda (x) x))",
				LitBoolean.FALSE, env, typeEnv);
	}

	@Test
	@DisplayName("Test Type Symbol")
	void testTypeSymbol() throws AppendableException {
		TypeSymbol typeSymbol = new TypeSymbol(TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			typeSymbol.toString();
			typeSymbol.hashCode();
			typeSymbol.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(typeSymbol);
		this.assertDifference(typeSymbol, new TypeSymbol(TypeAtom.TypeBoolNative));
		this.assertDifference(typeSymbol, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(typeSymbol, typeSymbol, env, typeEnv);

		Pair<Type, Substitution> p = typeSymbol.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, typeSymbol);
	}

	@Test
	@DisplayName("Test instance-of")
	void testInstanceOf() throws AppendableException {
		InstanceOf iof = new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			iof.toString();
			iof.hashCode();
			iof.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(iof);
		InstanceOf iof_isStrInt = new InstanceOf(new LitString("foo"), TypeAtom.TypeIntNative);
		this.assertDifference(iof, iof_isStrInt);
		InstanceOf iof_typevar = new InstanceOf(new LitInteger(42), new TypeVariable(NameGenerator.next()));
		this.assertDifference(iof, iof_typevar);
		InstanceOf iof_otherRepre = new InstanceOf(new LitInteger(42), TypeAtom.TypeIntRoman);
		this.assertDifference(iof, iof_otherRepre);
		this.assertDifference(iof,
				new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative));
		this.assertDifference(iof, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(iof, LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretationEquals(iof_isStrInt, LitBoolean.FALSE, env, typeEnv);
		this.assertInterpretationEquals(iof_typevar, LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretationEquals(iof_otherRepre, LitBoolean.TRUE, env, typeEnv);

		Pair<Type, Substitution> p = iof.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, iof);
	}

	@Test
	@DisplayName("Test instance-of-representation")
	void testInstanceOfRepresentation() throws AppendableException {
		InstanceOfRepresentation iofr = new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertAll(() -> {
			iofr.toString();
			iofr.hashCode();
			iofr.toClojureCode(env, typeEnv);
		});

		this.assertReflexivity(iofr);
		InstanceOfRepresentation iofr_isStrInt = new InstanceOfRepresentation(new LitString("foo"),
				TypeAtom.TypeIntNative);
		this.assertDifference(iofr, iofr_isStrInt);
		InstanceOfRepresentation iofr_typevar = new InstanceOfRepresentation(new LitInteger(42),
				new TypeVariable(NameGenerator.next()));
		this.assertDifference(iofr, iofr_typevar);
		InstanceOfRepresentation iofr_otherRepre = new InstanceOfRepresentation(new LitInteger(42),
				TypeAtom.TypeIntRoman);
		this.assertDifference(iofr, iofr_otherRepre);
		this.assertDifference(iofr, new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative));
		this.assertDifference(iofr, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(iofr, LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretationEquals(iofr_isStrInt, LitBoolean.FALSE, env, typeEnv);
		this.assertInterpretationEquals(iofr_typevar, LitBoolean.TRUE, env, typeEnv);
		this.assertInterpretationEquals(iofr_otherRepre, LitBoolean.FALSE, env, typeEnv);

		Pair<Type, Substitution> p = iofr.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeBoolNative, iofr);
	}

	@Test
	@DisplayName("Test custom cost function")
	void testCustomCostFunction() throws AppendableException {
		Symbol arg = new Symbol("a");
		Tuple elambda_args = new Tuple(arg);
		
		Lambda impl1 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("Int Native"));
		Lambda impl2 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntString),
				new LitString("Int String"));
		Lambda impl3 = new Lambda(
				elambda_args, 
				new TypeTuple(TypeAtom.TypeIntRoman),
				new LitString("Int Roman"));
		
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		Tuple args = new Tuple(new LitInteger(42));

		ExtendedLambda elambda_defaultCostFunction = 
				ExtendedLambda.makeExtendedLambda(Arrays.asList(
						impl1, 
						impl2, 
						impl3));
		AbstractionApplication app_defCostFunction = 
				new AbstractionApplication(
						elambda_defaultCostFunction, 
						args);
		this.assertInterpretationEquals(
				app_defCostFunction, 
				new LitString("Int Native"), 
				env, 
				typeEnv);
		
		Lambda costFunction = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeInt),
				new LitInteger(Long.MIN_VALUE));

		Map<Lambda, Expression> m = new TreeMap<Lambda, Expression>();
		m.put(impl1, impl1.defaultCostFunction());
		m.put(impl2, costFunction);
		m.put(impl3, impl3.defaultCostFunction());
		
		ExtendedLambda elambda_customCostFunction = 
				ExtendedLambda.makeExtendedLambda(m);

		AbstractionApplication app_customCostFunction = 
				new AbstractionApplication(
						elambda_customCostFunction, 
						args);
		this.assertInterpretationEquals(
				app_customCostFunction, 
				new LitString("Int String"), 
				env, 
				typeEnv);
		
//		Lambda preferIntRoman = new Lambda(
//				elambda_args,
//				new TypeTuple(TypeAtom.TypeInt),
//				new IfExpression(
//						new InstanceOfRepresentation(arg, TypeAtom.TypeIntRoman),
//						new LitInteger(Long.MIN_VALUE),
//						new LitInteger(Long.MAX_VALUE)));
//		
//		AbstractionApplication app_customCostOnAppl = 
//				new AbstractionApplication(
//					elambda_defaultCostFunction,
//					args,
//					preferIntRoman);
//		
//		testInterpretation(
//				app_customCostOnAppl,
//				new LitString("Int Roman"),
//				env,
//				typeEnv);
	}

	@Test
	@DisplayName("Test Java Array List")
	void testJavaArrayList() throws Exception {
		this.assertInterpretationEquals("(construct List JavaArray)",
				new LitComposite(new LitInteropObject(new ArrayList<Object>()), JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals(
				"(construct List JavaArray (build-list-native 2 (lambda (x) x)))", 
				new LitComposite(
						new LitInteropObject(new ArrayList<Object>(Arrays.asList(new LitInteger(0), new LitInteger(1)))), 
						JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals(
				"(construct List JavaArray 42)",
				new LitComposite(
						new LitInteropObject(new ArrayList<Object>(42)), JavaArrayList.TypeListJavaArray));

		ArrayList<Object> l = new ArrayList<Object>();
		l.add(new LitInteger(42));

		this.assertInterpretationEquals(
				"(" + JavaArrayList.addToEndSymbol_out.toString() + " (construct List JavaArray) 42)", LitBoolean.TRUE);
		this.assertInterpretationEquals(
				"(" + JavaArrayList.addToIndexSymbol_out.toString() + " (construct List JavaArray) 0 42)",
				Expression.EMPTY_EXPRESSION);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.addAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.containsSymbol_out + " l1 42)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaArrayList.containsSymbol_out + " l1 84)",
				LitBoolean.FALSE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 42)"
				+ "(" + JavaArrayList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.getSymbol_out + " l1 0)",
				new LitInteger(1));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.indexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.indexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		
		this.assertInterpretationEquals("(" + JavaArrayList.isEmptySymbol_out + " (construct List JavaArray))", 
				LitBoolean.TRUE);
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.isEmptySymbol_out + " l1)", 
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.lastIndexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.lastIndexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		/*this.testInterpretString(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol + " l1 2)" 
				+ "(" + JavaArrayList.removeSymbol + " l1 0)", 
				new LitInteger(1));*/
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.removeSymbol_out + " l1 2)", 
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.removeAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 4)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 5)"
				+ "(" + JavaArrayList.removeAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.retainAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaArrayList.retainAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.setSymbol_out + " l1 0 2)", 
				new LitInteger(1));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaArrayList.sizeSymbol_out + " l1)", 
				new LitInteger(2));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 84)" 
				+ "(" + JavaArrayList.sublistSymbol_out + " l1 0 1)", 
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)"  
				+ "(" + JavaArrayList.mapSymbol_out + " l1 (lambda (x) (* 2 x)))", 
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals("(define l1 (construct List JavaArray))\n"
				+ "(define l2 (construct List JavaArray))"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l2 21)"
				+ "(" + JavaArrayList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y)))",
				new LitComposite(new LitInteropObject(l), JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 21)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaArrayList.foldlSymbol_out + " + 0 l1)", 
				new LitInteger(63));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaArray))\n" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l1 4)"
				+ "(" + JavaArrayList.foldrSymbol_out + " / 16 l1)", 
				new LitInteger(2));
		
		LinkedList<Expression> converted = new LinkedList<Expression>();
		converted.add(new LitInteger(42));
		converted.add(new LitInteger(21));
		this.assertInterpretationEquals("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaArray List:JavaLinked l)", 
				new LitComposite(new LitInteropObject(converted), JavaLinkedList.TypeListJavaLinked));
		
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		this.assertInterpretationEquals("(define l (construct List JavaArray))\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaArrayList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaArray List:Native l)", 
				ListNative.makeListNativeExpression(new LitInteger(42), new LitInteger(21)).interpret(env, typeEnv));
		this.assertInterpretationEquals(
				"(java-array-list-everyp (construct List Native #t (construct List Native #t (construct List Native))) (lambda (x) x))",
				LitBoolean.TRUE);
		this.assertInterpretationEquals(
				"(java-array-list-everyp (construct List Native #t (construct List Native #f (construct List Native))) (lambda (x) x))",
				LitBoolean.FALSE);
	}
	
	@Test
	@DisplayName("Test Java Linked List")
	void testJavaLinkedList() throws Exception {
		this.assertInterpretationEquals("(construct List JavaLinked)",
				new LitComposite(new LitInteropObject(new LinkedList<Object>()), JavaLinkedList.TypeListJavaLinked));

		LinkedList<Object> l = new LinkedList<Object>();
		l.add(new LitInteger(42));

		this.assertInterpretationEquals(
				"(" + JavaLinkedList.addToEndSymbol_out.toString() + " (construct List JavaLinked) 42)", LitBoolean.TRUE);
		this.assertInterpretationEquals(
				"(" + JavaLinkedList.addToIndexSymbol_out.toString() + " (construct List JavaLinked) 0 42)",
				Expression.EMPTY_EXPRESSION);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.addAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		
		assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.toStrSymbol_out + " l1)",
				new LitString("[[42], [42]]"));
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.containsSymbol_out + " l1 42)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
				+ "(" + JavaLinkedList.containsSymbol_out + " l1 84)",
				LitBoolean.FALSE);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 42)"
				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.getSymbol_out + " l1 0)",
				new LitInteger(1));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 42)",
				new LitInteger(-1));
		
		this.assertInterpretationEquals(
				"(" + JavaLinkedList.isEmptySymbol_out + " (construct List JavaLinked))", 
				LitBoolean.TRUE);
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.isEmptySymbol_out + " l1)", 
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 1)",
				new LitInteger(0));
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 42)",
				new LitInteger(-1));

		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.removeSymbol_out + " l1 2)", 
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.removeAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 4)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 5)"
				+ "(" + JavaLinkedList.removeAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.retainAllSymbol_out + " l1 l2)",
				LitBoolean.TRUE);
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
				+ "(" + JavaLinkedList.retainAllSymbol_out + " l2 l1)",
				LitBoolean.FALSE);
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.setSymbol_out + " l1 0 2)", 
				new LitInteger(1));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
				+ "(" + JavaLinkedList.sizeSymbol_out + " l1)", 
				new LitInteger(2));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 84)" 
				+ "(" + JavaLinkedList.sublistSymbol_out + " l1 0 1)", 
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"  
				+ "(" + JavaLinkedList.mapSymbol_out + " l1 (lambda (x) (* 2 x)))", 
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		this.assertInterpretationEquals("(define l1 (construct List JavaLinked))\n"
				+ "(define l2 (construct List JavaLinked))"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 21)"
				+ "(" + JavaLinkedList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y)))",
				new LitComposite(new LitInteropObject(l), JavaLinkedList.TypeListJavaLinked));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
				+ "(" + JavaLinkedList.foldlSymbol_out + " + 0 l1)", 
				new LitInteger(63));
		
		this.assertInterpretationEquals(
				"(define l1 (construct List JavaLinked))\n" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 4)"
				+ "(" + JavaLinkedList.foldrSymbol_out + " / 16 l1)", 
				new LitInteger(2));
		
		ArrayList<Expression> converted = new ArrayList<Expression>();
		converted.add(new LitInteger(42));
		converted.add(new LitInteger(21));
		this.assertInterpretationEquals("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaLinked List:JavaArray l)", 
				new LitComposite(new LitInteropObject(converted), JavaArrayList.TypeListJavaArray));
		
		this.assertInterpretationEquals("(define l (construct List JavaLinked))\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
				+ "(convert List:JavaLinked List:Native l)", 
				ListNative.makeListNativeExpression(new LitInteger(42), new LitInteger(21)));
		
		this.assertInterpretationEquals(
				"(java-linked-list-everyp (construct List Native #t (construct List Native #f (construct List Native))) (lambda (x) x))",
				LitBoolean.FALSE);
		this.assertInterpretationEquals(
				"(java-linked-list-everyp (construct List Native #t (construct List Native #t (construct List Native))) (lambda (x) x))",
				LitBoolean.TRUE);
		
		LinkedList<LitInteger> ll = new LinkedList<LitInteger>();
		ll.add(new LitInteger(0));
		ll.add(new LitInteger(2));
		ll.add(new LitInteger(4));
		ll.add(new LitInteger(6));
		ll.add(new LitInteger(8));
		ll.add(new LitInteger(10));
		ll.add(new LitInteger(12));
		ll.add(new LitInteger(14));
		ll.add(new LitInteger(16));
		ll.add(new LitInteger(18));
		ListIterator<LitInteger> li = ll.listIterator(0);
		this.assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))"
						+ "(linked-list-iterator-next (java-linked-list-iterator l 0))",
				li.next());
		
		li.previous();
		li.add(new LitInteger(42));
		this.assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 0))\n"
						+ "(linked-list-iterator-next (linked-list-iterator-add it 42))"
						+ "(java-linked-list-to-str l)",
				new LitString("[[42], [0], [2], [4], [6], [8], [10], [12], [14], [16], [18]]"));
		
		li.next();
		li.remove();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 0))\n"
						+ "(linked-list-iterator-has-next it)",
				li.hasNext() ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		li.previous();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 0))\n"
						+ "(linked-list-iterator-has-previous it)",
				li.hasPrevious() ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 0))\n"
						+ "(linked-list-iterator-next-index it)",
				new LitInteger(li.nextIndex()));
		
		li.next();
		li.next();
		li.next();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 3))\n"
						+ "(linked-list-iterator-previous it)",
				li.previous());
		
		li.next();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 3))\n"
						+ "(linked-list-iterator-previous-index it)",
				new LitInteger(li.previousIndex()));
		
		li.next();
		li.remove();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 3))\n"
						+ "(linked-list-iterator-next it)"
						+ "(linked-list-iterator-next (linked-list-iterator-remove it))"
						+ "(java-linked-list-to-str l)",
				new LitString("[[0], [2], [4], [8], [10], [12], [14], [16], [18]]"));
		
		li.next();
		li.add(new LitInteger(8));
		li.previous();
		li.set(new LitInteger(42));
		li.previous();
		assertInterpretationEquals(
				"(define l (construct List JavaLinked))\n"
						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
						+ "(define it (java-linked-list-iterator l 3))\n"
						+ "(linked-list-iterator-next it)"
						+ "(linked-list-iterator-next (linked-list-iterator-set it 42))",
				li.next());
	}
	
	@Test
	@DisplayName("Test Java Bit Set")
	void testJavaBitSet() throws AppendableException {
		BitSet bs = new BitSet();
		LitComposite bitSet = new LitComposite(
				new LitInteropObject(bs),
				TypeAtom.TypeSetBitSet);
		
		assertInterpretationEquals(
				"(construct Set BitSet)",
				bitSet);
		
		BitSet nbitsBs = new BitSet(2048);
		LitComposite nBitsBitSet = new LitComposite(
				new LitInteropObject(nbitsBs),
				TypeAtom.TypeSetBitSet);
		
		assertInterpretationEquals(
				"(construct Set BitSet 2048)",
				nBitsBitSet);
		
		bs.set(3);
		assertInterpretationEquals(
					"(define s (construct Set BitSet))\n"
				+ 	"(" + JavaBitSet.setSymbol_out.toString() + " s 3)",
				bitSet);
		assertInterpretationEquals(
					"(define s (construct Set BitSet))\n"
				+	"(" + JavaBitSet.setValueSymbol_out.toString() + " s 3 #t)",
				bitSet);
		bs.set(2, 5);
		assertInterpretationEquals(
					"(define s (construct Set BitSet))\n"
				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s 2 5)",
				bitSet);
		assertInterpretationEquals(
					"(define s (construct Set BitSet))\n"
				+	"(" + JavaBitSet.setIntervalValueSymbol_out.toString() + " s 2 5 #t)",
				bitSet);
		
		nbitsBs.set(4, 7);
		nbitsBs.and(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.andSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.andNot(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.andNotSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		assertInterpretationEquals(
					"(define s2 (construct Set BitSet))\n"
				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
				+	"(" + JavaBitSet.cardinalitySymbol_out.toString() + " s2)",
				new LitInteger(bs.cardinality()));
		
		nbitsBs.clear();
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.clearSymbol_out.toString() + " s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.clear(5);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.clearBitIndexSymbol_out.toString() + " s2 5)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.clear(5, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.clearIntervalSymbol_out.toString() + " s2 5 7)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.cloneSymbol_out.toString() + " s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.equalsSymbol_out.toString() + " s1 s2)",
			LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.flip(2);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.flipSymbol_out.toString() + " s2 2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.flip(2, 5);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.flipIntervalSymbol_out.toString() + " s2 2 5)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.getSymbol_out.toString() + " s2 5)",
			nbitsBs.get(5) ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.getIntervalSymbol_out.toString() + " s2 5 7)",
			new LitComposite(new LitInteropObject(nbitsBs.get(5, 7)), TypeAtom.TypeSetBitSet));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.intersectsSymbol_out.toString() + " s1 s2)",
			nbitsBs.intersects(bs) ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.isEmptySymbol_out.toString() + " s2)",
			nbitsBs.isEmpty() ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.lengthSymbol_out.toString() + " s2)",
			new LitInteger(nbitsBs.length()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.nextClearBitSymbol_out.toString() + " s2 5)",
			new LitInteger(nbitsBs.nextClearBit(5)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.nextSetBitSymbol_out.toString() + " s2 0)",
			new LitInteger(nbitsBs.nextSetBit(0)));
		
		nbitsBs.set(4, 7);
		nbitsBs.or(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.orSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.previousClearBitSymbol_out.toString() + " s2 5)",
			new LitInteger(nbitsBs.previousClearBit(5)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.previousSetBitSymbol_out.toString() + " s2 9)",
			new LitInteger(nbitsBs.previousSetBit(9)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 2 5)\n"
			+	"(" + JavaBitSet.sizeSymbol_out.toString() + " s1)",
			new LitInteger(bs.size()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.strSymbol_out.toString() + " s2)",
			new LitString(nbitsBs.toString()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.xor(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.xorSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
	}
	
	@Test
	@DisplayName("Test logging")
	void testLogging() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		assertNotEquals(new LitInteger(0),
				this.parseString("(timestamp)")
					.get(0).interpret(env, typeEnv));
		
		this.assertInterpretationEquals("(init-logger \"test-log\")", Expression.EMPTY_EXPRESSION);
		Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		logger.info("test");
		
		this.assertInterpretationEquals("(log \"test-2\")", Expression.EMPTY_EXPRESSION);
	}
	
	@Test
	@DisplayName("Test deep inference")
	void testDeepInference() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		String code = "(define build-list-native-aux\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f) (Int:Native i))\n" + 
				"                    (if (< i n)\n" + 
				"                        (construct List Native (f i) (build-list-native-aux n f (+ i 1)))\n" + 
				"                        (construct List Native)))))\n" + 
				"        (define build-list-native-t\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f))\n" + 
				"                    (build-list-native-aux n f 0))))\n" + 
				"        (build-list-native-t\n" + 
				"            1 (lambda ((Int:Native x)) (build-list-native-t x (lambda ((Int:Native y)) y))))";
		
		List<Expression> exprs = this.parseString_multipleExpression(code);
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
		
		this.assertReflexivity(get);
		this.assertDifference(get, get2);
		this.assertDifference(get, Expression.EMPTY_EXPRESSION);
		
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		assertAll(() -> {
			get.toString();
			get.toClojureCode(env, typeEnv);
			get.hashCode();
			get.infer(env, typeEnv);
		});
		
		this.assertInterpretationEquals(get, new LitInteger(42), env, typeEnv);
		this.assertInterpretationEquals(get2, new LitString("foo"), env, typeEnv);
		this.assertInterpretationEquals(get3, new LitInteger(42), env, typeEnv);
	}
	
	@Test
	@DisplayName("Test Loop Recur")
	void testLoopRecur() throws Exception {
		Expression e = this.parseString("(loop ((x 1)) (if (= x 2) x (recur (+ x 1))))")
						.get(0);
		
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		Pair<Type, Substitution> p = e.infer(env, typeEnv);
		this.assertInference(p, TypeAtom.TypeIntNative, e);
		
		this.assertInterpretationEquals(e, new LitInteger(2), env, typeEnv);
		
		//Testing side effects
		this.assertInterpretationEquals(
				"(loop ((x 1) (a (construct List JavaArray))) (if (= x 2) a (let ((z (java-array-list-add-to-end a x))) (recur (+ x 1) a))))",
				new LitComposite(
						new LitInteropObject(
								new ArrayList<Expression>(Arrays.asList(new LitInteger(1)))),
						JavaArrayList.TypeListJavaArray));
		
		//Testing nested loops
		this.assertInterpretationEquals(
				"(loop ((x 0) (s \"\")) (if (= x 3) s (recur (+ x 1) (loop ((y 0) (z s)) (if (= y 2) z (recur (+ y 1) (concat z \"a\")))))))",
				new LitString("aaaaaa"));
	}
	
	@Test
	@DisplayName("Test let")
	void testLet() throws Exception {
//		this.testInterpretString(
//				"(define screw-inference\r\n"
//				+ "	(extended-lambda\r\n"
//				+ "		((Int i))\r\n"
//				+ "		((Int:Native) \"foo\")\r\n"
//				+ "		((Int:String) \"bar\")))\r\n"
//				+ "\r\n"
//				+ "(define selection-fail\r\n"
//				+ "        (lambda ((List:Native impls) (List:Native args))\r\n"
//				+ "            (let ((i (get-list-native args 0)))\r\n"
//				+ "                 (if (equals? \"foo\" (screw-inference i))\r\n"
//				+ "                     (head-list-native (filter-list-native \r\n"
//				+ "						impls \r\n"
//				+ "						(lambda (x) (instance-of-representation x ((Int:Native) #> String:Native)))))\r\n"
//				+ "                     (head-list-native (filter-list-native \r\n"
//				+ "						impls \r\n"
//				+ "						(lambda (x) (instance-of-representation x ((Int:String) #> String:Native)))))))))\r\n"
//				+ "\r\n"
//				+ "(define let-issue-test\r\n"
//				+ "	(extended-lambda-selection\r\n"
//				+ "		((Int i))\r\n"
//				+ "		selection-fail\r\n"
//				+ "		((Int:Native) \"Int:Native\")\r\n"
//				+ "		((Int:String) \"Int:String\")))\n"
//				+ "(let-issue-test 42)",
//				new LitString("Int:Native"));
	}
	
	@Test
	@DisplayName("let-type inference")
	void letTypeInference() throws AppendableException {
		String code = "(define foo (let-type (A) (lambda ((A a)) a)))" + "(tuple (foo 42) (foo \"bar\"))";
		List<Expression> l = this.parseString_multipleExpression(code);
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		//Pair<Type, Substitution> p1 = l.get(0).infer(env, typeEnv);
		l.get(0).interpret(env, typeEnv);
		Pair<Type, Substitution> p2 = l.get(1).infer(env, typeEnv);

		this.assertInference(p2, new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative),
				l.get(1));
	}
	
	@Test
	@DisplayName("test extend")
	void extendTest() throws AppendableException {
		Tuple elambda_args = new Tuple(new Symbol("x"));
		
		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(
				Arrays.asList(
						new Lambda(
								elambda_args,
								new TypeTuple(TypeAtom.TypeIntNative),
								new LitString("foo"))));
		
		Lambda implementation = new Lambda(
										elambda_args,
										new TypeTuple(TypeAtom.TypeIntString),
										new LitString("bar"));
		
		Extend extend = new Extend(elambda, implementation);
		
		assertReflexivity(extend);
		assertDifference(extend, 
				new Extend(Expression.EMPTY_EXPRESSION, implementation));
		assertDifference(extend,
				new Extend(elambda, Expression.EMPTY_EXPRESSION));
		
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		Pair<Type, Substitution> p = extend.infer(env, typeEnv);
		assertInference(p, 
				RepresentationOr.makeRepresentationOr(
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeStringNative),
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntString), TypeAtom.TypeStringNative)), 
				extend);
		
		Map<Function, Expression> m = new TreeMap<Function, Expression>();
		
		Lambda impl = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("foo"));
		m.put((Function) impl.interpret(env, typeEnv),
			  new Function(new TypeTuple(TypeAtom.TypeInt),
					  elambda_args,
					  new AbstractionApplication(
							  Operators.ConversionCost,
							  new Tuple(impl, elambda_args)),
					  env));
		
		impl = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeIntString),
				new LitString("bar"));
		m.put((Function) impl.interpret(env, typeEnv),
			  new Function(new TypeTuple(TypeAtom.TypeInt),
					  elambda_args,
					  new AbstractionApplication(
							  Operators.ConversionCost,
							  new Tuple(impl, elambda_args)),
					  env));
		
		assertInterpretationEquals(
				extend, 
				ExtendedFunction.makeExtendedFunction(
						m,
						env), 
				env, 
				typeEnv);
		
		
		Lambda costLambda = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeInt),
				new LitInteger(0));
		Extend extendWithCost = new Extend(elambda, implementation, costLambda);
		
		assertReflexivity(extendWithCost);
		assertDifference(extend, extendWithCost);
		
		p = extendWithCost.infer(env, typeEnv);
		assertInference(p, 
				RepresentationOr.makeRepresentationOr(
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeStringNative),
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntString), TypeAtom.TypeStringNative)), 
				extendWithCost);
		
		
		Map<Function, Expression> expectedImpls = new TreeMap<Function, Expression>();
		impl = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeIntNative),
				new LitString("foo"));
		expectedImpls.put(
				(Function) impl.interpret(env, typeEnv),
				new Function(new TypeTuple(TypeAtom.TypeInt),
						elambda_args,
						new AbstractionApplication(Operators.ConversionCost,
								  new Tuple(impl, elambda_args)),
						env));
		
		impl = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeIntString),
				new LitString("bar"));
		expectedImpls.put(
				(Function) impl.interpret(env, typeEnv),
				new Function(new TypeTuple(TypeAtom.TypeInt),
						elambda_args,
						new LitInteger(0),
						env));
		
		assertInterpretationEquals(
				extendWithCost,
				ExtendedFunction.makeExtendedFunction(expectedImpls, env),
				env,
				typeEnv);
		
		//Test if cross-used type variables inferes correctly (substitution merge)
		Expression f = parseString(
				"(let-type (A) (extend (extend (extended-lambda (A)) (lambda ((A x)) (floor x))) (lambda ((A x)) (+ x 1))))")
				.get(0);
		assertThrows(TypeSetDoesNotUnifyException.class, () -> f.infer(env, typeEnv));
	}
	
	@Test
	@DisplayName("Documentation generator test")
	void testDocumentationGeneration() throws Exception {
		LangbaseDocumentationGenerator generator = new LangbaseDocumentationGenerator();
		Map<Path, String> doc = generator.generate(Arrays.asList(
				Operators.class,
				ConversionOperators.class,
				JavaArrayList.class,
				JavaLinkedList.class,
				ListNative.class,
				ConstructorOperators.class));
		assertNotNull(doc);
	}
}
