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
import velka.core.application.DefineSymbol;
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
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
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
import velka.util.CostAggregation;
import velka.util.NameGenerator;
import velka.util.Pair;

class TestInterpretation extends VelkaTest{

	@Test
	@DisplayName("Test String Literal")
	void testLitString() throws AppendableException {
		LitString litString = new LitString("test");
		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			litString.toString();
			litString.hashCode();
			litString.toClojureCode(env);
		});

		this.assertReflexivity(litString);
		this.assertDifference(litString, new LitString(" "));
		this.assertDifference(litString, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litString, litString, env);

		Pair<Type, Substitution> p = litString.infer(env);
		this.assertInference(p, TypeAtom.TypeStringNative, litString, true);
	}

	@Test
	@DisplayName("Test Integer Literal")
	public void testLitInteger() throws AppendableException {
		LitInteger litInteger = new LitInteger(128);
		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			litInteger.toString();
			litInteger.hashCode();
			litInteger.toClojureCode(env);
		});

		this.assertReflexivity(litInteger);
		this.assertDifference(litInteger, new LitInteger(0));
		this.assertDifference(litInteger, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litInteger, litInteger, env);

		Pair<Type, Substitution> p = litInteger.infer(env);
		this.assertInference(p, TypeAtom.TypeIntNative, litInteger, true);
	}

	@Test
	@DisplayName("Test Double Literal")
	public void testLitDouble() throws AppendableException {
		LitDouble litDouble = new LitDouble(3.14);
		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			litDouble.toString();
			litDouble.hashCode();
			litDouble.toClojureCode(env);
		});

		this.assertReflexivity(litDouble);
		this.assertDifference(litDouble, new LitDouble(0));
		this.assertDifference(litDouble, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(litDouble, litDouble, env);

		Pair<Type, Substitution> p = litDouble.infer(env);
		this.assertInference(p, TypeAtom.TypeDoubleNative, litDouble, true);
	}

	@Test
	@DisplayName("Test Boolean Literal")
	public void testLitBoolean() throws AppendableException {
		this.assertReflexivity(LitBoolean.TRUE);
		this.assertDifference(LitBoolean.TRUE, LitBoolean.FALSE);
		this.assertDifference(LitBoolean.FALSE, LitBoolean.TRUE);
		this.assertDifference(LitBoolean.TRUE, Expression.EMPTY_EXPRESSION);

		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretationEquals(LitBoolean.TRUE, LitBoolean.TRUE, env);

		assertAll(() -> {
			LitBoolean.TRUE.toString();
			LitBoolean.TRUE.toClojureCode(env);
		});

		Pair<Type, Substitution> p = LitBoolean.TRUE.infer(env);
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

		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretationEquals(enumValue1, enumValue1, env);

		Pair<Type, Substitution> p = enumValue1.infer(env);
		this.assertInference(p, type, enumValue1, true);

		assertAll(() -> {
			enumValue1.toString();
			// Not implemented yet
			// enumValue1.toClojureCode(env);
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

		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInterpretationEquals(composite1, composite1, env);

		Pair<Type, Substitution> p = composite1.infer(env);
		this.assertInference(p, type, composite1);

		assertAll(() -> {
			composite1.toString();
			composite1.toClojureCode(env);
		});
	}

	@Test
	@DisplayName("Test Type Holder")
	public void testTypeHolder() throws AppendableException {
		TypeHolder typeHolder = new TypeHolder(TypeTuple.EMPTY_TUPLE);
		Environment env = TopLevelEnvironment.instantiate();
		

		assertThrows(AppendableException.class, () -> typeHolder.interpret(env));
		assertThrows(AppendableException.class, () -> typeHolder.toClojureCode(env));
		assertAll(() -> {
			typeHolder.toString();
			typeHolder.hashCode();
		});

		this.assertReflexivity(typeHolder);
		this.assertDifference(typeHolder, new TypeHolder(TypeAtom.TypeIntNative));
		this.assertDifference(typeHolder, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = typeHolder.infer(env);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, typeHolder, true);

		TypeHolder placeholder = new TypeHolder(TypeAtom.TypeInt, new Symbol("x"));
		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitInteger(42));

		this.assertInterpretationEquals(placeholder, new LitInteger(42), bound);
		assertThrows(AppendableException.class, () -> placeholder.interpret(env));

		TypeHolder placeholder2 = new TypeHolder(TypeAtom.TypeInt, new Symbol("__q"));
		env.put(new Symbol("__q"), placeholder2);
		assertThrows(AppendableException.class, () -> placeholder2.interpret(env));

		Environment bound2 = Environment.create(bound);
		bound2.put(new Symbol("x"), placeholder);
		this.assertInterpretationEquals(placeholder, new LitInteger(42), bound2);
	}

	@Test
	@DisplayName("Test Symbol")
	public void testVariable() throws AppendableException {
		Symbol variable = new Symbol("x");
		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			variable.toString();
			variable.hashCode();
			variable.toClojureCode(env);
		});

		this.assertReflexivity(variable);
		this.assertDifference(variable, new Symbol("y"));
		this.assertDifference(variable, Expression.EMPTY_EXPRESSION);

		LitInteger value = new LitInteger(128);
		Environment bound = Environment.create(env);
		bound.put(variable, value);

		this.assertInterpretationEquals(variable, variable, env);
		Pair<Type, Substitution> p = variable.infer(env);
		this.assertInferenceClass(p, TypeVariable.class, variable);

		this.assertInterpretationEquals(variable, value, bound);
		p = variable.infer(bound);
		this.assertInference(p, TypeAtom.TypeIntNative, variable);

		final Environment fault = Environment.create(env);
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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env)
					throws AppendableException {
				return null;
			}
		});
		assertThrows(AppendableException.class, () -> variable.infer(fault));
	}

	@Test
	@DisplayName("Test Empty Expression")
	public void testEmptyExpression() throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		

		this.assertInference(Expression.EMPTY_EXPRESSION.infer(env), TypeTuple.EMPTY_TUPLE,
				Expression.EMPTY_EXPRESSION, true);
		this.assertInterpretationEquals(Expression.EMPTY_EXPRESSION, Expression.EMPTY_EXPRESSION, env);

		assertAll(() -> {
			Expression.EMPTY_EXPRESSION.toClojureCode(env);
		});
	}

	@Test
	@DisplayName("Test Tuple")
	public void testTuple() throws Exception {
		final Tuple tuple = new Tuple(Arrays.asList(new LitInteger(128), new Symbol("x"), LitBoolean.FALSE));
		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			tuple.get(0);
			tuple.size();
			tuple.toString();
			tuple.hashCode();
			tuple.toClojureCode(env);
			tuple.stream();
		});

		assertThrows(ArrayIndexOutOfBoundsException.class, () -> tuple.get(4));

		this.assertReflexivity(tuple);
		this.assertDifference(tuple, Tuple.EMPTY_TUPLE);
		this.assertDifference(tuple, Expression.EMPTY_EXPRESSION);
		this.assertDifference(tuple,
				new Tuple(Arrays.asList(tuple.get(0), new LitDouble(3.14), tuple.get(2))));

		this.assertInterpretationEquals(tuple, tuple, env);
		Pair<Type, Substitution> p = tuple.infer(env);
		assertTrue(p.first instanceof TypeTuple);
		assertEquals(((TypeTuple) p.first).get(0), TypeAtom.TypeIntNative);
		assertTrue(((TypeTuple) p.first).get(1) instanceof TypeVariable);
		assertEquals(((TypeTuple) p.first).get(2), TypeAtom.TypeBoolNative);

		Environment bound = Environment.create(env);
		bound.put(new Symbol("x"), new LitDouble(3.14));

		this.assertInterpretationEquals(tuple,
				new Tuple(Arrays.asList(new LitInteger(128), new LitDouble(3.14), LitBoolean.FALSE)), bound);
		p = tuple.infer(bound);
		this.assertInference(p,
				new TypeTuple(
						Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeDoubleNative, TypeAtom.TypeBoolNative)),
				tuple);

		assertThrows(AppendableException.class, () -> (new Tuple(Arrays.asList(new Expression() {

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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env)
					throws AppendableException {
				return null;
			}
		}))).infer(env));
		
		//Test if cross-used bound variables in tuples are infered correctly
		Expression e = parseString("(let-type (A) (lambda ((A x)) (tuple (+ x 0) (floor x))))")
				.get(0);
		
		assertThrows(SubstitutionsCannotBeMergedException.class, () -> e.infer(env));
		
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
		Environment env = TopLevelEnvironment.instantiate();
		

		assertThrows(UserException.class, () -> exception.interpret(env));

		assertAll(() -> {
			exception.toClojureCode(env);
			exception.toString();
			exception.hashCode();
		});

		this.assertReflexivity(exception);
		this.assertDifference(exception, new ExceptionExpr(new LitString("fail")));
		this.assertDifference(exception, Expression.EMPTY_EXPRESSION);

		Pair<Type, Substitution> p = exception.infer(env);
		this.assertInferenceClass(p, TypeVariable.class, exception);

		assertThrows(AppendableException.class, () -> new ExceptionExpr(new Expression() {

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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env)
					throws AppendableException {
				// TODO Auto-generated method stub
				return null;
			}
		}).infer(env));
	}

	@Test
	@DisplayName("Test Define Expression")
	void testDefExpression() throws AppendableException {
		DefineSymbol defExpression = new DefineSymbol(new Symbol("pi"), new LitDouble(Math.PI));
		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			defExpression.toString();
			defExpression.hashCode();
			defExpression.toClojureCode(top);
		});

		this.assertReflexivity(defExpression);
		this.assertDifference(defExpression, new DefineSymbol(new Symbol("e"), new LitDouble(Math.E)));
		this.assertDifference(defExpression, new DefineSymbol(new Symbol("pi"), new LitDouble(3.141521)));
		this.assertDifference(defExpression, Expression.EMPTY_EXPRESSION);

		Environment env = Environment.create(top);
		this.assertInterpretationEquals(defExpression, Expression.EMPTY_EXPRESSION, env);
		assertTrue(env.containsVariable(new Symbol("pi")));

		Pair<Type, Substitution> p = defExpression.infer(top);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, defExpression);
		//assertNotEquals(p.second, Substitution.EMPTY);
		//assertNotEquals(p.second.variableStream().findAny().get(), TypeAtom.TypeDoubleNative);

		DefineSymbol recursiveExpression = (DefineSymbol) this
				.parseString("(define fact (lambda (x) (if (= x 1) 1 (* x (fact (- x 1))))))").get(0);
		p = recursiveExpression.infer(top);
		this.assertInference(p, TypeTuple.EMPTY_TUPLE, recursiveExpression);
		assertNotEquals(p.second, Substitution.EMPTY);

		assertThrows(AppendableException.class, () -> new DefineSymbol(new Symbol("fail"), new Expression() {

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

			@Override
			protected Expression doConvert(Type from, Type to, Environment env)
					throws AppendableException {
				// TODO Auto-generated method stub
				return null;
			}
		}).infer(top));
	}

	@Test
	@DisplayName("Test Lambda")
	void testLambda() throws AppendableException {
		final Lambda lambda = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)), new Symbol("x"));
		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top);
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

		Expression e = lambda.interpret(top);
		assertTrue(e instanceof Function);

		Pair<Type, Substitution> p = lambda.infer(top);
		this.assertInferenceClass(p, TypeArrow.class, lambda);

		assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))")
						.get(0).infer(top));
	}

	@Test
	@DisplayName("Test Function")
	void testFunction() throws AppendableException {
		Environment top = TopLevelEnvironment.instantiate();
		

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

		this.assertInterpretationEquals(function, function, top);

		Pair<Type, Substitution> p = function.infer(top);
		this.assertInferenceClass(p, TypeArrow.class, function);

		assertThrows(AppendableException.class,
				() -> (new Function(new TypeTuple(Arrays.asList(new TypeVariable("x"))),
						new Tuple(Arrays.asList(Expression.EMPTY_EXPRESSION)), new Symbol("y"), top)).infer(top));
		assertThrows(TypesDoesNotUnifyException.class, () -> this
				.parseString("(lambda ((String x)) (+ x x))")
					.get(0).interpret(top).infer(top));
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

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top);
			lambda.interpret(top);
		});

		Pair<Type, Substitution> p = lambda.infer(top);

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
								new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), new Symbol("x"))))).infer(top));
	}

	@Test
	@DisplayName("Test Extended Function")
	public void testExtendedFunction() throws AppendableException {
		Environment top = TopLevelEnvironment.instantiate();
		

		Environment bound = Environment.create(top);
		bound.put(new Symbol("x"), new LitInteger(42));
		List<Function> implementations = Arrays.asList(
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound),
				new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
						new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), bound));

		ExtendedFunction function = Extend.makeExtendedFunction(implementations, bound);

		this.assertReflexivity(function);

		List<Function> tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.remove(0);
		this.assertDifference(function, Extend.makeExtendedFunction(tmpImpls, bound));

		tmpImpls = new LinkedList<Function>(implementations);
		tmpImpls.add(new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
				new Tuple(Arrays.asList(new Symbol("y"))), new Symbol("y"), top));
		this.assertDifference(function, Extend.makeExtendedFunction(tmpImpls, bound));

		this.assertDifference(function, Extend.makeExtendedFunction(implementations, top));

		this.assertDifference(function, Expression.EMPTY_EXPRESSION);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		Pair<Type, Substitution> p = function.infer(top);
		this.assertInference(p,
				RepresentationOr.makeRepresentationOr(Arrays.asList(
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman),
						new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeIntString))),
				function);

		assertThrows(AppendableException.class, () -> Extend
				.makeExtendedFunction(Arrays.asList(
						new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
								new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), top),
						new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
								new Tuple(Arrays.asList(new Symbol("x"))), new Symbol("x"), top)),
						bound)
				.infer(top));
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

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			application.toString();
			application.toClojureCode(top);
			application.hashCode();
		});

		assertThrows(InvalidArgumentsException.class,
				() -> new AbstractionApplication(new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("y"))),
						new TypeTuple(Arrays.asList(new TypeVariable("_x"), new TypeVariable("y"))), new Symbol("x")),
						new Tuple(Arrays.asList(new LitInteger(42)))).interpret(top));
		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(Expression.EMPTY_EXPRESSION, Tuple.EMPTY_TUPLE).interpret(top));

		this.assertInterpretationEquals(application, new LitInteger(42), top);
		Pair<Type, Substitution> p = application.infer(top);
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

		this.assertInterpretationEquals(lexicalClojureTest, new LitInteger(128), evaluation);
		p = lexicalClojureTest.infer(top);
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
				top);
		p = autoConRep.infer(top);
		this.assertInference(p, TypeAtom.TypeIntString, autoConRep);

		// Test elambda/efunction comparation
		ExtendedLambda elambda = ExtendedLambda.makeExtendedLambda(List.of(
				new Lambda(new Tuple(new Symbol("x")),
						new TypeTuple(TypeAtom.TypeIntString), new Symbol("x")),
				new Lambda(new Tuple(new Symbol("x")),
						new TypeTuple(TypeAtom.TypeIntRoman), new Symbol("x"))));
		
		AbstractionApplication useString = new AbstractionApplication(elambda, new Tuple(
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString)));
		
		this.assertInterpretationEquals(useString,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString), top);
		
		p = useString.infer(top);
		this.assertInference(p,
				RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda, new Tuple(
				Arrays.asList(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman))));
		this.assertInterpretationEquals(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman), top);
		p = useRoman.infer(top);
		this.assertInference(p,
				RepresentationOr.makeRepresentationOr(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useRoman);

		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(elambda, new Tuple(Arrays.asList(new LitString("fail")))).infer(top));

	}

	@Test
	@DisplayName("Test If")
	void testIfExpression() throws AppendableException {
		IfExpression ifExprT = new IfExpression(LitBoolean.TRUE, new LitInteger(42), new LitInteger(21));
		IfExpression ifExprF = new IfExpression(LitBoolean.FALSE, new LitInteger(21), new LitInteger(42));

		this.assertReflexivity(ifExprF);
		this.assertDifference(ifExprT, ifExprF);
		this.assertDifference(ifExprT, Expression.EMPTY_EXPRESSION);

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			ifExprT.toString();
			ifExprT.toClojureCode(top);
			ifExprT.hashCode();
		});

		this.assertInterpretationEquals(ifExprT, new LitInteger(42), top);
		this.assertInterpretationEquals(ifExprF, new LitInteger(42), top);

		Pair<Type, Substitution> p = ifExprT.infer(top);
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

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			andExpressionT.toString();
			andExpressionT.toClojureCode(top);
			andExpressionT.hashCode();
		});

		this.assertInterpretationEquals(andExpressionT, LitBoolean.TRUE, top);
		this.assertInterpretationEquals(andExpressionF, LitBoolean.FALSE, top);

		Pair<Type, Substitution> p = andExpressionT.infer(top);
		this.assertInference(p, TypeAtom.TypeBoolNative, andExpressionT);
	}

	@Test
	void testOrExpression() throws AppendableException {
		OrExpression orExpressionT = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.TRUE)));
		OrExpression orExpressionF = new OrExpression(new Tuple(Arrays.asList(LitBoolean.FALSE, LitBoolean.FALSE)));

		this.assertReflexivity(orExpressionT);
		this.assertDifference(orExpressionT, orExpressionF);
		this.assertDifference(orExpressionT, Expression.EMPTY_EXPRESSION);

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			orExpressionT.toString();
			orExpressionF.toClojureCode(top);
			orExpressionT.hashCode();
		});

		this.assertInterpretationEquals(orExpressionT, LitBoolean.TRUE, top);
		this.assertInterpretationEquals(orExpressionF, LitBoolean.FALSE, top);

		Pair<Type, Substitution> p = orExpressionT.infer(top);
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
//		this.assertOperator(Operators.CanUnifyRepresentations,
//				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
//				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyRepresentations,
//				new Tuple(
//						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
//				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyRepresentations,
//				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
//				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyRepresentations, new Tuple(
//				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
//				LitBoolean.FALSE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyTypes,
//				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative),
//						new TypeSymbol(new TypeVariable(NameGenerator.next())))),
//				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyTypes,
//				new Tuple(
//						Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntNative))),
//				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyTypes,
//				new Tuple(Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeIntRoman))),
//				LitBoolean.TRUE, TypeAtom.TypeBoolNative);
//		this.assertOperator(Operators.CanUnifyTypes, new Tuple(
//				Arrays.asList(new TypeSymbol(TypeAtom.TypeIntNative), new TypeSymbol(TypeAtom.TypeStringNative))),
//				LitBoolean.FALSE, TypeAtom.TypeBoolNative);

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
		this.assertOperator(Operators.ToStr, new Tuple(new LitInteger(42)), new LitString("42"), TypeAtom.TypeStringNative);
		
		File tempOut = File.createTempFile("velka_read_test", null);
        String content  = "hello world !!";       
        Files.writeString(tempOut.toPath(), content);
        
        this.assertOperator(
        		Operators.ReadFile, 
        		new Tuple(new LitString(tempOut.toPath().toString())), 
        		new LitString(content), 
        		TypeAtom.TypeStringNative);
        
        tempOut.delete();
        
        Environment env = TopLevelEnvironment.instantiate();
        
        
        this.assertOperator(Operators.StrSplit, 
        		new Tuple(new LitString("foo bar baz"), 
        		new LitString(" ")),
        		ListNative.of(new LitString("foo"), new LitString("bar"), new LitString("baz")).interpret(env), 
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
				new LitDouble(CostAggregation.instance().defaultConversionRank()),
				TypeAtom.TypeIntNative);
//		this.assertOperator(Operators.ConversionCost,
//				new Tuple(new Lambda(new Tuple(new Symbol("x")), new TypeTuple(TypeAtom.TypeIntNative), new LitString("foo")),
//						new Tuple(new LitInteger(42))),
//				new LitDouble(1d),
//				TypeAtom.TypeIntNative);
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

		Environment top = TopLevelEnvironment.instantiate();
		

		this.assertInterpretationEquals(e, new LitInteger(84), top);
	}

	@Test
	@DisplayName("Test Environment")
	void testEnvironment() throws AppendableException {
		Environment top = TopLevelEnvironment.instantiate();

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
	@DisplayName("Test Define Conversion")
	void testDefConversionExpression() throws AppendableException {
		TypeName name = new TypeName("__defConversionTest");
		TypeAtom typeAtomNative = new TypeAtom(name, TypeRepresentation.NATIVE);
		TypeAtom typeAtomWildcard = new TypeAtom(name, TypeRepresentation.WILDCARD);
		DefineConversion defCon = new DefineConversion(typeAtomNative, typeAtomWildcard,
				new Tuple(Arrays.asList(new Symbol("x"))),
				new LitComposite(new Tuple(Arrays.asList(new Symbol("x"))), typeAtomWildcard));

		Environment top = TopLevelEnvironment.instantiate();

		assertAll(() -> {
			defCon.toString();
			defCon.hashCode();
			defCon.toClojureCode(top);
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

		this.assertInterpretationEquals(defCon, Expression.EMPTY_EXPRESSION, top);

		Pair<Type, Substitution> p = defCon.infer(top);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top).first, defCon);
		
		
		var parsed = this.parseString("(conversion Type:Native Type:Other (x) x (lambda ((Type:Native x)) 42))");
		var e = parsed.get(0);
		
		var top2 = TopLevelEnvironment.instantiate();
		
		e.interpret(top2);
	}

	@Test
	@DisplayName("Test Define Constructor")
	void testDefinceConstructorExpression() throws AppendableException {
		TypeName name = new TypeName("__defConstructorTest");
		TypeAtom type = new TypeAtom(name, TypeRepresentation.NATIVE);
		Lambda constructor = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
				new LitComposite(Expression.EMPTY_EXPRESSION, type));

		DefineConstructor defCon = new DefineConstructor(type, constructor);

		Environment top = TopLevelEnvironment.instantiate();

		assertAll(() -> {
			defCon.toString();
			defCon.hashCode();
			defCon.toClojureCode(top);
		});

		this.assertReflexivity(defCon);
		this.assertDifference(defCon, new DefineConstructor(TypeAtom.TypeIntNative, constructor));
		this.assertDifference(defCon, new DefineConstructor(type, Lambda.identity));

		this.assertInterpretationEquals(defCon, Expression.EMPTY_EXPRESSION, top);

		Pair<Type, Substitution> p = defCon.infer(top);
		this.assertInference(p, Expression.EMPTY_EXPRESSION.infer(top).first, defCon);
	}

	@Test
	@DisplayName("Test Construct")
	void testConstruct() throws AppendableException {
		Construct construct = new Construct(TypeAtom.TypeIntRoman, new Tuple(new LitString("XLII")));

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			construct.toString();
			construct.hashCode();
			construct.toClojureCode(top);
		});

		this.assertReflexivity(construct);
		this.assertDifference(construct,
				new Construct(TypeAtom.TypeIntString, new Tuple(Arrays.asList(new LitString("XLII")))));
		this.assertDifference(construct,
				new Construct(TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(new LitString("XXI")))));
		this.assertDifference(construct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(construct, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman),
				top);

		Pair<Type, Substitution> p = construct.infer(top);
		this.assertInference(p, TypeAtom.TypeIntRoman, construct);
	}

	@Test
	@DisplayName("Test Deconstruct")
	void testDeconstruct() throws AppendableException {
		Deconstruct deconstruct = new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
				TypeAtom.TypeStringNative);

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			deconstruct.toString();
			deconstruct.hashCode();
			deconstruct.toClojureCode(top);
		});

		this.assertReflexivity(deconstruct);
		this.assertDifference(deconstruct, new Deconstruct(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeStringNative));
		this.assertDifference(deconstruct,
				new Deconstruct(new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		this.assertDifference(deconstruct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(deconstruct, new LitString("42"), top);

		Pair<Type, Substitution> p = deconstruct.infer(top);
		this.assertInference(p, TypeAtom.TypeStringNative, deconstruct);
	}

	@Test
	@DisplayName("Test Can Deconstruct As")
	void testCanDeconstructAs() throws AppendableException {
		CanDeconstructAs canDeconstruct = new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeStringNative);
		CanDeconstructAs cannotDeconstrut = new CanDeconstructAs(
				new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman), TypeAtom.TypeIntNative);

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			canDeconstruct.toString();
			canDeconstruct.hashCode();
			canDeconstruct.toClojureCode(top);
		});

		this.assertReflexivity(canDeconstruct);
		this.assertDifference(canDeconstruct, new CanDeconstructAs(
				new LitComposite(new LitString("42"), TypeAtom.TypeIntString), TypeAtom.TypeIntNative));
		this.assertDifference(canDeconstruct,
				new CanDeconstructAs(new LitDouble(3.14), TypeAtom.TypeStringNative));
		this.assertDifference(canDeconstruct, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(canDeconstruct, LitBoolean.TRUE, top);
		this.assertInterpretationEquals(cannotDeconstrut, LitBoolean.FALSE, top);

		Pair<Type, Substitution> p = canDeconstruct.infer(top);
		this.assertInference(p, TypeAtom.TypeBoolNative, canDeconstruct);
	}

	@Test
	@DisplayName("Test Convert")
	void testConvert() throws AppendableException {
		Convert convert = new Convert(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, new LitInteger(42));

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			convert.hashCode();
			convert.toString();
			convert.toClojureCode(top);
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
				top);

		Pair<Type, Substitution> p = convert.infer(top);
		this.assertInference(p, TypeAtom.TypeIntRoman, convert);
	}

	@Test
	@DisplayName("Test List Native")
	void testListNative() throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		
		
		assertEquals(
					new LitInteropObject(new LinkedList<Expression>(
								Arrays.asList(new LitInteger(42), new LitInteger(21), new LitInteger(2))),
						TypeAtom.TypeListNative),
				ListNative.of(new LitInteger(42), new LitInteger(21), new LitInteger(2)).interpret(env));

		this.assertInterpretedStringEquals("(construct List:Native)",
				ListNative.EMPTY_LIST_NATIVE, env);
		this.assertInterpretedStringEquals("(construct List:Native 42 (construct List:Native))",
				ListNative.of(new LitInteger(42)).interpret(env), env);

		this.assertInterpretedStringEquals("(is-list-native-empty (construct List:Native))", LitBoolean.TRUE, env);
		this.assertInterpretedStringEquals(
				"(is-list-native-empty (construct List:Native 42 (construct List:Native)))", LitBoolean.FALSE, env);

		this.assertInterpretedStringEquals("(head-list-native (construct List:Native 42 (construct List:Native)))",
				new LitInteger(42), env);
		assertThrows(UserException.class,
				() -> this.assertInterpretedStringEquals("(head-list-native (construct List:Native))",
						Expression.EMPTY_EXPRESSION, env));

		this.assertInterpretedStringEquals("(tail-list-native (construct List:Native 42 (construct List:Native)))",
				ListNative.EMPTY_LIST_NATIVE, env);
		assertThrows(UserException.class,
				() -> this.assertInterpretedStringEquals("(tail-list-native (construct List:Native))",
						Expression.EMPTY_EXPRESSION, env));

		this.assertInterpretedStringEquals(
				"(map-list-native (lambda (x) (+ x 1)) (construct List:Native 42 (construct List:Native)))",
				ListNative.of(new LitInteger(43)).interpret(env), env);

		this.assertInterpretedStringEquals(
				"(map2-list-native + (construct List:Native 21 (construct List:Native 21 (construct List:Native))) (construct List:Native 21 (construct List:Native 21 (construct List:Native))))",
				ListNative.of(new LitInteger(42), new LitInteger(42)).interpret(env),
				env);

		this.assertInterpretedStringEquals(
				"(foldl-list-native + 0 (construct List:Native 1 (construct List:Native 2 (construct List:Native))))",
				new LitInteger(3), env);
		
		this.assertInterpretedStringEquals(
				"(" + ListNative.addToEndSymbol_out + " (construct List:Native 21 (construct List:Native)) 42)",
				ListNative.of(new LitInteger(21), new LitInteger(42)).interpret(env), env);
		
		ArrayList<Expression> al = new ArrayList<Expression>();
		al.add(new LitInteger(42));
		al.add(new LitInteger(21));
		this.assertInterpretedStringEquals(
				"(convert List:Native List:JavaArray (construct List:Native 42 (construct List:Native 21 (construct List:Native))))",
				new LitInteropObject(al, TypeAtom.TypeListJavaArray), env);
		
		LinkedList<Expression> ll = new LinkedList<Expression>();
		ll.add(new LitInteger(42));
		ll.add(new LitInteger(21));
		this.assertInterpretedStringEquals(
				"(convert List:Native List:JavaLinked (construct List:Native 42 (construct List:Native 21 (construct List:Native))))",
				new LitInteropObject(ll, TypeAtom.TypeListJavaLinked), env);
				
		this.assertInterpretedStringEquals("(contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 42)", LitBoolean.TRUE, env);
		this.assertInterpretedStringEquals("(contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 84)", LitBoolean.FALSE, env);
		
		this.assertInterpretedStringEquals("(filter-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
				ListNative.of(LitBoolean.TRUE).interpret(env), env);
		this.assertInterpretedStringEquals("(get-list-native (construct List:Native 42 (construct List:Native)) 0)", new LitInteger(42), env);
		this.assertInterpretedStringEquals("(build-list-native 2 (lambda (x) x))", ListNative.of(new LitInteger(0), new LitInteger(1)).interpret(env), env);
		
		this.assertInterpretedStringEquals("(remove-list-native (build-list-native 2 (lambda (x) x)) 1)", ListNative.of(new LitInteger(0)).interpret(env), env);
		this.assertInterpretedStringEquals("(size-list-native (build-list-native 42 (lambda (x) x)))", new LitInteger(42), env);
		this.assertInterpretedStringEquals("(append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42)))", 
				ListNative.of(new LitInteger(21), new LitInteger(42)).interpret(env), env);
		
		this.assertInterpretedStringEquals("(reverse-list-native (build-list-native 3 (lambda (x) x)))",
				ListNative.of(new LitInteger(2), new LitInteger(1), new LitInteger(0))
						.interpret(env),
				env);
		
		this.assertInterpretedStringEquals("(everyp-list-native (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))",
				LitBoolean.TRUE, env);
		this.assertInterpretedStringEquals("(everyp-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
				LitBoolean.FALSE, env);
	}

	@Test
	@DisplayName("Test instance-of")
	void testInstanceOf() throws AppendableException {
		InstanceOf iof = new InstanceOf(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			iof.toString();
			iof.hashCode();
			iof.toClojureCode(env);
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

		this.assertInterpretationEquals(iof, LitBoolean.TRUE, env);
		this.assertInterpretationEquals(iof_isStrInt, LitBoolean.FALSE, env);
		this.assertInterpretationEquals(iof_typevar, LitBoolean.TRUE, env);
		this.assertInterpretationEquals(iof_otherRepre, LitBoolean.TRUE, env);

		Pair<Type, Substitution> p = iof.infer(env);
		this.assertInference(p, TypeAtom.TypeBoolNative, iof);
	}

	@Test
	@DisplayName("Test instance-of-representation")
	void testInstanceOfRepresentation() throws AppendableException {
		InstanceOfRepresentation iofr = new InstanceOfRepresentation(new LitInteger(42), TypeAtom.TypeIntNative);

		Environment env = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			iofr.toString();
			iofr.hashCode();
			iofr.toClojureCode(env);
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

		this.assertInterpretationEquals(iofr, LitBoolean.TRUE, env);
		this.assertInterpretationEquals(iofr_isStrInt, LitBoolean.FALSE, env);
		this.assertInterpretationEquals(iofr_typevar, LitBoolean.TRUE, env);
		this.assertInterpretationEquals(iofr_otherRepre, LitBoolean.FALSE, env);

		Pair<Type, Substitution> p = iofr.infer(env);
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
		
		Environment env = TopLevelEnvironment.instantiate();
		
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
				env);
		
		Lambda costFunction = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeInt),
				new LitDouble(1));

		Map<Lambda, Expression> m = new TreeMap<Lambda, Expression>();
		m.put(impl1, Lambda.constFun(elambda_args.size(), new LitDouble(CostAggregation.instance().defaultImplementationRank())));
		m.put(impl2, costFunction);
		m.put(impl3, Lambda.constFun(elambda_args.size(), new LitDouble(CostAggregation.instance().defaultImplementationRank())));
		
		ExtendedLambda elambda_customCostFunction = 
				ExtendedLambda.makeExtendedLambda(m);

		AbstractionApplication app_customCostFunction = 
				new AbstractionApplication(
						elambda_customCostFunction, 
						args);
		this.assertInterpretationEquals(
				app_customCostFunction, 
				new LitString("Int String"), 
				env);
	}
	
	@Test
	@DisplayName("Test Java Bit Set")
	void testJavaBitSet() throws AppendableException {
		BitSet bs = new BitSet();
		var bitSet = 
				new LitInteropObject(bs,
				TypeAtom.TypeSetBitSet);
		
		assertInterpretationEquals(
				"(construct Set:BitSet)",
				bitSet);
		
		BitSet nbitsBs = new BitSet(2048);
		var nBitsBitSet = 
				new LitInteropObject(nbitsBs,
				TypeAtom.TypeSetBitSet);
		
		assertInterpretationEquals(
				"(construct Set:BitSet 2048)",
				nBitsBitSet);
		
		bs.set(3);
		assertInterpretationEquals(
					"(define s (construct Set:BitSet))\n"
				+ 	"(" + JavaBitSet.setSymbol_out.toString() + " s 3)",
				bitSet);
		assertInterpretationEquals(
					"(define s (construct Set:BitSet))\n"
				+	"(" + JavaBitSet.setValueSymbol_out.toString() + " s 3 #t)",
				bitSet);
		bs.set(2, 5);
		assertInterpretationEquals(
					"(define s (construct Set:BitSet))\n"
				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s 2 5)",
				bitSet);
		assertInterpretationEquals(
					"(define s (construct Set:BitSet))\n"
				+	"(" + JavaBitSet.setIntervalValueSymbol_out.toString() + " s 2 5 #t)",
				bitSet);
		
		nbitsBs.set(4, 7);
		nbitsBs.and(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.andSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.andNot(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.andNotSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		assertInterpretationEquals(
					"(define s2 (construct Set:BitSet))\n"
				+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
				+	"(" + JavaBitSet.cardinalitySymbol_out.toString() + " s2)",
				new LitInteger(bs.cardinality()));
		
		nbitsBs.clear();
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.clearSymbol_out.toString() + " s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.clear(5);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.clearBitIndexSymbol_out.toString() + " s2 5)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.clear(5, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.clearIntervalSymbol_out.toString() + " s2 5 7)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.cloneSymbol_out.toString() + " s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.equalsSymbol_out.toString() + " s1 s2)",
			LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.flip(2);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.flipSymbol_out.toString() + " s2 2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.flip(2, 5);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.flipIntervalSymbol_out.toString() + " s2 2 5)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.getSymbol_out.toString() + " s2 5)",
			nbitsBs.get(5) ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.getIntervalSymbol_out.toString() + " s2 5 7)",
			new LitInteropObject(nbitsBs.get(5, 7), TypeAtom.TypeSetBitSet));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.intersectsSymbol_out.toString() + " s1 s2)",
			nbitsBs.intersects(bs) ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.isEmptySymbol_out.toString() + " s2)",
			nbitsBs.isEmpty() ? LitBoolean.TRUE : LitBoolean.FALSE);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.lengthSymbol_out.toString() + " s2)",
			new LitInteger(nbitsBs.length()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.nextClearBitSymbol_out.toString() + " s2 5)",
			new LitInteger(nbitsBs.nextClearBit(5)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.nextSetBitSymbol_out.toString() + " s2 0)",
			new LitInteger(nbitsBs.nextSetBit(0)));
		
		nbitsBs.set(4, 7);
		nbitsBs.or(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.orSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.previousClearBitSymbol_out.toString() + " s2 5)",
			new LitInteger(nbitsBs.previousClearBit(5)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.previousSetBitSymbol_out.toString() + " s2 9)",
			new LitInteger(nbitsBs.previousSetBit(9)));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 2 5)\n"
			+	"(" + JavaBitSet.sizeSymbol_out.toString() + " s1)",
			new LitInteger(bs.size()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		assertInterpretationEquals(
				"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n"
			+	"(" + JavaBitSet.strSymbol_out.toString() + " s2)",
			new LitString(nbitsBs.toString()));
		
		nbitsBs.clear();
		nbitsBs.set(4, 7);
		nbitsBs.xor(bs);
		assertInterpretationEquals(
				"(define s1 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n"
			+	"(define s2 (construct Set:BitSet))\n"
			+	"(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n"
			+	"(" + JavaBitSet.xorSymbol_out.toString() + " s1 s2)",
			nBitsBitSet);
	}
	
	@Test
	@DisplayName("Test logging")
	void testLogging() throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		
		
		assertNotEquals(new LitInteger(0),
				this.parseString("(timestamp)")
					.get(0).interpret(env));
		
		this.assertInterpretationEquals("(init-logger \"test-log\")", Expression.EMPTY_EXPRESSION);
		Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		logger.info("test");
		
		this.assertInterpretationEquals("(log \"test-2\")", Expression.EMPTY_EXPRESSION);
	}
	
	@Test
	@DisplayName("Test deep inference")
	void testDeepInference() throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		
		
		String code = "(define build-list-native-aux\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f) (Int:Native i))\n" + 
				"                    (if (< i n)\n" + 
				"                        (construct List:Native (f i) (build-list-native-aux n f (+ i 1)))\n" + 
				"                        (construct List:Native)))))\n" + 
				"        (define build-list-native-t\n" + 
				"            (let-type (A)\n" + 
				"                (lambda ((Int:Native n) (((Int:Native) #> A) f))\n" + 
				"                    (build-list-native-aux n f 0))))\n" + 
				"        (build-list-native-t\n" + 
				"            1 (lambda ((Int:Native x)) (build-list-native-t x (lambda ((Int:Native y)) y))))";
		
		List<Expression> exprs = this.parseString_multipleExpression(code);
		for(Expression e : exprs) {
			e.infer(env);
			e.interpret(env);
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
		
		Environment env = TopLevelEnvironment.instantiate();
		
		
		assertAll(() -> {
			get.toString();
			get.toClojureCode(env);
			get.hashCode();
			get.infer(env);
		});
		
		this.assertInterpretationEquals(get, new LitInteger(42), env);
		this.assertInterpretationEquals(get2, new LitString("foo"), env);
		this.assertInterpretationEquals(get3, new LitInteger(42), env);
	}
	
	@Test
	@DisplayName("Test Loop Recur")
	void testLoopRecur() throws Exception {
		Expression e = this.parseString("(loop ((x 1)) (if (= x 2) x (recur (+ x 1))))")
						.get(0);
		
		Environment env = TopLevelEnvironment.instantiate();
		
		
		Pair<Type, Substitution> p = e.infer(env);
		this.assertInference(p, TypeAtom.TypeIntNative, e);
		
		this.assertInterpretationEquals(e, new LitInteger(2), env);
		
		//Testing side effects
		this.assertInterpretationEquals(
				"(loop ((x 1) (a (construct List:JavaArray))) (if (= x 2) a (let ((z (java-array-list-add-to-end a x))) (recur (+ x 1) a))))",
				
						new LitInteropObject(
								new ArrayList<Expression>(Arrays.asList(new LitInteger(1))),
						TypeAtom.TypeListJavaArray));
		
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
		Environment env = TopLevelEnvironment.instantiate();
		

		//Pair<Type, Substitution> p1 = l.get(0).infer(env);
		l.get(0).interpret(env);
		Pair<Type, Substitution> p2 = l.get(1).infer(env);

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
		
		Environment env = TopLevelEnvironment.instantiate();
		
		
		Pair<Type, Substitution> p = extend.infer(env);
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
		m.put((Function) impl.interpret(env),
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
		m.put((Function) impl.interpret(env),
			  new Function(new TypeTuple(TypeAtom.TypeInt),
					  elambda_args,
					  new AbstractionApplication(
							  Operators.ConversionCost,
							  new Tuple(impl, elambda_args)),
					  env));
		
//		assertInterpretationEquals(
//				extend, 
//				ExtendedFunction.makeExtendedFunction(
//						m,
//						env), 
//				env, 
//				typeEnv);
		
		
		Lambda costLambda = new Lambda(
				elambda_args,
				new TypeTuple(TypeAtom.TypeInt),
				new LitDouble(0));
		Extend extendWithCost = new Extend(elambda, implementation, costLambda);
		
		assertReflexivity(extendWithCost);
		assertDifference(extend, extendWithCost);
		
		p = extendWithCost.infer(env);
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
				(Function) impl.interpret(env),
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
				(Function) impl.interpret(env),
				new Function(new TypeTuple(TypeAtom.TypeInt),
						elambda_args,
						new LitDouble(0),
						env));
		
//		assertInterpretationEquals(
//				extendWithCost,
//				ExtendedFunction.makeExtendedFunction(expectedImpls, env),
//				env,
//				typeEnv);
		
		//Test if cross-used type variables inferes correctly (substitution merge)
		Expression f = parseString(
				"(let-type (A) (extend (extend (extended-lambda (A)) (lambda ((A x)) (floor x))) (lambda ((A x)) (+ x 1))))")
				.get(0);
		assertThrows(TypeSetDoesNotUnifyException.class, () -> f.infer(env));
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
