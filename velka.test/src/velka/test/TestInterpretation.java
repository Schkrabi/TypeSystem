package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.logging.LogManager;
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
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.java.runtime.TypedObject;
import velka.java.runtime.VelkaTuple;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.RankAggregation;
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
		
		this.assertJExprEquals("foo", (new LitString("foo")));
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
		
		this.assertJExprEquals(Integer.valueOf(42), (new LitInteger(42)));
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
		
		this.assertJExprEquals(Double.valueOf(42.0d), (new LitDouble(42d)));
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
		
		this.assertJExprEquals(Boolean.TRUE, LitBoolean.TRUE);
		this.assertJExprEquals(Boolean.FALSE, LitBoolean.FALSE);
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
		
		this.assertJExprEquals(new TypedObject(Integer.valueOf(42), type), (new LitComposite(new LitInteger(42), type)));
		
//		this.assertJExprsEquals(new TypedObject("42", TypeAtom.TypeIntString), 
//				List.of(ConstructorOperators.IntStringConstructor,
//						JavaTypeSystem.codeInstance().invoke("construct")
//							.arg(TypeUtil.instance().type2java(TypeAtom.TypeIntString))
//							.arg(TypeUtil.instance().type2java(new TypeTuple(TypeAtom.TypeStringNative)))
//							.arg((new Tuple(new LitString("42"))))
//							.arg(JExpr._null())));
		
		this.assertJExprsEquals(new TypedObject("42", TypeAtom.TypeIntString), 
				List.of(
						(new Construct(TypeAtom.TypeIntString, new Tuple(new LitString("42"))))));
		
		this.assertJExprsEquals(Integer.valueOf(42), 
				List.of(
						(new Construct(TypeAtom.TypeIntNative, new Tuple(new LitInteger(42))))));
		
		this.assertJExprsEquals(new TypedObject("XLII", TypeAtom.TypeIntRoman), 
				List.of(
						(new Construct(TypeAtom.TypeIntRoman, new Tuple(new LitString("XLII"))))));
		
		this.assertJExprsEquals("foo", 
				List.of(
						(new Construct(TypeAtom.TypeStringNative, new Tuple(new LitString("foo"))))));
		
		this.assertJExprsEquals(Double.valueOf(42), 
				List.of(
						(new Construct(TypeAtom.TypeDoubleNative, new Tuple(new LitDouble(42.0))))));
		
		this.assertJExprsEquals(Boolean.TRUE, 
				List.of(
						(new Construct(TypeAtom.TypeBoolNative, new Tuple(LitBoolean.TRUE)))));
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
		
		this.assertJExprEquals(TypedObject.VELKA_EMPTY, 
				Expression.EMPTY_EXPRESSION);
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
		
		this.assertJExprEquals(new VelkaTuple(List.of(Integer.valueOf(42), Boolean.TRUE, "foo"), 
												new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeBoolNative, TypeAtom.TypeStringNative)), 
				(new Tuple(new LitInteger(42), LitBoolean.TRUE, new LitString("foo"))));
	}

	@Test
	@DisplayName("Test User Exception")
	void testExceptionExpr() throws AppendableException {
		final ExceptionExpr exception = new ExceptionExpr(new LitString("test"));
		Environment env = TopLevelEnvironment.instantiate();
		

		assertThrows(RuntimeException.class, () -> exception.interpret(env));

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
		final Lambda lambda = new Lambda(
				new Symbol("x"),
				List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative), Pair.of(new Symbol("y"), TypeAtom.TypeIntNative)));
		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top);
		});

		this.assertReflexivity(lambda);
		this.assertDifference(lambda, 
				new Lambda(new Symbol("x"),
				List.of(Pair.of(new Symbol("z"), TypeAtom.TypeIntNative), 
						Pair.of(new Symbol("y"), TypeAtom.TypeIntNative))));
		this.assertDifference(lambda, new Lambda(new Symbol("x"),
				List.of(Pair.of(new Symbol("x"), TypeAtom.TypeDoubleNative), Pair.of(new Symbol("y"), TypeAtom.TypeIntNative))));
		this.assertDifference(lambda,
				new Lambda(Expression.EMPTY_EXPRESSION,
						List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative), 
								Pair.of(new Symbol("y"), TypeAtom.TypeIntNative))));
		this.assertDifference(lambda, Expression.EMPTY_EXPRESSION);

		Expression e = lambda.interpret(top);
		assertTrue(e instanceof Function);

		Pair<Type, Substitution> p = lambda.infer(top);
		this.assertInferenceClass(p, TypeArrow.class, lambda);

		assertThrows(TypesDoesNotUnifyException.class,
				() -> this.parseString("(lambda ((String x)) (+ x x))")
						.get(0).infer(top));
		
		this.assertJExprEquals(Integer.valueOf(42), new AbstractionApplication(
				(new Lambda(new LitInteger(42), List.of())), Tuple.EMPTY_TUPLE));
		
		this.assertJExprEquals(Integer.valueOf(42), new AbstractionApplication(
				(new Lambda(new Symbol("a"),
						List.of(Pair.of(new Symbol("a"), TypeAtom.TypeIntNative)))),
				new Tuple(new LitInteger(42))));

		this.assertJExprEquals(Integer.valueOf(42), new AbstractionApplication(
				(new Lambda(new Symbol("a"),
						List.of(Pair.of(new Symbol("a"), TypeAtom.TypeIntNative)))),
				new Tuple(new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman))));
	}

	@Test
	@DisplayName("Test Function")
	void testFunction() throws AppendableException {
		Environment top = TopLevelEnvironment.instantiate();
		

		Environment bound = Environment.create(top);
		bound.put(new Symbol("bound"), new LitDouble(3.141521));

		final Function function = new Function(bound, new Symbol("bound"),
				List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative)));

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		this.assertReflexivity(function);
		this.assertDifference(function,
				new Function(bound,
						new Symbol("bound"),
						List.of(Pair.of(new Symbol("x"), TypeAtom.TypeDoubleNative))));
		this.assertDifference(function, 
				new Function(bound,
						new Symbol("bound"),
						List.of(Pair.of(new Symbol("y"), TypeAtom.TypeIntNative))));
		this.assertDifference(function, 
				new Function(bound, Expression.EMPTY_EXPRESSION,
				List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative))));
		this.assertDifference(function, 
				new Function(top,
						new Symbol("bound"),
						List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative))));
		this.assertDifference(function, Expression.EMPTY_EXPRESSION);

		this.assertInterpretationEquals(function, function, top);

		Pair<Type, Substitution> p = function.infer(top);
		this.assertInferenceClass(p, TypeArrow.class, function);

		assertThrows(RuntimeException.class, () -> this
				.parseString("(lambda ((String x)) (+ x x))")
					.get(0).interpret(top).infer(top));
	}

	@Test
	@DisplayName("Test Extended Lambda")
	void testExpendedLambda() throws AppendableException {
		var lambda = new ExtendedLambda(new TypeTuple(TypeAtom.TypeInt));

		this.assertReflexivity(lambda);
		this.assertDifference(lambda,
				new ExtendedLambda(new TypeTuple()));

		Environment top = TopLevelEnvironment.instantiate();
		
		assertAll(() -> {
			lambda.toString();
			lambda.hashCode();
			lambda.toClojureCode(top);
			lambda.interpret(top);
		});

		Pair<Type, Substitution> p = lambda.infer(top);

		this.assertInference(p,
				new TypeArrow(new TypeTuple(TypeAtom.TypeInt), new TypeVariable(NameGenerator.next())),
				lambda);
	}

	@Test
	@DisplayName("Test Extended Function")
	public void testExtendedFunction() throws AppendableException {
		Environment top = TopLevelEnvironment.instantiate();
		

		Environment bound = Environment.create(top);
		bound.put(new Symbol("x"), new LitInteger(42));
		
		var function = new ExtendedFunction(bound);
		var function2 = function.extend(
				new Function(bound, new Symbol("y"), List.of(Pair.of(new Symbol("y"), TypeAtom.TypeIntRoman))),
				new Function(bound, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
						List.of(Pair.of(new Symbol("y"), TypeAtom.TypeInt))));
		
		var function3 = function2.extend(
				new Function(bound, new Symbol("y"), List.of(Pair.of(new Symbol("y"), TypeAtom.TypeIntString))),
				new Function(bound, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
						List.of(Pair.of(new Symbol("y"), TypeAtom.TypeInt))));

		this.assertReflexivity(function);
		this.assertReflexivity(function2);
		this.assertReflexivity(function3);

		this.assertDifference(function, function2);
		this.assertDifference(function, function3);
		this.assertDifference(function2, function3);
		this.assertDifference(function, Expression.EMPTY_EXPRESSION);

		assertAll(() -> {
			function.toString();
			function.hashCode();
		});

		Pair<Type, Substitution> p = function.infer(top);
		this.assertInference(p,
				RepresentationOr.factory(List.of(
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntRoman), TypeAtom.TypeIntRoman),
						new TypeArrow(new TypeTuple(TypeAtom.TypeIntString), TypeAtom.TypeIntString))),
				function);

//		assertThrows(RuntimeException.class, () -> 
//				function2.extend(
//						new Function(bound, new LitString("foo"), List.of(Pair.of(new Symbol("Y"), TypeAtom.TypeIntRoman))),
//						new Function(bound, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
//								List.of(Pair.of(new Symbol("y"), TypeAtom.TypeInt)))));
//		
//		assertThrows(RuntimeException.class, () -> 
//			function2.extend(
//				new Function(bound, new Symbol("y"), List.of(Pair.of(new Symbol("y"), TypeAtom.TypeStringNative))),
//				new Function(bound, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
//						List.of(Pair.of(new Symbol("y"), TypeAtom.TypeInt)))));
//		
//		assertThrows(RuntimeException.class, () -> 
//			function2.extend(
//				new Function(bound, new Symbol("y"), List.of(Pair.of(new Symbol("y"), TypeAtom.TypeIntNative))),
//				new Function(bound, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
//						List.of(Pair.of(new Symbol("y"), TypeAtom.TypeStringNative)))));
	}

	@Test
	@DisplayName("Test Application")
	void testApplication() throws AppendableException {
		AbstractionApplication application = new AbstractionApplication(
				new Lambda(new Symbol("x"),
						List.of(Pair.of(new Symbol("x"), new TypeVariable("y")))),
				new Tuple(Arrays.asList(new LitInteger(42))));

		this.assertReflexivity(application);
		this.assertDifference(application,
				new AbstractionApplication(
						new Lambda( new Symbol("x"),
								List.of(Pair.of(new Symbol("y"), new TypeVariable("x")))),
						new Tuple(Arrays.asList(new LitInteger(42)))));
		this.assertDifference(application,
				new AbstractionApplication(
						new Lambda(new Symbol("x"),
								List.of(Pair.of(new Symbol("x"), new TypeVariable("y")))),
						new Tuple(Arrays.asList(new LitInteger(21)))));
		this.assertDifference(application, Expression.EMPTY_EXPRESSION);

		Environment top = TopLevelEnvironment.instantiate();
		

		assertAll(() -> {
			application.toString();
			application.toClojureCode(top);
			application.hashCode();
		});

		assertThrows(RuntimeException.class,
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
				new Function(creation, 
						new Symbol("x"),
						List.of(Pair.of(new Symbol("y"), new TypeVariable("a")))),
				new Tuple(LitBoolean.TRUE));

		this.assertInterpretationEquals(lexicalClojureTest, new LitInteger(128), evaluation);
		p = lexicalClojureTest.infer(top);
		this.assertInference(p, TypeAtom.TypeIntNative, lexicalClojureTest);

		// Test autoconvert representations
		AbstractionApplication autoConRep = new AbstractionApplication(
				new Lambda( new Symbol("x"),
						List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntString))),
				new Tuple(Arrays.asList(new LitComposite(new LitString("V"), TypeAtom.TypeIntRoman))));
		this.assertInterpretationEquals(autoConRep, new LitComposite(new LitString("5"), TypeAtom.TypeIntString),
				top);
		p = autoConRep.infer(top);
		this.assertInference(p, TypeAtom.TypeIntString, autoConRep);

		// Test elambda/efunction comparation
		var elambda = new Extend(
				new Extend(
						new ExtendedLambda(new TypeTuple(TypeAtom.TypeInt)),
						new Lambda(new Symbol("x"), List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntString)))),
				new Lambda(new Symbol("x"), List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntRoman)))); 
		
		var useString = new AbstractionApplication(elambda, new Tuple(
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString)));
		
		this.assertInterpretationEquals(useString,
				new LitComposite(new LitString("5"), TypeAtom.TypeIntString), top);
		
		p = useString.infer(top);
		this.assertInference(p,
				RepresentationOr.factory(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useString);

		AbstractionApplication useRoman = new AbstractionApplication(elambda,
				new Tuple(new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman)));
		this.assertInterpretationEquals(useRoman,
				new LitComposite(new Tuple(Arrays.asList(new LitString("V"))), TypeAtom.TypeIntRoman), top);
		p = useRoman.infer(top);
		this.assertInference(p,
				RepresentationOr.factory(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman), useRoman);

		assertThrows(AppendableException.class,
				() -> new AbstractionApplication(elambda, new Tuple(Arrays.asList(new LitString("fail")))).infer(top));

		this.assertJExprEquals(Integer.valueOf(42), 
				new AbstractionApplication(
						new Lambda(new Symbol("a"), List.of(Pair.of(new Symbol("a"), TypeAtom.TypeIntNative))),
						new Tuple(new LitInteger(42))));
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
		var constructor = new Lambda(new LitComposite(Expression.EMPTY_EXPRESSION, type), List.of());

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
		this.assertInterpretationEquals(
				"(let ((f (extend (extend (extended-lambda (Int))"
				+ "(lambda ((Int:Native x)) \"foo\") (lambda ((Int:* x)) 0.1))"
				+ "(lambda ((Int:Roman x)) \"bar\") (lambda ((Int:* x)) 0.9999999))))"
				+ "(f 42))",
				new LitString("bar"));
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
		
		LogManager.getLogManager().reset();
		this.assertJExprEquals(TypedObject.VELKA_EMPTY, 
				(new AbstractionApplication(Operators.InitLogger, new Tuple(new LitString("test-log2")))));
		logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		logger.info("test");
		
		this.assertJExprEquals(TypedObject.VELKA_EMPTY, 
				(new AbstractionApplication(Operators.Log, new Tuple(new LitString("test-java")))));
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
				"(loop ((x 1) (a (construct List:Native))) (if (= x 2) a (let ((z (list-native-add-to-end-in-place a x))) (recur (+ x 1) a))))",
				
						new LitInteropObject(
								new ArrayList<Object>(List.of(1)),
						TypeAtom.TypeListNative));
		
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
		var ef = new ExtendedFunction(env);
		
		this.assertInterpretationEquals(
				"(extended-lambda (Int))",
				ef);
		
		assertAll(() -> {
			ef.extend(
					new Function(env, new LitInteger(1), List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative))),
					new Function(this.env, new LitDouble(RankAggregation.instance().defaultImplementationRank()),
							List.of(Pair.of(new Symbol("x"), TypeAtom.TypeInt))));
			
			this.parseString("(extend (extended-lambda (Int)) (lambda ((Int:Native x)) 1))").get(0)
					.interpret(this.env);
		});
	}
	
	@Test
	@DisplayName("Documentation generator test")
	void testDocumentationGeneration() throws Exception {
		LangbaseDocumentationGenerator generator = new LangbaseDocumentationGenerator();
		Map<Path, String> doc = generator.generate(Arrays.asList(
				Operators.class,
				ConversionOperators.class,
				JavaLinkedList.class,
				ListNative.class,
				ConstructorOperators.class));
		assertNotNull(doc);
	}
}
