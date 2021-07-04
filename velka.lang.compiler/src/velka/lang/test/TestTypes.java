package velka.lang.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.lang.abstraction.Lambda;
import velka.lang.conversions.Conversions;
import velka.lang.coreExceptions.ConversionException;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.expression.Symbol;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitString;
import velka.lang.parserExceptions.TypeNotRecognizedException;
import velka.lang.types.*;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

class TestTypes {

	@Test
	@DisplayName("Exceptions")
	void testExceptions() {
		assertAll(() -> {
			new TypeNotRecognizedException("test");
			new TypesDoesNotUnifyException(TypeAtom.TypeInt, TypeAtom.TypeBool);
			new UnexpectedTypeException(TypeAtom.TypeInt, TypeArrow.class);
			new ConversionException(TypeAtom.TypeInt, TypeAtom.TypeBool, Expression.EMPTY_EXPRESSION);
		});
	}

	@Test
	@DisplayName("Substitution")
	void testSubstitution() throws AppendableException {
		final Substitution subst = new Substitution(
				Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt),
						new Pair<TypeVariable, Type>(new TypeVariable("b"), new TypeVariable("a")),
						new Pair<TypeVariable, Type>(new TypeVariable("c"), TypeAtom.TypeIntString)));
		assertAll(() -> {
			subst.toString();
			subst.variableStream();
		});

		assertEquals(subst, subst);

		int hash1 = subst.hashCode();
		int hash2 = subst.hashCode();
		assertEquals(hash1, hash2);

		assertNotEquals(subst, Substitution.EMPTY);
		assertNotEquals(subst, TypeTuple.EMPTY_TUPLE);

		TypeVariable tv = new TypeVariable("a");
		assertTrue(subst.containsVariable(tv));

		Optional<Type> o = subst.get(tv);
		assertTrue(o.isPresent());
		assertEquals(o.get(), TypeAtom.TypeInt);

		tv = new TypeVariable("d");
		assertFalse(subst.containsVariable(tv));

		o = subst.get(tv);
		assertFalse(o.isPresent());

		assertAll(() -> {
			TestTypes.testSubstUnion(
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))));
			TestTypes.testSubstUnion(
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeInt))),
					new Substitution(
							Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt),
									new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeInt))));
		});

		assertEquals(Optional.empty(),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt)))
						.union(new Substitution(Arrays
								.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeBool)))));
	}

	@Test
	@DisplayName("Test Type Variable")
	void testTypeVariable() throws AppendableException {
		TypeVariable variable = new TypeVariable("x");
		assertAll(() -> {
			variable.toString();
		});

		TestTypes.testReflexivity(variable);
		// TestTypes.testDifference(variable, new TypeVariable("y"));
		TestTypes.testDifference(variable, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(variable, Arrays.asList(new TypeVariable("x")));
		TestTypes.testConvertTo(variable, Expression.EMPTY_EXPRESSION, TypeAtom.TypeInt, Expression.EMPTY_EXPRESSION);

		TestTypes.testApply(variable, Substitution.EMPTY, variable);
		TestTypes.testApply(variable,
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(variable, TypeAtom.TypeInt))),
				TypeAtom.TypeInt);
		TestTypes
				.testApply(variable,
						new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(variable, new TypeVariable("y")),
								new Pair<TypeVariable, Type>(new TypeVariable("y"), TypeAtom.TypeBool))),
						TypeAtom.TypeBool);
	}

	@Test
	@DisplayName("Test Type Tuple")
	void testTypeTuple() throws AppendableException {
		TypeTuple tuple = new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, new TypeVariable("a"), TypeAtom.TypeBool));
		assertAll(() -> {
			tuple.stream();
			tuple.iterator();
			tuple.toString();
		});

		assertEquals(tuple.get(0), TypeAtom.TypeIntRoman);
		assertEquals(tuple.get(1), new TypeVariable("a"));
		assertEquals(tuple.get(2), TypeAtom.TypeBool);
		assertEquals(tuple.size(), 3);

		TestTypes.testReflexivity(tuple);
		// TestTypes.testDifference(tuple,
		// new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, new TypeVariable("b"),
		// TypeAtom.TypeBool)));
		TestTypes.testDifference(tuple, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(tuple, Arrays.asList(new TypeVariable("a")));

		TestTypes.testConvertTo(tuple,
				new Tuple(Arrays.asList(new LitComposite(new LitString("XIII"), TypeAtom.TypeIntRoman), new Symbol("x"),
						LitBoolean.TRUE)),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString, TypeAtom.TypeBool)),
				new Tuple(Arrays.asList(new LitComposite(new LitString("13"), TypeAtom.TypeIntString), new Symbol("x"),
						LitBoolean.TRUE)));
		TestTypes.testConvertTo(tuple,
				new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
				new TypeVariable("b"),
				new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)));

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(ConversionException.class,
				() -> Conversions.convert(tuple,
						new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
						TypeAtom.TypeInt, typeEnv));
		assertThrows(ConversionException.class,
				() -> Conversions.convert(tuple,
						new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString)), typeEnv));

		TestTypes.testApply(tuple,
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, TypeAtom.TypeInt, TypeAtom.TypeBool)));

		assertTrue(tuple.tupleDistance(TypeTuple.EMPTY_TUPLE) >= 0);
		assertTrue(tuple.tupleDistance(tuple) == 0);
		TypeTuple other = new TypeTuple(
				Arrays.asList(TypeAtom.TypeIntString, new TypeVariable("b"), TypeAtom.TypeBool));
		assertTrue(tuple.tupleDistance(other) == 1);
	}

	@Test
	@DisplayName("Test Type Arrow")
	void testTypeArrow() throws AppendableException {
		TypeArrow typeArrow = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
				TypeAtom.TypeIntString);
		assertAll(() -> {
			typeArrow.toString();
		});

		TestTypes.testReflexivity(typeArrow);
		TestTypes.testDifference(typeArrow,
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), TypeAtom.TypeBool));
		TestTypes.testDifference(typeArrow, new TypeArrow(new TypeVariable("b"), TypeAtom.TypeIntString));
		TestTypes.testDifference(typeArrow, new TypeArrow(new TypeVariable("b"), TypeAtom.TypeBool));
		TestTypes.testDifference(typeArrow, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(new TypeArrow(new TypeVariable("b"), TypeAtom.TypeIntString),
				Arrays.asList(new TypeVariable("b")));
		TestTypes.testConvertTo(typeArrow, Expression.EMPTY_EXPRESSION, new TypeVariable("c"),
				Expression.EMPTY_EXPRESSION);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		Expression e = Conversions.convert(typeArrow,
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman), typeEnv);

		assertTrue(e instanceof Lambda);

		Pair<Type, Substitution> p = e.infer(env, typeEnv);
		assertEquals(p.first,
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman));

		assertThrows(ConversionException.class,
				() -> Conversions.convert(typeArrow, Expression.EMPTY_EXPRESSION, TypeAtom.TypeInt, typeEnv));

		TestTypes.testApply(new TypeArrow(TypeAtom.TypeBool, new TypeVariable("a")),
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntRoman))),
				new TypeArrow(TypeAtom.TypeBool, TypeAtom.TypeIntRoman));
	}

	@Test
	@DisplayName("Test Type Representation")
	void testTypeRepresentation() {
		TypeRepresentation rep = new TypeRepresentation("test");

		assertEquals(rep, rep);
		assertEquals(rep.hashCode(), rep.hashCode());
		assertTrue(rep.compareTo(rep) == 0);

		Object other = new TypeRepresentation("other");
		assertNotEquals(rep, other);
		assertTrue(rep.compareTo((TypeRepresentation) other) != 0);

		assertNotEquals(rep, Expression.EMPTY_EXPRESSION);
	}

	@Test
	@DisplayName("Test Type Name")
	void testTypeName() {
		TypeName name = new TypeName("test");

		assertEquals(name, name);
		assertEquals(name.hashCode(), name.hashCode());
		assertEquals(name.compareTo(name), 0);

		Object other = new TypeName("other");
		assertNotEquals(name, other);
		assertNotEquals(name.compareTo((TypeName) other), 0);

		assertNotEquals(name, Expression.EMPTY_EXPRESSION);
	}

	@Test
	@DisplayName("Test Type Atom")
	void testTypeAtom() throws AppendableException {
		TypeAtom atom = new TypeAtom(new TypeName("Test"), TypeRepresentation.WILDCARD);
		assertAll(() -> {
			atom.toString();
		});

		TestTypes.testReflexivity(atom);
		TestTypes.testDifference(atom, TypeAtom.TypeInt);
		TestTypes.testDifference(atom, new TypeAtom(new TypeName("Test"), TypeRepresentation.NATIVE));
		TestTypes.testDifference(atom, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(atom, Arrays.asList());

		TestTypes.testConvertTo(TypeAtom.TypeIntString, new LitString("42"), new TypeVariable("a"),
				new LitString("42"));
		TestTypes.testConvertTo(TypeAtom.TypeIntString, new LitComposite(new LitString("42"), TypeAtom.TypeIntString),
				TypeAtom.TypeIntRoman, new LitComposite(new LitString("XLII"), TypeAtom.TypeIntRoman));

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertThrows(ConversionException.class, () -> Conversions.convert(atom, Expression.EMPTY_EXPRESSION,
				new TypeArrow(TypeAtom.TypeInt, TypeAtom.TypeInt), typeEnv));
		assertThrows(ConversionException.class,
				() -> Conversions.convert(atom, Expression.EMPTY_EXPRESSION, TypeAtom.TypeIntString, typeEnv));
		assertThrows(ConversionException.class,
				() -> Conversions.convert((new TypeAtom(new TypeName("Test"), TypeRepresentation.NATIVE)),
						Expression.EMPTY_EXPRESSION, new TypeAtom(new TypeName("Test"), TypeRepresentation.STRING),
						typeEnv));

		TestTypes.testApply(TypeAtom.TypeInt,
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntRoman))),
				TypeAtom.TypeInt);

		assertTrue(TypeAtom.isSameBasicType(TypeAtom.TypeInt, TypeAtom.TypeIntRoman));
		assertFalse(TypeAtom.isSameBasicType(TypeAtom.TypeDouble, TypeAtom.TypeIntRoman));
	}

	@Test
	@DisplayName("Test Representation Or")
	void testRepresentationOr() throws AppendableException {
		// Test construction
		assertEquals(RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt)), TypeAtom.TypeInt);

		assertAll(() -> {
			RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString));
		});

		RepresentationOr ror = (RepresentationOr) RepresentationOr
				.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString));
		assertAll(() -> {
			ror.toString();
			ror.getRepresentations();
		});

		assertTrue(ror instanceof RepresentationOr);

		assertThrows(AppendableException.class, () -> RepresentationOr.makeRepresentationOr(Arrays.asList()));
		assertThrows(TypeSetDoesNotUnifyException.class,
				() -> RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeBool)));

		// Equals & CompareTo
		TestTypes.testReflexivity(ror);
		TestTypes.testDifference(ror, RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt)));
		TestTypes.testDifference(ror, RepresentationOr.makeRepresentationOr(
				Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, TypeAtom.TypeIntRoman)));
		TestTypes.testDifference(ror,
				RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeIntRoman)));
		TestTypes.testDifference(ror, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(ror, Arrays.asList());

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		assertEquals(Expression.EMPTY_EXPRESSION,
				Conversions.convert(ror, Expression.EMPTY_EXPRESSION, TypeAtom.TypeIntRoman, typeEnv));

		TestTypes.testApply(
				RepresentationOr.makeRepresentationOr(
						Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, new TypeVariable("x")),
								new TypeArrow(TypeAtom.TypeIntString, new TypeVariable("x")))),
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("x"), TypeAtom.TypeIntString))),
				RepresentationOr.makeRepresentationOr(
						Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeIntString),
								new TypeArrow(TypeAtom.TypeIntString, TypeAtom.TypeIntString))));
	}

	@Test
	@DisplayName("Test Type")
	void testType() throws AppendableException {
		TestTypes.testUnify(TypeAtom.TypeInt, TypeAtom.TypeInt, Substitution.EMPTY);
		TestTypes.testUnify(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, Substitution.EMPTY);

		TestTypes.testUnify(new TypeVariable("a"), TypeAtom.TypeIntNative, new Substitution(
				Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntNative))));
		TestTypes.testUnify(TypeAtom.TypeIntNative, new TypeVariable("a"), new Substitution(
				Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntNative))));

		TestTypes.testUnify(new TypeArrow(new TypeVariable("a"), TypeAtom.TypeIntRoman),
				new TypeArrow(TypeAtom.TypeIntRoman, new TypeVariable("b")),
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntRoman),
								new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeIntRoman))));

		TestTypes.testUnify(new TypeTuple(Arrays.asList(new TypeVariable("a"), TypeAtom.TypeBoolNative)),
				new TypeTuple(Arrays.asList(TypeAtom.TypeDouble, new TypeVariable("b"))),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeDouble),
						new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeBoolNative))));

		assertEquals(Optional.empty(), Type.unifyTypes(TypeAtom.TypeBool, TypeAtom.TypeInt));
		assertEquals(Optional.empty(),
				Type.unifyTypes(new TypeArrow(TypeAtom.TypeInt, TypeAtom.TypeInt), TypeAtom.TypeInt));
		assertEquals(Optional.empty(),
				Type.unifyTypes(new TypeTuple(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeInt)), TypeAtom.TypeInt));
		assertEquals(Optional.empty(), Type.unifyTypes(new TypeTuple(TypeAtom.TypeInt, TypeAtom.TypeInt),
				new TypeTuple(TypeAtom.TypeInt, TypeAtom.TypeInt, TypeAtom.TypeInt)));

		TestTypes.testUnify(new TypeVariable("x"),
				RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman)),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("x"), RepresentationOr
						.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman))))));
		TestTypes.testUnify(
				RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman)),
				new TypeVariable("x"),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("x"), RepresentationOr
						.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman))))));
		TestTypes.testUnify(
				RepresentationOr.makeRepresentationOr(
						Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, new TypeVariable("x")),
								new TypeArrow(TypeAtom.TypeIntString, new TypeVariable("x")))),
				new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative), new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("x"), TypeAtom.TypeStringNative))));
		TestTypes.testUnify(new TypeArrow(new TypeVariable("x"), TypeAtom.TypeStringNative),
				RepresentationOr.makeRepresentationOr(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative),
						new TypeArrow(TypeAtom.TypeIntString, TypeAtom.TypeStringNative)),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("x"), RepresentationOr
						.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString))))));
		TestTypes.testUnify(
				RepresentationOr.makeRepresentationOr(
						Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, new TypeVariable("x")),
								new TypeArrow(TypeAtom.TypeIntString, new TypeVariable("x")))),
				RepresentationOr
						.makeRepresentationOr(Arrays.asList(new TypeArrow(new TypeVariable("y"), TypeAtom.TypeIntRoman),
								new TypeArrow(new TypeVariable("y"), TypeAtom.TypeIntNative))),
				new Substitution(Arrays.asList(
						new Pair<TypeVariable, Type>(new TypeVariable("x"),
								RepresentationOr.makeRepresentationOr(
										Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman))),
						new Pair<TypeVariable, Type>(new TypeVariable("y"), RepresentationOr.makeRepresentationOr(
								Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeIntNative))))));
	}

	static void testReflexivity(Type type) {
		assertNotNull(type);
		assertEquals(type, type);
		assertEquals(type.hashCode(), type.hashCode());
		assertEquals(type.compareTo(type), 0);
	}

	static void testDifference(Type type, Type other) {
		assertNotNull(type);
		assertNotNull(other);
		assertNotEquals(type, other);
		assertNotEquals(type.compareTo(other), 0);
	}

	static void testGetUnconstrainedVariables(Type type, Collection<TypeVariable> shouldContain) {
		assertNotNull(type);
		assertNotNull(shouldContain);
		Set<TypeVariable> unconstrained = type.getVariables();

		if (!unconstrained.containsAll(shouldContain)) {
			Set<TypeVariable> s = new TreeSet<TypeVariable>(shouldContain);
			s.removeAll(unconstrained);
			fail("Variables " + s + " are missing in " + type + ".getUnconstrainedVariables()" + " got "
					+ unconstrained);
		}
	}

	static void testConvertTo(Type type, Expression from, Type to, Expression expected) throws AppendableException {
		assertNotNull(type);
		assertNotNull(from);
		assertNotNull(to);
		assertNotNull(expected);

		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

		Expression converted = Conversions.convert(type, from, to, typeEnv).interpret(env, typeEnv);

		assertEquals(converted, expected);
	}

	static void testApply(Type type, Substitution s, Type expected) throws AppendableException {
		assertNotNull(type);
		assertNotNull(s);
		assertNotNull(expected);
		Type got = type.apply(s);
		assertEquals(got, expected);
	}

	static void testSubstUnion(Substitution subst1, Substitution subst2, Substitution expected)
			throws AppendableException {
		assertNotNull(subst1);
		assertNotNull(subst2);
		assertNotNull(expected);
		Optional<Substitution> composed = subst1.union(subst2);
		if (composed.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(subst1, subst2);
		}

		assertEquals(expected, composed.get());
	}

	static void testUnify(Type first, Type second, Substitution expected) throws AppendableException {
		assertNotNull(first);
		assertNotNull(second);
		assertNotNull(expected);
		Optional<Substitution> s = Type.unifyTypes(first, second);
		if (s.isEmpty()) {
			throw new TypesDoesNotUnifyException(first, second);
		}

		assertEquals(expected, s.get());
	}
}
