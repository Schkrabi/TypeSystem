package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import abstraction.Lambda;
import abstraction.Operator;
import application.AbstractionApplication;
import expression.Expression;
import expression.Tuple;
import expression.Symbol;
import interpretation.Environment;
import literal.LitBoolean;
import literal.LitString;
import semantic.TypeEnvironment;
import types.*;
import util.AppendableException;
import util.Pair;

class TestTypes {

	private static boolean initFlag = false;

	@BeforeEach
	void setUp() throws Exception {
		if (!initFlag) {
			TypeEnvironment.initBasicTypes();
			initFlag = true;
		}
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testExceptions() {
		new TypeNotRecognizedException("test");
		new TypesDoesNotUnifyException(TypeAtom.TypeInt, TypeAtom.TypeBool);
		new UnexpectedTypeException(TypeAtom.TypeInt, TypeArrow.class);
		new ConversionException(TypeAtom.TypeInt, TypeAtom.TypeBool, Expression.EMPTY_EXPRESSION);
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testSubstitution() throws AppendableException {
		Substitution subst = new Substitution(
				Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt),
						new Pair<TypeVariable, Type>(new TypeVariable("b"), new TypeVariable("a")),
						new Pair<TypeVariable, Type>(new TypeVariable("c"), TypeAtom.TypeIntString)));

		subst.toString();
		subst.variableStream();

		if (!subst.equals(subst)) {
			fail(subst + ".equals(" + subst + ") != true");
		}
		int hash1 = subst.hashCode();
		int hash2 = subst.hashCode();
		if (hash1 != hash2) {
			fail("Hash of " + subst + " is incosnsistent got " + hash1 + " and " + hash2);
		}
		if (subst.equals(Substitution.EMPTY)) {
			fail(subst + ".equals(" + Substitution.EMPTY + ") != false");
		}
		if (subst.equals(TypeTuple.EMPTY_TUPLE)) {
			fail(subst + ".equals(" + TypeTuple.EMPTY_TUPLE + ") != false");
		}

		TypeVariable tv = new TypeVariable("a");
		if (!subst.containsVariable(tv)) {
			fail(subst + ".containsVariable(" + tv + ") == false and should be true");
		}
		Optional<Type> o = subst.get(tv);
		if (!o.isPresent()) {
			fail(subst + ".get(" + tv + ") is not present!");
		}
		if (o.get() != TypeAtom.TypeInt) {
			fail(subst + ".get(" + tv + ").get() != " + TypeAtom.TypeInt + " got " + o.get());
		}

		tv = new TypeVariable("d");
		if (subst.containsVariable(tv)) {
			fail(subst + ".containsVariable(" + tv + ") == true and should be false");
		}
		o = subst.get(tv);
		if (o.isPresent()) {
			fail(subst + ".get(" + tv + ") is present. Should be " + o.get());
		}

		TestTypes.testSubstUnion(
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))));
		TestTypes.testSubstUnion(
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeInt))),
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt),
						new Pair<TypeVariable, Type>(new TypeVariable("b"), TypeAtom.TypeInt))));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> (new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))))
								.union(new Substitution(Arrays.asList(
										new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeBool)))));
	}

	@Test
	void testTypeVariable() throws AppendableException {
		TypeVariable variable = new TypeVariable("x");

		variable.toString();

		TestTypes.testReflexivity(variable);
		TestTypes.testDifference(variable, new TypeVariable("y"));
		TestTypes.testDifference(variable, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(variable, Arrays.asList(new TypeVariable("x")));
		TestTypes.testConvertTo(variable, Expression.EMPTY_EXPRESSION, TypeAtom.TypeInt, Expression.EMPTY_EXPRESSION);
		TestTypes.testRemoveRepresentationInfo(variable, variable);

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
	void testTypeTuple() throws AppendableException {
		TypeTuple tuple = new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, new TypeVariable("a"), TypeAtom.TypeBool));

		Type t = tuple.get(0);
		if (!t.equals(TypeAtom.TypeIntRoman)) {
			fail(tuple + ".get(0) got " + t + " expected " + TypeAtom.TypeIntRoman);
		}
		t = tuple.get(1);
		if (!t.equals(new TypeVariable("a"))) {
			fail(tuple + ".get(1) got " + t + " expected " + new TypeVariable("a"));
		}
		t = tuple.get(2);
		if (!t.equals(TypeAtom.TypeBool)) {
			fail(tuple + ".get(2) got " + t + " expected " + TypeAtom.TypeBool);
		}

		int s = tuple.size();
		if (s != 3) {
			fail(tuple + ".size() got " + s + " expected " + 3);
		}

		tuple.stream();
		tuple.iterator();
		tuple.toString();

		TestTypes.testReflexivity(tuple);
		TestTypes.testDifference(tuple,
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, new TypeVariable("b"), TypeAtom.TypeBool)));
		TestTypes.testDifference(tuple, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(tuple, Arrays.asList(new TypeVariable("a")));

		TestTypes.testConvertTo(tuple,
				new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString, TypeAtom.TypeBool)),
				new Tuple(Arrays.asList(
						new AbstractionApplication(Operator.IntRomanToIntString, new Tuple(Arrays.asList(new LitString("XIII")))),
						new Symbol("x"), LitBoolean.TRUE)));
		TestTypes.testConvertTo(tuple,
				new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
				new TypeVariable("b"),
				new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)));

		Assertions.assertThrows(ConversionException.class,
				() -> tuple.convertTo(
						new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
						TypeAtom.TypeInt));
		Assertions.assertThrows(ConversionException.class, () -> tuple.convertTo(Expression.EMPTY_EXPRESSION,
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString, TypeAtom.TypeBool))));
		Assertions.assertThrows(ConversionException.class,
				() -> tuple.convertTo(
						new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"), LitBoolean.TRUE)),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString))));
		Assertions.assertThrows(ConversionException.class,
				() -> tuple.convertTo(new Tuple(Arrays.asList(new LitString("XIII"), new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString, TypeAtom.TypeString, TypeAtom.TypeBool))));

		TestTypes.testApply(tuple,
				new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeInt))),
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, TypeAtom.TypeInt, TypeAtom.TypeBool)));

		TestTypes.testRemoveRepresentationInfo(tuple,
				new TypeTuple(Arrays.asList(TypeAtom.TypeInt, new TypeVariable("a"), TypeAtom.TypeBool)));

		int d = tuple.tupleDistance(TypeTuple.EMPTY_TUPLE);
		if (d <= 0) {
			fail(tuple + ".distance(" + TypeTuple.EMPTY_TUPLE + ") == " + d + " should be bigger than 0");
		}
		d = tuple.tupleDistance(tuple);
		if (d != 0) {
			fail(tuple + ".distance(" + tuple + ") == " + d + " should be 0");
		}
		d = tuple.tupleDistance(
				new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman, new TypeVariable("b"), TypeAtom.TypeBool)));
		if (d != 1) {
			fail(tuple + ".distance(" + tuple + ") == " + d + " should be 1");
		}
	}

	@Test
	void testTypeArrow() throws AppendableException {
		TypeArrow typeArrow = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
				TypeAtom.TypeIntString);

		typeArrow.toString();

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

		Expression e = typeArrow.convertTo(
				new Lambda(new Tuple(Arrays.asList(new Symbol("x"))),
						new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Symbol("x")),
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman));
		if (!(e instanceof Lambda)) {
			fail("Conversion of typearrow to typearrow should yield lambda got " + e + " of class "
					+ e.getClass().getName());
		}
		Pair<Type, Substitution> p = e.infer(Environment.topLevelEnvironment);
		if (!p.first
				.equals(new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), TypeAtom.TypeIntRoman))) {
			fail("Infered type of converted expression is " + p.first + " expected "
					+ new TypeArrow(TypeAtom.TypeIntRoman, TypeAtom.TypeIntRoman));
		}

		Assertions.assertThrows(ConversionException.class,
				() -> typeArrow.convertTo(Expression.EMPTY_EXPRESSION, TypeAtom.TypeInt));

		TestTypes.testRemoveRepresentationInfo(typeArrow,
				new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeInt)), TypeAtom.TypeInt));

		TestTypes.testApply(new TypeArrow(TypeAtom.TypeBool, new TypeVariable("a")),
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntRoman))),
				new TypeArrow(TypeAtom.TypeBool, TypeAtom.TypeIntRoman));
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testTypeRepresentation() {
		TypeRepresentation rep = new TypeRepresentation("test");

		if (!rep.equals(rep)) {
			fail(rep + ".equals(" + rep + ") != true");
		}
		int hash1 = rep.hashCode();
		int hash2 = rep.hashCode();
		if (hash1 != hash2) {
			fail("Hash of " + rep + " is incosnsistent got " + hash1 + " and " + hash2);
		}
		int cmp = rep.compareTo(rep);
		if (cmp != 0) {
			fail(rep + ".compareTo(" + rep + ") != 0");
		}

		Object other = new TypeRepresentation("other");
		if (rep.equals(other)) {
			fail(rep + ".equaks(" + other + ") == true");
		}
		cmp = rep.compareTo((TypeRepresentation) other);
		if (cmp == 0) {
			fail(rep + ".compareTo(" + other + ") == 0");
		}

		if (rep.equals(Expression.EMPTY_EXPRESSION)) {
			fail(rep + ".equaks(" + Expression.EMPTY_EXPRESSION + ") == true");
		}
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testTypeName() {
		TypeName name = new TypeName("test");

		if (!name.equals(name)) {
			fail(name + ".equals(" + name + ") != true");
		}
		int hash1 = name.hashCode();
		int hash2 = name.hashCode();
		if (hash1 != hash2) {
			fail("Hash of " + name + " is incosnsistent got " + hash1 + " and " + hash2);
		}
		int cmp = name.compareTo(name);
		if (cmp != 0) {
			fail(name + ".compareTo(" + name + ") != 0");
		}

		Object other = new TypeName("other");
		if (name.equals(other)) {
			fail(name + ".equaks(" + other + ") == true");
		}
		cmp = name.compareTo((TypeName) other);
		if (cmp == 0) {
			fail(name + ".compareTo(" + other + ") == 0");
		}

		if (name.equals(Expression.EMPTY_EXPRESSION)) {
			fail(name + ".equaks(" + Expression.EMPTY_EXPRESSION + ") == true");
		}
	}

	@Test
	void testTypeAtom() throws AppendableException {
		TypeAtom atom = new TypeAtom(new TypeName("Test"), TypeRepresentation.WILDCARD);

		atom.toString();

		TestTypes.testReflexivity(atom);
		;
		TestTypes.testDifference(atom, TypeAtom.TypeInt);
		TestTypes.testDifference(atom, new TypeAtom(new TypeName("Test"), TypeRepresentation.NATIVE));
		TestTypes.testDifference(atom, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(atom, Arrays.asList());

		TestTypes.testConvertTo(TypeAtom.TypeIntString, new LitString("42"), new TypeVariable("a"),
				new LitString("42"));
		TestTypes.testConvertTo(TypeAtom.TypeIntString, new LitString("42"), TypeAtom.TypeIntRoman,
				new AbstractionApplication(Operator.IntStringToIntRoman, new Tuple(Arrays.asList(new LitString("42")))));

		Assertions.assertThrows(ConversionException.class,
				() -> atom.convertTo(Expression.EMPTY_EXPRESSION, new TypeArrow(TypeAtom.TypeInt, TypeAtom.TypeInt)));
		Assertions.assertThrows(ConversionException.class,
				() -> atom.convertTo(Expression.EMPTY_EXPRESSION, TypeAtom.TypeIntString));
		Assertions
				.assertThrows(ConversionException.class,
						() -> (new TypeAtom(new TypeName("Test"), TypeRepresentation.NATIVE)).convertTo(
								Expression.EMPTY_EXPRESSION,
								new TypeAtom(new TypeName("Test"), TypeRepresentation.STRING)));

		TestTypes.testRemoveRepresentationInfo(TypeAtom.TypeIntNative, TypeAtom.TypeInt);
		TestTypes.testApply(TypeAtom.TypeInt,
				new Substitution(
						Arrays.asList(new Pair<TypeVariable, Type>(new TypeVariable("a"), TypeAtom.TypeIntRoman))),
				TypeAtom.TypeInt);

		if (!TypeAtom.isSameBasicType(TypeAtom.TypeInt, TypeAtom.TypeIntRoman)) {
			fail("TypeAtom.isSameBasicType(" + TypeAtom.TypeInt + ", " + TypeAtom.TypeIntRoman + " != true");
		}
		if (TypeAtom.isSameBasicType(TypeAtom.TypeDouble, TypeAtom.TypeIntRoman)) {
			fail("TypeAtom.isSameBasicType(" + TypeAtom.TypeDouble + ", " + TypeAtom.TypeIntRoman + " == true");
		}
	}

	@Test
	void testRepresentationOr() throws AppendableException {
		RepresentationOr ror;

		// Test construction
		Type rslt = RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt));
		if (!(rslt instanceof TypeAtom) || !rslt.equals(TypeAtom.TypeInt)) {
			fail("makeRepresentationOr error, expected " + TypeAtom.TypeInt + " got " + rslt);
		}
		rslt = RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString));
		if (!(rslt instanceof RepresentationOr)) {
			fail("makeRepresentationOr error, expected RepresentationOr got " + rslt);
		}
		ror = (RepresentationOr) rslt;

		Assertions.assertThrows(AppendableException.class,
				() -> RepresentationOr.makeRepresentationOr(Arrays.asList()));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeBool)));

		// ToString
		ror.toString();
		ror.getRepresentations();
		Assertions.assertThrows(AppendableException.class, () -> ror.convertToClojure("", TypeTuple.EMPTY_TUPLE));

		// Equals & CompareTo
		TestTypes.testReflexivity(ror);
		TestTypes.testDifference(ror, RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt)));
		TestTypes.testDifference(ror, RepresentationOr.makeRepresentationOr(
				Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, TypeAtom.TypeIntRoman)));
		TestTypes.testDifference(ror,
				RepresentationOr.makeRepresentationOr(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeIntRoman)));
		TestTypes.testDifference(ror, TypeTuple.EMPTY_TUPLE);

		TestTypes.testGetUnconstrainedVariables(ror, Arrays.asList());

		Assertions.assertThrows(AppendableException.class,
				() -> ror.convertTo(Expression.EMPTY_EXPRESSION, TypeAtom.TypeIntRoman));

		TestTypes.testRemoveRepresentationInfo(ror, ror);

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

		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> Type.unify(TypeAtom.TypeBool, TypeAtom.TypeInt));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> Type.unify(new TypeArrow(TypeAtom.TypeInt, TypeAtom.TypeInt), TypeAtom.TypeInt));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> Type.unify(new TypeTuple(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeInt)), TypeAtom.TypeInt));
		Assertions.assertThrows(TypesDoesNotUnifyException.class,
				() -> Type.unify(new TypeTuple(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeInt)),
						new TypeTuple(Arrays.asList(TypeAtom.TypeInt, TypeAtom.TypeInt, TypeAtom.TypeInt))));

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
				RepresentationOr.makeRepresentationOr(
						Arrays.asList(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeStringNative),
								new TypeArrow(TypeAtom.TypeIntString, TypeAtom.TypeStringNative))),
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
		if (!type.equals(type)) {
			fail(type + ".equals(" + type + ") != true");
		}
		int hash1 = type.hashCode();
		int hash2 = type.hashCode();
		if (hash1 != hash2) {
			fail("Hash of " + type + " is incosnsistent got " + hash1 + " and " + hash2);
		}
		int cmp = type.compareTo(type);
		if (cmp != 0) {
			fail(type + ".compareTo(" + type + ") != 0 got " + cmp);
		}

	}

	static void testDifference(Type type, Type other) {
		if (type.equals(other)) {
			fail(type + ".equals(" + other + ") != false");
		}
		int cmp = type.compareTo(other);
		if (cmp == 0) {
			fail(type + ".compareTo(" + type + ") != 0 got " + cmp);
		}
	}

	static void testGetUnconstrainedVariables(Type type, Collection<TypeVariable> shouldContain) {
		Set<TypeVariable> unconstrained = type.getUnconstrainedVariables();

		if (!unconstrained.containsAll(shouldContain)) {
			Set<TypeVariable> s = new TreeSet<TypeVariable>(shouldContain);
			s.removeAll(unconstrained);
			fail("Variables " + s + " are missing in " + type + ".getUnconstrainedVariables()" + " got "
					+ unconstrained);
		}
	}

	static void testConvertTo(Type type, Expression from, Type to, Expression expected) throws AppendableException {
		Expression converted = type.convertTo(from, to);

		if (!converted.equals(expected)) {
			fail(type + ".convertTo(" + from + ", " + to + ") failed got " + converted + " expected " + expected);
		}
	}

	static void testRemoveRepresentationInfo(Type type, Type expected) {
		Type got = type.removeRepresentationInfo();
		if (!got.equals(expected)) {
			fail(type + ".removeRepresentationInfo() expected " + expected + " got " + got);
		}
	}

	static void testApply(Type type, Substitution s, Type expected) throws AppendableException {
		Type got = type.apply(s);
		if (!got.equals(expected)) {
			fail(type + ".apply(" + s + ") expected " + expected + " got " + got);
		}
	}

	static void testSubstUnion(Substitution subst1, Substitution subst2, Substitution expected)
			throws AppendableException {
		Substitution composed = subst1.union(subst2);
		if (!composed.equals(expected)) {
			fail(subst1 + ".compose(" + subst2 + ") yields " + composed + " expected " + expected);
		}
	}

	static void testUnify(Type first, Type second, Substitution expected) throws AppendableException {
		Substitution s = Type.unify(first, second);
		if (!s.equals(expected)) {
			fail("Type.unify(" + first + ", " + second + ") got " + s + " expected " + expected);
		}
	}
}
