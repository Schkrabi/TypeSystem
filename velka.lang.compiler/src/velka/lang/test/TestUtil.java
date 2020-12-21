package velka.lang.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import velka.lang.abstraction.Lambda;
import velka.lang.application.AbstractionApplication;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.Symbol;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitInteger;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.exceptions.InvalidNumberOfArgumentsException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;
import velka.lang.util.RomanNumbers;
import velka.lang.util.ThrowingBinaryOperator;
import velka.lang.util.ThrowingConsumer;
import velka.lang.util.ThrowingFunction;
import velka.lang.exceptions.UnboundVariableException;

class TestUtil {

	@Test
	@DisplayName("AppendableException")
	void testAppendableException() {
		assertAll(() -> {
			AppendableException e = new AppendableException("Test");
			e.appendMessage(" in One");
			e.appendMessage(" in two");
			e.getMessage();
			new AppendableException();
		});
	}

	@Test
	@DisplayName("ClojureCodeGenerator")
	public void testClojureCodeGeneratorBasic() throws IOException, Exception {
		Environment env = Environment.initTopLevelEnvitonment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		assertAll(() -> new ClojureCodeGenerator(),
				() -> ClojureCodeGenerator.toClojureCode(
						Arrays.asList(new LitInteger(128), Expression.EMPTY_EXPRESSION, LitBoolean.TRUE),
						new StringWriter(), env, typeEnv));
	}

	@Test
	@DisplayName("NameGenerator")
	public void testNameGenerator() {
		int size = 100000;

		assertAll(() -> new NameGenerator());

		Set<String> s = new TreeSet<String>();

		for (int i = 0; i < size; i++) {
			assertAll(() -> s.add(NameGenerator.next()));
		}

		assertEquals(s.size(), size);
	}

	@Test
	@DisplayName("Pair")
	public void testPair() {
		assertAll(() -> {
			Pair<Object, Object> p = new Pair<Object, Object>(true, 128);
			p.toString();
			Object o = new Pair<Object, Object>(true, 128);
			assertEquals(p, o);
			o = new Pair<Object, Object>(true, true);
			assertNotEquals(p, o);
			o = new Pair<Object, Object>(128, 128);
			assertNotEquals(p, o);
			o = new Object();
			assertNotEquals(p, o);
		});
	}

	@Test
	@DisplayName("RomanNumbers")
	public void testRomanNumbers() throws Exception {
		assertAll(() -> new RomanNumbers());

		String r1989 = "MCMLXXXIX";
		assertTrue(RomanNumbers.check(r1989));

		String fail = "fail";
		assertFalse(RomanNumbers.check(fail));

		List<Pair<String, Integer>> ns = Arrays.asList(new Pair<String, Integer>("I", 1),
				new Pair<String, Integer>("II", 2), new Pair<String, Integer>("III", 3),
				new Pair<String, Integer>("IV", 4), new Pair<String, Integer>("V", 5),
				new Pair<String, Integer>("VI", 6), new Pair<String, Integer>("VII", 7),
				new Pair<String, Integer>("VIII", 8), new Pair<String, Integer>("IX", 9),
				new Pair<String, Integer>("X", 10), new Pair<String, Integer>("L", 50),
				new Pair<String, Integer>("XL", 40), new Pair<String, Integer>("C", 100),
				new Pair<String, Integer>("XC", 90), new Pair<String, Integer>("D", 500),
				new Pair<String, Integer>("CD", 400), new Pair<String, Integer>("M", 1000),
				new Pair<String, Integer>("CM", 900));

		for (Pair<String, Integer> p : ns) {
			String s = RomanNumbers.int2roman(p.second);
			assertEquals(p.first, s);
			Integer i = RomanNumbers.roman2int(p.first);
			assertEquals(p.second, i);
		}

		assertThrows(AppendableException.class, () -> RomanNumbers.roman2int("fail!"));

		assertThrows(AppendableException.class, () -> RomanNumbers.value('q'));
	}

	@Test
	@DisplayName("ThrowingFunction")
	void testThrowingFunction() {
		assertAll(() -> Arrays.asList(1, 2, 3, 4, 5).stream().map(ThrowingFunction.wrapper(x -> x))
				.collect(Collectors.toList()));
		assertThrows(RuntimeException.class,
				() -> Arrays.asList(1, 2, 3, 4, 5).stream().map(ThrowingFunction.wrapper(x -> {
					throw new AppendableException("test");
				})).collect(Collectors.toList()));
	}

	@Test
	@DisplayName("ThrowingBinaryOperator")
	void testThrowingBinaryOperator() {
		assertThrows(RuntimeException.class,
				() -> Arrays.asList(1, 2, 3, 4, 5).stream().reduce(0, ThrowingBinaryOperator.wrapper((x, y) -> {
					if (x < 5)
						return x + y;
					throw new AppendableException("test");
				})));
	}

	@Test
	@DisplayName("ThrowingConsumer")
	void testThrowingConsumer() {
		assertAll(() -> Arrays.asList(1, 2, 3, 4, 5).stream().forEach(ThrowingConsumer.wrapper(x -> {
			if (x < 5)
				x++;
		})));
		assertThrows(RuntimeException.class,
				() -> Arrays.asList(1, 2, 3, 4, 5).stream().forEach(ThrowingConsumer.wrapper(x -> {
					if (x < 5)
						x++;
					throw new AppendableException("test");
				})));
	}

	@Test
	@DisplayName("Exceptions")
	void testExceptions() {
		assertAll(() -> new UnboundVariableException(new Symbol("x")),
				() -> new InvalidNumberOfArgumentsException(2, Expression.EMPTY_EXPRESSION,
						new AbstractionApplication(
								new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE, Expression.EMPTY_EXPRESSION),
								Tuple.EMPTY_TUPLE)));
	}

	@Test
	@DisplayName("Sandbox (various experiments)")
	void testSandbox() throws ClassNotFoundException, InstantiationException, IllegalAccessException,
			NoSuchMethodException, SecurityException, IllegalArgumentException, InvocationTargetException {
		assertAll(() -> {
			Class<?> clazz = Class.forName("java.util.HashMap");
			Object inst = clazz.getDeclaredConstructor().newInstance();
			Method met = clazz.getMethod("put", Object.class, Object.class);
			Object[] args = { new String("key"), Integer.valueOf(42) };
			met.invoke(inst, args);
		});
	}

}
