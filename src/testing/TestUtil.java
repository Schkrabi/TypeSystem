package testing;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import expression.Expression;
import expression.LitBoolean;
import expression.LitInteger;
import util.AppendableException;
import util.ClojureCodeGenerator;
import util.NameGenerator;
import util.Pair;
import util.RomanNumbers;

class TestUtil {

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testAppendableException() {
		AppendableException e = new AppendableException("Test");

		e.appendMessage(" in One");
		e.appendMessage(" in two");

		e.getMessage();

		new AppendableException();
	}

	@Test
	public void testClojureCodeGeneratorBasic() throws IOException, Exception {
		new ClojureCodeGenerator();

		ClojureCodeGenerator.toClojureCode(
				Arrays.asList(new LitInteger(128), Expression.EMPTY_EXPRESSION, LitBoolean.TRUE), new StringWriter());
	}

	@Test
	public void testNameGenerator() {
		int size = 100000;

		new NameGenerator();

		Set<String> s = new TreeSet<String>();
		for (int i = 0; i < size; i++) {
			s.add(NameGenerator.next());
		}

		if (s.size() != size) {
			fail("Duplicated string found there is " + s.size() + " string instead of " + size);
		}
	}

	@Test
	public void testPair() {
		Pair<Object, Object> p = new Pair<Object, Object>(true, 128);

		p.toString();

		Object o = new Pair<Object, Object>(true, 128);
		if (!p.equals(o)) {
			fail(p.toString() + " and " + o.toString() + " are equal!");
		}
		o = new Pair<Object, Object>(true, true);
		if (p.equals(o)) {
			fail(p.toString() + " and " + o.toString() + " are not equal!");
		}
		o = new Pair<Object, Object>(128, 128);
		if (p.equals(o)) {
			fail(p.toString() + " and " + o.toString() + " are not equal!");
		}
		o = new Object();
		if (p.equals(o)) {
			fail(p.toString() + " and " + o.toString() + " are not equal!");
		}
	}

	@Test
	public void testRomanNumbers() throws Exception {
		new RomanNumbers();

		String r1989 = "MCMLXXXIX";
		if (!RomanNumbers.check(r1989)) {
			fail(r1989 + " is a roman number");
		}
		String fail = "fail";
		if (RomanNumbers.check(fail)) {
			fail(fail + " is not a roman number");
		}

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
			if (!p.first.equals(s)) {
				fail("Error: " + p.toString() + " != " + s);
			}
			Integer i = RomanNumbers.roman2int(p.first);
			if (!p.second.equals(i)) {
				fail("Error: " + p.second.toString() + " != " + i.toString());
			}
		}
		
		Assertions.assertThrows(AppendableException.class, () -> RomanNumbers.roman2int("fail!"));
		
		Assertions.assertThrows(AppendableException.class, () -> RomanNumbers.value('q'));
	}

}
