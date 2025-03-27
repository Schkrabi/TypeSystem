package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.application.List;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.ListNative;
import velka.core.langbase.TreeMap;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.java.runtime.TypedObject;
import velka.java.runtime.VelkaTuple;
import velka.types.TypeAtom;

class TestTreeMap extends VelkaTest {
	
	private Environment env;

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testConstructor() throws Exception {
//		assertAll(() ->
//		{
			Expression e = this.parseString("(construct Map:Tree (lambda (x y) -1))").get(0);
			e.interpret(this.env);
//		});
		
		this.assertIntprtAndCompPrintSameValues("(construct Map:Tree (lambda (x y) -1))");
	}
	
	@Test
	void testPut() throws Exception {				
		this.assertVelkaCode(
				"(map-tree-put (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))) 1 \"foo\")",
				new java.util.TreeMap<Integer, String>(Map.of(1, "foo")));
	}
	
	@Test
	void testGet() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(map-tree-get m 1))",
				new LitString("foo"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (map-tree-get m 1)))");
		
		this.assertJExprEquals("foo", 
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(map-tree-get m 1))", 
				env);
	}

	@Test
	void testCeilingEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(car (map-tree-ceiling-entry m 1)))",
				tm.ceilingEntry(1).getKey());
	}
	
	@Test
	void testCeilingKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-ceiling-key m 1))",
				tm.ceilingKey(1));
	}
	
	@Test
	void testContainsKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(map-tree-contains-key m 1))",
				tm.containsKey(1));
	}
	
	@Test
	void testContainsValue() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-contains-value m \"foo\"))",
				tm.containsValue("foo"));
	}
	
	@Test
	void testFirstEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(car (map-tree-first-entry m)))",
				tm.firstEntry().getKey());
	}
	
	@Test
	void testFirstKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-first-key m))",
				tm.firstKey());
	}
	
	@Test
	void testFloorEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-floor-entry m 1)))",
				tm.floorEntry(1).getKey());
	}
	
	@Test
	void testFloorKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-floor-key m 1))",
				tm.floorKey(1));
	}
	
	@Test
	void testHeadMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 1))",
				new LitString("foo"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 1)))");
		
		this.assertJExprEquals(
				"foo", 
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
						+ "(" + TreeMap.getSymbol_out.toString() + " m2 1))", 
				env);
	}
	
	@Test
	void testHeadMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #t)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #t)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
		
		this.assertJExprEquals(
				"bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #t)))"
						+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				env);
	}
	
	@Test
	void testHigherEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-higher-entry m 1)))",
				tm.higherEntry(1).getKey());
	}
	
	@Test
	void testHigherKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-higher-key m 1))",
				tm.higherKey(1));
	}
	
	@Test
	void testKeys() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-keys m))",
				new java.util.ArrayList<Object>(tm.keySet()));
	}
	
	@Test
	void testLasttEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-last-entry m)))",
				tm.lastEntry().getKey());
	}
	
	@Test
	void testLastKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-last-key m))",
				tm.lastKey());
	}
	
	@Test
	void testLowerEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-lower-entry m 3)))",
				tm.lowerEntry(3).getKey());
	}
	
	@Test
	void testLowerKey() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-lower-key m 3))",
				tm.lowerKey(3));
	}
	
	@Test
	void testPollFirstEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-first-entry m)))",
				tm.firstEntry().getKey());
	}
	
	@Test
	void testPollLastEntry() throws Exception {
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(car (map-tree-poll-last-entry m)))",
				tm.pollLastEntry().getKey());
	}
	
	@Test
	void testPutAll() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.putAllSymbol_out.toString() + " (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))) m)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.putAllSymbol_out.toString() + " (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))) m)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
		
		this.assertJExprEquals("bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.putAllSymbol_out.toString() + " (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))) m)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				this.env);
	}
	
	@Test 
	void testRemove() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.removeSymbol_out + " m 2))",
				new LitString("bar"),
				this.env
				);
		
		assertThrows(
				RuntimeException.class,
				() -> 
				{
					Expression e = this.parseString("(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0))))))"
							+ "(" + TreeMap.removeSymbol_out.toString() + " m 2))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.removeSymbol_out + " m 2)))");
		
		this.assertJExprEquals("bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.removeSymbol_out + " m 2))",
				this.env);
		
		assertThrows(
				RuntimeException.class,
				() -> 
				{
					this.assertJExprEquals(null,
							"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0))))))"
							+ "(" + TreeMap.removeSymbol_out.toString() + " m 2))",
							this.env);
				});
	}
	
	@Test
	void testSize() throws Exception{
		var tm = new java.util.TreeMap<Object, Object>(Map.of(1, "foo", 2, "bar", 3, "baz"));
		
		this.assertVelkaCode(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(map-tree-size m))",
				tm.size());
	}
	
	@Test
	void testSubMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
		
		this.assertJExprEquals("bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				this.env);
	}
	
	@Test
	void testSubMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
		
		this.assertJExprEquals("bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				this.env);
	}
	
	@Test
	void testTailMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))",
				new LitString("baz"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 3)))");
		
		this.assertJExprEquals("baz",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))",
				this.env);
	}
	
	@Test
	void testTailMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #t)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #t)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
		
		this.assertJExprEquals("bar",
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #t)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				this.env);
	}
	
	@Test
	void testValues() throws Exception {
		this.assertInterpretedStringEquals(
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.valuesSymbol_out.toString() + " m))",
				new LitInteropObject(java.util.List.of("foo", "bar", "baz"), TypeAtom.TypeListNative),
				this.env
				);
		
//		this.assertIntprtAndCompPrintSameValues(
//				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
//						+ "(tmp (map-tree-put m 1 \"foo\"))"
//						+ "(tmp (map-tree-put m 2 \"bar\"))"
//						+ "(tmp (map-tree-put m 3 \"baz\")))"
//				+ "(println (" + TreeMap.valuesSymbol_out.toString() + " m)))");
		
		this.assertJExprEquals(java.util.List.of("foo", "bar", "baz"),
				"(let ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.valuesSymbol_out.toString() + " m))",
				this.env);
	}
}
