package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.ListNative;
import velka.core.langbase.TreeMap;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;

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
//		assertAll(() ->
//		{
			Expression e = this.parseString("(map-tree-put (construct Map:Tree (lambda (x y) -1)) 1 \"foo\")").get(0);
			e.interpret(this.env);
//		});
		
		this.assertIntprtAndCompPrintSameValues("(map-tree-put (construct Map:Tree (lambda (x y) -1)) 1 \"foo\")");
	}
	
	@Test
	void testGet() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(map-tree-get m 1))",
				new LitString("foo"),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(map-tree-get (construct Map:Tree (lambda (x y) -1)) 42)").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (map-tree-get m 1)))");
	}

	@Test
	void testCeilingEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(map-tree-ceiling-entry m 1))",
				new Tuple(new LitInteger(1), new LitString("foo")),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(map-tree-ceiling-entry (construct Map:Tree (lambda (x y) -1)) 42)").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (map-tree-ceiling-entry m 1)))");
	}
	
	@Test
	void testCeilingKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(map-tree-ceiling-key m 1))",
				new LitInteger(1),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(map-tree-ceiling-key (construct Map:Tree (lambda (x y) -1)) 42)").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (map-tree-ceiling-key m 1)))");
	}
	
	@Test
	void testContainsKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.containsKeySymbol_out + " m 1))",
				LitBoolean.TRUE,
				this.env);
		
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.containsKeySymbol_out + " m 42))",
				LitBoolean.FALSE,
				this.env);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.containsKeySymbol_out + " m 1)))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.containsKeySymbol_out + " m 42)))");
	}
	
	@Test
	void testContainsValue() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.containsValueSymbol_out + " m \"foo\"))",
				LitBoolean.TRUE,
				this.env);
		
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.containsValueSymbol_out + " m \"bar\"))",
				LitBoolean.FALSE,
				this.env);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.containsValueSymbol_out + " m \"foo\")))");
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.containsValueSymbol_out + " m \"bar\")))");
	}
	
	@Test
	void testFirstEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.firstEntrySymbol_out.toString() + " m))",
				new Tuple(new LitInteger(1), new LitString("foo")),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.firstEntrySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println ("+ TreeMap.firstEntrySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testFirstKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.firstKeySymbol_out.toString() + " m))",
				new LitInteger(1),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.firstKeySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.firstKeySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testFloorEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.floorEntrySymbol_out.toString() + " m 1))",
				new Tuple(new LitInteger(1), new LitString("foo")),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.floorEntrySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)) 42)").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.floorEntrySymbol_out.toString() + " m 1)))");
	}
	
	@Test
	void testFloorKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.floorKeySymbol_out.toString() + " m 1))",
				new LitInteger(1),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.floorKeySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)) 42)").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.floorKeySymbol_out.toString() + " m 1)))");
	}
	
	@Test
	void testHeadMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 1))",
				new LitString("foo"),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\"))"
							+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
							+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapSymbol_out.toString() + " m 2)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 1)))");
	}
	
	@Test
	void testHeadMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #t)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\"))"
							+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #f)))"
							+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.headMapInclSymbol_out.toString() + " m 2 #t)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
	}
	
	@Test
	void testHigherEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.higherEntrySymbol_out.toString() + " m 1))",
				new Tuple(new LitInteger(2), new LitString("bar")),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\")))"
							+ "(" + TreeMap.higherEntrySymbol_out.toString() + " m 3))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.higherEntrySymbol_out.toString() + " m 1)))");
	}
	
	@Test
	void testHigherKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.higherKeySymbol_out.toString() + " m 1))",
				new LitInteger(2),
				this.env);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\")))"
					+ "(" + TreeMap.higherKeySymbol_out.toString() + " m 3))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(println (" + TreeMap.higherKeySymbol_out.toString() + " m 1)))");
	}
	
	@Test
	void testKeys() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.keysSymbol_out.toString() + " m))",
				ListNative.of(new LitInteger(1), new LitInteger(2), new LitInteger(3)),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(println (" + TreeMap.keysSymbol_out.toString() + " m)))");
	}
	
	@Test
	void testLasttEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.lastEntrySymbol_out.toString() + " m))",
				new Tuple(new LitInteger(1), new LitString("foo")),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.lastEntrySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println ("+ TreeMap.lastEntrySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testLastKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\")))"
				+ "(" + TreeMap.lastKeySymbol_out.toString() + " m))",
				new LitInteger(1),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(" + TreeMap.lastKeySymbol_out.toString() + " (construct Map:Tree (lambda (x y) -1)))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\")))"
						+ "(println (" + TreeMap.lastKeySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testLowerEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.lowerEntrySymbol_out.toString() + " m 3))",
				new Tuple(new LitInteger(2), new LitString("bar")),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\")))"
							+ "(" + TreeMap.lowerEntrySymbol_out.toString() + " m 1))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.lowerEntrySymbol_out.toString() + " m 3)))");
	}
	
	@Test
	void testLowerKey() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.lowerKeySymbol_out.toString() + " m 3))",
				new LitInteger(2),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\")))"
					+ "(" + TreeMap.lowerKeySymbol_out.toString() + " m 1))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(println (" + TreeMap.lowerKeySymbol_out.toString() + " m 3)))");
	}
	
	@Test
	void testPollFirstEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.pollFirstEntrySymbol_out.toString() + " m))",
				new Tuple(new LitInteger(1), new LitString("foo")),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0))))))"
							+ "(" + TreeMap.pollFirstEntrySymbol_out.toString() + " m))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.pollFirstEntrySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testPollLastEntry() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.pollLastEntrySymbol_out.toString() + " m))",
				new Tuple(new LitInteger(3), new LitString("baz")),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0))))))"
							+ "(" + TreeMap.pollLastEntrySymbol_out.toString() + " m))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.pollLastEntrySymbol_out.toString() + " m)))");
	}
	
	@Test
	void testPutAll() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.putAllSymbol_out.toString() + " (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))) m)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.putAllSymbol_out.toString() + " (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))) m)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
	}
	
	@Test 
	void testRemove() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.removeSymbol_out + " m 2))",
				new LitString("bar"),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0))))))"
							+ "(" + TreeMap.removeSymbol_out.toString() + " m 2))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.removeSymbol_out + " m 2)))");
	}
	
	@Test
	void testSize() throws Exception{
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.sizeSymbol_out + " m))",
				new LitInteger(3),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
						+ "(println (" + TreeMap.sizeSymbol_out + " m)))");
	}
	
	@Test
	void testSubMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString(
							"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
									+ "(tmp (map-tree-put m 1 \"foo\"))"
									+ "(tmp (map-tree-put m 2 \"bar\"))"
									+ "(tmp (map-tree-put m 3 \"baz\"))"
									+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
									+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))")
							.get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.subMapInclSymbol_out.toString() + " m 2 #t 3 #f)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
	}
	
	@Test
	void testSubMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString(
							"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
									+ "(tmp (map-tree-put m 1 \"foo\"))"
									+ "(tmp (map-tree-put m 2 \"bar\"))"
									+ "(tmp (map-tree-put m 3 \"baz\"))"
									+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
									+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))")
							.get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.subMapSymbol_out.toString() + " m 2 3)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
	}
	
	@Test
	void testTailMap() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 3))",
				new LitString("baz"),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\"))"
							+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
							+ "(" + TreeMap.getSymbol_out.toString() + " m2 1))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.tailMapSymbol_out.toString() + " m 2)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 3)))");
	}
	
	@Test
	void testTailMapIncl() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
				+ "(tmp (map-tree-put m 1 \"foo\"))"
				+ "(tmp (map-tree-put m 2 \"bar\"))"
				+ "(tmp (map-tree-put m 3 \"baz\"))"
				+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #t)))"
				+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))",
				new LitString("bar"),
				this.env
				);
		
		assertThrows(
				UserException.class,
				() -> 
				{
					Expression e = this.parseString("(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
							+ "(tmp (map-tree-put m 1 \"foo\"))"
							+ "(tmp (map-tree-put m 2 \"bar\"))"
							+ "(tmp (map-tree-put m 3 \"baz\"))"
							+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #f)))"
							+ "(" + TreeMap.getSymbol_out.toString() + " m2 2))").get(0);
					e.interpret(this.env);
				});
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\"))"
						+ "(m2 (" + TreeMap.tailMapInclSymbol_out.toString() + " m 2 #t)))"
						+ "(println (" + TreeMap.getSymbol_out.toString() + " m2 2)))");
	}
	
	@Test
	void testValues() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(" + TreeMap.valuesSymbol_out.toString() + " m))",
				ListNative.of(new LitString("foo"), new LitString("bar"), new LitString("baz")),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((m (construct Map:Tree (lambda (x y) (if (< x y) -1 (if (< y x) 1 0)))))"
						+ "(tmp (map-tree-put m 1 \"foo\"))"
						+ "(tmp (map-tree-put m 2 \"bar\"))"
						+ "(tmp (map-tree-put m 3 \"baz\")))"
				+ "(println (" + TreeMap.valuesSymbol_out.toString() + " m)))");
	}
}
