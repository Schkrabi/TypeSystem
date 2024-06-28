package velka.test;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;

class TestOperators extends VelkaTest{

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
	void testTypeStr() throws Exception {
		this.assertInterpretedStringEquals(
				"(type-str 42)",
				new LitString("Int:*"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (type-str 42))");
	}
	
	@Test
	void testRepresentationStr() throws Exception {
		this.assertInterpretedStringEquals(
				"(representation-str 42)",
				new LitString("Int:Native"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (representation-str 42))");
	}
	
	@Test
	void testSubstr() throws Exception {
		this.assertInterpretedStringEquals(
				"(substr \"hamburger\" 4 8)",
				new LitString("urge"),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (substr \"hamburger\" 4 8))");
	}
	
	@Test
	void testStrlen() throws Exception {
		this.assertInterpretedStringEquals(
				"(strlen \"foo\")",
				new LitInteger(3),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (strlen \"foo\"))");
	}

	@Test
	void testLesserThanOrEquals() throws Exception {
		this.assertInterpretedStringEquals(
				"(<= 42 1)",
				LitBoolean.FALSE,
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (<= 42 1))");
	}
	
	@Test
	void testGreaterThan() throws Exception {
		this.assertInterpretedStringEquals(
				"(> 42 1)",
				LitBoolean.TRUE,
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (> 42 1))");
	}
	
	@Test
	void testGreaterThanOrEquals() throws Exception {
		this.assertInterpretedStringEquals(
				"(>= 42 1)",
				LitBoolean.TRUE,
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (>= 42 1))");
	}
	
	@Test
	void testMax() throws Exception {
		this.assertInterpretedStringEquals(
				"(max 42 1)",
				new LitInteger(42),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (max 42 1))");
	}
	
	@Test
	void testMin() throws Exception {
		this.assertInterpretedStringEquals(
				"(min 42 1)",
				new LitInteger(1),
				this.env);
		
		this.assertIntprtAndCompPrintSameValues("(println (min 42 1))");
	}
}
