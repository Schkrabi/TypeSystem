package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;

class TestOperators extends VelkaTest{

	private Environment env;
	private TypeEnvironment typeEnv;
	
	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
		env = Environment.initTopLevelEnvironment();
		typeEnv = TypeEnvironment.initBasicTypes(env);
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testTypeStr() throws Exception {
		this.assertInterpretedStringEquals(
				"(type-str 42)",
				new LitString("Int:*"),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues("(println (type-str 42))");
	}
	
	@Test
	void testRepresentationStr() throws Exception {
		this.assertInterpretedStringEquals(
				"(representation-str 42)",
				new LitString("Int:Native"),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues("(println (representation-str 42))");
	}
	
	@Test
	void testSubstr() throws Exception {
		this.assertInterpretedStringEquals(
				"(substr \"hamburger\" 4 8)",
				new LitString("urge"),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues("(println (substr \"hamburger\" 4 8))");
	}
	
	@Test
	void testStrlen() throws Exception {
		this.assertInterpretedStringEquals(
				"(strlen \"foo\")",
				new LitInteger(3),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues("(println (strlen \"foo\"))");
	}

}
