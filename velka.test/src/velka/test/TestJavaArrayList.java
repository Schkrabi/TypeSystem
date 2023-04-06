package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.util.AppendableException;

class TestJavaArrayList extends VelkaTest {

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
	void testBuild() throws Exception {
		this.assertInterpretedStringEquals(
				"(java-array-list-build 5 (lambda (x) (+ x 1)))",
				new LitComposite(
						new LitInteropObject(new ArrayList<Expression>(Arrays.asList(
								new LitInteger(1), new LitInteger(2), new LitInteger(3), new LitInteger(4), new LitInteger(5)))),
						JavaArrayList.TypeListJavaArray),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues(
				"(println (java-array-list-get (java-array-list-build 5 (lambda (x) (+ x 1))) 2))");
	}

}
