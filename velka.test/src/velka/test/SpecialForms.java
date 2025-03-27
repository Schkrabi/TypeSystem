package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.exceptions.UserException;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;

class SpecialForms extends VelkaTest {

private Environment env;
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}

	@Test
	void testExceptionExpr() {
		assertThrows(RuntimeException.class,
				() -> {
					this.assertInterpretationEquals("(error \"test\")", null);
				});
		
		assertThrows(RuntimeException.class,
				() -> {
					this.assertJExprEquals(null, "(error \"test\")", env);
				});
	}

	@Test
	void testAnd() {
		this.assertVelkaCode(
				"(and #t #f)",
				false);
		
		this.assertVelkaCode(
				"(and (< 4 5) (> 5 4))", 
				true);
		
		this.assertVelkaCode(
				"(and (< 4 5) (< 5 4))",
				false);
	}
	
	@Test
	void testCanDeconstructAs() {
		this.assertVelkaCode(
				"(can-deconstruct-as (construct Int:Roman \"XLII\") String:Native)",
				true);
	}
	
	@Test
	void testDeconstructAs() {
		this.assertVelkaCode(
				"(deconstruct (construct Int:Roman \"XLII\") String:Native)",
				"XLII");
	}
	
	@Test
	void testDefConstructor() {
		this.assertVelkaCode(
				"(let ((tmp (constructor T:R ((String:Native s)) s))"
				+ "(v (construct T:R \"foo\")))"
				+ "(deconstruct v String:Native))",
				"foo");
	}
	
	@Test
	void testDefConversion() {
		this.assertVelkaCode(
				"(let ((tmp (constructor T:R ((String:Native s)) s))"
				+ "(tmp2 (constructor T:R2 ((String:Native s)) s))"
				+ "(tmp3 (conversion T:R T:R2 (e) (construct T:R2 \"bar\")))"
				+ "(v (construct T:R \"foo\")))"
				+ "(deconstruct (convert T:R T:R2 v) String:Native))",
				"bar");
	}
	
	@Test
	void testGet() {
		this.assertVelkaCode(
				"(let ((t (tuple 1 \"foo\")))"
				+ "(get t 0))",
				1);
	}
	
	@Test
	void testInstanceOf() {
		this.assertVelkaCode(
				"(instance-of 1 Int:Roman)",
				true);
		
		this.assertVelkaCode(
				"(instance-of \"foo\" Int:Native)",
				false);
	}
	
	@Test
	void testInstanceOfRepresentation() {
		this.assertVelkaCode(
				"(instance-of-representation 1 Int:Native)",
				true);
		
		this.assertVelkaCode(
				"(instance-of-representation 1 Int:Roman)",
				false);
	}
	
	@Test
	void testOr() {
		this.assertVelkaCode(
				"(or #t #f)",
				true);
	}
}
