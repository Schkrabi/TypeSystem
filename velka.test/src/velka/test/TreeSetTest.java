/**
 * 
 */
package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitInteger;

/**
 * 
 */
class TreeSetTest extends VelkaTest {

	private Environment env;
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}
	
	@Test
	void testConstructor() throws Exception {
		Expression e = this.parseString("(construct Set:Tree (lambda (x y) -1))").get(0);
		e.interpret(this.env);
	
		this.assertIntprtAndCompPrintSameValues("(construct Set:Tree (lambda (x y) -1))");
	}
	
	@Test
	void testAdd() throws Exception {
		var e = this.parseString("(set-tree-add (construct Set:Tree (lambda (x y) -1)) 42)").get(0);
		e.interpret(env);
		
		this.assertIntprtAndCompPrintSameValues("(println (set-tree-add (construct Set:Tree (lambda (x y) -1)) 42))");
	}
	
	@Test
	void testAddAll() throws Exception {
		var e = this.parseString("(set-tree-add-all (construct Set:Tree (lambda (x y) -1)) (build-list-native 5 (lambda (x) (* x x))))").get(0);
		e.interpret(env);
		
		this.assertIntprtAndCompPrintSameValues("(println (set-tree-add-all (construct Set:Tree (lambda (x y) -1)) (build-list-native 5 (lambda (x) (* x x)))))");
	}
	
	@Test
    void testCeiling() throws Exception {
        // Test ceiling method
        var ceilingExpr = this.parseString(
        		"(let ((set (construct Set:Tree (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
        		+ "(tmp (set-tree-add-all set (build-list-native 5 (lambda (x) (* x x))))))"
        		+ "(set-tree-ceiling set 4))").get(0);
        assertEquals(new LitInteger(4), ceilingExpr.interpret(env));

        // Assert that the interpreted and compiled versions print the same values
        this.assertIntprtAndCompPrintSameValues(
        		"(let ((set (construct Set:Tree (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
        		+ "(tmp (set-tree-add-all set (build-list-native 5 (lambda (x) (* x x))))))"
        		+ "(println (set-tree-ceiling set 4)))");
    }
}
