/**
 * 
 */
package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.types.TypeAtom;

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
	void testCopyConstructor() throws Exception{
		var ts = new LitInteropObject(new java.util.TreeSet<Object>(Set.of(3l, 6l, 9l)), TypeAtom.TypeSetTree);
		
		this.assertInterpretationEquals(
				"(let ((ts1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add ts1 3))"
				+ "(tmp (set-tree-add ts1 6))"
				+ "(tmp (set-tree-add ts1 9)))"
				+ "(construct Set:Tree ts1))", 
				ts);
		
		this.assertIntprtAndCompPrintSameValues("(let ((ts1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add ts1 3))"
				+ "(tmp (set-tree-add ts1 6))"
				+ "(tmp (set-tree-add ts1 9)))"
				+ "(println (construct Set:Tree ts1)))");
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
	
	@Test
	void testToBitSet() throws Exception {
		var bs = new java.util.BitSet();
		bs.set(3);
		bs.set(6);
		bs.set(9);
		var lio = new LitInteropObject(bs, TypeAtom.TypeSetBitSet);
		
		this.assertInterpretationEquals(
				"(let ((ts (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add ts 3))"
				+ "(tmp (set-tree-add ts 6))"
				+ "(tmp (set-tree-add ts 9)))"
				+ "(convert Set:Tree Set:BitSet ts))", 
				lio);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((ts (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
						+ "(tmp (set-tree-add ts 3))"
						+ "(tmp (set-tree-add ts 6))"
						+ "(tmp (set-tree-add ts 9)))"
						+ "(println (bit-set-str (convert Set:Tree Set:BitSet ts))))");
	}
}
