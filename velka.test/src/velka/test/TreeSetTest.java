/**
 * 
 */
package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitDouble;
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
		
		this.assertInterpretationEquals("(conversion-cost (lambda ((Set:BitSet x)) x) (tuple (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))))", new LitDouble(0.8d));
		this.assertInterpretationEquals("(let ((s (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add-all s (build-list-native 1000 (lambda (x) x)))))"
				+ "(conversion-cost (lambda ((Set:BitSet x)) x) (tuple s)))", new LitDouble(0.5d));
		
		this.assertIntprtAndCompPrintSameValues("(println (conversion-cost (lambda ((Set:BitSet x)) x) (tuple (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))))");
		this.assertIntprtAndCompPrintSameValues("(let ((s (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add-all s (build-list-native 1000 (lambda (x) x)))))"
				+ "(println (conversion-cost (lambda ((Set:BitSet x)) x) (tuple s))))");
	}
	
	@Test
	void testIntersect() throws Exception {
		var s = new LitInteropObject(new java.util.TreeSet<Object>(List.of(2l, 3l)), TypeAtom.TypeSetTree);
		
		this.assertInterpretationEquals(
				"(let ((s1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s1 1))"
				+ "(tmp (set-tree-add s1 2))"
				+ "(tmp (set-tree-add s1 3))"
				+ "(s2 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s2 2))"
				+ "(tmp (set-tree-add s2 3))"
				+ "(tmp (set-tree-add s2 4)))"
				+ "(set-tree-intersect s1 s2))", 
				s);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((s1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s1 1))"
				+ "(tmp (set-tree-add s1 2))"
				+ "(tmp (set-tree-add s1 3))"
				+ "(s2 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s2 2))"
				+ "(tmp (set-tree-add s2 3))"
				+ "(tmp (set-tree-add s2 4)))"
				+ "(println (set-tree-intersect s1 s2)))");
	}
	
	@Test
	void testUnion() throws Exception {
		var s = new LitInteropObject(new java.util.TreeSet<Object>(List.of(1l, 2l, 3l, 4l)), TypeAtom.TypeSetTree);
		
		this.assertInterpretationEquals(
				"(let ((s1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s1 1))"
				+ "(tmp (set-tree-add s1 2))"
				+ "(tmp (set-tree-add s1 3))"
				+ "(s2 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s2 2))"
				+ "(tmp (set-tree-add s2 3))"
				+ "(tmp (set-tree-add s2 4)))"
				+ "(set-tree-union s1 s2))", 
				s);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((s1 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s1 1))"
				+ "(tmp (set-tree-add s1 2))"
				+ "(tmp (set-tree-add s1 3))"
				+ "(s2 (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add s2 2))"
				+ "(tmp (set-tree-add s2 3))"
				+ "(tmp (set-tree-add s2 4)))"
				+ "(println (set-tree-union s1 s2)))");
	}
	
	@Test
	void testMap() throws Exception {
		this.assertInterpretationEquals(
				"(let ((s (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add-all s (build-list-native 5 (lambda (x) x)))))"
				+ "(set-tree-map s (lambda (x) (+ x 1))))", 
				new LitInteropObject(new java.util.TreeSet<Expression>(
						List.of(new LitInteger(1), new LitInteger(2), new LitInteger(3), new LitInteger(4), new LitInteger(5))), 
						TypeAtom.TypeSetTree));
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((s (construct Set:Tree (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))"
				+ "(tmp (set-tree-add-all s (build-list-native 5 (lambda (x) x)))))"
				+ "(println (set-tree-map s (lambda (x) (+ x 1)))))");
	}
}
