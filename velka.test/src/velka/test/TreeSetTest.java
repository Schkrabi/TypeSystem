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
import velka.core.langbase.JavaLinkedList;
import velka.core.literal.LitBoolean;
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
		this.assertVelkaCode(
				"(construct Set:Tree (lambda (x y) -1))",
				Set.of());
	}
	
	@Test
	void testCopyConstructor() throws Exception{		
		var ts = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		
		this.assertVelkaCode(
				"(construct Set:Tree (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))",
				ts);
	}
	
	@Test
	void testAdd() throws Exception {
		var ts = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		ts.add(12);
		
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
				+ "(tmp (set-tree-add s 12)))"
				+ "s)",
				ts);
	}
	
	@Test
	void testAddAll() throws Exception {
		var ts = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		ts.addAll(List.of(12, 15, 18));
		
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
				+ "(tmp (set-tree-add-all s (list 12 15 18))))"
				+ "s)",
				ts);
	}
	
	@Test
    void testCeiling() throws Exception {
		var ts = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-ceiling s 4))",
				ts.ceiling(4));
    }
	
	@Test
	void testToBitSet() throws Exception {
		var bs = new java.util.BitSet();
		bs.set(3);
		bs.set(6);
		bs.set(9);
		
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(convert Set:Tree Set:BitSet s))",
				bs);
	}
	
	@Test
	void testIntersect() throws Exception {
		var ts1 = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		var ts2 = new java.util.TreeSet<Object>(Set.of(6, 9, 12));
		ts1.retainAll(ts2);
		
		this.assertVelkaCode(
				"(let ((s1 (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
				+ "(s2 (set-tree-from-list (list 6 9 12) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-intersect s1 s2))",
				ts1);
	}
	
	@Test
	void testUnion() throws Exception {
		var ts1 = new java.util.TreeSet<Object>(Set.of(3, 6, 9));
		var ts2 = new java.util.TreeSet<Object>(Set.of(6, 9, 12));
		ts1.addAll(ts2);
		
		this.assertVelkaCode(
				"(let ((s1 (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1)))))"
				+ "(s2 (set-tree-from-list (list 6 9 12) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-union s1 s2))",
				ts1);
	}
	
	@Test
	void testMap() throws Exception {
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-map s (lambda (x) (+ x 1))))",
				io.vavr.collection.Stream.of(4, 7, 10));
	}
	
	@Test
	void testIsEmpty() throws Exception {
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-is-empty s))",
				Boolean.FALSE);
		
		this.assertVelkaCode(
				"(let ((s (construct Set:Tree (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-is-empty s))",
				Boolean.TRUE);
	}
	
	@Test
	void testToList() throws Exception {
		this.assertVelkaCode(
				"(let ((s (set-tree-from-list (list 3 6 9) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))))"
				+ "(set-tree-to-list s))",
				io.vavr.collection.Stream.of(3, 6, 9));
	}
	
	@Test
	void testFromList() throws Exception {
		var ts = new java.util.TreeSet<Object>(Set.of(1, 2, 3));
		
		this.assertVelkaCode(
				"(set-tree-from-list (list 1 2 3) (lambda (x y) (if (< x y) -1 (if (= x y) 0 1))))",
				ts);
	}
	
	@Test
	void testToHashSet() throws Exception {
		var hs = new java.util.HashSet<Object>(Set.of(1, 2, 3));
			
		this.assertVelkaCode(
			"(convert Set:Tree Set:Hash (set-tree-from-list (list 1 2 3) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))", 
			hs);
	}
	
	@Test
	void containsAll() throws Exception {
		this.assertVelkaCode("(set-tree-contains-all (set-tree-from-list (list 1 2 3) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))) (list 2 3))",
				Boolean.TRUE);
		this.assertVelkaCode("(set-tree-contains-all (set-tree-from-list (list 1 2 3) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))) (list 2 0))",
				Boolean.FALSE);
	}
	
	@Test
	void retainAll() throws Exception {
		this.assertVelkaCode("(set-tree-retain-all (set-tree-from-list (list 1 2 3) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))) (list 2 3))",
				new java.util.TreeSet<Object>(List.of(2, 3)));
	}
	
	@Test
	void removeAll() throws Exception {
		this.assertVelkaCode("(set-tree-remove-all (set-tree-from-list (list 1 2 3) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))) (list 2 3))",
				new java.util.TreeSet<Object>(List.of(1)));
	}
}
