package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.java.runtime.TypedObject;
import velka.types.TypeAtom;

class HashSet extends VelkaTest {

	private Environment env;
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}
	
	@Test
	void testConstructor() throws Exception {
		this.assertVelkaCode("(construct Set:Hash)",
				new java.util.HashSet<Object>());
	}
	
	@Test
	void testFromList() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		this.assertVelkaCode("(set-hash-from-list (list 1 2 3))",
				hs);
	}
	
	@Test
	void testAdd() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		this.assertVelkaCode(
				"(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add s 3)))" + "s)",
				hs);
	}
	
	@Test
	void testClear() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		hs.clear();
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-clear s)))" + "s)",
	            hs);
	}

	@Test
	void testContains() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-contains s 2))",
	            hs.contains(2));
	}

	@Test
	void testIsEmpty() throws Exception {
		var hs = new java.util.HashSet<Object>();
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list))))" + "(set-hash-is-empty s))",
	            hs.isEmpty());
	}

	@Test
	void testRemove() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		hs.remove(2);
		this.assertVelkaCode(        
				"(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-remove s 2)))" + "s)",
	            hs);
	}

	@Test
	void testSize() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-size s))",
	            hs.size());
	}

	@Test
	void testAddAll() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2));
		hs.addAll(List.of(3, 4));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add-all s (list 3 4))))" + "s)",
	            hs);
	}

	@Test
	void testContainsAll() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-contains-all s (list 1 2)))",
	            hs.containsAll(List.of(1, 2)));
	}

	@Test
	void testRemoveAll() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3, 4));
		hs.removeAll(List.of(2, 3));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-remove-all s (list 2 3))))" + "s)",
	            hs);
	}

	@Test
	void testRetainAll() throws Exception {
		var hs = new java.util.HashSet<Object>(List.of(1, 2, 3, 4));
		hs.retainAll(List.of(2, 3));
		this.assertVelkaCode(
	            "(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-retain-all s (list 2 3))))" + "s)",
	            hs);
	}
	
	@Test
	void testToList() throws Exception {
		this.assertVelkaCode(
				"(set-hash-to-list (set-hash-from-list (list 1 2 3)))",
				io.vavr.collection.Stream.of(1, 2, 3));
	}

	@Test
	void testLargest() throws Exception {
		this.assertVelkaCode(
				"(set-hash-largest (set-hash-from-list (list 1 2 3)) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))",
				Integer.valueOf(3));
	}
}
