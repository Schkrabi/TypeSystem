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
import velka.types.TypeAtom;

class HashSet extends VelkaTest {

private Environment env;
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}
	
	@Test
	void testConstructor() throws Exception {
		Expression e = this.parseString("(construct Set:Hash)").get(0);
		e.interpret(this.env);
	
		this.assertIntprtAndCompPrintSameValues("(construct Set:Hash)");
	}

	
	@Test
	void testFromList() throws Exception {
		this.assertInterpretationEquals("(set-hash-from-list (list 1 2 3))",
				new LitInteropObject(
						new java.util.HashSet<Object>(
								List.of(1l, 2l, 3l)),
						TypeAtom.TypeSetHash));
		
		this.assertIntprtAndCompPrintSameValues("(println (set-hash-from-list (list 1 2 3)))");
	}
	
	@Test
	void testAdd() throws Exception {
		this.assertInterpretationEquals(
				"(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add s 3)))" + "s)",
				new LitInteropObject(
						new java.util.HashSet<Object>(
								List.of(1l, 2l, 3l)),
						TypeAtom.TypeSetHash));
		
		this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add s 3)))" + "(println s))");
	}
	
	@Test
	void testClear() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-clear s)))" + "s)",
	            new LitInteropObject(
	                    new java.util.HashSet<Object>(), // Expected empty set after clear
	                    TypeAtom.TypeSetHash));
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-clear s)))" + "(println s))");
	}

	@Test
	void testContains() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-contains s 2))",
	            LitBoolean.TRUE);
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3))))" + "(println (set-hash-contains s 2)))");
	}

	@Test
	void testIsEmpty() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list))))" + "(set-hash-is-empty s))",
	            LitBoolean.TRUE); // Expecting true for empty set
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list))))" + "(println (set-hash-is-empty s)))");
	}

	@Test
	void testRemove() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-remove s 2)))" + "s)",
	            new LitInteropObject(
	                    new java.util.HashSet<Object>(List.of(1l, 3l)), // Expected set after removing '2'
	                    TypeAtom.TypeSetHash));
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3)))" + "(tmp (set-hash-remove s 2)))" + "(println s))");
	}

	@Test
	void testSize() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-size s))",
	            new LitInteger(3l)); // Expecting size of 3
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3))))" + "(println (set-hash-size s)))");
	}

	@Test
	void testAddAll() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add-all s (list 3 4))))" + "s)",
	            new LitInteropObject(
	                    new java.util.HashSet<Object>(List.of(1l, 2l, 3l, 4l)), // Expected set after adding all
	                    TypeAtom.TypeSetHash));
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2)))" + "(tmp (set-hash-add-all s (list 3 4))))" + "(println s))");
	}

	@Test
	void testContainsAll() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3))))" + "(set-hash-contains-all s (list 1 2)))",
	            LitBoolean.TRUE); // Expecting true as 1 and 2 are in the set
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3))))" + "(println (set-hash-contains-all s (list 1 2))))");
	}

	@Test
	void testRemoveAll() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-remove-all s (list 2 3))))" + "s)",
	            new LitInteropObject(
	                    new java.util.HashSet<Object>(List.of(1l, 4l)), // Expected set after removing 2 and 3
	                    TypeAtom.TypeSetHash));
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-remove-all s (list 2 3))))" + "(println s))");
	}

	@Test
	void testRetainAll() throws Exception {
	    this.assertInterpretationEquals(
	            "(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-retain-all s (list 2 3))))" + "s)",
	            new LitInteropObject(
	                    new java.util.HashSet<Object>(List.of(2l, 3l)), // Expected set after retaining 2 and 3
	                    TypeAtom.TypeSetHash));
	    
	    this.assertIntprtAndCompPrintSameValues("(let ((s (set-hash-from-list (list 1 2 3 4)))" + "(tmp (set-hash-retain-all s (list 2 3))))" + "(println s))");
	}
	
	@Test
	void testToList() throws Exception {
		this.assertInterpretationEquals(
				"(set-hash-to-list (set-hash-from-list (list 1 2 3)))",
				new LitInteropObject(
						List.of(new LitInteger(1l), new LitInteger(2l), new LitInteger(3l)),
						TypeAtom.TypeListNative));
		
		this.assertIntprtAndCompPrintSameValues("(println (set-hash-to-list (set-hash-from-list (list 1 2 3))))");
	}

	@Test
	void testLargest() throws Exception {
		this.assertInterpretationEquals(
				"(set-hash-largest (set-hash-from-list (list 1 2 3)) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))",
				new LitInteger(3));
		this.assertIntprtAndCompPrintSameValues("(println  (set-hash-largest (set-hash-from-list (list 1 2 3)) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))");
		
		this.assertInterpretationEquals(
				"(set-hash-largest (set-hash-from-list (list)) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))", 
				Expression.EMPTY_EXPRESSION);
		
		this.assertIntprtAndCompPrintSameValues("(println (set-hash-largest (set-hash-from-list (list)) (lambda (x y) (if (= x y) 0 (if (< x y) -1 1)))))");
	}
}
