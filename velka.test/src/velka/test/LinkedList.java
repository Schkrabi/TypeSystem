 package velka.test;

import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.application.List;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.JavaLinkedList;
import velka.core.literal.Literal;
import velka.java.runtime.TypedObject;
import velka.java.runtime.VelkaTuple;
import velka.util.AppendableException;

class LinkedList extends VelkaTest {
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}
	
	
	
	@Test
	void constructor() throws Exception {
		this.assertVelkaCode("(construct List:JavaLinked)", new java.util.LinkedList<Object>());
	}
	
	@Test
	void copyConstructor() throws Exception {
		this.assertVelkaCode("(construct List:JavaLinked (list 1 2))",
				new java.util.LinkedList<Object>(java.util.List.of(1, 2)));
	}

	@Test
	void addToEnd() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.addToEndSymbol_out.toString() + " (construct List:JavaLinked) 42)",
				Boolean.TRUE);
	}
	
	@Test
	void addToIndex() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.addToIndexSymbol_out.toString() + " (construct List:JavaLinked) 0 42)",
				TypedObject.VELKA_EMPTY);
	}
	
	@Test
	void addAll() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.addToEndSymbol_out + " (construct List:JavaLinked) (list 42 42 1 2))",
				Boolean.TRUE);
		
	}
	
	@Test
	void toStr() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.toStrSymbol_out + " (construct List:JavaLinked (list 42 42)))",
				java.util.List.of(42, 42).toString());	
	}
	
	@Test 
	void contains() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.containsSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 2)",
				Boolean.TRUE);
		
		this.assertVelkaCode("(" + JavaLinkedList.containsSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 0)",
				Boolean.FALSE);
	}
	
	@Test
	void containsAll() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.containsAllSymbol_out + " (construct List:JavaLinked (list 1 2 3)) (list 2 3))",
				Boolean.TRUE);
		this.assertVelkaCode("(" + JavaLinkedList.containsAllSymbol_out + " (construct List:JavaLinked (list 1 2 3)) (list 2 0))",
				Boolean.FALSE);
	}
	
	@Test
	void get() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.getSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 0)",
				Integer.valueOf(1));
	}
	
	@Test
	void indexOf() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.indexOfSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 1)",
				Integer.valueOf(0));
	}
	
	@Test 
	void isEmpty() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.isEmpty + " (construct List:JavaLinked (list 1 2 3)))",
				Boolean.FALSE);
		
		this.assertVelkaCode("(" + JavaLinkedList.isEmpty + " (construct List:JavaLinked))",
				Boolean.TRUE);
	}
	
	@Test
	void lastIndexOf() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.lastIndexOfSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 1)",
				Integer.valueOf(0));
	}
	
	@Test
	void remove() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.removeSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 1)",
				Boolean.TRUE);
	}
	
	@Test
	void removeAll() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.removeAllSymbol_out + " (construct List:JavaLinked (list 1 2 3)) (list 1 2))",
				new java.util.LinkedList<>(java.util.List.of(3)));
	}
	
	@Test
	void retainAll() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.retainAllSymbol_out + " (construct List:JavaLinked (list 1 2 3)) (list 1 2))",
				new java.util.LinkedList<>(java.util.List.of(1, 2)));
	}
	
	@Test
	void set() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.setSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 0 42)",
				Integer.valueOf(1));
	}
	
	@Test
	void size() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.sizeSymbol_out + " (construct List:JavaLinked (list 1 2 3)))",
				Integer.valueOf(3));
	}
	
	@Test
	void sublist() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.sublistSymbol_out + " (construct List:JavaLinked (list 1 2 3)) 0 2)",
				new java.util.LinkedList<Object>(java.util.List.of(1, 2)));
	}
	
	@Test
	void map() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.mapSymbol_out + " (construct List:JavaLinked (list 1 2 3)) (lambda (x) (+ x 1)))",
				new java.util.LinkedList<Object>(java.util.List.of(2, 3, 4)));
	}
	
	@Test
	void map2() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.map2Symbol_out + " (construct List:JavaLinked (list 1 2 3)) (construct List:JavaLinked (list 1 2 3)) (lambda (x y) (+ x y)))",
				new java.util.LinkedList<Object>(java.util.List.of(2, 4, 6)));
	}
	
	@Test 
	void foldl() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.foldlSymbol_out + " (lambda (x y) (+ x y)) 0 (construct List:JavaLinked (list 1 2 3)))",
				Integer.valueOf(6));
	}
	
	@Test
	void foldr() throws Exception {
		this.assertVelkaCode("(" + JavaLinkedList.foldrSymbol_out + " / 16 (construct List:JavaLinked (list 1 2 4)))",
				Integer.valueOf(2));
	}
	
	@Test
	void convertToListNative() throws Exception {
		this.assertVelkaCode("(convert List:JavaLinked List:Native (construct List:JavaLinked (list 1 2)))",
				io.vavr.collection.Stream.of(1, 2));
	}
	
	@Test
	void everyp() throws Exception {
		this.assertVelkaCode("(java-linked-list-everyp (construct List:JavaLinked (list #t #f)) (lambda (x) x))",
				Boolean.FALSE);
		this.assertVelkaCode("(java-linked-list-everyp (construct List:JavaLinked (list #t #t)) (lambda (x) x))",
				Boolean.TRUE);
	}
	
	@Test
	void itNext() throws Exception {
		var pos = 0;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-next it))",
				it.next());
	}
	
	@Test
	void itAdd() throws Exception {
		var pos = 0;
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-add it 10))",
				TypedObject.VELKA_EMPTY);
	}
	
	@Test
	void itHasNext() throws Exception {
		var pos = 0;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-has-next it))",
				it.hasNext());
	}
	
	@Test
	void itHasPrevious() throws Exception {
		var pos = 3;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-has-previous it))",
				it.hasPrevious());
	}
	
	@Test
	void itNextIndex() throws Exception {
		var pos = 3;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-next-index it))",
				Integer.valueOf(it.nextIndex()));
	}
	
	@Test
	void itPrev() throws Exception {
		var pos = 3;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-previous it))",
				it.previous());
	}
	
	@Test
	void itPrevIndex() throws Exception {
		var pos = 3;
		var l = new java.util.LinkedList<Object>(java.util.List.of(0, 2, 4, 6, 8));
		var it = l.listIterator(pos);
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + ")))"
				+ "(list-iterator-previous-index it))",
				Integer.valueOf(it.previousIndex()));
	}
	
	@Test
	void itRemove() throws Exception {
		var pos = 3;
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
				+ "(it (java-linked-list-iterator l " + pos + "))"
						+ "(tmp (list-iterator-next it)))"
				+ "(list-iterator-remove it))",
				TypedObject.VELKA_EMPTY);
	}
	
	@Test
	void itSet() throws Exception {
		var pos = 3;
		
		this.assertVelkaCode(
				"(let ((l (construct List:JavaLinked (list 0 2 4 6 8)))"
						+ "(it (java-linked-list-iterator l " + pos + "))"
								+ "(tmp (list-iterator-next it)))"
						+ "(list-iterator-set it 42))",
				TypedObject.VELKA_EMPTY);
	}
}
