package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Stream;

import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.types.TypeAtom;
import velka.util.AppendableException;

class ListNativeTest extends VelkaTest {

	Environment env = TopLevelEnvironment.instantiate();

	@Test
    void testConstructEmptyListNative() throws Exception {
		this.assertVelkaCode("(construct List:Native)", io.vavr.collection.Stream.empty());
    }

    @Test
    void testConstructListNativeWithElement() throws Exception {
    	this.assertVelkaCode("(construct List:Native 42 (construct List:Native))",
    			io.vavr.collection.Stream.empty().prepend(42));
    }

    @Test
    void testIsListNativeEmpty() throws Exception {
    	this.assertVelkaCode(
    			"(is-list-native-empty (construct List:Native))", 
    			Boolean.TRUE);
    }

    @Test
    void testIsListNativeEmptyWithElement() throws Exception {
    	this.assertVelkaCode(
                "(is-list-native-empty (construct List:Native 42 (construct List:Native)))",
                Boolean.FALSE);
    }

    @Test
    void testHeadListNative() throws Exception {
    	this.assertVelkaCode("(list-native-head (construct List:Native 42 (construct List:Native)))",
                Integer.valueOf(42));
    }

    @Test
    void testHeadListNativeEmpty() throws Exception {
    	this.assertVelkaThrows("(list-native-head (construct List:Native))");
    }

    @Test
    void testTailListNative() throws Exception {
    	this.assertVelkaCode("(list-native-tail (construct List:Native 42 (construct List:Native)))",
                io.vavr.collection.Stream.empty());
    }

    @Test
    void testTailListNativeEmpty() throws Exception {
    	this.assertVelkaThrows("(list-native-tail (construct List:Native))");
    }

    @Test
    void testMapListNative() throws Exception {
    	this.assertVelkaCode(
                "(list-native-map (lambda (x) (+ x 1)) (construct List:Native 42 (construct List:Native)))",
                io.vavr.collection.Stream.of(43));
    }

    @Test
    void testMap2ListNative() throws Exception {
        this.assertVelkaCode(
                "(list-native-map2 + (construct List:Native 21 (construct List:Native 21 (construct List:Native))) (construct List:Native 21 (construct List:Native 21 (construct List:Native))))",
                io.vavr.collection.Stream.of(42, 42));
    }

    @Test
    void testFoldlListNative() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-foldl concat \"\" (list \"foo\" \"bar\" \"baz\"))",
                "bazbarfoo");
    }

    @Test
    void testAddToEndSymbol() throws Exception {
//        this.assertVelkaCode(
//                "(" + ListNative.addToEndSymbol_out + " (construct List:Native 21 (construct List:Native)) 42)",
//                List.of(21, 42));
    }

    @Test
    void testConvertToListJavaLinked() throws Exception {
        this.assertVelkaCode(
                "(convert List:Native List:JavaLinked (construct List:Native 42 (construct List:Native 21 (construct List:Native))))",
                new java.util.LinkedList<Object>(List.of(42, 21)));
    }

//    @Test
//    void testContainsListNativeTrue() throws Exception {
//        this.assertVelkaCode("(list-native-contains (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 42)",
//        		Boolean.TRUE);
//    }
//
//    @Test
//    void testContainsListNativeFalse() throws Exception {
//        this.assertVelkaCode(
//    	"(list-native-contains (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 84)",
//    	Boolean.FALSE);
//    }

    @Test
    void testFilterListNative() throws Exception {
        this.assertVelkaCode(
    		"(list-native-filter (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
                io.vavr.collection.Stream.of(true));
    }

    @Test
    void testGetListNative() throws Exception {
    	this.assertVelkaCode(
        	"(list-native-get (construct List:Native 42 (construct List:Native)) 0)",
        	Integer.valueOf(42));
    }

    @Test
    void testBuildListNative() throws Exception {
    	this.assertVelkaCode(
        	"(list-native-build 2 (lambda (x) x))", 
        	io.vavr.collection.Stream.of(0, 1));
    }

//    @Test
//    void testRemoveListNative() throws Exception {
//    	this.assertVelkaCode(
//    			"(list-native-remove (list 0 1) 1)", 
//    			List.of(0));
//    }

    @Test
    void testSizeListNative() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-size (list-native-build 42 (lambda (x) x)))", 
    			Integer.valueOf(42));
    }

    @Test
    void testAppendListNative() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-append (list 21) (list 42))", 
                io.vavr.collection.Stream.of(21, 42));
    }

    @Test
    void testReverseListNative() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-reverse (list 0 1 2))",
                io.vavr.collection.Stream.of(2, 1, 0));
    }

    @Test
    void testEverypListNativeTrue() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-everyp (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))",
                Boolean.TRUE);
    }

    @Test
    void testEverypListNativeFalse() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-everyp (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
                Boolean.FALSE);
    }
    
//    @Test
//    void testListSpecialForm() throws Exception {
//    	this.assertVelkaCode(
//    			"(list 1 2)", 
//    			List.of(1, 2));
//    }
    
//    @Test
//    void testAddToEndInPlace() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	l.add(0);
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2))"
//    			+ "(tmp (list-native-add-to-end-in-place l 0)))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testAddToIndexSymbol() throws Exception {
//    	var l = new ArrayList<Object>();
//    	l.add(0, 42);
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (construct List:Native))"
//    			+ "(tmp (list-native-add-to-index l 0 42)))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testAddAllSymbol() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (construct List:Native))"
//    			+ "(tmp (list-native-add-all l (list 1 2))))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testContainsAll() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2)))"    			
//    			+ "(list-native-contains-all l (list 1 2)))",
//    			l.containsAll(java.util.List.of(1, 2)));
//    }
    
//    @Test
//    void testIndexOf() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2)))"
//    			+ "(list-native-index-of l 1))",
//    			l.indexOf(1));
//    }
    
//    @Test
//    void testLastIndexOf() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2)))"
//    			+ "(list-native-last-index-of l 1))",
//    			l.lastIndexOf(1));
//    }
    
//    @Test
//    void testRemoveInPlace() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	l.remove((Object)Integer.valueOf(1));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2))"
//    			+ "(tmp (list-native-remove-in-place l 1)))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testRemoveAll() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	l.removeAll(java.util.List.of(1, 2));
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2))"
//    			+ "(tmp (list-native-remove-all l (list 1 2))))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testRetainAll() throws Exception {
//    	var l1 = new ArrayList<Object>(List.of(1, 2, 3));
//    	var l2 = new ArrayList<Object>(List.of(2, 3, 4));
//    	l1.retainAll(l2);
//    	
//    	this.assertVelkaCode(
//    			"(let ((l1 (list 1 2 3))"
//    			+ "(l2 (list 2 3 4))"
//    			+ "(tmp (list-native-retain-all l1 l2)))"
//    			+ "l1)",
//    			l1);
//    }
    
//    @Test
//    void testSet() throws Exception {
//    	var l = new ArrayList<Object>(java.util.List.of(1, 2));
//    	l.set(0, 42);
//    	
//    	this.assertVelkaCode(
//    			"(let ((l (list 1 2))"
//    			+ "(tmp (list-native-set l 0 42)))"
//    			+ "l)",
//    			l);
//    }
    
//    @Test
//    void testSubList() throws Exception {
//    	var l = new ArrayList<Object>(List.of(1, 2, 3, 4));
//    	
//    	this.assertVelkaCode("(let ((l (list 1 2 3 4))) (list-native-sublist l 1 2))",
//    			l.subList(1, 2));
//    }
    
    @Test
    void testFoldr() throws Exception {
    	this.assertVelkaCode(
    			"(list-native-foldr concat \"\" (list \"foo\" \"bar\" \"baz\"))",
    			"foobarbaz");
    }
}
