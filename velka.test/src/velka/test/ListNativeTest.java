package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;

import org.junit.jupiter.api.Test;

import velka.core.application.List;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.JavaArrayList;
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
	void testUtil() throws AppendableException {
		assertEquals(
				new LitInteropObject(new LinkedList<Expression>(
							Arrays.asList(new LitInteger(42), new LitInteger(21), new LitInteger(2))),
					TypeAtom.TypeListNative),
			ListNative.of(new LitInteger(42), new LitInteger(21), new LitInteger(2)).interpret(env));
	}

	@Test
    void testConstructEmptyListNative() throws Exception {
        this.assertInterpretedStringEquals("(construct List:Native)", ListNative.EMPTY_LIST_NATIVE, env);
        this.assertIntprtAndCompPrintSameValues("(println (construct List:Native))");
    }

    @Test
    void testConstructListNativeWithElement() throws Exception {
        this.assertInterpretedStringEquals("(construct List:Native 42 (construct List:Native))",
                ListNative.of(new LitInteger(42)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (construct List:Native 42 (construct List:Native)))");
    }

    @Test
    void testIsListNativeEmpty() throws Exception {
        this.assertInterpretedStringEquals("(is-list-native-empty (construct List:Native))", LitBoolean.TRUE, env);
        this.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List:Native)))");
    }

    @Test
    void testIsListNativeEmptyWithElement() throws Exception {
        this.assertInterpretedStringEquals(
                "(is-list-native-empty (construct List:Native 42 (construct List:Native)))", LitBoolean.FALSE, env);
        this.assertIntprtAndCompPrintSameValues("(println (is-list-native-empty (construct List:Native 42 (construct List:Native))))");
    }

    @Test
    void testHeadListNative() throws Exception {
        this.assertInterpretedStringEquals("(head-list-native (construct List:Native 42 (construct List:Native)))",
                new LitInteger(42), env);
        this.assertIntprtAndCompPrintSameValues("(println (head-list-native (construct List:Native 42 (construct List:Native))))");
    }

    @Test
    void testHeadListNativeEmpty() throws Exception {
        assertThrows(UserException.class,
                () -> this.assertInterpretedStringEquals("(head-list-native (construct List:Native))",
                        Expression.EMPTY_EXPRESSION, env));
    }

    @Test
    void testTailListNative() throws Exception {
        this.assertInterpretedStringEquals("(tail-list-native (construct List:Native 42 (construct List:Native)))",
                ListNative.EMPTY_LIST_NATIVE, env);
        this.assertIntprtAndCompPrintSameValues("(println (tail-list-native (construct List:Native 42 (construct List:Native))))");
    }

    @Test
    void testTailListNativeEmpty() throws Exception {
        assertThrows(UserException.class,
                () -> this.assertInterpretedStringEquals("(tail-list-native (construct List:Native))",
                        Expression.EMPTY_EXPRESSION, env));
    }

    @Test
    void testMapListNative() throws Exception {
        this.assertInterpretedStringEquals(
                "(map-list-native (lambda (x) (+ x 1)) (construct List:Native 42 (construct List:Native)))",
                ListNative.of(new LitInteger(43)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (map-list-native (lambda (x) (+ x 1)) (construct List:Native 42 (construct List:Native))))");
    }

    @Test
    void testMap2ListNative() throws Exception {
        this.assertInterpretedStringEquals(
                "(map2-list-native + (construct List:Native 21 (construct List:Native 21 (construct List:Native))) (construct List:Native 21 (construct List:Native 21 (construct List:Native))))",
                ListNative.of(new LitInteger(42), new LitInteger(42)).interpret(env),
                env);
        this.assertIntprtAndCompPrintSameValues("(println (map2-list-native + (construct List:Native 21 (construct List:Native 21 (construct List:Native))) (construct List:Native 21 (construct List:Native 21 (construct List:Native)))))");
    }

    @Test
    void testFoldlListNative() throws Exception {
        this.assertInterpretedStringEquals(
                "(foldl-list-native + 0 (construct List:Native 1 (construct List:Native 2 (construct List:Native))))",
                new LitInteger(3), env);
        this.assertIntprtAndCompPrintSameValues("(println (foldl-list-native + 0 (construct List:Native 1 (construct List:Native 2 (construct List:Native)))))");
    }

    @Test
    void testAddToEndSymbol() throws Exception {
        this.assertInterpretedStringEquals(
                "(" + ListNative.addToEndSymbol_out + " (construct List:Native 21 (construct List:Native)) 42)",
                ListNative.of(new LitInteger(21), new LitInteger(42)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (" + ListNative.headSymbol_out + "(" + ListNative.addToEndSymbol_out + " (construct List:Native 21 (construct List:Native)) 42)))");
    }

    @Test
    void testConvertToListJavaArray() throws Exception {
        ArrayList<Expression> al = new ArrayList<Expression>();
        al.add(new LitInteger(42));
        al.add(new LitInteger(21));
        this.assertInterpretedStringEquals(
                "(convert List:Native List:JavaArray (construct List:Native 42 (construct List:Native 21 (construct List:Native))))",
                new LitInteropObject(al, TypeAtom.TypeListJavaArray), env);
        this.assertIntprtAndCompPrintSameValues(
                "(define l (convert List:Native List:JavaArray (construct List:Native 42 (construct List:Native 21 (construct List:Native)))))"
                + "(println (" + JavaArrayList.getSymbol_out + " l 0))");
    }

    @Test
    void testConvertToListJavaLinked() throws Exception {
        LinkedList<Expression> ll = new LinkedList<Expression>();
        ll.add(new LitInteger(42));
        ll.add(new LitInteger(21));
        this.assertInterpretedStringEquals(
                "(convert List:Native List:JavaLinked (construct List:Native 42 (construct List:Native 21 (construct List:Native))))",
                new LitInteropObject(ll, TypeAtom.TypeListJavaLinked), env);
        this.assertIntprtAndCompPrintSameValues(
                "(define l (convert List:Native List:JavaLinked (construct List:Native 42 (construct List:Native 21 (construct List:Native)))))"
                + "(println (" + JavaLinkedList.getSymbol_out + " l 0))");
    }

    @Test
    void testContainsListNativeTrue() throws Exception {
        this.assertInterpretedStringEquals("(contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 42)", LitBoolean.TRUE, env);
        this.assertIntprtAndCompPrintSameValues("(println (contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 42))");
    }

    @Test
    void testContainsListNativeFalse() throws Exception {
        this.assertInterpretedStringEquals("(contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 84)", LitBoolean.FALSE, env);
        this.assertIntprtAndCompPrintSameValues("(println (contains-list-native (construct List:Native 42 (construct List:Native 21 (construct List:Native))) 84))");
    }

    @Test
    void testFilterListNative() throws Exception {
        this.assertInterpretedStringEquals("(filter-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
                ListNative.of(LitBoolean.TRUE).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (filter-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x)))");
    }

    @Test
    void testGetListNative() throws Exception {
        this.assertInterpretedStringEquals("(get-list-native (construct List:Native 42 (construct List:Native)) 0)", new LitInteger(42), env);
        this.assertIntprtAndCompPrintSameValues("(println (get-list-native (construct List:Native 42 (construct List:Native)) 0))");
    }

    @Test
    void testBuildListNative() throws Exception {
        this.assertInterpretedStringEquals("(build-list-native 2 (lambda (x) x))", ListNative.of(new LitInteger(0), new LitInteger(1)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (build-list-native 2 (lambda (x) x)))");
    }

    @Test
    void testRemoveListNative() throws Exception {
        this.assertInterpretedStringEquals("(remove-list-native (build-list-native 2 (lambda (x) x)) 1)", ListNative.of(new LitInteger(0)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (remove-list-native (build-list-native 2 (lambda (x) x)) 1))");
    }

    @Test
    void testSizeListNative() throws Exception {
        this.assertInterpretedStringEquals("(size-list-native (build-list-native 42 (lambda (x) x)))", new LitInteger(42), env);
        this.assertIntprtAndCompPrintSameValues("(println (size-list-native (build-list-native 42 (lambda (x) x))))");
    }

    @Test
    void testAppendListNative() throws Exception {
        this.assertInterpretedStringEquals("(append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42)))", 
                ListNative.of(new LitInteger(21), new LitInteger(42)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (append-list-native (build-list-native 1 (lambda (x) 21)) (build-list-native 1 (lambda (x) 42))))");
    }

    @Test
    void testReverseListNative() throws Exception {
        this.assertInterpretedStringEquals("(reverse-list-native (build-list-native 3 (lambda (x) x)))",
                ListNative.of(new LitInteger(2), new LitInteger(1), new LitInteger(0)).interpret(env), env);
        this.assertIntprtAndCompPrintSameValues("(println (reverse-list-native (build-list-native 3 (lambda (x) x))))");
    }

    @Test
    void testEverypListNativeTrue() throws Exception {
        this.assertInterpretedStringEquals("(everyp-list-native (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))",
                LitBoolean.TRUE, env);
        this.assertIntprtAndCompPrintSameValues("(everyp-list-native (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))");
    }

    @Test
    void testEverypListNativeFalse() throws Exception {
        this.assertInterpretedStringEquals("(everyp-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
                LitBoolean.FALSE, env);
        this.assertIntprtAndCompPrintSameValues("(everyp-list-native (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))");
    }
    
    @Test
    void testListSpecialForm() throws Exception {
    	this.assertInterpretationEquals("(list 1 2)", 
    			new LitInteropObject(java.util.List.of(new LitInteger(1), new LitInteger(2)), TypeAtom.TypeListNative));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (list 1 2))");
    }
}
