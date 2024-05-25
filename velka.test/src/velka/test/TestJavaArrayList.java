package velka.test;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.types.TypeAtom;

class TestJavaArrayList extends VelkaTest {

	private Environment env;
	private TypeEnvironment typeEnv;
	
	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
		env = Environment.initTopLevelEnvironment();
		typeEnv = TypeEnvironment.initBasicTypes(env);
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testBuild() throws Exception {
		this.assertInterpretedStringEquals(
				"(java-array-list-build 5 (lambda (x) (+ x 1)))",
						new LitInteropObject(new ArrayList<Expression>(Arrays.asList(
								new LitInteger(1), new LitInteger(2), new LitInteger(3), new LitInteger(4), new LitInteger(5))),
						TypeAtom.TypeListJavaArray),
				this.env,
				this.typeEnv);
		
		this.assertIntprtAndCompPrintSameValues(
				"(println (java-array-list-get (java-array-list-build 5 (lambda (x) (+ x 1))) 2))");
	}

	
	@Test
    void testConstructEmptyJavaArrayList() throws Exception {
        this.assertInterpretationEquals("(construct List:JavaArray)",
                new LitInteropObject(new ArrayList<Object>(), TypeAtom.TypeListJavaArray));
        this.assertIntprtAndCompPrintSameValues("(construct List:JavaArray)");
    }

    @Test
    void testConstructJavaArrayListWithElements() throws Exception {
        this.assertInterpretationEquals(
                "(construct List:JavaArray (build-list-native 2 (lambda (x) x)))", 
                new LitInteropObject(new ArrayList<Object>(Arrays.asList(new LitInteger(0), new LitInteger(1))), 
                TypeAtom.TypeListJavaArray));
        this.assertIntprtAndCompPrintSameValues("(construct List:JavaArray (build-list-native 2 (lambda (x) x)))");
    }

    @Test
    void testConstructJavaArrayListWithInitialCapacity() throws Exception {
        this.assertInterpretationEquals(
                "(construct List:JavaArray 42)",
                new LitInteropObject(new ArrayList<Object>(42), TypeAtom.TypeListJavaArray));
        this.assertIntprtAndCompPrintSameValues("(construct List:JavaArray 42)");
    }

    @Test
    void testAddToEndSymbol() throws Exception {
        ArrayList<Object> l = new ArrayList<Object>();
        l.add(new LitInteger(42));
        this.assertInterpretationEquals(
                "(" + JavaArrayList.addToEndSymbol_out.toString() + " (construct List:JavaArray) 42)", LitBoolean.TRUE);
        this.assertIntprtAndCompPrintSameValues("(println (" + JavaArrayList.addToEndSymbol_out.toString() + " (construct List:JavaArray) 42))");
    }

    @Test
    void testAddToIndexSymbol() throws Exception {
        this.assertInterpretationEquals(
                "(" + JavaArrayList.addToIndexSymbol_out.toString() + " (construct List:JavaArray) 0 42)",
                Expression.EMPTY_EXPRESSION);
        this.assertIntprtAndCompPrintSameValues("(println (" + JavaArrayList.addToIndexSymbol_out.toString() + " (construct List:JavaArray) 0 42))");
    }

    @Test
    void testAddAllSymbol() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(" + JavaArrayList.addAllSymbol_out + " l1 l2)",
                LitBoolean.TRUE);
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(println (" + JavaArrayList.addAllSymbol_out + " l1 l2))");
    }
    
    @Test
    void testInterpretationContains() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.containsSymbol_out + " l1 42)", LitBoolean.TRUE);
    }

    @Test
    void testIntprtAndCompPrintContains() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(println (" + JavaArrayList.containsSymbol_out + " l1 42))"
                + "(println (" + JavaArrayList.containsSymbol_out + " l1 84))");
    }

    @Test
    void testInterpretationContainsFalse() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 42)"
                + "(" + JavaArrayList.containsSymbol_out + " l1 84)", LitBoolean.FALSE);
    }

    @Test
    void testIntprtAndCompPrintContainsAll() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(println (" + JavaArrayList.containsAllSymbol_out + " l1 l2))"
                + "(println (" + JavaArrayList.containsAllSymbol_out + " l2 l1))");
    }

    @Test
    void testInterpretationGet() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.getSymbol_out + " l1 0)", new LitInteger(1));
    }

    @Test
    void testIntprtAndCompPrintGet() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(println (" + JavaArrayList.getSymbol_out + " l1 0))");
    }

    @Test
    void testInterpretationIndexOf() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.indexOfSymbol_out + " l1 1)", new LitInteger(0));
    }

    @Test
    void testIntprtAndCompPrintIndexOf() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(println (" + JavaArrayList.indexOfSymbol_out + " l1 1))"
                + "(println (" + JavaArrayList.indexOfSymbol_out + " l1 42))");
    }

    @Test
    void testInterpretationIsEmpty() throws Exception {
        this.assertInterpretationEquals("(" + JavaArrayList.isEmptySymbol_out + " (construct List:JavaArray))",
                LitBoolean.TRUE);
    }

    @Test
    void testIntprtAndCompPrintIsEmpty() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
                "(println (" + JavaArrayList.isEmptySymbol_out + " (construct List:JavaArray)))");
    }

    @Test
    void testInterpretationIsEmptyFalse() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.isEmptySymbol_out + " l1)", LitBoolean.FALSE);
    }

    @Test
    void testIntprtAndCompPrintIsEmptyDefined() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(println (" + JavaArrayList.isEmptySymbol_out + " l1))");
    }

    @Test
    void testInterpretationLastIndexOf() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.lastIndexOfSymbol_out + " l1 1)", new LitInteger(0));
    }

    @Test
    void testIntprtAndCompPrintLastIndexOf() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(println (" + JavaArrayList.lastIndexOfSymbol_out + " l1 1))"
                + "(println (" + JavaArrayList.lastIndexOfSymbol_out + " l1 42))");
    }

    @Test
    void testInterpretationRemove() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.removeSymbol_out + " l1 2)", LitBoolean.TRUE);
    }

    @Test
    void testIntprtAndCompPrintRemove() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(println (" + JavaArrayList.removeSymbol_out + " l1 2))");
    }

    @Test
    void testInterpretationRemoveAll() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(" + JavaArrayList.removeAllSymbol_out + " l1 l2)", LitBoolean.TRUE);
    }

    @Test
    void testIntprtAndCompPrintRemoveAll() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(println (" + JavaArrayList.removeAllSymbol_out + " l1 l2))");
    }

    @Test
    void testInterpretationRetainAll() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(" + JavaArrayList.retainAllSymbol_out + " l1 l2)", LitBoolean.TRUE);
    }

    @Test
    void testIntprtAndCompPrintRetainAll() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(define l2 (construct List:JavaArray))"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 2)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 3)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 1)"
                + "(" + JavaArrayList.addToEndSymbol_out + " l2 2)"
                + "(println (" + JavaArrayList.retainAllSymbol_out + " l1 l2))");
    }

    @Test
    void testInterpretationSet() throws Exception {
        this.assertInterpretationEquals("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.setSymbol_out + " l1 0 2)"
                + "(" + JavaArrayList.getSymbol_out + " l1 0)", new LitInteger(2));
    }

    @Test
    void testIntprtAndCompPrintSet() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaArray))\n"
                + "(" + JavaArrayList.addToEndSymbol_out + " l1 1)"
                + "(" + JavaArrayList.setSymbol_out + " l1 0 2)"
                + "(println (" + JavaArrayList.getSymbol_out + " l1 0))");
    }

    @Test
    void testInterpretationSize() throws Exception {
        this.assertInterpretationEquals("(" + JavaArrayList.sizeSymbol_out
                + "(construct List:JavaArray)))", new LitInteger(0));
    }

    @Test
    void testIntprtAndCompPrintSize() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(println (" + JavaArrayList.sizeSymbol_out
                + "(construct List:JavaArray)))");
    }

}
