package velka.test;

import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.langbase.JavaBitSet;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.types.TypeAtom;
import velka.util.AppendableException;

class BitSetTest extends VelkaTest {

	@Test
	void testConstructor() throws Exception {
		var bset = new java.util.BitSet();
		bset.set(3);
		bset.set(6);
		bset.set(9);
		
		var lio = new LitInteropObject(bset, TypeAtom.TypeSetBitSet);
		
		this.assertInterpretationEquals(
				"(let ((set (construct Set:BitSet))"
				+ "(tmp (bit-set-set set 3))"
				+ "(tmp (bit-set-set set 6))"
				+ "(tmp (bit-set-set set 9)))"
				+ "(construct Set:BitSet set))", 
				lio);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let ((set (construct Set:BitSet))"
				+ "(tmp (bit-set-set set 3))"
				+ "(tmp (bit-set-set set 6))"
				+ "(tmp (bit-set-set set 9)))"
				+ "(println (bit-set-str (construct Set:BitSet set))))");
	}
	
	@Test
    @DisplayName("Test bit-set-str with empty BitSet")
    void testBitSetStrEmpty() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (construct Set:BitSet)))");
    }

    @Test
    @DisplayName("Test bit-set-str with BitSet initialized to 2048")
    void testBitSetStrWithInitialSize() throws Exception {
        this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (construct Set:BitSet 2048)))");
    }

    @Test
    @DisplayName("Test set operation on BitSet")
    void testSetOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s (construct Set:BitSet))\n" +
            "(println (bit-set-str (" + JavaBitSet.setSymbol_out.toString() + " s 3)))"
        );
    }

    @Test
    @DisplayName("Test setValue operation on BitSet")
    void testSetValueOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s (construct Set:BitSet))\n" +
            "(println (bit-set-str (" + JavaBitSet.setValueSymbol_out.toString() + " s 3 #t)))"
        );
    }

    @Test
    @DisplayName("Test setInterval operation on BitSet")
    void testSetIntervalOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s (construct Set:BitSet))\n" +
            "(println (bit-set-str (" + JavaBitSet.setIntervalSymbol_out.toString() + " s 2 5)))"
        );
    }

    @Test
    @DisplayName("Test setIntervalValue operation on BitSet")
    void testSetIntervalValueOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s (construct Set:BitSet))\n" +
            "(println (bit-set-str (" + JavaBitSet.setIntervalValueSymbol_out.toString() + " s 2 5 #t)))"
        );
    }

    @Test
    @DisplayName("Test and operation on BitSet")
    void testAndOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (bit-set-str (" + JavaBitSet.andSymbol_out.toString() + " s1 s2)))"
        );
    }

    @Test
    @DisplayName("Test andNot operation on BitSet")
    void testAndNotOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (bit-set-str (" + JavaBitSet.andNotSymbol_out.toString() + " s1 s2)))"
        );
    }

    @Test
    @DisplayName("Test cardinality operation on BitSet")
    void testCardinalityOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (" + JavaBitSet.cardinalitySymbol_out.toString() + " s2))"
        );
    }

    @Test
    @DisplayName("Test clear operation on BitSet")
    void testClearOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (bit-set-str (" + JavaBitSet.clearSymbol_out.toString() + " s2)))"
        );
    }

    @Test
    @DisplayName("Test clearBitIndex operation on BitSet")
    void testClearBitIndexOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.clearBitIndexSymbol_out.toString() + " s2 5)))"
        );
    }

    @Test
    @DisplayName("Test clearInterval operation on BitSet")
    void testClearIntervalOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.clearIntervalSymbol_out.toString() + " s2 5 7)))"
        );
    }

    @Test
    @DisplayName("Test clone operation on BitSet")
    void testCloneOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.cloneSymbol_out.toString() + " s2)))"
        );
    }

    @Test
    @DisplayName("Test equals operation on BitSet")
    void testEqualsOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (" + JavaBitSet.equalsSymbol_out.toString() + " s1 s2))"
        );
    }

    @Test
    @DisplayName("Test flip operation on BitSet")
    void testFlipOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.flipSymbol_out.toString() + " s2 2)))"
        );
    }

    @Test
    @DisplayName("Test flipInterval operation on BitSet")
    void testFlipIntervalOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.flipIntervalSymbol_out.toString() + " s2 2 5)))"
        );
    }

    @Test
    @DisplayName("Test get operation on BitSet")
    void testGetOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.getSymbol_out.toString() + " s2 5))"
        );
    }

    @Test
    @DisplayName("Test getInterval operation on BitSet")
    void testGetIntervalOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (bit-set-str (" + JavaBitSet.getIntervalSymbol_out.toString() + " s2 5 7)))"
        );
    }

    @Test
    @DisplayName("Test intersects operation on BitSet")
    void testIntersectsOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (" + JavaBitSet.intersectsSymbol_out.toString() + " s1 s2))"
        );
    }

    @Test
    @DisplayName("Test isEmpty operation on BitSet")
    void testIsEmptyOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.isEmptySymbol_out.toString() + " s2))"
        );
    }

    @Test
    @DisplayName("Test length operation on BitSet")
    void testLengthOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.lengthSymbol_out.toString() + " s2))"
        );
    }

    @Test
    @DisplayName("Test nextClearBit operation on BitSet")
    void testNextClearBitOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.nextClearBitSymbol_out.toString() + " s2 5))"
        );
    }

    @Test
    @DisplayName("Test nextSetBit operation on BitSet")
    void testNextSetBitOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.nextSetBitSymbol_out.toString() + " s2 0))"
        );
    }

    @Test
    @DisplayName("Test or operation on BitSet")
    void testOrOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (bit-set-str (" + JavaBitSet.orSymbol_out.toString() + " s1 s2)))"
        );
    }

    @Test
    @DisplayName("Test previousClearBit operation on BitSet")
    void testPreviousClearBitOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.previousClearBitSymbol_out.toString() + " s2 5))"
        );
    }

    @Test
    @DisplayName("Test previousSetBit operation on BitSet")
    void testPreviousSetBitOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.previousSetBitSymbol_out.toString() + " s2 9))"
        );
    }

    @Test
    @DisplayName("Test size operation on BitSet")
    void testSizeOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 2 5)\n" +
            "(println (" + JavaBitSet.sizeSymbol_out.toString() + " s1))"
        );
    }

    @Test
    @DisplayName("Test str operation on BitSet")
    void testStrOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 4 7)\n" +
            "(println (" + JavaBitSet.strSymbol_out.toString() + " s2))"
        );
    }

    @Test
    @DisplayName("Test xor operation on BitSet")
    void testXorOperation() throws Exception {
        this.assertIntprtAndCompPrintSameValues(
            "(define s1 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s1 4 7)\n" +
            "(define s2 (construct Set:BitSet))\n" +
            "(" + JavaBitSet.setIntervalSymbol_out.toString() + " s2 2 5)\n" +
            "(println (bit-set-str (" + JavaBitSet.xorSymbol_out.toString() + " s1 s2)))"
        );
    }
    
    @Test
    @DisplayName("Test map")
    void testMap() throws Exception{
    	var bs = new java.util.BitSet();
    	bs.set(1, 6);
    	this.assertInterpretationEquals(
    			"(bit-set-map (bit-set-set-interval (construct Set:BitSet) 0 5) (lambda (x) (+ x 1)))", 
    			new LitInteropObject(bs, TypeAtom.TypeSetBitSet));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (bit-set-map (bit-set-set-interval (construct Set:BitSet) 0 5) (lambda (x) (+ x 1)))))");
    }
    
    @SuppressWarnings("unchecked")
	@Test
    @DisplayName("Test conversion BitSet to TreeSet")
    void test2TreeSet() throws Exception {    	
    	this.assertInterpretationEquals(
    			"(define bs (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))"
    			+ "(convert Set:BitSet Set:Tree bs)", 
    			new LitInteropObject(new java.util.TreeSet<Object>(List.of(3l, 6l, 9l)),
    					TypeAtom.TypeSetTree));
    	
    	this.assertIntprtAndCompPrintSameValues(
    			"(define bs (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))"
    	    	+ "(println (convert Set:BitSet Set:Tree bs))"
    			);
    	
    	this.assertInterpretationEquals("(conversion-cost (lambda ((Set:Tree x)) x) (tuple (construct Set:BitSet)))", new LitDouble(0.8d));
    	this.assertInterpretationEquals("(let ((s (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval s 0 2000)))"
    			+ "(conversion-cost (lambda ((Set:Tree x)) x) (tuple s)))", new LitDouble(0.5d));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (conversion-cost (lambda ((Set:Tree x)) x) (tuple (construct Set:BitSet))))");
    	this.assertIntprtAndCompPrintSameValues("(let ((s (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval s 0 2000)))"
    			+ "(println (conversion-cost (lambda ((Set:Tree x)) x) (tuple s))))");
    }
    
    @Test
    void testContainsAll() throws Exception{
    	this.assertInterpretationEquals(
    			"(let ((set (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval set 0 5))"
    			+ "(l (build-list-native 5 (lambda (x) x))))"
    			+ "(bit-set-contains-all set l))", 
    			LitBoolean.TRUE);
    	
    	this.assertInterpretationEquals(
    			"(let ((set (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval set 0 2))"
    			+ "(l (build-list-native 5 (lambda (x) x))))"
    			+ "(bit-set-contains-all set l))", 
    			LitBoolean.FALSE);
    	
    	this.assertIntprtAndCompPrintSameValues(
    			"(let ((set (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval set 0 5))"
    			+ "(l (build-list-native 5 (lambda (x) x))))"
    			+ "(println (bit-set-contains-all set l)))");
    	
    	this.assertIntprtAndCompPrintSameValues(
    			"(let ((set (construct Set:BitSet))"
    			+ "(tmp (bit-set-set-interval set 0 2))"
    			+ "(l (build-list-native 5 (lambda (x) x))))"
    			+ "(println (bit-set-contains-all set l)))");
    	
    	this.assertIntprtAndCompPrintSameValues(
    			"(println (bit-set-contains-all (bit-set-from-list (list 1 2)) (list 0 1 2)))")	;
    }
    
    @Test
    void testSetAll() throws Exception {
    	var bs = new java.util.BitSet();
    	bs.set(0, 2);
    	
    	this.assertInterpretationEquals(
    			"(let ((set (construct Set:BitSet))"
    			+ "(l (build-list-native 2 (lambda (x) x))))"
    			+ "(bit-set-set-all set l))", 
    			new LitInteropObject(bs, TypeAtom.TypeSetBitSet));
    	
    	this.assertIntprtAndCompPrintSameValues(
    			"(let ((set (construct Set:BitSet))"
    			+ "(l (build-list-native 2 (lambda (x) x))))"
    			+ "(println (bit-set-str (bit-set-set-all set l))))");
    }
    
    @Test
    void testFromList() throws Exception {
    	var bs = new java.util.BitSet();
    	bs.set(1, 4);
    	this.assertInterpretationEquals("(bit-set-from-list (list 1 2 3))", new LitInteropObject(bs, TypeAtom.TypeSetBitSet));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (bit-set-str (bit-set-from-list (list 1 2 3))))");
    }
    
    @Test
    void testToList() throws Exception{
    	this.assertInterpretationEquals(
    			"(bit-set-to-list (bit-set-from-list (list 1 2 3)))", 
    			new LitInteropObject(List.of(new LitInteger(1l), new LitInteger(2l), new LitInteger(3l)), TypeAtom.TypeListNative));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (bit-set-to-list (bit-set-from-list (list 1 2 3))))");
    }
    
    @Test
    void testToHashSet() throws Exception {
    	this.assertInterpretationEquals(
    			"(convert Set:BitSet Set:Hash (bit-set-from-list (list 1 2 3)))", 
    			new LitInteropObject(new java.util.HashSet<Object>(List.of(1l, 2l, 3l)), TypeAtom.TypeSetHash));
    	
    	this.assertIntprtAndCompPrintSameValues("(println (convert Set:BitSet Set:Hash (bit-set-from-list (list 1 2 3))))");
    }
}
