package velka.test;

import java.util.BitSet;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.JavaBitSet;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.types.TypeAtom;
import velka.util.AppendableException;

class BitSetTest extends VelkaTest {

	private Environment env;
	
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}
	
	@Test
	void testConstructor() throws Exception {
		var bset = new java.util.BitSet();
		bset.set(3);
		bset.set(6);
		bset.set(9);
		
		this.assertVelkaCode(
				"(to-str (let ((set (construct Set:BitSet))"
				+ "(tmp (bit-set-set set 3))"
				+ "(tmp (bit-set-set set 6))"
				+ "(tmp (bit-set-set set 9)))"
				+ "(construct Set:BitSet set)))",
				bset.toString());
	}
	
	@Test
    @DisplayName("Test bit-set-str with empty BitSet")
    void testBitSetStrEmpty() throws Exception {
		this.assertVelkaCode(
				"(bit-set-str (construct Set:BitSet))",
				(new java.util.BitSet()).toString());
    }

    @Test
    @DisplayName("Test bit-set-str with BitSet initialized to 2048")
    void testBitSetStrWithInitialSize() throws Exception {
    	this.assertVelkaCode("(bit-set-str (construct Set:BitSet 2048))",
    			(new java.util.BitSet(2048)).toString());
    }

    @Test
    @DisplayName("Test set operation on BitSet")
    void testSetOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(3);
    	this.assertVelkaCode(
    			"(bit-set-str (let ((s (construct Set:BitSet)) (tmp (bit-set-set s 3))) s))",
    			bs.toString());
    }
    
    @Test
    @DisplayName("Test setValue operation on BitSet")
    void testSetValueOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(3, true);
        this.assertVelkaCode(
        		"(bit-set-str (let ((s (construct Set:BitSet)) (tmp (bit-set-set-value s 3 #t))) s))",
        		bs.toString());
    }

    @Test
    @DisplayName("Test setInterval operation on BitSet")
    void testSetIntervalOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(2, 5);
        this.assertVelkaCode(
        		"(bit-set-str (let ((s (construct Set:BitSet)) (tmp (bit-set-set-interval s 2 5))) s))",
        		bs.toString());
    }

    @Test
    @DisplayName("Test setIntervalValue operation on BitSet")
    void testSetIntervalValueOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(2, 5, true);
        this.assertVelkaCode(
        		"(bit-set-str (let ((s (construct Set:BitSet)) (tmp (bit-set-set-interval-value s 2 5 #t))) s))",
        		bs.toString());
    }

    @Test
    @DisplayName("Test and operation on BitSet")
    void testAndOperation() throws Exception {
    	var bs1 = new BitSet();
        bs1.set(4, 7);
        var bs2 = new BitSet();
        bs2.set(2, 5);
        bs1.and(bs2);
        
        this.assertVelkaCode(
        		"(bit-set-str (let ((bs1 (construct Set:BitSet)) "
        		+ "(tmp (bit-set-set-interval bs1 2 5))"
        		+ "(bs2 (construct Set:BitSet))"
        		+ "(tmp (bit-set-set-interval bs2 4 7))"
        		+ "(tmp (bit-set-and bs1 bs2)))"
        		+ "bs1))",
        		bs1.toString());
    }

    @Test
    @DisplayName("Test andNot operation on BitSet")
    void testAndNotOperation() throws Exception {
    	var bs1 = new BitSet();
        bs1.set(4, 7);
        var bs2 = new BitSet();
        bs2.set(2, 5);
        bs1.andNot(bs2);
        
        this.assertVelkaCode(
        		"(bit-set-str (let ((bs1 (construct Set:BitSet)) "
        		+ "(tmp (bit-set-set-interval bs1 4 7))"
        		+ "(bs2 (construct Set:BitSet))"
        		+ "(tmp (bit-set-set-interval bs2 2 5))"
        		+ "(tmp (bit-set-and-not bs1 bs2)))"
        		+ "bs1))",
        		bs1.toString());
    }

    @Test
    @DisplayName("Test cardinality operation on BitSet")
    void testCardinalityOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(2, 5);
        
        this.assertVelkaCode(
        		"(let ((s (construct Set:BitSet)) (tmp (bit-set-set-interval s 2 5))) "
        		+ "(bit-set-cardinality s))",
        		bs.cardinality());
    }

    @Test
    @DisplayName("Test clear operation on BitSet")
    void testClearOperation() throws Exception {
    	var bs = new BitSet();
    	bs.set(2, 5);
    	bs.clear();
    	
    	this.assertVelkaCode(
    			"(bit-set-str (let ((s (construct Set:BitSet)) "
    			+ "(tmp (bit-set-set-interval s 2 5))"
    			+ "(tmp (bit-set-clear s))) "
    			+ "s))",
    			bs.toString());
    }

    @Test
    @DisplayName("Test clearBitIndex operation on BitSet")
    void testClearBitIndexOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(4, 7);
        bs.clear(5);
    	
    	this.assertVelkaCode(
    			"(bit-set-str (let ((s (construct Set:BitSet)) "
    			+ "(tmp (bit-set-set-interval s 4 7))"
    			+ "(tmp (bit-set-clear-bit-index s 5))) "
    			+ "s))",
    			bs.toString());
    }

    @Test
    @DisplayName("Test clearInterval operation on BitSet")
    void testClearIntervalOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(4, 7);
        bs.clear(5, 7);
    	
    	this.assertVelkaCode(
    			"(bit-set-str (let ((s (construct Set:BitSet)) "
    			+ "(tmp (bit-set-set-interval s 4 7))"
    			+ "(tmp (bit-set-clear-interval s 5 7)))"
    			+ "s))",
    			bs.toString());
    }

    @Test
    @DisplayName("Test clone operation on BitSet")
    void testCloneOperation() throws Exception {
    	var bs = new BitSet();
        bs.set(4, 7);
        
        this.assertVelkaCode(
        		"(bit-set-str (let ((s (construct Set:BitSet)) "
    			+ "(tmp (bit-set-set-interval s 4 7)))"
    			+ "(bit-set-clone s)))",
    			bs.toString());
    }

    @Test
    @DisplayName("Test equals operation on BitSet")
    void testEqualsOperation() throws Exception {
        var bs1 = new BitSet();
        bs1.set(2, 5);
        var bs2 = new BitSet();
        bs2.set(2, 5);
        boolean result = bs1.equals(bs2);
        
        this.assertVelkaCode(
            "(let ((bs1 (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs1 2 5))"
            + "(bs2 (construct Set:BitSet))"
            + "(tmp (bit-set-set-interval bs2 2 5)))"
            + "(bit-set-equalp bs1 bs2))",
            result
        );
    }

    @Test
    @DisplayName("Test flip operation on BitSet")
    void testFlipOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 5);
        bs.flip(3);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 5))"
            + "(tmp (bit-set-flip bs 3))) bs))",
            bs.toString()
        );
    }

    @Test
    @DisplayName("Test flip interval operation on BitSet")
    void testFlipIntervalOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 6);
        bs.flip(3, 5);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 6))"
            + "(tmp (bit-set-flip-interval bs 3 5))) bs))",
            bs.toString()
        );
    }

    @Test
    @DisplayName("Test get operation on BitSet")
    void testGetOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 5);
        boolean result = bs.get(3);
        
        this.assertVelkaCode(
            "(let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 5))) (bit-set-get bs 3))",
            result
        );
    }

    @Test
    @DisplayName("Test get interval operation on BitSet")
    void testGetIntervalOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 6);
        BitSet subSet = bs.get(3, 5);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 6))) (bit-set-get-interval bs 3 5)))",
            subSet.toString()
        );
    }

    @Test
    @DisplayName("Test intersects operation on BitSet")
    void testIntersectsOperation() throws Exception {
        var bs1 = new BitSet();
        bs1.set(2, 5);
        var bs2 = new BitSet();
        bs2.set(4, 7);
        
        this.assertVelkaCode(
            "(let ((bs1 (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs1 2 5))"
            + "(bs2 (construct Set:BitSet))"
            + "(tmp (bit-set-set-interval bs2 4 7)))"
            + "(bit-set-intersects bs1 bs2))",
            bs1.intersects(bs2)
        );
    }

    @Test
    @DisplayName("Test isEmpty operation on BitSet")
    void testIsEmptyOperation() throws Exception {
        var bs = new BitSet();
        boolean result = bs.isEmpty();
        
        this.assertVelkaCode(
            "(bit-set-is-empty (construct Set:BitSet))",
            Boolean.valueOf(result)
        );
    }

    @Test
    @DisplayName("Test length operation on BitSet")
    void testLengthOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 7);
        int result = bs.length();
        
        this.assertVelkaCode(
            "(let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 7)))"
            + "(bit-set-length bs))",
            Integer.valueOf(result)
        );
    }

    @Test
    @DisplayName("Test or operation on BitSet")
    void testOrOperation() throws Exception {
        var bs1 = new BitSet();
        bs1.set(2, 5);
        var bs2 = new BitSet();
        bs2.set(4, 7);
        bs1.or(bs2);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs1 (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs1 2 5))"
            + "(bs2 (construct Set:BitSet))"
            + "(tmp (bit-set-set-interval bs2 4 7))"
            + "(tmp (bit-set-or bs1 bs2))) bs1))",
            bs1.toString()
        );
    }

    @Test
    @DisplayName("Test previous clear bit operation on BitSet")
    void testPreviousClearBitOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 6);
        int result = bs.previousClearBit(4);
        
        this.assertVelkaCode(
            "(let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 6))) (bit-set-previous-clear-bit bs 4))",
            Integer.valueOf(result)
        );
    }

    @Test
    @DisplayName("Test previous set bit operation on BitSet")
    void testPreviousSetBitOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 6);
        int result = bs.previousSetBit(5);
        
        this.assertVelkaCode(
            "(let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 6))) (bit-set-previous-set-bit bs 5))",
            Integer.valueOf(result)
        );
    }

    @Test
    @DisplayName("Test size operation on BitSet")
    void testSizeOperation() throws Exception {
        var bs = new BitSet();
        int result = bs.size();
        
        this.assertVelkaCode(
            "(bit-set-size (construct Set:BitSet)))",
            Integer.valueOf(result)
        );
    }

    @Test
    @DisplayName("Test string representation of BitSet")
    void testStrOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2, 6);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs 2 6))) bs))",
            bs.toString()
        );
    }

    @Test
    @DisplayName("Test XOR operation on BitSet")
    void testXorOperation() throws Exception {
        var bs1 = new BitSet();
        bs1.set(2, 5);
        var bs2 = new BitSet();
        bs2.set(4, 7);
        bs1.xor(bs2);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs1 (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs1 2 5))"
            + "(bs2 (construct Set:BitSet))"
            + "(tmp (bit-set-set-interval bs2 4 7))"
            + "(tmp (bit-set-xor bs1 bs2))) bs1))",
            bs1.toString()
        );
    }

    @Test
    @DisplayName("Test containsAll operation on BitSet")
    void testContainsAllOperation() throws Exception {        
        this.assertVelkaCode(
            "(let ((bs1 (construct Set:BitSet)) "
            + "(tmp (bit-set-set-interval bs1 2 6)))"
            + "(bit-set-contains-all bs1 (list 3 4 5)))",
            Boolean.TRUE);
    }

    @Test
    @DisplayName("Test setAll operation on BitSet")
    void testSetAllOperation() throws Exception {
        var bs = new BitSet();
        bs.set(2);
        bs.set(4);
        bs.set(6);
        
        this.assertVelkaCode(
            "(bit-set-str (let ((bs (construct Set:BitSet)) "
            + "(tmp (bit-set-set-all bs (list 2 4 6)))) bs))",
            bs.toString()
        );
    }

    @Test
    @DisplayName("Test fromList operation on BitSet")
    void testFromListOperation() throws Exception {
        var bs = new BitSet();
        bs.set(1);
        bs.set(3);
        bs.set(5);
        
        this.assertVelkaCode(
            "(bit-set-str (bit-set-from-list (list 1 3 5))))",
            bs.toString()
        );
    }

    @Test
    @DisplayName("Test toList operation on BitSet")
    void testToListOperation() throws Exception {        
        this.assertVelkaCode(
            "(bit-set-to-list (bit-set-from-list (list 1 3 5)))",
            List.of(1, 3, 5)
        );
    }

    @Test
        @DisplayName("Test map")
        void testMap() throws Exception{
        	var bs = new java.util.BitSet();
        	bs.set(1, 6);
        	
        	this.assertVelkaCode(
        			"(let ((s (construct Set:BitSet))"
        			+ "(tmp (bit-set-set-interval s 0 5)))"
        			+ "(bit-set-map s (lambda (x) (+ x 1))))",
        			bs);
        }
    	
    	@Test
        void testToHashSet() throws Exception {
    		this.assertVelkaCode(
    				"(convert Set:BitSet Set:Hash (bit-set-from-list (list 1 2 3)))",
    				new java.util.HashSet<Object>(List.of(1, 2, 3)));
        }
        
        @Test
        void testToTreeSet() throws Exception {
        	this.assertVelkaCode(
        			"(convert Set:BitSet Set:Tree (bit-set-from-list (list 1 2 3)))",
        			new java.util.TreeSet<Object>(List.of(1, 2, 3)));
        }
}
