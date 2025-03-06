package velka.test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import velka.core.langbase.Operators;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.Pair;
import velka.util.RankAggregation;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;

class TestOperators extends VelkaTest{

	@Test
	void testTypeStr() throws Exception {
		this.assertVelkaCode(
				"(type-str 42)",
				"Int:*");
	}
	
	@Test
	void testRepresentationStr() throws Exception {
		this.assertVelkaCode(
				"(representation-str 42)",
				"Int:Native");
	}
	
	@Test
	void testSubstr() throws Exception {
		this.assertVelkaCode(
				"(substr \"hamburger\" 4 8)",
				"urge");
	}
	
	@Test
	void testStrlen() throws Exception {
		this.assertVelkaCode(
				"(strlen \"foo\")",
				3);
	}

	@Test
	void testLesserThanOrEquals() throws Exception {
		this.assertVelkaCode(
				"(<= 42 1)",
				false);
	}
	
	@Test
	void testGreaterThan() throws Exception {
		this.assertVelkaCode(
				"(> 42 1)",
				true);
	}
	
	@Test
	void testGreaterThanOrEquals() throws Exception {
		this.assertVelkaCode(
				"(>= 42 1)",
				true);
	}
	
	@Test
	void testMax() throws Exception {
		this.assertVelkaCode(
				"(max 42 1)",
				42);
	}
	
	@Test
	void testMin() throws Exception {
		this.assertVelkaCode(
				"(min 42 1)",
				1);
	}
	
	@Test
	void testLinearFunction() throws Exception {
		this.assertVelkaCode("((lin-fun 1.0 .0) .0)", 0.0);		
		this.assertVelkaCode("((lin-fun 1.0 .0) 1.0)", 1.0);
		this.assertVelkaCode("((lin-fun 1.0 .0) -2.0)", -2.0);
	}
	
	@Test
	void testLinearFunctionPoints() throws Exception {
		this.assertVelkaCode("((lin-fun-pts 0.0 0.0 1.0 1.0) .0)", 0.0);
		this.assertVelkaCode("((lin-fun-pts 0.0 0.0 1.0 1.0) 1.0)", 1.0);
		this.assertVelkaCode("((lin-fun-pts 0.0 0.0 1.0 1.0) -2.0)", -2.0);
	}
	
	@Test
	void testConvertIntNativeToIntRoman() throws Exception {
//		this.assertVelkaCode(
//				"(to-str (convert Int:Native Int:Roman 42))",
//				"XLII");
	}
	
	@Test
	void testConvertIntNativeToIntString() throws Exception {
//		this.assertVelkaCode(
//				"(to-str (convert Int:Native Int:String 42))",
//				"42");
	}
	
	@Test
	void testConvertIntRomanToIntNative() throws Exception {
		this.assertVelkaCode(
				"(convert Int:Roman Int:Native (construct Int:Roman \"XLII\"))",
				42);
	}
	
	@Test
	void testConvertIntRomanToIntString() throws Exception {
//		this.assertVelkaCode(
//				"(to-str (convert Int:Roman Int:String (construct Int:Roman \"XLII\")))",
//				"42");
	}
	
	@Test
	void testConvertIntStringToIntNative() throws Exception {
		this.assertVelkaCode( 
				"(convert Int:String Int:Native (construct Int:String \"42\"))",
				42);
	}
	
	@Test
	void testConvertIntStringToIntRoman() throws Exception {
//		this.assertVelkaCode(
//				"(to-str (convert Int:String Int:Roman (construct Int:String \"42\")))",
//				"XLII");
	}
	
	@Test
	@DisplayName("Test Addition Operator")
	void testAdditionOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(+ 21 21)",
				42);
	}

	@Test
	@DisplayName("Test BitAnd Operator")
	void testBitAndOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(bit-and 1 2)",
				0);
	}

	@Test
	@DisplayName("Test BitOr Operator")
	void testBitOrOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(bit-or 1 2)",
				3);
	}

	@Test
	@DisplayName("Test Car Operator")
	void testCarOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(car (tuple 42 \"foo\"))",
				42);
	}

	@Test
	@DisplayName("Test Cdr Operator")
	void testCdrOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(cdr (tuple 42 \"foo\"))",
				"foo");
	}

	@Test
	@DisplayName("Test Concatenation Operator")
	void testConcatenationOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(concat \"foo\" \"bar\")",
				"foobar");
	}

	@Test
	@DisplayName("Test Division Operator")
	void testDivisionOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(/ 84 2)",
				42);
	}

	@Test
	@DisplayName("Test Equals Operator")
	void testEqualsOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(equalp 42 21)",
				false);
		this.assertVelkaCode(
				"(equalp 42 42)",
				true);
	}

	@Test
	@DisplayName("Test LesserThan Operator")
	void testLesserThanOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(< 42 21)",
				false);
	}

	@Test
	@DisplayName("Test Multiplication Operator")
	void testMultiplicationOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(* 2 21)",
				42);
	}

	@Test
	@DisplayName("Test Not Operator")
	void testNotOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(not #t)",
				false);
	}

	@Test
	@DisplayName("Test Subtraction Operator")
	void testSubtractionOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(- 43 1)",
				42);
	}

	@Test
	@DisplayName("Test IsSameType Operator")
	void testIsSameTypeOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
			"(is-same-type 42 42)",
			true);
	}

	@Test
	@DisplayName("Test IsSameRepresentation Operator with Equal Integers")
	void testIsSameRepresentationEqualIntegers() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(is-same-representation 42 42)",
				true);
	}

	@Test
	@DisplayName("Test IsSameRepresentation Operator with Different Representations")
	void testIsSameRepresentationDifferentRepresentations() throws AppendableException, IOException {
		
	    this.assertOperator(Operators.IsSameRepresentation,
	            new Tuple(Arrays.asList(new LitInteger(42), 
	                    new LitComposite(new LitString("42"), TypeAtom.TypeIntString))),
	            LitBoolean.FALSE, TypeAtom.TypeBoolNative);
	    this.assertJExprEquals(Boolean.FALSE, 
	            (new AbstractionApplication(Operators.IsSameRepresentation, 
	                    new Tuple(Arrays.asList(new LitInteger(42), 
	                            new LitComposite(new LitString("42"), TypeAtom.TypeIntString))))));
	}

	@Test
	@DisplayName("Test IsSameRepresentation Operator with Integer and String")
	void testIsSameRepresentationIntegerAndString() throws AppendableException, IOException {
	    this.assertOperator(Operators.IsSameRepresentation,
	            new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))), 
	            LitBoolean.FALSE, TypeAtom.TypeBoolNative);
	    this.assertJExprEquals(Boolean.FALSE, 
	            (new AbstractionApplication(Operators.IsSameRepresentation, 
	                    new Tuple(Arrays.asList(new LitInteger(42), new LitString("42"))))));
	}

	@Test
	@DisplayName("Test BitShiftRight Operator")
	void testBitShiftRightOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.BitShiftRight, 
	            new Tuple(new LitInteger(2), new LitInteger(1)),
	            new LitInteger(1), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(1), 
	            (new AbstractionApplication(Operators.BitShiftRight, 
	                    new Tuple(new LitInteger(2), new LitInteger(1)))));
	}

	@Test
	@DisplayName("Test BitShiftLeft Operator")
	void testBitShiftLeftOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.BitShiftLeft, 
	            new Tuple(new LitInteger(2), new LitInteger(1)),
	            new LitInteger(4), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(4), 
	            (new AbstractionApplication(Operators.BitShiftLeft, 
	                    new Tuple(new LitInteger(2), new LitInteger(1)))));
	}

	@Test
	@DisplayName("Test UnsignedBitShiftRight Operator with Positive Number")
	void testUnsignedBitShiftRightPositive() throws AppendableException, IOException {
	    this.assertOperator(Operators.UnsignedBitShiftRight, 
	            new Tuple(new LitInteger(2), new LitInteger(1)),
	            new LitInteger(1), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(1), 
	            (new AbstractionApplication(Operators.UnsignedBitShiftRight, 
	                    new Tuple(new LitInteger(2), new LitInteger(1)))));
	}

	@Test
	@DisplayName("Test UnsignedBitShiftRight Operator with Negative Number")
	void testUnsignedBitShiftRightNegative() throws AppendableException, IOException {
	    this.assertOperator(Operators.UnsignedBitShiftRight, 
	            new Tuple(new LitInteger(-1), new LitInteger(10)), 
	            new LitInteger(-1 >>> 10), TypeAtom.TypeIntNative);
	}

	@Test
	@DisplayName("Test BitNot Operator")
	void testBitNotOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.BitNot, 
	            new Tuple(new LitInteger(6)), new LitInteger(-7), 
	            TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(-7), 
	            (new AbstractionApplication(Operators.BitNot, 
	                    new Tuple(new LitInteger(6)))));
	}

	@Test
	@DisplayName("Test BitXor Operator")
	void testBitXorOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.BitXor, 
	            new Tuple(new LitInteger(5), new LitInteger(6)), 
	            new LitInteger(3), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(3), 
	            (new AbstractionApplication(Operators.BitXor, 
	                    new Tuple(new LitInteger(5), new LitInteger(6)))));
	}

	@Test
	@DisplayName("Test ToStr Operator")
	void testToStrOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.ToStr, 
	            new Tuple(new LitInteger(42)), 
	            new LitString("42"), TypeAtom.TypeStringNative);
	    this.assertJExprEquals("42", 
	            (new AbstractionApplication(Operators.ToStr, 
	                    new Tuple(new LitInteger(42)))));
	}

	// Tests for File Read
	@Test
	@DisplayName("Test ReadFile Operator")
	void testReadFileOperator() throws Exception {
	    File tempOut = File.createTempFile("velka_read_test", null);
	    String content = "hello world !!";
	    Files.writeString(tempOut.toPath(), content);

	    this.assertOperator(Operators.ReadFile, 
	            new Tuple(new LitString(tempOut.toPath().toString())), 
	            new LitString(content), 
	            TypeAtom.TypeStringNative);
	    this.assertJExprEquals(content, 
	            (new AbstractionApplication(Operators.ReadFile, 
	                    new Tuple(new LitString(tempOut.toPath().toString())))));

	    tempOut.delete();
	}

	// Continue splitting other test cases similarly...

	@Test
	@DisplayName("Test StrSplit Operator")
	void testStrSplitOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.StrSplit, 
	            new Tuple(new LitString("foo bar baz"), new LitString(" ")),
	            new LitInteropObject(List.of("foo", "bar", "baz"), TypeAtom.TypeListNative), 
	            TypeAtom.TypeListNative);
	    this.assertJExprEquals(List.of("foo", "bar", "baz"), 
	            (new AbstractionApplication(Operators.StrSplit, 
	                    new Tuple(new LitString("foo bar baz"), new LitString(" ")))));
	}

	@Test
	@DisplayName("Test ParseInt Operator")
	void testParseIntOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.ParseInt, 
	            new Tuple(new LitString("42")), 
	            new LitInteger(42), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(42), 
	            (new AbstractionApplication(Operators.ParseInt, 
	                    new Tuple(new LitString("42")))));
	}

	@Test
	@DisplayName("Test IntToDouble Operator")
	void testIntToDoubleOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.IntToDouble, 
	            new Tuple(new LitInteger(42)), 
	            new LitDouble(42.0), TypeAtom.TypeDoubleNative);
	    this.assertJExprEquals(Double.valueOf(42.0), 
	            (new AbstractionApplication(Operators.IntToDouble, 
	                    new Tuple(new LitInteger(42)))));
	}

	@Test
	@DisplayName("Test Floor Operator")
	void testFloorOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.Floor, 
	            new Tuple(new LitDouble(3.14)), 
	            new LitInteger(3), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(3), 
	            (new AbstractionApplication(Operators.Floor, 
	                    new Tuple(new LitDouble(3.14)))));
	}

	@Test
	@DisplayName("Test DoubleAddition Operator")
	void testDoubleAdditionOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.DoubleAddition, 
	            new Tuple(new LitDouble(3.14), new LitDouble(3.14)), 
	            new LitDouble(6.28), TypeAtom.TypeDoubleNative);
	    this.assertJExprEquals(Double.valueOf(6.28), 
	            (new AbstractionApplication(Operators.DoubleAddition, 
	                    new Tuple(new LitDouble(3.14), new LitDouble(3.14)))));
	}

	@Test
	@DisplayName("Test DoubleLesserThan Operator - True Case")
	void testDoubleLesserThanTrue() throws AppendableException, IOException {
	    this.assertOperator(Operators.DoubleLesserThan, 
	            new Tuple(new LitDouble(3.14), new LitDouble(6.28)), 
	            LitBoolean.TRUE, TypeAtom.TypeBoolNative);
	    this.assertJExprEquals(Boolean.TRUE, 
	            (new AbstractionApplication(Operators.DoubleLesserThan, 
	                    new Tuple(new LitDouble(3.14), new LitDouble(6.28)))));
	}

	@Test
	@DisplayName("Test DoubleLesserThan Operator - False Case")
	void testDoubleLesserThanFalse() throws AppendableException, IOException {
	    this.assertOperator(Operators.DoubleLesserThan, 
	            new Tuple(new LitDouble(3.14), new LitDouble(3.14)), 
	            LitBoolean.FALSE, TypeAtom.TypeBoolNative);
	    this.assertJExprEquals(Boolean.FALSE, 
	            (new AbstractionApplication(Operators.DoubleLesserThan, 
	                    new Tuple(new LitDouble(3.14), new LitDouble(3.14)))));
	}

	@Test
	@DisplayName("Test Modulo Operator")
	void testModuloOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.Modulo, 
	            new Tuple(new LitInteger(5), new LitInteger(3)), 
	            new LitInteger(2), TypeAtom.TypeIntNative);
	    this.assertJExprEquals(Integer.valueOf(2), 
	            (new AbstractionApplication(Operators.Modulo, 
	                    new Tuple(new LitInteger(5), new LitInteger(3)))));
	}

	@Test
	@DisplayName("Test ConversionCost Operator")
	void testConversionCostOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.ConversionCost, 
	            new Tuple(
	                    new Lambda(new LitString("foo"),
	                            List.of(Pair.of(new Symbol("x"), TypeAtom.TypeIntNative))),
	                    new Tuple(new LitComposite(new LitString("IV"), TypeAtom.TypeIntRoman))),
	            new LitDouble(RankAggregation.instance().defaultConversionRank()), 
	            TypeAtom.TypeIntNative);
	}

	@Test
	@DisplayName("Test typeStr Operator")
	void testTypeStrOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.typeStr, 
	            new Tuple(new LitInteger(42)), 
	            new LitString(TypeAtom.TypeIntNative.removeRepresentationInformation().toString()), 
	            TypeAtom.TypeStringNative);
	    this.assertJExprEquals(TypeAtom.TypeIntNative.removeRepresentationInformation().toString(), 
	            (new AbstractionApplication(Operators.typeStr, 
	                    new Tuple(new LitInteger(42)))));
	}

	@Test
	@DisplayName("Test representationStr Operator")
	void testRepresentationStrOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.representationStr, 
	            new Tuple(new LitInteger(42)), 
	            new LitString(TypeAtom.TypeIntNative.toString()), 
	            TypeAtom.TypeStringNative);
	    this.assertJExprEquals(TypeAtom.TypeIntNative.toString(), 
	            (new AbstractionApplication(Operators.representationStr, 
	                    new Tuple(new LitInteger(42)))));
	}

	@Test
	@DisplayName("Test substr Operator")
	void testSubstrOperator() throws AppendableException, IOException {
	    this.assertOperator(Operators.substr, 
	            new Tuple(new LitString("hamburger"), new LitInteger(4), new LitInteger(8)), 
	            new LitString("urge"), TypeAtom.TypeStringNative);
	    this.assertJExprEquals("urge", 
	            (new AbstractionApplication(Operators.substr, 
	                    new Tuple(new LitString("hamburger"), new LitInteger(4), new LitInteger(8)))));
	}

	@Test
	@DisplayName("Test strlen Operator")
	void testStrlenOperator() throws AppendableException, IOException {
		this.assertVelkaCode(
				"(strlen \"hamburger\")",
				"hamburger".length());
	}



}
