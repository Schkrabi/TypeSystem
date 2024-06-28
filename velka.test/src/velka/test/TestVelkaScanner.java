/**
 * 
 */
package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.Scanner;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.TypeAtom;
import velka.util.AppendableException;

/**
 * @author r.skrabal
 *
 */
class TestVelkaScanner extends VelkaTest {

	private static Path scannedFilePath;
	private static final String scannedFileContents = "42 true 42.0 foo\nbar";
	private Environment env;

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeAll
	static void setUpBeforeClass() throws Exception {
		scannedFilePath = Files.createTempFile("scanned", "");
		Files.write(scannedFilePath, scannedFileContents.getBytes());
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterAll
	static void tearDownAfterClass() throws Exception {
		Files.delete(scannedFilePath);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeEach
	void setUp() throws Exception {
		env = TopLevelEnvironment.instantiate();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testConstructor() throws IOException, AppendableException {
		Expression parsed = this.parseString("(construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\")").get(0);
		Expression e = parsed.interpret(env);

		assertTrue(e instanceof LitInteropObject);
		var lc = (LitInteropObject)e;
		assertEquals(TypeAtom.TypeScannerNative, lc.type);
		assertTrue(lc.javaObject instanceof java.util.Scanner);
		java.util.Scanner s = (java.util.Scanner)lc.javaObject;		
		s.close();
	}
	
	@Test
	void testClose() throws IOException {
		java.util.Scanner s = new java.util.Scanner(scannedFilePath);
		Expression close = new AbstractionApplication(
				Scanner.closeSymbol_out,
				new Tuple(new LitInteropObject(s, TypeAtom.TypeScannerNative)));
		
		assertAll(() ->
			close.interpret(this.env));
		assertThrows(java.lang.IllegalStateException.class,
				() -> s.nextLine());
		s.close();
	}
	
	@Test
	void testNextLine() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(line (scanner-native-next-line s))"
				+ "(cls (scanner-native-close s)))"
				+ "line)",
				new LitString("42 true 42.0 foo"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(line (scanner-native-next-line s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println line))");
	}
	
//	@Test
	void testDelimiter() throws Exception {
		java.util.Scanner s = new java.util.Scanner(scannedFilePath);
		Expression delimiter = new AbstractionApplication(
				Scanner.delimiterSymbol_out,
				new Tuple(new LitInteropObject(s, TypeAtom.TypeScannerNative)));
		
		Expression rslt = delimiter.interpret(this.env );
		assertTrue(rslt instanceof LitString);
		LitString rslt_str = (LitString)rslt;
		assertEquals(s.delimiter().toString(), rslt_str.value);
		s.close();
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(del (scanner-native-delimiter s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println del))");		
	}
	
	@Test
	void testFindInLine() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(found (scanner-native-find-in-line s \"foo\"))"
				+ "(cls (scanner-native-close s)))"
				+ "found)",
				new LitString("foo"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(found (scanner-native-find-in-line s \"foo\"))"
						+ "(cls (scanner-native-close s)))"
						+ "(println found))");
	}
	
	@Test
	void testFindWithinHorzon() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(found (" + Scanner.findWithinHorizonSymbol_out.toString() + " s \"foo\" 42))"
				+ "(cls (scanner-native-close s)))"
				+ "found)",
				new LitString("foo"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(found (" + Scanner.findWithinHorizonSymbol_out.toString() + " s \"foo\" 42))"
						+ "(cls (scanner-native-close s)))"
						+ "(println found))");
	}
	
	@Test
	void testHasNext() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}

	@Test
	void testHasNextPattern() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextPatternSymbol_out.toString() + " s \"foo\"))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.FALSE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextPatternSymbol_out.toString() + " s \"foo\"))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testHasNextBoolean() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextBooleanSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.FALSE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextBooleanSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testHasNextDouble() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextDoubleSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextDoubleSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testHasNextInt() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextIntSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextIntSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test 
	void testHasNextIntRadix() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextIntRadixSymbol_out.toString() + " s 8))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextIntRadixSymbol_out.toString() + " s 8))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testHasNextLine() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.hasNextLineSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.hasNextLineSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testNext() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.nextSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitString("42"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.nextSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testNextPattern() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.nextPatternSymbol_out.toString() + " s \"\\d*\"))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitString("42"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.nextPatternSymbol_out.toString() + " s \"[0-9]*\"))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testNextBool() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(a (" + Scanner.nextSymbol_out + " s))"
				+ "(rslt (" + Scanner.nextBoolSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				LitBoolean.TRUE,
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(a (" + Scanner.nextSymbol_out + " s))"
						+ "(rslt (" + Scanner.nextBoolSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testNextDouble() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.nextDoubleSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitDouble(42),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.nextDoubleSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testNextInt() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.nextIntSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitInteger(42),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.nextIntSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testRadix() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitInteger(10),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testReset() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(s (" + Scanner.useRadixSymbol_out.toString() + " s 8))"
				+ "(s (" + Scanner.resetSymbol_out.toString() + " s))"
				+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitInteger(10),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(s (" + Scanner.useRadixSymbol_out.toString() + " s 8))"
						+ "(s (" + Scanner.resetSymbol_out.toString() + " s))"
						+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testSkip() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(rslt (" + Scanner.nextSymbol_out.toString() + " (" + Scanner.skipSymbol_out.toString() + " s \"[0-9]*\")))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitString("true"),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(rslt (" + Scanner.nextSymbol_out.toString() + " (" + Scanner.skipSymbol_out.toString() + " s \"[0-9]*\")))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
	
	@Test
	void testUseRadix() throws Exception {
		this.assertInterpretedStringEquals(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(s (" +  Scanner.useRadixSymbol_out.toString() + " s 8))"
				+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
				+ "(cls (scanner-native-close s)))"
				+ "rslt)",
				new LitInteger(8),
				this.env
				);
		
		this.assertIntprtAndCompPrintSameValues(
				"(let* ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
						+ "(s (" +  Scanner.useRadixSymbol_out.toString() + " s 8))"
						+ "(rslt (" + Scanner.radixSymbol_out.toString() + " s))"
						+ "(cls (scanner-native-close s)))"
						+ "(println rslt))");
	}
}
