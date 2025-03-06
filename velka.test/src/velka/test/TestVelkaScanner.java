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
	void testConstructorAndClose() throws IOException, AppendableException {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(tmp (scanner-native-close s)))"
				+ "\"foo\")",
				"foo");
	}
	
	@Test
	void testNextLine() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-next-line s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				"42 true 42.0 foo");
	}
	
	@Test
	void testFindInLine() throws Exception {
		this.assertVelkaCode(
			"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
			+ "(r (scanner-native-find-in-line s \"foo\"))"
			+ "(tmp (scanner-native-close s)))"
			+ "r)",
			"foo");
	}
	
	@Test
	void testFindWithinHorzon() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-find-within-horizon s \"foo\" 42))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				"foo");
	}
	
	@Test
	void testHasNext() throws Exception {
		this.assertVelkaCode(
			"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
			+ "(r (scanner-native-has-next s))"
			+ "(tmp (scanner-native-close s)))"
			+ "r)",
			Boolean.TRUE);
	}

	@Test
	void testHasNextPattern() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-pattern s \"foo\"))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.FALSE);
	}
	
	@Test
	void testHasNextBoolean() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-boolean s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.FALSE);
	}
	
	@Test
	void testHasNextDouble() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-double s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.TRUE);
	}
	
	@Test
	void testHasNextInt() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-int s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.TRUE);
	}
	
	@Test 
	void testHasNextIntRadix() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-int-radix s 8))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.TRUE);
	}
	
	@Test
	void testHasNextLine() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-has-next-line s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Boolean.TRUE);
	}
	
	@Test
	void testNext() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-next s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				"42");
	}
	
	@Test
	void testNextPattern() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-next-pattern s \"[0-9]*\"))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				"42");
	}
	
	@Test
	void testNextBool() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(tmp1 (scanner-native-skip s \"[0-9]*\"))"
				+ "(r (scanner-native-next-boolean s))"
				+ "(tmp2 (scanner-native-close s)))"
				+ "r)",
				Boolean.TRUE);
	}
	
	@Test
	void testNextDouble() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-next-double s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Double.valueOf(42));
	}
	
	@Test
	void testNextInt() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-next-int s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Integer.valueOf(42));
	}
	
	@Test
	void testRadix() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(r (scanner-native-radix s))"
				+ "(tmp (scanner-native-close s)))"
				+ "r)",
				Integer.valueOf(10));
	}
	
	@Test
	void testReset() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(tmp1 (scanner-native-use-radix s 8))"
				+ "(tmp2 (scanner-native-reset s))"
				+ "(r (scanner-native-radix s))"
				+ "(tmp3 (scanner-native-close s)))"
				+ "r)",
				Integer.valueOf(10));
	}
	
	@Test
	void testSkip() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(tmp1 (scanner-native-skip s \"[0-9]*\"))"
				+ "(r (scanner-native-next s))"
				+ "(tmp2 (scanner-native-close s)))"
				+ "r)",
				"true");
	}
	
	@Test
	void testUseRadix() throws Exception {
		this.assertVelkaCode(
				"(let ((s (construct Scanner:Native \"" + this.pathToStr(scannedFilePath) + "\"))"
				+ "(tmp1 (scanner-native-use-radix s 8))"
				+ "(r (scanner-native-radix s))"
				+ "(tmp2 (scanner-native-close s)))"
				+ "r)",
				Integer.valueOf(8));
	}
}
