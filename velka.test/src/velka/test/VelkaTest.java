package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

import velka.clojure.ClojureCodeGenerator;
import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.ConstructorOperators;
import velka.core.langbase.ConversionOperators;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.parser.Parser;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

public class VelkaTest {

	static Path tmpDir;
	private static String os = System.getProperty("os.name").toLowerCase();
	public static boolean IS_WINDOWS = (os.indexOf("win") >= 0);
	public static boolean IS_UNIX = (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0 || os.indexOf("aix") > 0);
	protected static final Path velkaUtilJar = Paths.get("C:", "Users", "r.skrabal", "Documents", "Mine", "Java", "TypeSystem", "lib", "velka.util.jar");
	protected static final Path velkaTypesJar = Paths.get("C:", "Users", "r.skrabal", "Documents", "Mine", "Java", "TypeSystem", "lib", "velka.types.jar");

	@BeforeAll
	static void setupTest() throws IOException {
		TestComplex.tmpDir = Files.createTempDirectory("cljTest");
		
		ClojureCodeGenerator.generateClojureProject(tmpDir);
		Files.copy(velkaUtilJar, tmpDir.resolve(Paths.get("velka.util.jar")), StandardCopyOption.REPLACE_EXISTING);
		Files.copy(velkaTypesJar, tmpDir.resolve(Paths.get("velka.types.jar")), StandardCopyOption.REPLACE_EXISTING);
	}

	@AfterAll
	static void breakDownTest() throws IOException {
		//Deletes the tmp dir recursively
		Files.walk(tmpDir)
	      .sorted(Comparator.reverseOrder())
	      .map(Path::toFile)
	      .forEach(File::delete);
	}

	protected void assertClojureFunction(String definitions, String testCase, String expectedResult)
			throws IOException, InterruptedException, AppendableException {
				//Test that definitions are sound
				StringBuilder sb = new StringBuilder();
			
				sb.append(ClojureHelper.declareNamespace(ClojureCodeGenerator.DEFAULT_NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(Operators.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(ListNative.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(ConstructorOperators.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(ConversionOperators.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(JavaArrayList.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(JavaLinkedList.NAMESPACE));
				sb.append(definitions);
				
				this.clojureCodeResult(sb.toString());
				//Test the testcase
				sb.append("\n");
				sb.append(testCase);
				String result = this.clojureCodeResult(sb.toString());
				if(IS_UNIX) {
					assertEquals(expectedResult + "\n", result);
				}else if (IS_WINDOWS) {
					assertEquals(expectedResult + "\r\n", result);
				}
			}

	protected List<Expression> parseString(String s) throws AppendableException {
		return Parser.read(s);
	}

	/**
	 * Asserts that interpreded string is equal to given expression
	 * @param code interpreted code
	 * @param expected expression
	 * @param env environment
	 * @param typeEnv type environment
	 * @throws AppendableException
	 */
	protected void assertInterpretedStringEquals(String code, Expression expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				Expression last = null;
				for (Expression e : this.parseString(code)) {
					@SuppressWarnings("unused")
					Pair<Type, Substitution> p = e.infer(env, typeEnv);
					last = e.interpret(env, typeEnv);
				}
			
				assertEquals(expected, last);
			}

	/**
	 * Compiles expression to clojure code
	 * @param l
	 * @param env
	 * @param typeEnv
	 * @return
	 * @throws AppendableException
	 */
	private String compileExpressionsToClojure(List<Expression> l, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				StringBuilder s = new StringBuilder();
				Iterator<Expression> i = l.iterator();
				while (i.hasNext()) {
					s.append(i.next().toClojureCode(env, typeEnv));
					if (i.hasNext()) {
						s.append('\n');
					}
				}
				return s.toString();
			}

	private String compileToClojure(String code, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<Expression> l = this.parseString(code);
		return compileExpressionsToClojure(l, env, typeEnv);
	}

	@SuppressWarnings("unused")
	private void assertCompiledExpressionEquals(Expression e, String expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				List<Expression> l = new LinkedList<Expression>();
				l.add(e);
				String s = this.compileExpressionsToClojure(l, env, typeEnv);
				assertEquals(expected, s);
			}

	protected void assertCompiledCodeEquals(String code, String expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				String s = this.compileToClojure(code, env, typeEnv);
			
				assertEquals(expected, s);
			}

	@SuppressWarnings("unused")
	private void assertCompiledCodeMatch(String code, String regex, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				String s = this.compileToClojure(code, env, typeEnv);
				if (!s.matches(regex)) {
					fail("Clojure compilation test failed, compiling " + code + " do not match " + regex + " got "
							+ s.toString());
				}
			}

	protected void assertCompile(String code, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		this.compileToClojure(code, env, typeEnv);
	}

	protected void assertIntprtAndCompPrintSameValues(String code) throws Exception {
		List<Expression> exprs = Parser.read(code);
		this.assertIntprtAndCompPrintSameValues(exprs);
	}

	protected void assertIntprtAndCompPrintSameValues(List<Expression> in) throws Exception {
		Environment intpEnv = Environment.initTopLevelEnvironment();
		TypeEnvironment intpTypeEnv = TypeEnvironment.initBasicTypes(intpEnv);
	
		String interpretationPrintOut = interpretationPrint(in, intpEnv, intpTypeEnv);
	
		Environment cmplEnv = Environment.initTopLevelEnvironment();
		TypeEnvironment cmplTypeEnv = TypeEnvironment.initBasicTypes(cmplEnv);
	
		String compilationPrintOut = clojureCompilationResult(in, cmplEnv, cmplTypeEnv);
	
		assertEquals(interpretationPrintOut, compilationPrintOut);
	}

	private String interpretationPrint(List<Expression> in, Environment env, TypeEnvironment typeEnv) throws Exception {
		PrintStream stdOut = System.out;
		ByteArrayOutputStream tmp = new ByteArrayOutputStream();
		System.setOut(new PrintStream(tmp));
	
		@SuppressWarnings("unused")
		List<Expression> rslt = velka.compiler.Compiler.eval(in, env, typeEnv);
	
		String result = tmp.toString();
		System.setOut(stdOut);
	
		return result;
	}

	protected String clojureCodeResult(String code)
			throws IOException, InterruptedException, AppendableException {
				if(IS_UNIX) {
					return clojureCodeResult_unix(code);
				}else if(IS_WINDOWS) {
					return clojureCodeResult_windows(code);
				}else {
					throw new AppendableException("Unsupported OS for testing!");
				}
			}

	private String clojureCodeResult_unix(String code)
			throws IOException, InterruptedException, AppendableException {				
				Path codeFile = Files.writeString(tmpDir.resolve(Paths.get("velka", "clojure", "user.clj")), code);
				
				ProcessBuilder pb = new ProcessBuilder("clj", codeFile.toAbsolutePath().toString());
				pb.inheritIO();
				pb.directory(tmpDir.toFile());
				
				File tempOut = File.createTempFile("velka_clojure_test_out", null);
				File tempErr = File.createTempFile("velka_clojure_test_err", null);
				
				pb.redirectOutput(tempOut);
				pb.redirectError(tempErr);
			
				Process p = pb.start();
				p.waitFor();
				
				String result = Files.readString(tempOut.toPath());
				String err = Files.readString(tempErr.toPath());
				tempOut.delete();	
				tempErr.delete();
				Files.delete(codeFile);
				
				if(!err.isEmpty()) {
					throw new AppendableException(err);
				}
				
				return result;
			}

	private String clojureCodeResult_windows(String code)
			throws IOException, InterruptedException, AppendableException {
				Path codeFile = Files.writeString(tmpDir.resolve(Paths.get("velka", "clojure", "user.clj")), code);
				
				ProcessBuilder pb = new ProcessBuilder("powershell", "-command", "clj",	"-M", codeFile.toAbsolutePath().toString());
				pb.inheritIO();
				pb.directory(tmpDir.toFile());
				
				pb.environment().put("PATH", "C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\windows\\system32;C:\\windows;C:\\windows\\System32\\Wbem;C:\\windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\windows\\System32\\OpenSSH\\;C:\\Program Files\\dotnet\\;C:\\Program Files\\Git\\cmd;C:\\Program Files (x86)\\dotnet\\;C:\\Java\\jdk-17.0.1\\bin;C:\\Java\\apache-ant-1.10.12\\bin;C:\\Users\\r.skrabal\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\r.skrabal\\.dotnet\\tools;C:\\windows\\System32;");
				
				File tempOut = File.createTempFile("velka_clojure_test_out", null);
				File tempErr = File.createTempFile("velka_clojure_test_err", null);
				
				pb.redirectOutput(tempOut);
				pb.redirectError(tempErr);
			
				Process p = pb.start();
				p.waitFor();
				
				String result = Files.readString(tempOut.toPath());
				String err = Files.readString(tempErr.toPath());
				tempOut.delete();	
				tempErr.delete();
				Files.delete(codeFile);
				
				if(!err.isEmpty()) {
					throw new AppendableException(err);
				}
				
				return result;
			}

	private String clojureCompilationResult(List<Expression> l, Environment env, TypeEnvironment typeEnv) throws Exception {
		String code = ClojureCodeGenerator.ExpressionListToClojureCode(l, env, typeEnv);
		
		String result = this.clojureCodeResult(code);
		return result;
	}

	protected void assertCompiledCodeEquals(String code, String expected) throws Exception {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
	
		this.assertCompiledCodeEquals(code, expected, env, typeEnv);
	}

	protected String escapeBrackets(String s) {
		return s.replaceAll("\\(", "\\\\(").replaceAll("\\)", "\\\\)").replaceAll("\\[", "\\\\[")
				.replaceAll("\\]", "\\\\]").replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
				.replaceAll("\\+", "\\\\+");
	}

	protected List<Expression> parseString_multipleExpression(String s) throws AppendableException {
		return Parser.read(s);
	}

	protected void assertReflexivity(Expression original) {
		Expression e = original;
		assertEquals(original, e);
		assertEquals(original.hashCode(), original.hashCode());
		assertEquals(original.compareTo(e), 0);
	}

	protected void assertDifference(Expression original, Expression e) {
		assertNotEquals(original, e);
		assertNotEquals(original.compareTo(e), 0);
	}

	protected void assertInference(Pair<Type, Substitution> result, Type expected, Expression infered) {
		this.assertInference(result, expected, infered, false);
	}

	protected void assertInference(Pair<Type, Substitution> p, Type expected, Expression infered, boolean shouldeBeSUbstEmpty) {
		assertEquals(expected, p.first);
		if (shouldeBeSUbstEmpty) {
			assertEquals(Substitution.EMPTY, p.second);
		}
	}

	protected void assertInferenceClass(Pair<Type, Substitution> p, Class<? extends Type> expected, Expression infered) {
		assertInferenceClass(p, expected, infered, false);
	}

	private void assertInferenceClass(Pair<Type, Substitution> p, Class<? extends Type> expected, Expression infered, boolean shouldBeSubstEmpty) {
		assertTrue(expected.isInstance(p.first));
		if (shouldBeSubstEmpty) {
			assertEquals(p.second, Substitution.EMPTY);
		}
	}

	protected void assertInterpretationEquals(Expression interpreted, Expression expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				Expression e = interpreted.interpret(env, typeEnv);
				assertEquals(expected, e);
			}

	protected void assertInterpretationLastEquals(Collection<Expression> interpreted, Expression expected, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
				Expression f = null;
				for(Expression e : interpreted) {
					f = e.interpret(env, typeEnv);
				}
				assertEquals(expected, f);
			}

	protected void assertOperator(final Operator operator, Tuple args, Expression expectedInterpret, Type expectedInference)
			throws AppendableException {
				AbstractionApplication application = new AbstractionApplication(operator, args);
				Environment env = Environment.initTopLevelEnvironment();
				TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
				this.assertInterpretationEquals(application, expectedInterpret, env, typeEnv);
				Pair<Type, Substitution> p = application.infer(env, typeEnv);
				if (expectedInference != null) {
					this.assertInference(p, expectedInference, application);
				}
			
				assertAll(() -> {
					operator.toString();
					operator.toClojureCode(env, typeEnv);
				});
			}

	protected void assertConversion(Abstraction conversion, Expression argument, Expression expectedInterpret, Type expectedInfer)
			throws AppendableException {
				assertAll(() -> {
					conversion.toString();
				});
			
				AbstractionApplication appl = new AbstractionApplication(conversion, new Tuple(Arrays.asList(argument)));
				Environment env = Environment.initTopLevelEnvironment();
				TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
				this.assertInterpretationEquals(appl, expectedInterpret, env, typeEnv);
				Pair<Type, Substitution> p = conversion.infer(env, typeEnv);
				this.assertInference(p, expectedInfer, conversion);
			}

	protected void assertInterpretationEquals(String interpreted, Expression expected) throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		this.assertInterpretedStringEquals(interpreted, expected, env, typeEnv);
	}
	
	/**
	 * Converts the path to string useable in Velka code
	 * @param path converted path
	 * @return string
	 */
	protected String pathToStr(Path path) {
		return path.toString().replace("\\", "/");
	}

	public VelkaTest() {
		super();
	}

}