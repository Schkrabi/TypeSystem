package velka.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.DiagnosticListener;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;

import velka.clojure.ClojureCodeGenerator;
import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.DefineSymbol;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.ConstructorOperators;
import velka.core.langbase.ConversionOperators;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.Operators;
import velka.core.literal.Literal;
import velka.core.util.Constants;
import velka.java.generate.ClassGenerator;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaTuple;
import velka.parser.Parser;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

public class VelkaTest {

	private boolean displayJavaWarning = false;
	private boolean displayJavaError = true;
	private boolean displayJavaOther = false;
	
	static Path tmpDir;
	private static String os = System.getProperty("os.name").toLowerCase();
	public static boolean IS_WINDOWS = (os.indexOf("win") >= 0);
	public static boolean IS_UNIX = (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0 || os.indexOf("aix") > 0);
	protected static final Path velkaUtilJar = Paths.get("C:", "Users", "r.skrabal", "Documents", "private-r.skrabal", "Java", "TypeSystem", "lib", "velka.util.jar");
	protected static final Path velkaTypesJar = Paths.get("C:", "Users", "r.skrabal", "Documents", "private-r.skrabal", "Java", "TypeSystem", "lib", "velka.types.jar");
	
	protected List<String> cljCmdArgs = new ArrayList<String>();
	protected Environment env;

	@BeforeAll
	static void setupTest() throws IOException {
		TestComplex.tmpDir = Files.createTempDirectory("cljTest");
		
		ClojureCodeGenerator.generateClojureProject(tmpDir);
		Files.copy(velkaUtilJar, tmpDir.resolve(Paths.get("velka.util.jar")), StandardCopyOption.REPLACE_EXISTING);
		Files.copy(velkaTypesJar, tmpDir.resolve(Paths.get("velka.types.jar")), StandardCopyOption.REPLACE_EXISTING);
	}
	
	@BeforeEach
	void setupCmdArgs() {
		this.env = TopLevelEnvironment.instantiate();
		this.cljCmdArgs = new ArrayList<String>();
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
			
				sb.append(ClojureHelper.declareNamespace(Constants.DEFAULT_NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
				sb.append(ClojureHelper.requireNamespace(Operators.singleton().getNamespace()));
				sb.append(ClojureHelper.requireNamespace(ListNative.singleton().getNamespace()));
				sb.append(ClojureHelper.requireNamespace(ConstructorOperators.singleton().getNamespace()));
				sb.append(ClojureHelper.requireNamespace(ConversionOperators.singleton().getNamespace()));
				sb.append(ClojureHelper.requireNamespace(JavaLinkedList.singleton().getNamespace()));
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
	protected void assertInterpretedStringEquals(String code, Expression expected, Environment env)
			throws AppendableException {
				Expression last = null;
				for (Expression e : this.parseString(code)) {
					@SuppressWarnings("unused")
					Pair<Type, Substitution> p = e.infer(env);
					last = e.interpret(env);
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
	private String compileExpressionsToClojure(List<Expression> l, Environment env)
			throws AppendableException {
				StringBuilder s = new StringBuilder();
				Iterator<Expression> i = l.iterator();
				while (i.hasNext()) {
					s.append(i.next().toClojureCode(env));
					if (i.hasNext()) {
						s.append('\n');
					}
				}
				return s.toString();
			}

	private String compileToClojure(String code, Environment env) throws AppendableException {
		List<Expression> l = this.parseString(code);
		return compileExpressionsToClojure(l, env);
	}

	@SuppressWarnings("unused")
	private void assertCompiledExpressionEquals(Expression e, String expected, Environment env)
			throws AppendableException {
				List<Expression> l = new LinkedList<Expression>();
				l.add(e);
				String s = this.compileExpressionsToClojure(l, env);
				assertEquals(expected, s);
			}

	protected void assertCompiledCodeEquals(String code, String expected, Environment env)
			throws AppendableException {
				String s = this.compileToClojure(code, env);
			
				assertEquals(expected, s);
			}

	@SuppressWarnings("unused")
	private void assertCompiledCodeMatch(String code, String regex, Environment env)
			throws AppendableException {
				String s = this.compileToClojure(code, env);
				if (!s.matches(regex)) {
					fail("Clojure compilation test failed, compiling " + code + " do not match " + regex + " got "
							+ s.toString());
				}
			}

	protected void assertCompile(String code, Environment env) throws AppendableException {
		this.compileToClojure(code, env);
	}

	protected void assertIntprtAndCompPrintSameValues(String code) throws Exception {
		List<Expression> exprs = Parser.read(code);
		this.assertIntprtAndCompPrintSameValues(exprs);
	}
	
	/** asserts compiled code prints the given value **/
	public void assertCompiledCodePrints(String code, String expectedPrintou) throws Exception {
		List<Expression> exprs = Parser.read(code);
		this.assertCompiledCodePrints(exprs, expectedPrintou);
	}
	
	
	/** asserts compiled code prints the given value */
	public void assertCompiledCodePrints(List<Expression> in, String expectedPrintout) throws Exception {
		Environment cmplEnv = TopLevelEnvironment.instantiate();
		
		String compilationPrintOut = clojureCompilationResult(in, cmplEnv);
	
		var eNorm = expectedPrintout.trim().replace("\t", " ").replaceAll("\\s+", " ");
		var aNorm = compilationPrintOut.trim().replace("\t", " ").replaceAll("\\s+", " ");
		
		assertEquals(
				eNorm, 
				aNorm);
	}

	protected void assertIntprtAndCompPrintSameValues(List<Expression> in) throws Exception {
		Environment intpEnv = TopLevelEnvironment.instantiate();
	
		String interpretationPrintOut = interpretationPrint(in, intpEnv);
	
		this.assertCompiledCodePrints(in, interpretationPrintOut);
	}

	private String interpretationPrint(List<Expression> in, Environment env) throws Exception {
		PrintStream stdOut = System.out;
		ByteArrayOutputStream tmp = new ByteArrayOutputStream();
		System.setOut(new PrintStream(tmp, true, "UTF-8"));
	
		@SuppressWarnings("unused")
		List<Expression> rslt = velka.compiler.Compiler.eval(in, env);
	
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
				pb.command().addAll(this.cljCmdArgs);
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
				Path codeFile = Files.writeString(tmpDir.resolve(ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH), code);
				
				ProcessBuilder pb = new ProcessBuilder("powershell", "-command", "clj",	"-M", codeFile.toAbsolutePath().toString());
				pb.command().addAll(this.cljCmdArgs);
				pb.inheritIO();
				pb.directory(tmpDir.toFile());
				
				pb.environment().put("PATH", "C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\windows\\system32;C:\\windows;C:\\windows\\System32\\Wbem;C:\\windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\windows\\System32\\OpenSSH\\;C:\\Program Files\\dotnet\\;C:\\Program Files\\Git\\cmd;C:\\Program Files (x86)\\dotnet\\;C:\\Java\\jdk-20.0.1\\bin;C:\\Java\\apache-ant-1.10.12\\bin;C:\\Users\\r.skrabal\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\r.skrabal\\.dotnet\\tools;C:\\windows\\System32;");
				
				File tempOut = File.createTempFile("velka_clojure_test_out", null);
				File tempErr = File.createTempFile("velka_clojure_test_err", null);
				
				pb.redirectOutput(tempOut);
				pb.redirectError(tempErr);
			
				Process p = pb.start();
				p.waitFor();
				
				String result = Files.readString(tempOut.toPath(), StandardCharsets.UTF_8);
				String err = Files.readString(tempErr.toPath());
				tempOut.delete();	
				tempErr.delete();
				Files.delete(codeFile);
				
				if(!err.isEmpty()) {
					throw new AppendableException(err);
				}
				
				return result;
			}

	private String clojureCompilationResult(List<Expression> l, Environment env) throws Exception {
		String code = ClojureCodeGenerator.ExpressionListToClojureCode(l, env);
		
		String result = this.clojureCodeResult(code);
		return result;
	}

	protected void assertCompiledCodeEquals(String code, String expected) throws Exception {
		Environment env = TopLevelEnvironment.instantiate();	
		this.assertCompiledCodeEquals(code, expected, env);
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
		//assertNotEquals(original.compareTo(e), 0);
	}

	protected void assertInference(Pair<Type, Substitution> result, Type expected, Expression infered) {
		this.assertInference(result, expected, infered, false);
	}

	protected void assertInference(Pair<Type, Substitution> p, Type expected, Expression infered, boolean shouldeBeSUbstEmpty) {
		assertTrue(Type.unifyRepresentation(expected, p.first).isPresent());
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

	protected void assertInterpretationEquals(Expression interpreted, Expression expected, Environment env)
			throws AppendableException {
				Expression e = interpreted.interpret(env);
				assertEquals(expected, e);
			}

	protected void assertInterpretationLastEquals(Collection<Expression> interpreted, Expression expected, Environment env)
			throws AppendableException {
				Expression f = null;
				for(Expression e : interpreted) {
					f = e.interpret(env);
				}
				assertEquals(expected, f);
			}

	protected void assertOperator(final Operator operator, Tuple args, Expression expectedInterpret, Type expectedInference)
			throws AppendableException {
				AbstractionApplication application = new AbstractionApplication(operator, args);
				Environment env = TopLevelEnvironment.instantiate();
			
				this.assertInterpretationEquals(application, expectedInterpret, env);
				Pair<Type, Substitution> p = application.infer(env);
				if (expectedInference != null) {
					this.assertInference(p, expectedInference, application);
				}
			
				assertAll(() -> {
					operator.toString();
					operator.toClojureCode(env);
				});
			}

	protected void assertConversion(Abstraction conversion, Expression argument, Expression expectedInterpret, Type expectedInfer)
			throws AppendableException {
				assertAll(() -> {
					conversion.toString();
				});
			
				AbstractionApplication appl = new AbstractionApplication(conversion, new Tuple(Arrays.asList(argument)));
				Environment env = TopLevelEnvironment.instantiate();
			
				this.assertInterpretationEquals(appl, expectedInterpret, env);
				Pair<Type, Substitution> p = conversion.infer(env);
				this.assertInference(p, expectedInfer, conversion);
			}

	protected void assertInterpretationEquals(String interpreted, Expression expected) throws AppendableException {
		Environment env = TopLevelEnvironment.instantiate();
		this.assertInterpretedStringEquals(interpreted, expected, env);
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
	
//	private com.sun.codemodel.JCodeModel prepareClass(Collection<JExpression> exprs, String className, String methodName) {
//		if(exprs.isEmpty()) {
//			throw new RuntimeException("No expressions to add to class!");
//		}
//		
//		var localCodeModel = new com.sun.codemodel.JCodeModel();
//		JDefinedClass testClass;
//		try {
//			testClass = localCodeModel._class(className);
//		} catch (JClassAlreadyExistsException e) {
//			throw new RuntimeException(e);
//		}
//		
//		var testMethod = testClass.method(JMod.PUBLIC, localCodeModel.ref(Object.class), methodName);
//		testMethod._throws(AppendableException.class);
//		
//		var it = exprs.iterator();
//		JExpression last = null;
//		while(it.hasNext()) {
//			var expr = it.next();
//			if(!it.hasNext()) {
//				last = expr;
//				break;
//			}
//			
//			if(expr instanceof com.sun.codemodel.JStatement js) {
//				testMethod.body().add(js);
//			}
//			else {
//				throw new RuntimeException("Adding non statement " + expr);
//			}
//		}
//		testMethod.body()._return(last);
//		
//		return localCodeModel;
//	}

	/** Compiles JExpression, evaluates it and returns its value */
	private Object compileJExprs(Collection<? extends Expression> exprs) {
		var symbol = "_test";
		final boolean showWarning = this.displayJavaWarning;
		final boolean showError = this.displayJavaError;
		final boolean showOther = this.displayJavaOther;
		
		try {			
			var es = new ArrayList<Expression>();
			var i = exprs.iterator();
			while(i.hasNext()) {
				var e = i.next();
				if(!i.hasNext()) {
					es.add(new DefineSymbol(new Symbol(symbol),
							new Lambda(e, List.of())));
				}
				else {
					es.add(e);
				}
			}
			
			var workingDir = Files.createTempDirectory("jcompilation-test").toFile();
			var localCodeModel = new com.sun.codemodel.JCodeModel();
			
			var generator = new ClassGenerator(localCodeModel);
			
			var files = generator.build(es, workingDir, false);
			
			var compiler = ToolProvider.getSystemJavaCompiler();
	        var fileManager = compiler.getStandardFileManager(null, null, null);
	        
	        //Add required libs
	        var velkautil = new File("../lib/velka.util.jar");
	        var velkatypes = new File("../lib/velka.types.jar");
	        var velkajava = new File("../lib/velka.java.jar");
	        
	        var classpath = workingDir.getPath() 
	        		+ File.pathSeparator + velkautil.getAbsolutePath()
	        		+ File.pathSeparator + velkatypes.getAbsolutePath()
	        		+ File.pathSeparator + velkajava.getAbsolutePath();
	        
	        var sourceFiles = (File[])files.values().stream().map(p -> p.toFile()).toArray(l -> new File[l]);
	        var compilationUnits = fileManager.getJavaFileObjects(sourceFiles);
	        
	        fileManager.setLocation(StandardLocation.CLASS_OUTPUT, java.util.Collections.singletonList(workingDir));
	        compiler.getTask(
	        		null, 
	        		fileManager, 
	        		new DiagnosticListener<JavaFileObject>() {
	                    @Override
	                    public void report(Diagnostic<? extends JavaFileObject> diagnostic) {
	                        switch(diagnostic.getKind()) {
	                        case WARNING:
	                        	if(showWarning) {
	                        		System.err.println(diagnostic.toString());
	                        	}
	                        	break;
	                        case ERROR:
	                        	if(showError) {
	                        		System.err.println(diagnostic.toString());
	                        	}
	                        	break;
	                        default:
	                        	if(showOther) {
	                        		System.out.println(diagnostic.toString());
	                        	}
	                        }
	                    }
	                }, 
	        		List.of("-classpath", classpath, "-Xlint:unchecked", "-g"), 
	        		null, 
	        		compilationUnits)
	        	.call();
	        fileManager.close();
			
	        var classLoader = URLClassLoader.newInstance(new URL[]{workingDir.toURI().toURL()});
	        
	        Map<String, Class<? extends Object>> loadedCls = new HashMap<String, Class<? extends Object>>();
	        
	        for(var name : files.keySet()) {
	        	var cl = Class.forName(name, true, classLoader);
	        	loadedCls.put(name, cl);
	        }
	        
	        var loadedClass = loadedCls.get(velka.core.util.Constants.DEFAULT_NAMESPACE);
	        
	        //var instance = loadedClass.getDeclaredConstructor().newInstance();
	        var _field = loadedClass.getField(symbol);
	        var _mthd = (VelkaAbstraction)_field.get(null);
	        
	        var ret = _mthd.apply(new VelkaTuple(List.of(), TypeTuple.EMPTY_TUPLE));
	        
			return ret;
		}
		catch(Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	protected void assertJExprEquals(Object expected, Expression expr) {
		this.assertJExprsEquals(expected, List.of(expr));
	}
	
	protected void assertJExprsEquals(Object expected, Collection<? extends Expression> exprs) {
		JavaTypeSystem.instance().reset();
		var ret = this.compileJExprs(exprs);
		assertEquals(expected, ret);
		JavaTypeSystem.instance().reset();
	}
	
	protected void assertJExprEquals(Object expected, String code, Environment env) {
		try {
			var exprs = this.parseString(code);
			this.assertJExprsEquals(expected, exprs);
		} catch (AppendableException ex) {
			throw new RuntimeException(ex);
		}		
	}
	
	protected void assertVelkaCode(String code, Object expected) {
		try {
			this.assertInterpretationEquals(code,
					Literal.objectToLiteral(expected));
			
			this.assertIntprtAndCompPrintSameValues("(println (to-str " + code + "))");
			
			this.assertJExprEquals(expected, code, env);
		} catch (Exception e) {
			fail(e);
		}
	}
	
	protected void assertVelkaThrows(String code) {
		Assertions.assertThrows(RuntimeException.class, () -> {
			this.assertInterpretationEquals(code, null);
		});
		
		Assertions.assertThrows(RuntimeException.class, () -> {
			this.assertJExprEquals(code, null);
		});
	}
}