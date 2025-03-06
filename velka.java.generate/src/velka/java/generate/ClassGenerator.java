package velka.java.generate;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMod;

import velka.core.application.DefineSymbol;
import velka.core.expression.Expression;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.OperatorBank;
import velka.core.util.OperatorBankUtil;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.VelkaTuple;
import velka.parser.Parser;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;

public class ClassGenerator {
	
	protected final com.sun.codemodel.JCodeModel codemodel;
	
	private boolean displayFileNamesInOutput = false;
	
	
	public ClassGenerator(com.sun.codemodel.JCodeModel codemodel) {
		this.codemodel = codemodel;
	}

	/** Generates code for an operator bank*/
	@SuppressWarnings("exports")
	public com.sun.codemodel.JDefinedClass generate(OperatorBank operatorBank){
		JDefinedClass cl = null;
		try {
			cl = this.codemodel._class(operatorBank.getNamespace());
		} catch (JClassAlreadyExistsException e) {
			throw new RuntimeException(e);
		}

		try {
			var env = TopLevelEnvironment.instantiate();

			for (var con : OperatorBankUtil.getConstructors(operatorBank.getClass())) {
				var code = con.toJavaExpr(env);
				if (code instanceof com.sun.codemodel.JStatement js) {
					cl.init().add(js);
				} else {
					throw new RuntimeException("malformed constructor operator");
				}
			}

			for (var conv : OperatorBankUtil.getConversions(operatorBank.getClass())) {
				var code = conv.toJavaExpr(env);
				if (code instanceof com.sun.codemodel.JStatement js) {
					cl.init().add(js);
				} else {
					throw new RuntimeException("malformed conversion operator");
				}
			}

			for (var op : OperatorBankUtil.getOperators(operatorBank.getClass())) {
				cl.field(JMod.PUBLIC | JMod.STATIC, VelkaAbstraction.class, op.getInternalSymbol().name,
						op.toJavaExpr(env));
			}
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}

		return cl;
	}
	
	public static final String _INTERNAL_MAIN_SYMBOL = "_main";
	public static final String _EXTERNAL_MAIN_SYMBOL = "main";
	
	/** Generates code for a list of expressions */
	@SuppressWarnings("exports")
	public com.sun.codemodel.JDefinedClass generate(String className, Collection<? extends Expression> exprs) {
		JDefinedClass cl = null;
		try {
			cl = this.codemodel._class(className);
		}catch(JClassAlreadyExistsException e) {
			throw new RuntimeException(e);
		}
		
		try {
			var env = TopLevelEnvironment.instantiate();
			
			for(var expr : exprs) {
				if(expr instanceof CompileableToJava cexpr) {
					var code = cexpr.toJavaExpr(env);
					
					if(expr instanceof DefineSymbol ds) {
						var type = ds.defined.infer(env).first;
						var jtype = TypeUtil.instance().velkaTypeToJType(type);
						code = JExpr.cast(jtype, code);
						
						if(ds.name.name.equals(_EXTERNAL_MAIN_SYMBOL)) {
							cl.field(JMod.PRIVATE | JMod.STATIC, jtype, _INTERNAL_MAIN_SYMBOL, code);
						}
						else {
							cl.field(JMod.PUBLIC | JMod.STATIC, jtype, ds.name.name, code);
						}
					}
					else if(code instanceof com.sun.codemodel.JStatement js) {
						cl.init().add(js);
					}
					else {
						throw new RuntimeException("Non statement code " + code.toString());
					}
				}
				else {
					throw new RuntimeException("Expression " + expr.toString() + " cannot be compiled to Java.");
				}
			}
			
		} catch (IllegalArgumentException | AppendableException e) {
			throw new RuntimeException(e);
		}
		
		return cl;
	}
	
	/** Generates code for input stream with velka code */
	public com.sun.codemodel.JDefinedClass generate(String className, InputStream stream){
		try {
			var exprs = Parser.read(stream);
			var cl = this.generate(className, exprs);
			return cl;
		} catch (IOException | AppendableException e) {
			throw new RuntimeException(e);
		}
	}
	
	/** Generates code for velka code string */
	public com.sun.codemodel.JDefinedClass generate(String className, String code){
		var stream = new ByteArrayInputStream(code.getBytes());
		var cl = this.generate(className, stream);
		return cl;
	}
	
	/** Generates entrypoint code for collection of expression */
	@SuppressWarnings("exports")
	public com.sun.codemodel.JDefinedClass generateEntrypoint(String className, Collection<? extends Expression> exprs){
		JDefinedClass cl = this.generate(className, exprs);
		
		var _main = cl.method(JMod.PUBLIC | JMod.STATIC, CodeModelInstance.instance().VOID, "main");
		var _args = _main.param(CodeModelInstance.instance().ref(String.class).array(), "args");
		
		var velkaTupleCl = CodeModelInstance.instance().ref(VelkaTuple.class);
		
		var tupleArgs = _main.body().decl(velkaTupleCl, "_targs", velkaTupleCl.staticInvoke("fromArgs").arg(_args));
		
		_main.body().add(cl.staticRef(_INTERNAL_MAIN_SYMBOL).invoke("apply").arg(tupleArgs));
		
		return cl;
	}
	
	/** Generates entrypoint code for input stream with Velka code */
	public com.sun.codemodel.JDefinedClass generateEntrypoint(String className, InputStream stream){
		List<Expression> exprs;
		try {
			exprs = Parser.read(stream);
		} catch (IOException | AppendableException e) {
			throw new RuntimeException(e);
		}
		var cl = this.generateEntrypoint(className, exprs);
		return cl;
	}
	
	/** Generates code for Velka code string */
	public com.sun.codemodel.JDefinedClass generateEntrypoint(String className, String code){
		var stream = new ByteArrayInputStream(code.getBytes());
		var cl = this.generateEntrypoint(className, stream);
		return cl;
	}
	
	/** Builds a single file project
	 * @param exprs list of expressions 
	 * @param workingDir directory where the source is generated
	 * @return Map of fully qualified class name : path to java file
	 */
	public Map<String, Path> build(Collection<? extends Expression> exprs, File workingDir) {
		var ret = new HashMap<String, Path>();
		var wdPath = Path.of(workingDir.getAbsolutePath());
		
		
		for(var operatorBank : OperatorBank.operatorBanks) {
			this.generate(operatorBank);
			ret.put(operatorBank.getNamespace(), wdPath.resolve(operatorBank.javaFilePath()));
		}
		
		this.generate(velka.core.util.Constants.DEFAULT_NAMESPACE, exprs);
		ret.put(velka.core.util.Constants.DEFAULT_NAMESPACE, wdPath.resolve(velka.core.util.Constants.DEFAULT_JAVA_FILE));
		
		try {
			var originalOut = System.out;

			if(!this.isDisplayFileNamesInOutput()) {
		        System.setOut(new PrintStream(new OutputStream() {
		            @Override
		            public void write(int b) {
		                // Do nothing
		            }
		        }));
			}
			
			this.codemodel.build(workingDir);
			
			if(!this.isDisplayFileNamesInOutput()) {
				System.setOut(originalOut);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
		return ret;
	}
	
	/** Builds a single file project*/
	public Map<String, Path> build(InputStream stream, File workingDirectory){
		List<Expression> exprs;
		try {
			exprs = Parser.read(stream);
		} catch (IOException | AppendableException e) {
			throw new RuntimeException(e);
		}
		var ret = this.build(exprs, workingDirectory);
		return ret;
	}

	public boolean isDisplayFileNamesInOutput() {
		return displayFileNamesInOutput;
	}

	public void setDisplayFileNamesInOutput(boolean displayFileNamesInOutput) {
		this.displayFileNamesInOutput = displayFileNamesInOutput;
	}
}
