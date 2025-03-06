package velka.core.interfaces;

import com.sun.codemodel.JExpression;

import velka.core.interpretation.Environment;

public interface CompileableToJava {

	/** Compiles this expression to java expression*/
	JExpression toJavaExpr(Environment env);
}
