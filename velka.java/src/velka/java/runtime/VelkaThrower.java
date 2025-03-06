package velka.java.runtime;

import velka.java.CodeModelInstance;

public class VelkaThrower {
	public static Object thrower(String message) {
		throw new RuntimeException(message);
	}
	
	public static com.sun.codemodel.JInvocation _throw(com.sun.codemodel.JExpression message){
		return CodeModelInstance.instance().ref(VelkaThrower.class)
				.staticInvoke("thrower")
				.arg(message);
	}
}
