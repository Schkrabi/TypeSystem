package velka.java;

import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JVar;

import velka.java.runtime.TypedObject;

/**
 * Holds reference to codemodel object used to generate Java code
 */
public class CodeModelInstance {

	private static JCodeModel codeModel = null;
	
	public static JCodeModel instance() {
		if(codeModel == null) {
			codeModel = new JCodeModel();
		}
		return codeModel;
	}
	
	public static void reset() {
		codeModel = new JCodeModel();
	}
	
	public static com.sun.codemodel.JDefinedClass findOrCreateClass(String fullyQualifiedName){
		var cl = CodeModelInstance.instance()._getClass(fullyQualifiedName);
		if(cl == null) {
			try {
				cl = CodeModelInstance.instance()._class(fullyQualifiedName);
			} catch (JClassAlreadyExistsException e) {
				//Unlikely
				throw new RuntimeException(e);
			}
		}
		return cl;
	}
	
	private static JCodeModel dumpModel = null;
	private static JDefinedClass dumpClass = null;
	private static JMethod dumpMethod = null;
	
	private static JCodeModel iDumpModel() {
		if(dumpModel == null) {
			dumpModel = new JCodeModel();
		}
		return dumpModel;
	}
	
	private static JDefinedClass iDumpClass() {
		if(dumpClass == null) {
			try {
				dumpClass = iDumpModel()._class("velka.java.dump");
			}catch(JClassAlreadyExistsException e) {
				throw new RuntimeException(e);
			}
		}
		return dumpClass;
	}
	
	private static JMethod iDumpMethod() {
		if(dumpMethod == null) {
			dumpMethod = iDumpClass().method(JMod.PUBLIC, iDumpModel().VOID, "dump");
		}
		return dumpMethod;
	}
	
	public static JVar makeJVar(String name) {
		var v = iDumpMethod().body().decl(iDumpModel().ref(Object.class), name);
		return v;
	}
	
	public static com.sun.codemodel.JExpression emptyExpression() {
		return instance().ref(TypedObject.class).staticRef("VELKA_EMPTY");
	}
}
