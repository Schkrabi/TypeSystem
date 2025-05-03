package velka.java.runtime;

import java.util.Map;

import velka.java.CodeModelInstance;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.typeSystem.ImplementationSelector;
import velka.types.typeSystem.TypeSystem;
import velka.types.typeSystem.VelkaAbstraction;

/** Type system singleton instance for java runtime*/
public class JavaTypeSystem extends TypeSystem {

	private ImplementationSelector implementationSelector = new ImplementationSelector(this);
	
	private JavaTypeSystem() {
		super(new JavaConversionEngine());
	}
	
	private static JavaTypeSystem singleton = null;
	
	public static JavaTypeSystem instance() {
		if(singleton == null) {
			singleton = new JavaTypeSystem();
		}
		return singleton;
	}
	
	public Type getType(Object o) {
		if(o instanceof VelkaTuple t) {
			return t.type;
		}
		else if(o instanceof TypedObject to) {
			return to.velkaType;
		}
		else if(o instanceof VelkaAbstraction va) {
			return va.getType();
		}
		var t = TypeAtom.javaClassToType(o.getClass());
		if(t != null) {
			return t;
		}
		throw new RuntimeException(new StringBuilder()
				.append("Type ")
				.append(o.getClass())
				.append(" not recognized.")
				.toString());
	}
	
	public void reset() {
		this.typeInfo.clear();
	}
	
	/** Gets the type system instance in the codemodel */
	public static com.sun.codemodel.JExpression codeInstance() {
		return CodeModelInstance.instance().ref(JavaTypeSystem.class).staticInvoke("instance");	
	}

	@Override
	public ImplementationSelector getImplementationSelector() {
		return this.implementationSelector;
	}
}