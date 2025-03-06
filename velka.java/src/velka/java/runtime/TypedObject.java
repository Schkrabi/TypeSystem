/**
 * 
 */
package velka.java.runtime;

import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;

/**
 * Class for object associated with velka type
 */
public class TypedObject {
	public final Object object;
	public final Type velkaType;
	
	public TypedObject(Object object, Type velkaType) {
		this.object = object;
		this.velkaType = velkaType;
	}
	
	@Override
	public String toString() {
		return this.object.toString();
	}
	
	@Override
	public int hashCode() {
		return this.object.hashCode();
	}
	
	@Override
	public boolean equals(Object o) {
		if(o instanceof TypedObject other) {
			return this.velkaType.equals(other.velkaType)
					&& (	this.object == other.object
						|| 	this.object.equals(other.object));
		}
		return false;
	}
	
	public static final TypedObject VELKA_EMPTY = new TypedObject(null, TypeTuple.EMPTY_TUPLE);
	
	public static String toStrHelper(Type type, Object str) {
		String s = str.toString();
		if(type.equals(TypeAtom.TypeListNative)) {
			s = s.replace('[', '(').replace(']', ')').replace(",", "");
		}
		else if (type.equals(TypeAtom.TypeSetTree) || type.equals(TypeAtom.TypeSetHash)) {
			s = s.replace("[", "#{").replace(']', '}').replace(",", "");
		}
		return s;
	}
	
	public static boolean canDeconstructAs(Object o, Type as) {
		if(o instanceof TypedObject to) {
			var type = JavaTypeSystem.instance().getType(to.object);
			return Type.unifyRepresentation(type, as).isPresent();
		}
		return false;
	}
	
	public static Object deconstructAs(Object o, Type as) {
		if(o instanceof TypedObject to) {
			var type = JavaTypeSystem.instance().getType(to.object);
			if(Type.unifyRepresentation(type, as).isEmpty()) {
				throw new RuntimeException("Illegal deconstruction " + o + " as " + as);
			}
			return to.object;
		}
		throw new RuntimeException("Illegal deconstruction " + o + " as " + as);
	}
}
