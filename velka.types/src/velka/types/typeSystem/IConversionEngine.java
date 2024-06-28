package velka.types.typeSystem;

import java.util.Collection;

import velka.types.TypeArrow;
import velka.types.TypeTuple;

/** Interface for converting specific language constructs */
public interface IConversionEngine {

	public Object convertTuple(TypeSystem typeSystem, TypeTuple from, TypeTuple to, Object o, Object env);
	
	public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env);
	
	public Collection<? extends Object> instantiateCollection(Object o);
}
