package velka.types.typeSystem;

public interface ITypeSystemInitializer {

	public void initializeConstructors(TypeSystem typeSystem);
	
	public void initializeConversions(TypeSystem typeSystem);
}
