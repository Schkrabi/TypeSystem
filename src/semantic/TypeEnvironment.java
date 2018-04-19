package semantic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import expression.TypeConstructionLambda;
import parser.SemanticNode;
import parser.SemanticNode.Pair;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.AppendableException;

public class TypeEnvironment {

	private Set<TypeConcrete> types = new HashSet<TypeConcrete>();
	private Map<TypeConcrete, Set<TypeConstructionLambda>> constructorMap = new HashMap<TypeConcrete, Set<TypeConstructionLambda>>();

	public TypeEnvironment() {
		// Int
		types.add(TypeConcrete.TypeInt);
		Set<TypeConstructionLambda> set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntPrimitiveConstructor);
		constructorMap.put(TypeConcrete.TypeInt, set);
		types.add(TypeRepresentation.TypeIntRoman);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntRomanConstructor);
		constructorMap.put(TypeRepresentation.TypeIntRoman, set);
		types.add(TypeRepresentation.TypeIntString);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntStringConstructor);
		constructorMap.put(TypeRepresentation.TypeIntString, set);

		// Bool
		types.add(TypeConcrete.TypeBool);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.BoolPrimitiveConstructor);
		constructorMap.put(TypeConcrete.TypeBool, set);

		// String
		types.add(TypeConcrete.TypeString);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.StringPrimitiveConstructor);
		constructorMap.put(TypeConcrete.TypeString, set);

		// Double
		types.add(TypeConcrete.TypeDouble);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.DoublePrimitiveConstructor);
		constructorMap.put(TypeConcrete.TypeDouble, set);
	}

	/**
	 * Gets the type by given name
	 * 
	 * @param typeName
	 *            name of the type
	 * @return Optional containing the type if it exists
	 */
	public Optional<TypeConcrete> getType(final String typeName) {
		Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>() {

			@Override
			public boolean test(TypeConcrete x) {
				return (!(x instanceof TypeRepresentation)) && x.name.equals(typeName);
			}
		}).findAny();
		return o;
	}

	/**
	 * Gets te type by given base type name and representation name
	 * 
	 * @param typeName
	 *            name of the base type
	 * @param representationName
	 *            name of the representaion
	 * @return Optional containing the type if it exists
	 */
	public Optional<TypeConcrete> getType(final String typeName, final String representationName) {
		Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>() {
			@Override
			public boolean test(TypeConcrete x) {
				return (x instanceof TypeRepresentation) && x.name.equals(representationName)
						&& ((TypeRepresentation) x).baseType.name.equals(typeName);
			}
		}).findAny();
		return o;
	}

	/**
	 * Checks if given semantic node is previously defined type
	 * 
	 * @param node
	 *            inspected node
	 * @return true if node is a node containing type name
	 */
	public boolean isType(SemanticNode node) {
		if (node.type == SemanticNode.NodeType.SYMBOL) {
			try {
				return this.getType(node.asSymbol()).isPresent();
			} catch (Exception e) {
				return false;
			}
		}
		if (node.type == SemanticNode.NodeType.PAIR) {
			Pair p;
			try {
				p = node.asPair();
			} catch (Exception e) {
				return false;
			}
			return this.getType(p.lvalue, p.rvalue).isPresent();
		}
		return false;
	}

	/**
	 * Gets the constructor for given type
	 * 
	 * @param type
	 *            searched type
	 * @return constructor for this type if it exists
	 */
	public TypeConstructionLambda getConstructor(TypeConcrete type, final int argCount) {
		return this.constructorMap.get(type).stream().filter(new Predicate<TypeConstructionLambda>(){

			@Override
			public boolean test(TypeConstructionLambda arg) {
				return arg.args.values.length == argCount;
			}
			
		}).findAny().get();
	}

	/**
	 * Adds the type to type set
	 * 
	 * @param newType
	 * @return Newly added type
	 * @throws AppendableException
	 */
	public TypeConcrete addType(String typeName) throws AppendableException {
		Optional<TypeConcrete> o = this.getType(typeName);
		if (o.isPresent()) {
			throw new AppendableException("Type " + typeName + " is already defined");
		}

		TypeConcrete type = new TypeConcrete(typeName);
		types.add(type);
		return type;
	}

	/**
	 * Add the type representation to the type set
	 * 
	 * @param baseTypeName
	 * @param repName
	 * @return newly added representation
	 * @throws AppendableException
	 */
	public TypeConcrete addRepresentation(String baseTypeName, String repName) throws AppendableException {
		Optional<TypeConcrete> o = this.getType(baseTypeName, repName);
		if (o.isPresent()) {
			throw new AppendableException("Type " + o.get() + " is already defined");
		}
		o = this.getType(baseTypeName);
		if (!o.isPresent()) {
			throw new AppendableException("Unknown base type: " + baseTypeName);
		}

		TypeRepresentation type = new TypeRepresentation(repName, o.get());
		types.add(type);
		return type;
	}

	/**
	 * Maps the construcotr to given type
	 * 
	 * @param newType
	 * @param constructor
	 * @throws AppendableException
	 */
	public void addConstructor(TypeConcrete newType, final TypeConstructionLambda constructor) throws AppendableException {
		Set<TypeConstructionLambda> set;
		
		if (constructorMap.containsKey(newType)) {
			set = this.constructorMap.get(newType); 
			
			if(set.stream().anyMatch(new Predicate<TypeConstructionLambda>(){
				@Override
				public boolean test(TypeConstructionLambda arg) {
					return arg.args.values.length == constructor.args.values.length;
				}})){
				throw new AppendableException("Constructor for " + newType + " is already defined");
			}
			set.add(constructor);
			return;
		}
		set = new TreeSet<TypeConstructionLambda>();
		set.add(constructor);
		constructorMap.put(newType, set);
	}
	
	/**
	 * Adds new conversion to the environment
	 * @param fromType Type which is converted
	 * @param toType Type to which is converted
	 * @param conversionConstructor Conversion lambda (constructor)
	 */
	public void addConversion(TypeConcrete fromType, TypeConcrete toType, TypeConstructionLambda conversionConstructor) throws AppendableException {
		if(!this.types.contains(fromType)) {
			throw new UndefinedTypeException(fromType.toString());
		}
		if(!this.types.contains(toType)) {
			throw new UndefinedTypeException(toType.toString());
		}
		
		fromType.addConversion(toType, conversionConstructor);
	}
}
