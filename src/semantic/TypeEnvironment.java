package semantic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import expression.TypeConstructionLambda;
import parser.SemanticNode;
import parser.SemanticPair;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import util.AppendableException;

public class TypeEnvironment {

	private Set<TypeAtom> types = new HashSet<TypeAtom>();
	private Map<TypeAtom, Set<TypeConstructionLambda>> constructorMap = new HashMap<TypeAtom, Set<TypeConstructionLambda>>();

	public TypeEnvironment() {
		// Int
		types.add(TypeAtom.TypeInt);
		types.add(TypeAtom.TypeIntNative);
		Set<TypeConstructionLambda> set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntPrimitiveConstructor);
		constructorMap.put(TypeAtom.TypeIntNative, set);
		types.add(TypeAtom.TypeIntRoman);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntRomanConstructor);
		constructorMap.put(TypeAtom.TypeIntRoman, set);
		types.add(TypeAtom.TypeIntString);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.IntStringConstructor);
		constructorMap.put(TypeAtom.TypeIntString, set);

		// Bool
		types.add(TypeAtom.TypeBool);
		types.add(TypeAtom.TypeBoolNative);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.BoolPrimitiveConstructor);
		constructorMap.put(TypeAtom.TypeBoolNative, set);

		// String
		types.add(TypeAtom.TypeString);
		types.add(TypeAtom.TypeStringNative);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.StringPrimitiveConstructor);
		constructorMap.put(TypeAtom.TypeStringNative, set);

		// Double
		types.add(TypeAtom.TypeDouble);
		types.add(TypeAtom.TypeDoubleNative);
		set = new TreeSet<TypeConstructionLambda>();
		set.add(TypeConstructionLambda.DoublePrimitiveConstructor);
		constructorMap.put(TypeAtom.TypeDoubleNative, set);
	}

	/**
	 * Gets the type by given name
	 * 
	 * @param typeName name of the type
	 * @return Optional containing the type if it exists
	 */
	public Optional<TypeAtom> getType(final String typeName) {
		Optional<TypeAtom> o = types.stream().filter(new java.util.function.Predicate<TypeAtom>() {

			@Override
			public boolean test(TypeAtom x) {
				return x.equals(new TypeAtom(new TypeName(typeName), TypeRepresentation.WILDCARD));
			}
		}).findAny();
		return o;
	}

	/**
	 * Gets te type by given base type name and representation name
	 * 
	 * @param typeName           name of the base type
	 * @param representationName name of the representaion
	 * @return Optional containing the type if it exists
	 */
	public Optional<TypeAtom> getType(final String typeName, final String representationName) {
		Optional<TypeAtom> o = types.stream().filter(new java.util.function.Predicate<TypeAtom>() {
			@Override
			public boolean test(TypeAtom x) {
				return x.name.name.equals(typeName) && x.representation.name.equals(representationName);
			}
		}).findAny();
		return o;
	}

	/**
	 * Checks if given semantic node is previously defined type
	 * 
	 * @param node inspected node
	 * @return true if node is a node containing type name
	 * @throws AppendableException
	 */
	public boolean isType(SemanticNode node) throws AppendableException {
		if (node.type == SemanticNode.NodeType.SYMBOL) {
			return this.getType(node.asSymbol()).isPresent();
		}
		if (node.type == SemanticNode.NodeType.PAIR) {
			SemanticPair p;
			p = node.asPair();
			return this.getType(p.first, p.second).isPresent();
		}
		return false;
	}

	/**
	 * Gets the constructor for given type
	 * 
	 * @param typeName searched type
	 * @return constructor for this type if it exists
	 */
	public TypeConstructionLambda getConstructor(TypeAtom type, final int argCount) {
		Set<TypeConstructionLambda> s = this.constructorMap.get(type);
		if (s == null) {
			throw new NoSuchElementException();
		}

		return s.stream().filter(new Predicate<TypeConstructionLambda>() {

			@Override
			public boolean test(TypeConstructionLambda arg) {
				return arg.args.size() == argCount;
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
	public TypeAtom addType(String typeName) throws AppendableException {
		Optional<TypeAtom> o = this.getType(typeName);
		if (o.isPresent()) {
			throw new AppendableException("Type " + typeName + " is already defined");
		}

		TypeAtom type = new TypeAtom(new TypeName(typeName), TypeRepresentation.WILDCARD);
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
	public TypeAtom addRepresentation(String baseTypeName, String repName) throws AppendableException {
		Optional<TypeAtom> o = this.getType(baseTypeName, repName);
		if (o.isPresent()) {
			throw new AppendableException("Type " + o.get() + " is already defined");
		}
		o = this.getType(baseTypeName);
		if (!o.isPresent()) {
			throw new UndefinedTypeException(baseTypeName);
		}

		TypeAtom type = new TypeAtom(new TypeName(baseTypeName), new TypeRepresentation(repName));
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
	public void addConstructor(TypeAtom newType, final TypeConstructionLambda constructor) throws AppendableException {
		Set<TypeConstructionLambda> set;

		if (constructorMap.containsKey(newType)) {
			set = this.constructorMap.get(newType);

			if (set.stream().anyMatch(new Predicate<TypeConstructionLambda>() {
				@Override
				public boolean test(TypeConstructionLambda arg) {
					return arg.args.size() == constructor.args.size();
				}
			})) {
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
	 * 
	 * @param fromType              Type which is converted
	 * @param toType                Type to which is converted
	 * @param conversionConstructor Conversion lambda (constructor)
	 */
	public void addConversion(TypeAtom fromType, TypeAtom toType, TypeConstructionLambda conversionConstructor)
			throws AppendableException {
		if (!this.types.contains(fromType)) {
			throw new UndefinedTypeException(fromType.toString());
		}
		if (!this.types.contains(toType)) {
			throw new UndefinedTypeException(toType.toString());
		}
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new AppendableException(
					"You can only define conversion between representations, got " + fromType + " and " + toType);
		}

		fromType.addConversion(toType.representation, conversionConstructor);
	}
}
