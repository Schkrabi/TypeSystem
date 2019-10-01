package semantic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import conversions.IntNativeToIntRomanWrapper;
import conversions.IntNativeToIntStringWrapper;
import conversions.IntRomanToIntNativeWrapper;
import conversions.IntRomanToIntStringWrapper;
import conversions.IntStringToIntNativeWrapper;
import conversions.IntStringToIntRomanWrapper;
import expression.Application;
import expression.Expression;
import expression.Function;
import expression.Tuple;
import parser.SemanticNode;
import parser.SemanticPair;
import types.ConversionException;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import util.AppendableException;
import util.Pair;

public class TypeEnvironment {	
	private Set<TypeAtom> atomicTypes =  new TreeSet<TypeAtom>();
	private Map<TypeAtom, Function> constructorMap = new TreeMap<TypeAtom, Function>();
	private Map<Pair<TypeAtom, TypeAtom>, Function> conversions = new HashMap<Pair<TypeAtom, TypeAtom>, Function>();

	private TypeEnvironment() {
	}

	/**
	 * Gets the type by given name
	 * 
	 * @param typeName name of the type
	 * @return Optional containing the type if it exists
	 */
	public Optional<TypeAtom> getType(final String typeName) {
		Optional<TypeAtom> o = this.atomicTypes.stream().filter(new java.util.function.Predicate<TypeAtom>() {

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
		Optional<TypeAtom> o = constructorMap.keySet().stream().filter(new java.util.function.Predicate<TypeAtom>() {
			@Override
			public boolean test(TypeAtom x) {
				return x.name.name.equals(typeName) && x.representation.name.equals(representationName);
			}
		}).findAny();
		return o;
	}
	
	/**
	 * Tries to get type from semantic node, if node is not pair or symbol throws
	 * @param node parsed node
	 * @return optional of type or empty optional if type is not recognized
	 * @throws AppendableException
	 */
	public Optional<TypeAtom> getType(SemanticNode node) throws AppendableException {
		if(node.type == SemanticNode.NodeType.PAIR) {
			return this.getType(node.asPair().first, node.asPair().second);
		}
		return this.getType(node.asSymbol());
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
	public Function getConstructor(TypeAtom type) {
		if(this.constructorMap.containsKey(type))
			return this.constructorMap.get(type);
		throw new NoSuchElementException();
	}
	
	public void addType(TypeName name) {
		this.atomicTypes.add(new TypeAtom(name, TypeRepresentation.WILDCARD));
	}

	/**
	 * Maps the construcotr to given type
	 * 
	 * @param newType
	 * @param constructor
	 * @throws AppendableException
	 */
	public void addRepresentation(TypeAtom newType, final Function constructor) throws AppendableException {
		if(this.constructorMap.containsKey(newType)) {
			throw new DuplicateTypeConstructorException(newType, this.constructorMap.get(newType), constructor);
		}
		this.constructorMap.put(newType, constructor);
	}

	/**
	 * Adds new conversion to the environment
	 * 
	 * @param fromType              Type which is converted
	 * @param toType                Type to which is converted
	 * @param conversionConstructor Conversion lambda (constructor)
	 */
	public void addConversion(TypeAtom fromType, TypeAtom toType, Function conversionConstructor)
			throws AppendableException {
		if(!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new AppendableException("Can define conversions between representations!");
		}
		Pair<TypeAtom, TypeAtom> conversion = new Pair<TypeAtom, TypeAtom>(fromType, toType);
		if(this.conversions.containsKey(conversion)) {
			throw new DuplicateConversionException(fromType, toType, this.conversions.get(conversion), conversionConstructor);
		}
		this.conversions.put(conversion, conversionConstructor);
	}
	
	/**
	 * Instantiates conversion of two type atoms
	 * @param converted converted expression
	 * @param fromType type atom from which conversion is carried
	 * @param toType type to which is converted
	 * @return expression converting converted to toType
	 * @throws ConversionException 
	 */
	public Expression convertTo(Expression converted, TypeAtom fromType, TypeAtom toType) throws ConversionException {
		Pair<TypeAtom, TypeAtom> conversion = new Pair<TypeAtom, TypeAtom>(fromType, toType);
		if(!this.conversions.containsKey(conversion)) {
			throw new ConversionException(fromType, toType, converted);
		}
		return new Application(this.conversions.get(conversion), new Tuple(Arrays.asList(converted)));
	}
	
	/**
	 * Type environment only one exists
	 */
	public static TypeEnvironment singleton = new TypeEnvironment();
	
	/**
	 * Initializes basic types
	 * @throws AppendableException 
	 */
	public static void initBasicTypes() throws AppendableException {
		// Int
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeInt);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntNative, Function.IntNativeConstructor);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntRoman, Function.IntRomanConstructor);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntString, Function.IntStringConstructor);
		
		// Bool
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeBool);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeBoolNative, Function.BoolNativeConstructor);
		
		// String
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeString);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeStringNative, Function.StringNativeConstructor);

		// Double
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeDouble);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeDoubleNative, Function.DoubleNativeConstructor);
		
		//Conversions
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, IntNativeToIntRomanWrapper.IntToIntRoman);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, IntNativeToIntStringWrapper.IntToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative, IntRomanToIntNativeWrapper.IntRomanToInt);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString, IntRomanToIntStringWrapper.IntRomanToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntNative, IntStringToIntNativeWrapper.IntStringToInt);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, IntStringToIntRomanWrapper.IntStringToIntRoman);
	}
}
