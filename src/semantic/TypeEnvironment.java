package semantic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import expression.Application;
import expression.Expression;
import expression.Function;
import expression.Tuple;
import operators.Operator;
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
	private Map<Pair<TypeAtom, TypeAtom>, Expression> conversions = new HashMap<Pair<TypeAtom, TypeAtom>, Expression>();

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
	public void addConversion(TypeAtom fromType, TypeAtom toType, Expression conversionConstructor)
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
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntNative, Operator.IntNativeConstructor);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntRoman, Operator.IntRomanConstructor);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeIntString, Operator.IntStringConstructor);
		
		// Bool
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeBool);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeBoolNative, Operator.BoolNativeConstructor);
		
		// String
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeString);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeStringNative, Operator.StringNativeConstructor);

		// Double
		TypeEnvironment.singleton.atomicTypes.add(TypeAtom.TypeDouble);
		TypeEnvironment.singleton.constructorMap.put(TypeAtom.TypeDoubleNative, Operator.DoubleNativeConstructor);
		
		//Conversions
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, Operator.IntNativeToIntRoman);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, Operator.IntNativeToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative, Operator.IntRomanToIntNative);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString, Operator.IntRomanToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntNative, Operator.IntStringToIntNative);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, Operator.IntStringToIntRoman);
	}
	
	/**
	 * Creates name for conversion
	 * @param from 
	 * @param to
	 * @return string with conversion name
	 */
	public static String makeConversionName(TypeAtom from, TypeAtom to) {
		return from.name.toString() + from.representation.toString() + "2" + to.name.toString() + to.representation.toString();
	}
}
