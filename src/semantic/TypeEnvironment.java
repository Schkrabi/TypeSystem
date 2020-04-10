package semantic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import abstraction.Abstraction;
import abstraction.Lambda;
import abstraction.Operator;
import application.AbstractionApplication;
import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import literal.LitComposite;
import parser.SemanticNode;
import parser.SemanticPair;
import types.ConversionException;
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypesDoesNotUnifyException;
import types.TypeArrow;
import util.AppendableException;
import util.Pair;

public class TypeEnvironment {
	private Set<TypeAtom> atomicTypes = new TreeSet<TypeAtom>();
	private Map<TypeAtom, ConstructorHolder> constructorMap = new TreeMap<TypeAtom, ConstructorHolder>();
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
	 * 
	 * @param node parsed node
	 * @return optional of type or empty optional if type is not recognized
	 * @throws AppendableException
	 */
	public Optional<TypeAtom> getType(SemanticNode node) throws AppendableException {
		if (node.type == SemanticNode.NodeType.PAIR) {
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
	 * @param argsType args type of the constructor
	 * @return constructor for this type if it exists
	 * @throws AppendableException
	 */
	public Abstraction getConstructor(TypeAtom type, TypeTuple argsType) throws AppendableException {
		if (this.constructorMap.containsKey(type)) {
			return this.constructorMap.get(type).findConstructor(argsType);
		}
		throw new AppendableException("No constructor for " + type + " found!");
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
	public void addRepresentation(TypeAtom newType) throws AppendableException {
		if (this.constructorMap.containsKey(newType)) {
			throw new AppendableException("Representation " + newType + " already exists!");
		}
		this.constructorMap.put(newType, new ConstructorHolder(newType));
	}

	/**
	 * 
	 * @param typeAtom
	 * @param constructorLambda
	 * @throws AppendableException
	 */
	public void addConstructor(TypeAtom typeAtom, Lambda constructorLambda) throws AppendableException {
		if (!this.constructorMap.containsKey(typeAtom)) {
			throw new UndefinedTypeException(typeAtom.toString());
		}
		ConstructorHolder ch = this.constructorMap.get(typeAtom);

		TypeTuple argsType = (TypeTuple) ((TypeArrow) (constructorLambda
				.infer(Environment.topLevelEnvironment).first)).ltype;

		if (ch.containsKey(argsType)) {
			throw new DuplicateTypeConstructorException(typeAtom, ch.get(argsType), constructorLambda);
		}

		Lambda constructor = TypeEnvironment.createConstructorFromLambda(typeAtom, constructorLambda);

		ch.put(argsType, constructor);
	}

	/**
	 * Adds constructor for primitive types. For internal use only
	 * 
	 * @param typeAtom primitive type
	 * @throws AppendableException
	 */
	private void addPrimitiveConstructor(TypeAtom typeAtom) throws AppendableException {
		if (!this.constructorMap.containsKey(typeAtom)) {
			throw new UndefinedTypeException(typeAtom.toString());
		}
		ConstructorHolder ch = this.constructorMap.get(typeAtom);

		TypeTuple argsType = new TypeTuple(Arrays.asList(typeAtom));

		if (ch.containsKey(argsType)) {
			throw new DuplicateTypeConstructorException(typeAtom, ch.get(argsType), Lambda.identity);
		}

		ch.put(argsType, Lambda.identity);
	}

	/**
	 * Creates constructor of LitComposite from lambda and given TypeAtom
	 * 
	 * @param typeAtom
	 * @param lambda
	 * @return
	 */
	private static Lambda createConstructorFromLambda(TypeAtom typeAtom, Lambda lambda) {
		return new Lambda(lambda.args, lambda.argsType, new LitComposite(lambda.body, typeAtom));
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
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new AppendableException("Can define conversions between representations!");
		}
		Pair<TypeAtom, TypeAtom> conversion = new Pair<TypeAtom, TypeAtom>(fromType, toType);
		if (this.conversions.containsKey(conversion)) {
			throw new DuplicateConversionException(fromType, toType, this.conversions.get(conversion),
					conversionConstructor);
		}
		this.conversions.put(conversion, conversionConstructor);
	}

	/**
	 * Instantiates conversion of two type atoms
	 * 
	 * @param converted converted expression
	 * @param fromType  type atom from which conversion is carried
	 * @param toType    type to which is converted
	 * @return expression converting converted to toType
	 * @throws ConversionException
	 */
	public Expression convertTo(Expression converted, TypeAtom fromType, TypeAtom toType) throws ConversionException {
		Pair<TypeAtom, TypeAtom> conversion = new Pair<TypeAtom, TypeAtom>(fromType, toType);
		if (!this.canConvert(fromType, toType)) {
			throw new ConversionException(fromType, toType, converted);
		}
		return new AbstractionApplication(this.conversions.get(conversion), new Tuple(Arrays.asList(converted)));
	}

	/**
	 * Returns true if from type is convertable to to type. Otherwise returns false
	 * 
	 * @param from type
	 * @param to   type
	 * @return true or false.
	 */
	public boolean canConvert(TypeAtom from, TypeAtom to) {
		Pair<TypeAtom, TypeAtom> conversion = new Pair<TypeAtom, TypeAtom>(from, to);
		return this.conversions.containsKey(conversion);
	}

	/**
	 * Type environment only one exists
	 */
	public static TypeEnvironment singleton = new TypeEnvironment();

	/**
	 * Initializes basic types
	 * 
	 * @throws AppendableException
	 */
	public static void initBasicTypes() throws AppendableException {
		// Int
		TypeEnvironment.singleton.addType(TypeAtom.TypeInt.name);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeIntNative);
		TypeEnvironment.singleton.addPrimitiveConstructor(TypeAtom.TypeIntNative);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeIntRoman);
		TypeEnvironment.singleton.addConstructor(TypeAtom.TypeIntRoman, Lambda.makeIdentity(TypeAtom.TypeStringNative));
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeIntString);
		TypeEnvironment.singleton.addConstructor(TypeAtom.TypeIntString,
				Lambda.makeIdentity(TypeAtom.TypeStringNative));

		// Bool
		TypeEnvironment.singleton.addType(TypeAtom.TypeBool.name);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeBoolNative);
		TypeEnvironment.singleton.addPrimitiveConstructor(TypeAtom.TypeBoolNative);

		// String
		TypeEnvironment.singleton.addType(TypeAtom.TypeString.name);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeStringNative);
		TypeEnvironment.singleton.addPrimitiveConstructor(TypeAtom.TypeStringNative);

		// Double
		TypeEnvironment.singleton.addType(TypeAtom.TypeDouble.name);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeDoubleNative);
		TypeEnvironment.singleton.addPrimitiveConstructor(TypeAtom.TypeDoubleNative);

		// Conversions
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman,
				Operator.IntNativeToIntRoman);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntString,
				Operator.IntNativeToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative,
				Operator.IntRomanToIntNative);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
				Operator.IntRomanToIntString);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntNative,
				Operator.IntStringToIntNative);
		TypeEnvironment.singleton.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman,
				Operator.IntStringToIntRoman);
	}

	/**
	 * Creates name for conversion
	 * 
	 * @param from
	 * @param to
	 * @return string with conversion name
	 */
	public static String makeConversionName(TypeAtom from, TypeAtom to) {
		return from.name.toString() + from.representation.toString() + "2" + to.name.toString()
				+ to.representation.toString();
	}

	/**
	 * Class for holding multiple constructors associated with their argument types
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class ConstructorHolder extends TreeMap<TypeTuple, Lambda> implements Comparable<ConstructorHolder> {
		/**
		 * 
		 */
		private static final long serialVersionUID = -1608923962922744264L;
		/**
		 * Constructed type
		 */
		public final TypeAtom constructedType;

		public ConstructorHolder(TypeAtom constructedType) {
			this.constructedType = constructedType;
		}

		/**
		 * Finds constructor
		 * 
		 * @param argTypes type of arguments for constructor
		 * @return construcotr
		 * @throws AppendableException if constructor is not 
		 */
		public Lambda findConstructor(TypeTuple argsType) throws AppendableException {
			for (java.util.Map.Entry<TypeTuple, Lambda> entry : this.entrySet()) {
				TypeTuple type = entry.getKey();
				try {
					//Possible bottleneck?
					Type.unify(type, argsType); 
				}catch(TypesDoesNotUnifyException e) {
					continue;
				}				

				//If unification exists, return the constructor
				return entry.getValue();
			}
			throw new AppendableException(
					"No suitable constructor for " + this.constructedType + " with arguments " + argsType + " found");
		}

		@Override
		public int compareTo(ConstructorHolder other) {
			return this.constructedType.compareTo(other.constructedType);
		}
	}
}
