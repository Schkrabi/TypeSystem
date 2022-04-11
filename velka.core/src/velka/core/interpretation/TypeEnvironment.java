package velka.core.interpretation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.ConversionOperators;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.ConversionException;
import velka.core.exceptions.DuplicateConversionException;
import velka.core.exceptions.DuplicateTypeConstructorException;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.exceptions.UndefinedTypeException;
import velka.core.exceptions.UnrecognizedConstructorException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.literal.LitComposite;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * This class instance stores all information related to types not directly
 * included in instances of class Type: Contructors, conversions and
 * deconstructions.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeEnvironment {

	/**
	 * Environment associated with this type environment
	 */
	private final Environment environment;

	/**
	 * Type information table
	 */
	private Map<TypeAtom, TypeInformation> typeInfo = new HashMap<TypeAtom, TypeInformation>();

	private TypeEnvironment(Environment env) {
		this.environment = env;
	}

	/**
	 * Gets TypeInformation for given TypeAtom
	 * 
	 * @param typeAtom searched type
	 * @return TypeInformation instace
	 * @throws UndefinedTypeException if this type has no type information
	 */
	private TypeInformation getTypeInfo(TypeAtom typeAtom) throws UndefinedTypeException {
		if (!this.typeInfo.containsKey(typeAtom)) {
			throw new UndefinedTypeException(typeAtom.toString());
		}
		return this.typeInfo.get(typeAtom);
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
		TypeInformation info = this.getTypeInfo(type);
		return info.getConstructor(argsType);
	}

	/**
	 * Adds type without representation
	 * 
	 * @param name name of type
	 * @throws DuplicateTypeDefinitionException if such type already exists
	 */
	public void addType(TypeName name) throws DuplicateTypeDefinitionException {
		TypeAtom typeAtom = new TypeAtom(name, TypeRepresentation.WILDCARD);
		if (this.existType(name)) {
			throw new DuplicateTypeDefinitionException(typeAtom);
		}
		this.typeInfo.put(typeAtom, new TypeInformation(typeAtom, NameGenerator.next(), this));
	}

	/**
	 * Adds type with representation
	 * 
	 * @param newType defined type
	 * @throws DuplicateTypeDefinitionException if such type already exists
	 */
	public void addRepresentation(TypeAtom newType) throws DuplicateTypeDefinitionException {
		if (this.existsTypeAtom(newType)) {
			throw new DuplicateTypeDefinitionException(newType);
		}
		this.typeInfo.put(newType, new TypeInformation(newType, NameGenerator.next(), this));
	}

	/**
	 * Adds constructor to a type
	 * 
	 * @param typeAtom          type to which constructor is added
	 * @param constructorLambda constructing lambda
	 * @throws AppendableException if type does not exists or constructor with same
	 *                             argument types already exists
	 */
	public void addConstructor(TypeAtom typeAtom, Abstraction constructorLambda, Environment env)
			throws AppendableException {
		TypeInformation info = this.getTypeInfo(typeAtom);
		info.addConstructor(constructorLambda, env);
	}

	/**
	 * Adds constructor for primitive types. For internal use only
	 * 
	 * @param typeAtom primitive type
	 * @throws AppendableException if type does not exists or constructor with same
	 *                             argument types already exists
	 */
	private void addPrimitiveConstructor(TypeAtom typeAtom, Environment env) throws AppendableException {
		this.addConstructor(typeAtom, Lambda.identity, env);
	}

	/**
	 * Creates constructor of LitComposite from lambda and given TypeAtom
	 * 
	 * @param typeAtom Constructed type
	 * @param lambda   lambda used for construction of wrapped expression
	 * @return construtor
	 */
	private Lambda createConstructorFromLambda(TypeAtom typeAtom, Abstraction abst) throws AppendableException {
		Pair<Type, Substitution> infered = abst.infer(this.environment, this);
		Type abstType = infered.first;
		if (!(abstType instanceof TypeArrow)) {
			throw new AppendableException("Contstructor " + abst + " infered " + abstType + " expected TypeArrow!");
		}
		TypeArrow applAbstType = (TypeArrow) abstType;
		if (!(applAbstType.ltype instanceof TypeTuple)) {
			throw new AppendableException(
					"Constructor " + abst + " infered arguments " + applAbstType.ltype + " expected TypeTuple!");
		}
		TypeTuple abstArgType = (TypeTuple) applAbstType.ltype;

		Tuple args = new Tuple(
				abstArgType.stream().map(x -> new Symbol(NameGenerator.next())).collect(Collectors.toList()));

		return new Lambda(args, abstArgType, new LitComposite(new AbstractionApplication(abst, args), typeAtom));
	}

	/**
	 * Adds new conversion to the environment
	 * 
	 * @param fromType              Type which is converted
	 * @param toType                Type to which is converted
	 * @param conversionConstructor Conversion lambda (constructor)
	 * @throws AppendableException if any TypeAtom is not recognized or if such
	 *                             conversion already exists
	 */
	public void addConversion(TypeAtom fromType, TypeAtom toType, Expression conversionConstructor)
			throws AppendableException {
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new AppendableException("Can only define conversions between representations!");
		}
		if (!this.existsTypeAtom(toType)) {
			throw new UndefinedTypeException(toType.toString());
		}

		TypeInformation info = this.getTypeInfo(fromType);
		info.addConversion(toType, conversionConstructor);
	}

	/**
	 * Returns true if from type is convertable to to type. Otherwise returns false
	 * 
	 * @param from type
	 * @param to   type
	 * @return true or false.
	 */
	public boolean canConvert(TypeAtom from, TypeAtom to) {
		TypeInformation info = null;
		try {
			info = this.getTypeInfo(from);
		} catch (UndefinedTypeException e) {
			return false;
		}
		if (!this.existsTypeAtom(to)) {
			return false;
		}

		return info.canConvertTo(to);
	}

	/**
	 * Gets converision constructor for given fromType and toType
	 * 
	 * @param fromType type converted from
	 * @param toType   type converted to
	 * @return conversion constructor expression
	 * @throws AppendableException if any of fromType and toType does not exists or
	 *                             if the conversion is invalid
	 */
	private Expression getConversionConstructor(TypeAtom fromType, TypeAtom toType) throws AppendableException {
		if (!this.canConvert(fromType, toType)) {
			throw new ConversionException(fromType, toType, null);
		}
		TypeInformation info = this.getTypeInfo(fromType);
		return info.getConversionConstructorTo(toType).get();
	}

	/**
	 * Instantiates conversion of two type atoms
	 * 
	 * @param converted converted expression
	 * @param fromType  type atom from which conversion is carried
	 * @param toType    type to which is converted
	 * @return expression converting converted to toType
	 * @throws AppendableException if any of fromType and toType does not exists or
	 *                             if the conversion is invalid
	 */
	public Expression convertTo(Expression converted, TypeAtom fromType, TypeAtom toType) throws AppendableException {
		Expression conversionConstructor = this.getConversionConstructor(fromType, toType);

		return new AbstractionApplication(conversionConstructor, new Tuple(Arrays.asList(converted)));
	}

	/**
	 * Returns true if type was defined in TypeEnvironment. Otherwise returns false.
	 * 
	 * @param type TypeAtom
	 * @return true or false
	 */
	public boolean existsTypeAtom(TypeAtom type) {
		return this.typeInfo.containsKey(type);
	}

	/**
	 * Returns true if type was defined in TypeEnvironment. Otherwise returns false.
	 * 
	 * @param type TypeAtom
	 * @return true or false
	 */
	public boolean existType(TypeName typeName) {
		return this.typeInfo.containsKey(new TypeAtom(typeName, TypeRepresentation.WILDCARD));
	}

	/**
	 * Gets name of deconstruction function for given type
	 * 
	 * @param typeAtom searched type
	 * @return String containing name of clojure function for deconstruct check
	 * @throws UndefinedTypeException if type is not known by TypeEnvitonment
	 */
	public String getDeconstructionCheckFunctionName(TypeAtom typeAtom) throws UndefinedTypeException {
		TypeInformation info = this.getTypeInfo(typeAtom);
		return info.deconstructionCheckFunctionName;
	}

	/**
	 * Initializes basic types
	 * 
	 * @throws AppendableException
	 */
	public static TypeEnvironment initBasicTypes(Environment env) throws AppendableException {
		TypeEnvironment typeEnvironment = new TypeEnvironment(env);

		// Int
		typeEnvironment.addType(TypeAtom.TypeInt.name);
		typeEnvironment.addRepresentation(TypeAtom.TypeIntNative);
		typeEnvironment.addPrimitiveConstructor(TypeAtom.TypeIntNative, env);
		typeEnvironment.addRepresentation(TypeAtom.TypeIntRoman);
		typeEnvironment.addConstructor(TypeAtom.TypeIntRoman, Lambda.makeIdentity(TypeAtom.TypeStringNative), env);
		typeEnvironment.addRepresentation(TypeAtom.TypeIntString);
		typeEnvironment.addConstructor(TypeAtom.TypeIntString, Lambda.makeIdentity(TypeAtom.TypeStringNative), env);

		// Bool
		typeEnvironment.addType(TypeAtom.TypeBool.name);
		typeEnvironment.addRepresentation(TypeAtom.TypeBoolNative);
		typeEnvironment.addPrimitiveConstructor(TypeAtom.TypeBoolNative, env);

		// String
		typeEnvironment.addType(TypeAtom.TypeString.name);
		typeEnvironment.addRepresentation(TypeAtom.TypeStringNative);
		typeEnvironment.addPrimitiveConstructor(TypeAtom.TypeStringNative, env);

		// Double
		typeEnvironment.addType(TypeAtom.TypeDouble.name);
		typeEnvironment.addRepresentation(TypeAtom.TypeDoubleNative);
		typeEnvironment.addPrimitiveConstructor(TypeAtom.TypeDoubleNative, env);

		// List
		typeEnvironment.addType(TypeAtom.TypeList.name);
		typeEnvironment.addRepresentation(TypeAtom.TypeListNative);
		typeEnvironment.addConstructor(TypeAtom.TypeListNative, ListNative.constructorEmpty, env);
		typeEnvironment.addConstructor(TypeAtom.TypeListNative, ListNative.constructor, env);
		
		// List Java Array
		typeEnvironment.addRepresentation(JavaArrayList.TypeListJavaArray);
		typeEnvironment.addConstructor(JavaArrayList.TypeListJavaArray, JavaArrayList.constructor, env);
		typeEnvironment.addConstructor(JavaArrayList.TypeListJavaArray, JavaArrayList.constructorFromList, env);
		typeEnvironment.addConstructor(JavaArrayList.TypeListJavaArray, JavaArrayList.constructorCapacity, env);
		
		// List Java Linked
		typeEnvironment.addRepresentation(JavaLinkedList.TypeListJavaLinked);
		typeEnvironment.addConstructor(JavaLinkedList.TypeListJavaLinked, JavaLinkedList.constructor, env);

		// Conversions
		typeEnvironment.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, ConversionOperators.IntNativeToIntRoman);
		typeEnvironment.addConversion(TypeAtom.TypeIntNative, TypeAtom.TypeIntString, ConversionOperators.IntNativeToIntString);
		typeEnvironment.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative, ConversionOperators.IntRomanToIntNative);
		typeEnvironment.addConversion(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString, ConversionOperators.IntRomanToIntString);
		typeEnvironment.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntNative, ConversionOperators.IntStringToIntNative);
		typeEnvironment.addConversion(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, ConversionOperators.IntStringToIntRoman);
		
		typeEnvironment.addConversion(JavaArrayList.TypeListJavaArray, JavaLinkedList.TypeListJavaLinked, JavaArrayList.ArrayListToLinkedList);
		typeEnvironment.addConversion(JavaArrayList.TypeListJavaArray, TypeAtom.TypeListNative, JavaArrayList.ArrayListToNativeList);
		typeEnvironment.addConversion(JavaLinkedList.TypeListJavaLinked, JavaArrayList.TypeListJavaArray, JavaLinkedList.LinkedListToArrayList);
		typeEnvironment.addConversion(JavaLinkedList.TypeListJavaLinked, TypeAtom.TypeListNative, JavaLinkedList.LinkedListToNativeList);
		typeEnvironment.addConversion(TypeAtom.TypeListNative, JavaArrayList.TypeListJavaArray, ListNative.ListNativeToArrayListOperator);
		typeEnvironment.addConversion(TypeAtom.TypeListNative, JavaLinkedList.TypeListJavaLinked, ListNative.ListNativeToLinkedListOperator);

		return typeEnvironment;
	}

	// TODO Remove?
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
	 * Class for storing informations about types
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class TypeInformation {
		/**
		 * Type to which information is related
		 */
		public final TypeAtom type;
		/**
		 * Constructors of the type
		 */
		private Map<TypeTuple, Abstraction> constructors;
		/**
		 * Conversions of the type
		 */
		private Map<TypeAtom, Expression> conversions;
		/**
		 * Name of deconstruction function
		 */
		public final String deconstructionCheckFunctionName;

		/**
		 * Type environment this typeinfomration belongs to
		 */
		public final TypeEnvironment typeEnvironment;

		public TypeInformation(TypeAtom type, String checkDeconstructionFunctionName, TypeEnvironment typeEnv) {
			this.type = type;
			this.deconstructionCheckFunctionName = checkDeconstructionFunctionName;
			this.constructors = new TreeMap<TypeTuple, Abstraction>();
			this.conversions = new TreeMap<TypeAtom, Expression>();
			this.typeEnvironment = typeEnv;
		}

		/**
		 * Tries to find constructor with specific argument types
		 * 
		 * @param argsType argument types
		 * @return Optional of Lambda
		 * @throws AppendableException if anything goes awry during unification
		 */
		private Optional<Abstraction> findConstrutor(TypeTuple argsType) throws AppendableException {
			for (java.util.Map.Entry<TypeTuple, Abstraction> entry : this.constructors.entrySet()) {
				TypeTuple type = entry.getKey();
				if(Type.unifyTypes(type, argsType).isPresent()) {
					// If unification exists, return the constructor
					return Optional.of(entry.getValue());
				}				
			}
			return Optional.empty();
		}

		/**
		 * Gets constructor lambda as it was defined by user
		 * 
		 * @param argsType types of constructor arguments
		 * @return lambda expression
		 * @remark This constructor does not create instance of type, it only creates
		 *         underlying data
		 * @throws AppendableException if no such constructor exists
		 */
		public Abstraction getRawConstructor(TypeTuple argsType) throws AppendableException {
			Optional<Abstraction> o = this.findConstrutor(argsType);
			if (!o.isPresent()) {
				throw new UnrecognizedConstructorException(this.type, argsType);
			}
			return o.get();
		}

		/**
		 * Gets constructor of this.type for given argument types
		 * 
		 * @param argsType types of constructor arguments
		 * @return constructor
		 * @throws AppendableException if no such constructor exists
		 */
		public Lambda getConstructor(TypeTuple argsType) throws AppendableException {
			Abstraction rawConstructor = this.getRawConstructor(argsType);
			return this.typeEnvironment.createConstructorFromLambda(this.type, rawConstructor);
		}

		/**
		 * Adds constructor to constructor map
		 * 
		 * @param constructorLambda constructor lambda expression
		 * @throws AppendableException
		 */
		public void addConstructor(Abstraction constructorLambda, Environment env) throws AppendableException {
			TypeTuple argsType = (TypeTuple) ((TypeArrow) (constructorLambda.infer(env,
					this.typeEnvironment).first)).ltype;

			if (this.findConstrutor(argsType).isPresent()) {
				throw new DuplicateTypeConstructorException(this.type, this.constructors.get(argsType),
						constructorLambda);
			}

			this.constructors.put(argsType, constructorLambda);
		}

		/**
		 * Add conversion to specified type atom
		 * 
		 * @param toType                specified type atom
		 * @param conversionConstructor constructor for the conversion
		 * @throws DuplicateConversionException if such conversion already exists
		 */
		public void addConversion(TypeAtom toType, Expression conversionConstructor)
				throws DuplicateConversionException {
			if (this.conversions.containsKey(toType)) {
				throw new DuplicateConversionException(this.type, toType, this.conversions.get(toType),
						conversionConstructor);
			}
			this.conversions.put(toType, conversionConstructor);
		}

		/**
		 * Tries to find conversion constructor for specified TypeAtom
		 * 
		 * @param toType type to convert to
		 * @return Optional with conversion, or empty Optional if no such conversion
		 *         exists
		 */
		public Optional<Expression> getConversionConstructorTo(TypeAtom toType) {
			if (!this.conversions.containsKey(toType)) {
				return Optional.empty();
			}
			return Optional.of(this.conversions.get(toType));
		}

		/**
		 * Predicate if TypeAtom can be converted to another
		 * 
		 * @param toType type to convert to
		 * @return true if conversion exists, false otherwise
		 */
		public boolean canConvertTo(TypeAtom toType) {
			return this.getConversionConstructorTo(toType).isPresent();
		}

		@Override
		public String toString() {
			return "Type information :" + this.type.toString() + " checkDeconstructionFunctionName: "
					+ this.deconstructionCheckFunctionName + " constructors: " + this.constructors.hashCode()
					+ " conversion :" + this.conversions.hashCode();
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof TypeInformation) {
				return this.type.equals(((TypeInformation) other).type)
						&& this.deconstructionCheckFunctionName
								.equals(((TypeInformation) other).deconstructionCheckFunctionName)
						&& this.constructors.equals(((TypeInformation) other).constructors)
						&& this.conversions.equals(((TypeInformation) other).conversions);
			}
			return false;
		}

		@Override
		public int hashCode() {
			return this.type.hashCode() * this.deconstructionCheckFunctionName.hashCode() * this.constructors.hashCode()
					* this.conversions.hashCode();
		}

	}
}
