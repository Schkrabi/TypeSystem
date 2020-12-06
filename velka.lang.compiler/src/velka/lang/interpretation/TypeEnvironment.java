package velka.lang.interpretation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import velka.lang.abstraction.Abstraction;
import velka.lang.abstraction.Lambda;
import velka.lang.abstraction.Operator;
import velka.lang.application.AbstractionApplication;
import velka.lang.exceptions.UndefinedTypeException;
import velka.lang.exceptions.DuplicateTypeDefinitionException;
import velka.lang.exceptions.DuplicateConversionException;
import velka.lang.exceptions.UnrecognizedConstructorException;
import velka.lang.exceptions.DuplicateTypeConstructorException;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.langbase.ListNative;
import velka.lang.literal.LitComposite;
import velka.lang.exceptions.ConversionException;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.types.TypeArrow;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;

/**
 * This class instance stores all information related to types not directly
 * included in instances of class Type: Contructors, conversions and
 * deconstructions.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeEnvironment {
	private Map<TypeAtom, TypeInformation> typeInfo = new HashMap<TypeAtom, TypeInformation>();

	private TypeEnvironment() {
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
		this.typeInfo.put(typeAtom, new TypeInformation(typeAtom, NameGenerator.next()));
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
		this.typeInfo.put(newType, new TypeInformation(newType, NameGenerator.next()));
	}

	/**
	 * Adds constructor to a type
	 * 
	 * @param typeAtom          type to which constructor is added
	 * @param constructorLambda constructing lambda
	 * @throws AppendableException if type does not exists or constructor with same
	 *                             argument types already exists
	 */
	public void addConstructor(TypeAtom typeAtom, Lambda constructorLambda) throws AppendableException {
		TypeInformation info = this.getTypeInfo(typeAtom);
		info.addConstructor(constructorLambda);
	}

	/**
	 * Adds constructor for primitive types. For internal use only
	 * 
	 * @param typeAtom primitive type
	 * @throws AppendableException if type does not exists or constructor with same
	 *                             argument types already exists
	 */
	private void addPrimitiveConstructor(TypeAtom typeAtom) throws AppendableException {
		this.addConstructor(typeAtom, Lambda.identity);
	}

	/**
	 * Creates constructor of LitComposite from lambda and given TypeAtom
	 * 
	 * @param typeAtom Constructed type
	 * @param lambda   lambda used for construction of wrapped expression
	 * @return construtor
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
		
		// List
		TypeEnvironment.singleton.addType(TypeAtom.TypeList.name);
		TypeEnvironment.singleton.addRepresentation(TypeAtom.TypeListNative);
		TypeEnvironment.singleton.addConstructor(TypeAtom.TypeListNative, ListNative.constructorEmpty);
		TypeEnvironment.singleton.addConstructor(TypeAtom.TypeListNative, ListNative.constructor);

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

	//TODO Remove?
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
		private Map<TypeTuple, Lambda> constructors;
		/**
		 * Conversions of the type
		 */
		private Map<TypeAtom, Expression> conversions;
		/**
		 * Name of deconstruction function
		 */
		public final String deconstructionCheckFunctionName;

		public TypeInformation(TypeAtom type, String checkDeconstructionFunctionName) {
			this.type = type;
			this.deconstructionCheckFunctionName = checkDeconstructionFunctionName;
			this.constructors = new TreeMap<TypeTuple, Lambda>();
			this.conversions = new TreeMap<TypeAtom, Expression>();
		}

		/**
		 * Tries to find constructor with specific argument types
		 * 
		 * @param argsType argument types
		 * @return Optional of Lambda
		 * @throws AppendableException if anything goes awry during unification
		 */
		private Optional<Lambda> findConstrutor(TypeTuple argsType) throws AppendableException {
			for (java.util.Map.Entry<TypeTuple, Lambda> entry : this.constructors.entrySet()) {
				TypeTuple type = entry.getKey();
				try {
					// Possible bottleneck?
					Type.unifyTypes(type, argsType);
				} catch (TypesDoesNotUnifyException e) {
					continue;
				} catch (RuntimeException re) {
					if (re.getCause() instanceof AppendableException) {
						continue;
					}
				}

				// If unification exists, return the constructor
				return Optional.of(entry.getValue());
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
		public Lambda getRawConstructor(TypeTuple argsType) throws AppendableException {
			Optional<Lambda> o = this.findConstrutor(argsType);
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
			Lambda rawConstructor = this.getRawConstructor(argsType);
			return TypeEnvironment.createConstructorFromLambda(this.type, rawConstructor);
		}

		/**
		 * Adds constructor to constructor map
		 * 
		 * @param constructorLambda constructor lambda expression
		 * @throws AppendableException
		 */
		public void addConstructor(Lambda constructorLambda) throws AppendableException {
			TypeTuple argsType = (TypeTuple) ((TypeArrow) (constructorLambda
					.infer(Environment.topLevelEnvironment).first)).ltype;

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
