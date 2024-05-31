package velka.core.interpretation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.ConversionException;
import velka.core.exceptions.DuplicateConversionException;
import velka.core.exceptions.DuplicateTypeConstructorException;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.exceptions.UndefinedTypeException;
import velka.core.exceptions.UnrecognizedConstructorException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.langbase.OperatorBank;
import velka.core.literal.LitInteger;
import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.NameGenerator;

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
		var typeInfo = this.typeInfo.get(typeAtom);
		if(typeInfo == null) {
			try {
				this.addRepresentation(typeAtom);
			} catch (DuplicateTypeDefinitionException e) {
				// Unlikely
				throw new RuntimeException(e);
			}
			typeInfo = this.typeInfo.get(typeAtom);
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
	
	/** Gets the cost of representation conversion */
	public Optional<Long> conversionCost(Type from, Type to, Expression e, Environment env, TypeEnvironment typeEnv) {
		if(from.equals(to)) {
			return Optional.of(0l);
		}
		else if(!this.canConvert(from, to)) {
			return Optional.empty();
		}
		else if(to instanceof TypeVariable
				|| from instanceof RepresentationOr
				|| to instanceof RepresentationOr) {
			return Optional.of(0l);
		}
		else if(from instanceof TypeAtom) {
			try {
				var ti = this.getTypeInfo((TypeAtom)from);
				
				if(from.equals(to)) return Optional.of(0l);	
				
				if(!(to instanceof TypeAtom)) {
					System.out.println("test");
				}
				
				var c = ti.getConversionCost((TypeAtom)to);
				if(c.isEmpty()) {
					return Optional.empty();
				}
				var costApl = new AbstractionApplication(c.get(), new Tuple(e));
				var cost = costApl.interpret(env, typeEnv);
				if(!(cost instanceof LitInteger)) {
					throw new RuntimeException("Invalid conversion cost: conversion " + from + " to " + to + " has improper cost function.");
				}
				return Optional.of(((LitInteger)cost).value);
			} catch (AppendableException ex) {
				throw new RuntimeException(ex);
			}
		}
		else if(from instanceof TypeTuple) {
			long sum = 0;
			var ite = ((Tuple)e).iterator();
			var itf = ((TypeTuple)from).iterator();
			var itt = ((TypeTuple)to).iterator();
			while(itf.hasNext()) {
				var sef = itf.next();
				var set = itt.next();
				var te = ite.next();
				var cost = this.conversionCost(sef, set, te, env, typeEnv);
				if(cost.isEmpty()) return Optional.empty();
				
				sum += cost.get();
			}
			return Optional.of(sum);
		}
		else if(from instanceof TypeArrow) {
			// The time to convert the function is constant
			// What can change is the execution time of the function
			// However that is not traceable for Velka
			return Optional.of(1l);
		}
		throw new RuntimeException("Invalid conversion cost: unrecognized type: " + from + " or " + to);
	}

	/**
	 * Adds constructor for primitive types. For internal use only
	 * 
	 * @param typeAtom primitive type
	 * @throws AppendableException if type does not exists or constructor with same
	 *                             argument types already exists
	 */
	public void addPrimitiveConstructor(TypeAtom typeAtom, Environment env) throws AppendableException {
		this.addConstructor(typeAtom, Lambda.identity, env);
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
	public void addConversion(TypeAtom fromType, TypeAtom toType, Expression conversionConstructor, Expression cost)
			throws AppendableException {
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new AppendableException("Can only define conversions between representations!");
		}

		TypeInformation info = this.getTypeInfo(fromType);
		info.addConversion(toType, conversionConstructor, cost);
	}
	
	/**
	 * Returns true if first type is converable to the second
	 * @param from type
	 * @param to type
	 * @return True if types are convertable, false otherwise.
	 */
	public boolean canConvert(Type from, Type to) {
		return from.canConvertTo(to, (t, o) -> this.canConvertAtom(t, o));		
	}

	/**
	 * Returns true if from type is convertable to to type. Otherwise returns false
	 * 
	 * @param from type
	 * @param to   type
	 * @return true or false.
	 */
	public boolean canConvertAtom(TypeAtom from, TypeAtom to) {
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
		if (!this.canConvertAtom(fromType, toType)) {
			throw new ConversionException(toType, new TypeHolder(fromType));
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
		
		for(OperatorBank operatorBank : OperatorBank.operatorBanks) {
			operatorBank.initInTypeEnvironment(env, typeEnvironment);
		}

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
		private Map<TypeAtom, ConversionInfo> conversions;
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
			this.conversions = new TreeMap<TypeAtom, ConversionInfo >();
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
		public Abstraction getConstructor(TypeTuple argsType) throws AppendableException {
			Abstraction rawConstructor = this.getRawConstructor(argsType);
			//return this.typeEnvironment.createConstructorFromLambda(this.type, rawConstructor);
			return rawConstructor;
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
		public void addConversion(TypeAtom toType, Expression conversionConstructor, Expression cost)
				throws DuplicateConversionException {
			if (this.conversions.containsKey(toType)) {
				throw new DuplicateConversionException(this.type, toType, this.conversions.get(toType).conversion,
						conversionConstructor);
			}
			this.conversions.put(toType, new ConversionInfo(this.type, toType, conversionConstructor, cost));
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
			return Optional.of(this.conversions.get(toType).conversion);
		}
		
		/** Gets the cost expression of the conversion */
		public Optional<Expression> getConversionCost(TypeAtom to){
			var ci = this.conversions.get(to);
			if(ci == null) return Optional.empty();
			return Optional.of(ci.cost);
		}

		/**
		 * Predicate if TypeAtom can be converted to another
		 * 
		 * @param toType type to convert to
		 * @return true if conversion exists, false otherwise
		 */
		public boolean canConvertTo(TypeAtom toType) {
			return toType.representation.equals(TypeRepresentation.WILDCARD)
					|| this.type.representation.equals(TypeRepresentation.WILDCARD)
					|| this.getConversionConstructorTo(toType).isPresent();
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
		
		private static class ConversionInfo {
			public final Expression conversion;
			public final Expression cost;
			public final Type from;
			public final Type to;
			
			public ConversionInfo(Type from, Type to, Expression conversion, Expression cost) {
				this.from = from;
				this.to = to;
				this.conversion = conversion;
				this.cost = cost;
			}
		}

	}
}
