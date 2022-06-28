package velka.core.literal;

import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeRepresentation;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Abstract expression class for literals implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Literal extends Expression {

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		return clojureValueToClojureLiteral(this.valueToClojure(env, typeEnv), p.first);
	}

	/**
	 * Compiles value of this literal into clojure
	 * 
	 * @return String with clojure code
	 * @throws AppendableException if anything goes wrong during compilation
	 */
	protected abstract String valueToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	/**
	 * Creates code for literal with metadata type in clojure
	 * @param clojureValue clojure code providing value for the literal
	 * @param type type of the literal
	 * @return clojure code
	 */
	public static String clojureValueToClojureLiteral(String clojureValue, Type type) {
		return Type.addTypeMetaInfo("[" + clojureValue + "]", type);
	}
	
	@Override
	public Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if(!(to instanceof TypeAtom)) {
			throw new ConversionException(to, this);
		}		
		TypeAtom to_typeAtom = (TypeAtom)to;
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		TypeAtom myType = (TypeAtom)p.first;
		
		if(!TypeAtom.isSameBasicType(myType, to_typeAtom)) {
			throw new ConversionException(to, this);
		}
		if(to_typeAtom.representation.equals(TypeRepresentation.WILDCARD)) {
			return this;
		}
		
		Expression e = typeEnv.convertTo(this, myType, to_typeAtom);
		
		return e.interpret(env, typeEnv);
	}
}
