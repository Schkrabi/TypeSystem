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
		return clojureValue;
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
	
	public static Object literalToObject(Expression e) {
		if(e instanceof LitBoolean lb) {
			return Boolean.valueOf(lb.value);
		}
		else if(e instanceof LitInteger li) {
			return Long.valueOf(li.value);
		}
		else if(e instanceof LitDouble ld) {
			return Double.valueOf(ld.value);
		}
		else if(e instanceof LitString ls) {
			return ls.value;
		}
		else if(e instanceof LitInteropObject li) {
			return li.javaObject;
		}
		else if(e instanceof LitComposite lc) {
			return lc;
		}
		throw new RuntimeException("Unexpected argument type");
	}
	
	public static Literal objectToLiteral(Object o) {
		if(o instanceof Byte || o instanceof Short || o instanceof Integer || o instanceof Long) {
			Number n = (Number)o;
			return new LitInteger(n.longValue());
		}
		else if(o instanceof Float || o instanceof Double) {
			Number n = (Number)o;
			return new LitDouble(n.doubleValue());
		}
		else if(o instanceof String s) {
			return new LitString(s);
		}
		else if(o instanceof Boolean b) {
			return b ? LitBoolean.TRUE : LitBoolean.FALSE;
		}
		else if(o instanceof Literal l) {
			return l;
		}
		else {
			var t = TypeAtom.javaTypeMapping.get(o.getClass());
			if(t == null) {
				throw new RuntimeException("Unsupported type!" + o.getClass().getName());
			}
			return new LitInteropObject(o, t);
		}
	}
}
