package expression;

import java.util.Arrays;
import java.util.Iterator;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.Pair;

/**
 * A class for type constructors. Basically a special case of lambda.
 * @author r.SKRABAL
 *
 */
public class TypeConstructionLambda extends Lambda {
	
	/**
	 * Constructed type
	 */
	public final Type constructedType;

	public TypeConstructionLambda(Type constructedType, Tuple args, TypeTuple argsType, Expression body) {
		super(args, argsType, body);
		this.constructedType = constructedType; 
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException{
		try {
			Pair<Type, Substitution> infered = super.infer(env);
			
			return new Pair<Type, Substitution>(new TypeArrow(((TypeArrow)infered.first).ltype, this.constructedType), infered.second);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}
	
	@Override
	public String toString(){
		StringBuilder s = new StringBuilder("(func:");
		s.append(this.constructedType);
		s.append(" (");
		
		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		
		while(i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			
			if(t instanceof TypeVariable) {
				s.append(e);
			}else {
				s.append("(");
				s.append(t);
				s.append(" ");
				s.append(e);
				s.append(")");
			}
			if(i.hasNext())
				s.append(" ");
		}
		s.append(") ");
		
		s.append(this.body.toString());
		s.append(")");
		
		return s.toString();
	}
	
	@Override
	public Expression interpret(Environment env) {
		return new Constructor(this.argsType, this.args, this.body, this.constructedType, env);
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof TypeConstructionLambda) {
			int c = this.constructedType.compareTo(((TypeConstructionLambda) other).constructedType);
			if(c != 0)
				return c;
		}
		return super.compareTo(other);
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof TypeConstructionLambda) {
			return this.constructedType.equals(((TypeConstructionLambda) other).constructedType) && super.equals(other);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode() * this.constructedType.hashCode();
	}
	
	/**
	 * Constructor for primitive Int
	 */
	public static TypeConstructionLambda IntPrimitiveConstructor = new TypeConstructionLambda(TypeAtom.TypeIntNative,
																		new Tuple(Arrays.asList(new Variable("x"))),
																		new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
																		new Variable("x"));
	/**
	 * Constructor for primitive String
	 */
	public static TypeConstructionLambda StringPrimitiveConstructor = new TypeConstructionLambda(	TypeAtom.TypeStringNative,
																			new Tuple(Arrays.asList(new Variable("x"))),
																			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
																			new Variable("x"));
	/**
	 * Constructor for primitive Double
	 */
	public static TypeConstructionLambda DoublePrimitiveConstructor = new TypeConstructionLambda(	TypeAtom.TypeDoubleNative,
																			new Tuple(Arrays.asList(new Variable("x"))),
																			new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
																			new Variable("x"));
	/**
	 * Constructor for primitive Boolean
	 */
	public static TypeConstructionLambda BoolPrimitiveConstructor = new TypeConstructionLambda(TypeAtom.TypeBoolNative,
																		 new Tuple(Arrays.asList(new Variable("x"))),
																		 new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
																		 new Variable("x"));
	
	/**
	 * Constructor for Int represented by String value
	 */
	public static TypeConstructionLambda IntStringConstructor = new TypeConstructionLambda(	TypeAtom.TypeIntString,
																		new Tuple(Arrays.asList(new Variable("x"))),
																		new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
																		new Variable("x"));
	/**
	 * Constructor for Int represented by Roman String value
	 */
	public static TypeConstructionLambda IntRomanConstructor = new TypeConstructionLambda(	TypeAtom.TypeIntRoman,
																		new Tuple(Arrays.asList(new Variable("x"))),
																		new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
																		new Variable("x"));
}
