package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
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
		String s = super.toString();
		return "<" + " " + s + this.constructedType.toString() + ">";
	}
	
	@Override
	public Expression interpret(Environment env) throws Exception{
		Constructor c = new Constructor(this.argsType, this.args, this.body, this.constructedType, env);
		c.infer(env);
		return c;
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
	
	/**
	 * Constructor for primitive Int
	 */
	public static TypeConstructionLambda IntPrimitiveConstructor = new TypeConstructionLambda(TypeConcrete.TypeInt,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new TypeTuple(new Type[] { TypeConcrete.TypeInt}),
																		new Variable("x"));
	/**
	 * Constructor for primitive String
	 */
	public static TypeConstructionLambda StringPrimitiveConstructor = new TypeConstructionLambda(	TypeConcrete.TypeString,
																			new Tuple(new Expression[] {new Variable("x")}),
																			new TypeTuple(new Type[] { TypeConcrete.TypeString}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Double
	 */
	public static TypeConstructionLambda DoublePrimitiveConstructor = new TypeConstructionLambda(	TypeConcrete.TypeDouble,
																			new Tuple(new Expression[] {new Variable("x")}),
																			new TypeTuple(new Type[] { TypeConcrete.TypeDouble}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Boolean
	 */
	public static TypeConstructionLambda BoolPrimitiveConstructor = new TypeConstructionLambda(TypeConcrete.TypeBool,
																		 new Tuple(new Expression[] {new Variable("x")}),
																		 new TypeTuple(new Type[] { TypeConcrete.TypeBool}),
																		 new Variable("x"));
	
	/**
	 * Constructor for Int represented by String value
	 */
	public static TypeConstructionLambda IntStringConstructor = new TypeConstructionLambda(	TypeRepresentation.TypeIntString,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new TypeTuple(new Type[] { TypeConcrete.TypeString}),
																		new Variable("x"));
	/**
	 * Constructor for Int represented by Roman String value
	 */
	public static TypeConstructionLambda IntRomanConstructor = new TypeConstructionLambda(	TypeRepresentation.TypeIntRoman,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new TypeTuple(new Type[] { TypeConcrete.TypeString}),
																		new Variable("x"));
}
