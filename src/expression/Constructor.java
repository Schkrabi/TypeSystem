package expression;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeRepresentation;

/**
 * A class for type constructors. Basically a special case of lambda.
 * @author r.SKRABAL
 *
 */
public class Constructor extends Lambda {
	
	/**
	 * Constructed type
	 */
	public final Type constructedType;

	public Constructor(Type constructedType, Tuple args, Expression body) {
		super(args, body);
		this.constructedType = constructedType; 
	}

	@Override
	public Type infer() throws Exception{
		Type infered = super.infer();
		if(!infered.isApplicableType()){
			throw new Exception("Badly typed constructor " + this.toString() + " infered to not-Arrow type " + infered);
		}
		if(infered instanceof ForallType) {
			infered = ((ForallType)infered).getBoundType();
		}
		
		TypeArrow inferedType = (TypeArrow)infered;
		
		return new TypeArrow(inferedType.ltype, this.constructedType);
	}
	
	@Override
	public String toString(){
		String s = super.toString();
		return "<" + " " + s + this.constructedType.toString() + ">";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		Lambda l = (Lambda)super.substituteTopLevelVariables(topLevel);
		return new Constructor(this.constructedType, l.args, l.getBody());
	}
	
	/**
	 * Constructor for primitive Int
	 */
	public static Constructor IntPrimitiveConstructor = new Constructor(TypeConcrete.TypeInt,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
	/**
	 * Constructor for primitive String
	 */
	public static Constructor StringPrimitiveConstructor = new Constructor(	TypeConcrete.TypeString,
																			new Tuple(new Expression[] {new Variable("x")}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Double
	 */
	public static Constructor DoublePrimitiveConstructor = new Constructor(	TypeConcrete.TypeDouble,
																			new Tuple(new Expression[] {new Variable("x")}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Boolean
	 */
	public static Constructor BoolPrimitiveConstructor = new Constructor(TypeConcrete.TypeBool,
																		 new Tuple(new Expression[] {new Variable("x")}),
																		 new Variable("x"));
	
	/**
	 * Constructor for Int represented by String value
	 */
	public static Constructor IntStringConstructor = new Constructor(	TypeRepresentation.TypeIntString,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
	/**
	 * Constructor for Int represented by Roman String value
	 */
	public static Constructor IntRomanConstructor = new Constructor(	TypeRepresentation.TypeIntRoman,
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
}
