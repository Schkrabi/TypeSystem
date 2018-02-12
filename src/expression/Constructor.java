package expression;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;

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
	/**
	 * Type of constructor arguments
	 */
	public final TypeTuple argsType;

	public Constructor(Type constructedType, TypeTuple argsType, Tuple args, Expression body) {
		super(args, body);
		this.constructedType = constructedType; 
		this.argsType = argsType;
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
		
		if(!Type.unify(inferedType.ltype, this.argsType)) {
			throw new Exception("Real constructor arguments " + inferedType.ltype + " and declared constructor argument type " + this.argsType + " do not unify");
		}		
		
		return new TypeArrow(this.argsType, this.constructedType);
	}
	
	@Override
	public String toString(){
		String s = super.toString();
		return "<" + this.constructedType.toString() + " " + s + ">";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		Lambda l = (Lambda)super.substituteTopLevelVariables(topLevel);
		return new Constructor(this.constructedType, this.argsType, l.args, l.getBody());
	}
	
	/**
	 * Constructor for primitive Int
	 */
	public static Constructor IntPrimitiveConstructor = new Constructor(TypeConcrete.TypeInt,
																		new TypeTuple(new Type[] {TypeConcrete.TypeInt}), 
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
	/**
	 * Constructor for primitive String
	 */
	public static Constructor StringPrimitiveConstructor = new Constructor(	TypeConcrete.TypeString,
																			new TypeTuple(new Type[] {TypeConcrete.TypeString}),
																			new Tuple(new Expression[] {new Variable("x")}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Double
	 */
	public static Constructor DoublePrimitiveConstructor = new Constructor(	TypeConcrete.TypeDouble,
																			new TypeTuple(new Type[] {TypeConcrete.TypeDouble}),
																			new Tuple(new Expression[] {new Variable("x")}),
																			new Variable("x"));
	/**
	 * Constructor for primitive Boolean
	 */
	public static Constructor BoolPrimitiveConstructor = new Constructor(TypeConcrete.TypeBool,
																		 new TypeTuple(new Type[] {TypeConcrete.TypeBool}),
																		 new Tuple(new Expression[] {new Variable("x")}),
																		 new Variable("x"));
	
	/**
	 * Constructor for Int represented by String value
	 */
	public static Constructor IntStringConstructor = new Constructor(	TypeRepresentation.TypeIntString,
																		new TypeTuple(new Type[] {TypeConcrete.TypeString}),
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
	/**
	 * Constructor for Int represented by Roman String value
	 */
	public static Constructor IntRomanConstructor = new Constructor(	TypeRepresentation.TypeIntRoman,
																		new TypeTuple(new Type[] {TypeConcrete.TypeString}),
																		new Tuple(new Expression[] {new Variable("x")}),
																		new Variable("x"));
}
