package expression;

import types.Type;
import types.TypeConcrete;

import java.util.ArrayList;
import java.util.List;

import interpretation.Environment;

/**
 * Boolean literal implementation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitBoolean extends Literal {

	/**
	 * Literal value
	 */
	public final boolean value;

	private LitBoolean(boolean value) { 
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return Boolean.toString(this.value);
	}

	public static final LitBoolean TRUE = new LitBoolean(true);
	public static final LitBoolean FALSE = new LitBoolean(false);

	@Override
	public Type infer() throws Exception {
		this.setType(TypeConcrete.TypeBool);
		return TypeConcrete.TypeBool;
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeBool;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		return l; // TODO ??
	}

	@Override
	public Literal toDefaultRepresentation() {
		return this;
	}

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) {
		// No non-default representations for Bool
		return this;
	}
	
	public static Expression parseList(List<Object> l) throws Exception{
	    Object first = l.get(0);
	    List<Object> working = new ArrayList<Object>();
	    working.addAll(l);
	    working.remove(0);
	    
	    for(Object o : working) {
	    	if(!(o instanceof Expression)) {
	    		throw new Exception("Syntax error unexpected " + o.toString());
	    	}
	    }
	    
	    if(first instanceof String) { //Reserved word
	    	String word = (String)first;
	    	switch(word){
	    		case "IF":
	    			if(working.size() != 3) {
	    				throw new Exception("Syntax error IF requires exactly 3 arguments got " + working.size() + " " + working.toString());
	    			}
	    			return new IfExpression((Expression)working.get(0), (Expression)working.get(1), (Expression)working.get(2));
	    		case "LAMBDA":
	    			
	    	}
	    }
	    else if(first instanceof Expression) {
	    	Tuple t = new Tuple((Expression[])working.toArray());
	    	return new Application((Expression)first, t);
	    }
	    throw new Exception("Parsing error invalid object on the first place of list");
	}
}
