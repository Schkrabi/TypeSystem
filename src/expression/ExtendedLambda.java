package expression;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;

public class ExtendedLambda extends Expression {
	
	public final Tuple args;
	
	private final Map<TypeTuple, Expression> implementations;
	
	public ExtendedLambda(Tuple args, Expression expr){
		this.args = args;
		this.implementations = new TreeMap<TypeTuple, Expression>();
		this.implementations.put(TypeTuple.EMPTY_TUPLE, expr); //Default implementation
	}
	
	public ExtendedLambda(Tuple args, Expression defaultImplementation, Map<TypeTuple, Expression> specificImplementations){
		this.args = args;
		this.implementations = new TreeMap<TypeTuple, Expression>();
		this.implementations.put(TypeTuple.EMPTY_TUPLE, defaultImplementation);
		this.implementations.putAll(specificImplementations);
	}
	
	public Expression getDefaultUmplementation(){
		return this.implementations.get(TypeTuple.EMPTY_TUPLE);
	}
	
	public Expression getImplementation(TypeTuple prefferedType){
		return this.implementations.get(prefferedType);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public Type infer() throws Exception {
		Type argsType = this.args.infer();
		Type defImplType = this.getDefaultUmplementation().infer();
		
		for(Map.Entry<TypeTuple, Expression> e : this.implementations.entrySet()){
			TypeTuple targs = e.getKey();
			Type timpl = e.getValue().infer();
			
			if(!Type.unify(targs, argsType)){
				throw new Exception(	"Bad extended lambda specialized arguments of type " 
									+ 	targs.getRep().toString() 
									+ " do not unify with default arguments " 
									+ argsType.getRep().toString());
			}
			
			if(!Type.unify(defImplType, timpl)){
				throw new Exception(	"Bad extended lambda specialized implementation "
									+	e.getValue().toString() + "of type " + timpl.toString()
									+	"do not unify with default implementation "
									+ 	this.getDefaultUmplementation().toString() + " of type " + defImplType.toString());
			}
		}
		
		Type t = new TypeArrow(argsType, defImplType);
		
		for(TypeVariable v : t.getUnconstrainedVariables()){
			t = new ForallType(v, t);
		}
		
		return t;
	}

}
