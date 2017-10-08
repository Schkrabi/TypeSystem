package expression;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;
import types.Type;
import types.TypeTuple;

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
		// TODO Auto-generated method stub
		return null;
	}

}
