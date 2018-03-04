package parser;

import java.util.List;
import java.util.stream.Collector;

public class SemanticNode {
	public final NodeType type;
	private final Object value;
	
	public SemanticNode(NodeType type, Object value) /*throws Exception*/{
		//if(!isAdequateType(type, value)){
		//	throw new Exception("invalid token type - object combination " + type + " - " + value);
		//}		
		this.type = type;
		this.value = value;
	}
	
	private static boolean isAdequateType(NodeType type, Object value){
		switch(type){
		case SYMBOL:
			return value instanceof String;
		case PAIR:
			return value instanceof Pair;
		case INT:
			return value instanceof Integer;
		case DOUBLE:
			return value instanceof Double;
		case STRING:
			return value instanceof String;
		case BOOL:
			return value instanceof Boolean;
		case LIST:
			return value instanceof List;
		}
		return false;
	}
	
	public String asSymbol() throws Exception{
		if(this.type != NodeType.SYMBOL){
			throw new Exception("" + this +  " is not a symbol");
		}
		return (String)this.value;
	}
	
	public Pair asPair() throws Exception{
		if(this.type != NodeType.PAIR){
			throw new Exception("" + this + " is not a pair");
		}
		return (Pair)this.value;
	}
	
	public Integer asInt() throws Exception{
		if(this.type != NodeType.INT){
			throw new Exception("" + this + " is not an integer");
		}
		return (Integer)this.value;
	}
	
	public Double asDouble() throws Exception{
		if(this.type != NodeType.DOUBLE){
			throw new Exception("" + this + " is not a double");
		}
		return (Double)this.value;
	}
	
	public String asString() throws Exception{
		if(this.type != NodeType.STRING){
			throw new Exception("" + this + " is not a string");
		}
		return (String)this.value;
	}
	
	public Boolean asBool() throws Exception{
		if(this.type != NodeType.BOOL){
			throw new Exception("" + this + " is not a bool");
		}
		return (Boolean)this.value;
	}
	
	public List<SemanticNode> asList() throws Exception{
		if(this.type != NodeType.LIST){
			throw new Exception("" + this + " is not a list");
		}
		//TODO add generic type check
		return (List<SemanticNode>)this.value;
	}
	
	public enum NodeType {
		SYMBOL, PAIR, INT, DOUBLE, STRING, BOOL, LIST
	}
	
	public static class Pair{
		public final String lvalue;
		public final String rvalue;
		
		public Pair(String lvalue, String rvalue){
			this.lvalue = lvalue;
			this.rvalue = rvalue;
		}
	}
	
	@Override
	public String toString(){
		if(this.value == null){
			return "null";
		}
		return this.value.toString();
	}
}
