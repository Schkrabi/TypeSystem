package parser;

import java.util.List;

import expression.Variable;
import util.AppendableException;

public class SemanticNode {
	public final NodeType type;
	private final Object value;
	
	public SemanticNode(NodeType type, Object value) {	
		this.type = type;
		this.value = value;
	}
	
	public static boolean isAdequateType(NodeType type, Object value){
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
	
	public String asSymbol() throws AppendableException{
		if(this.type != NodeType.SYMBOL){
			throw new AppendableException("" + this +  " is not a symbol");
		}
		return (String)this.value;
	}
	
	public Pair asPair() throws AppendableException{
		if(this.type != NodeType.PAIR){
			throw new AppendableException("" + this + " is not a pair");
		}
		return (Pair)this.value;
	}
	
	public Integer asInt() throws AppendableException{
		if(this.type != NodeType.INT){
			throw new AppendableException("" + this + " is not an integer");
		}
		return (Integer)this.value;
	}
	
	public Double asDouble() throws AppendableException{
		if(this.type != NodeType.DOUBLE){
			throw new AppendableException("" + this + " is not a double");
		}
		return (Double)this.value;
	}
	
	public String asString() throws AppendableException{
		if(this.type != NodeType.STRING){
			throw new AppendableException("" + this + " is not a string");
		}
		return (String)this.value;
	}
	
	public Boolean asBool() throws AppendableException{
		if(this.type != NodeType.BOOL){
			throw new AppendableException("" + this + " is not a bool");
		}
		return (Boolean)this.value;
	}
	
	@SuppressWarnings("unchecked")
	public List<SemanticNode> asList() throws AppendableException{
		if(this.type != NodeType.LIST
				|| !(this.value instanceof List<?>)){
			throw new AppendableException("" + this + " is not a list");
		}
		return (List<SemanticNode>)this.value;
	}
	
	@Override
	public String toString(){
		if(this.value == null){
			return "null";
		}
		return this.value.toString();
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
		
		@Override
		public String toString() {
			return this.lvalue + ":" + this.rvalue;
		}
		
		public Variable asVariable() {
			return new Variable(this.toString());
		}
	}
}
