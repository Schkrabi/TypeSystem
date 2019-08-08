package parser;

import expression.Variable;
import util.Pair;

public class SemanticPair extends Pair<String, String>{
	
	public SemanticPair(String lvalue, String rvalue){
		super(lvalue, rvalue);
	}
	
	@Override
	public String toString() {
		return this.first + ":" + this.second;
	}
	
	public Variable asVariable() {
		return new Variable(this.toString());
	}
}