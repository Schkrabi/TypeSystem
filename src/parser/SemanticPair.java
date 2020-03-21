package parser;

import expression.Symbol;
import util.Pair;

public class SemanticPair extends Pair<String, String> {

	public SemanticPair(String lvalue, String rvalue) {
		super(lvalue, rvalue);
	}

	@Override
	public String toString() {
		return this.first + ":" + this.second;
	}

	public Symbol asVariable() {
		return new Symbol(this.toString());
	}
}