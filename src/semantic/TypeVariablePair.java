package semantic;

import types.Type;
import util.Pair;
import expression.Symbol;

/**
 * Class for variable type pairs
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeVariablePair extends Pair<Type, Symbol> {

	public TypeVariablePair(Type type, Symbol variable) {
		super(type, variable);
	}

	@Override
	public String toString() {
		return "<" + this.first.toString() + " , " + this.second.toString() + ">";
	}
}