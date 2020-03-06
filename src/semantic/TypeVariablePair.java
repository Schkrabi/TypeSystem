package semantic;

import types.Type;
import util.Pair;
import expression.Variable;

/**
 * Class for variable type pairs
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeVariablePair extends Pair<Type, Variable> {

	public TypeVariablePair(Type type, Variable variable) {
		super(type, variable);
	}

	@Override
	public String toString() {
		return "<" + this.first.toString() + " , " + this.second.toString() + ">";
	}
}