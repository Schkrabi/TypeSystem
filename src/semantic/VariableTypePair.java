package semantic;

import types.Type;
import expression.Variable;

/**
 * Class for variable type pairs
 * @author Mgr. Radomir Skrabal
 *
 */
class VariableTypePair {
	public final Type type;
	public final Variable variable;

	public VariableTypePair(Type type, Variable variable) {
		this.type = type;
		this.variable = variable;
	}
}