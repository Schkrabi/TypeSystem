package velka.parser.semantic;

import velka.util.Pair;
import velka.core.expression.Symbol;
import velka.types.Type;

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