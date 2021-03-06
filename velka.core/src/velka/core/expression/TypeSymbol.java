package velka.core.expression;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Special type carying symbol used in user specified ranking functions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeSymbol extends Expression {
	
	/**
	 * Caried type
	 */
	public final Type type;
	
	public TypeSymbol(Type type) {
		this.type = type;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
		return new Pair<Type, Substitution>(this.type, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return "(with-meta [" + this.type.clojureTypeRepresentation() + "] {:lang-type " + this.type.clojureTypeRepresentation() + "})";
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof TypeSymbol) {
			return this.type.equals(((TypeSymbol) other).type);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.type.hashCode();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof TypeSymbol) {
			return this.type.compareTo(((TypeSymbol) other).type);
		}
		return super.compareTo(other);
	}

}
