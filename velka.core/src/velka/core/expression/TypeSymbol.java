package velka.core.expression;

import velka.core.exceptions.ConversionException;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitComposite;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
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
		return LitComposite.clojureValueToClojureLiteral(ClojureHelper.stringHelper(this.type.toString()), this.type);
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
	
	@Override
	public String toString() {
		return "[\"" + this.type.toString() + "\"]";
	}

	@Override
	public Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		throw new ConversionException(to, this);
	}

}
