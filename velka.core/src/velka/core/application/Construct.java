package velka.core.application;

import java.util.Iterator;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Special form for constructing types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Construct extends Expression {
	
	/**
	 * Symbol for construct special form
	 */
	public static final String CONSTRUCT = "construct";

	/**
	 * Constructed type atom
	 */
	public final TypeAtom constructedType;

	/**
	 * Arguments supplied to the constructor
	 */
	public final Tuple arguments;

	public Construct(TypeAtom constructedType, Tuple arguments) {
		this.constructedType = constructedType;
		this.arguments = arguments;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		TypeTuple argumentsType = (TypeTuple) this.arguments.infer(env).first;
		var o = env.getTypeSystem().construct(this.constructedType, argumentsType, this.arguments.toList(), env);
		if(!(o instanceof Expression)) {
			throw new RuntimeException("Invalid constructor for type " + this.constructedType + " with arguments " + this.arguments);
		}
		return (Expression)o;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> argumentsInfered = this.arguments.infer(env);
		return new Pair<Type, Substitution>(this.constructedType, argumentsInfered.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var arg = "_arg";
		var type = "_type";
		var code = ClojureHelper.letHelper(
				ClojureHelper.applyClojureFunction(".construct", 
						ClojureCoreSymbols.typeSystem_full,
						this.constructedType.clojureTypeRepresentation(),
						type, 
						arg,
						"nil"),
				Pair.of(arg, this.arguments.toClojureCode(env)),
				Pair.of(type, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, arg)));
		return code;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("(");
		s.append(CONSTRUCT);
		s.append(" ");
		s.append(this.constructedType.name.toString());
		s.append(" ");
		s.append(this.constructedType.representation.toString());
		if (!this.arguments.equals(Tuple.EMPTY_TUPLE)) {
			s.append(" ");

			Iterator<Expression> i = this.arguments.iterator();
			while (i.hasNext()) {
				Expression e = i.next();
				s.append(e.toString());
				if (i.hasNext()) {
					s.append(" ");
				}
			}
		}
		s.append(")");
		return s.toString();
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Construct) {
			return this.constructedType.equals(((Construct) other).constructedType)
					&& this.arguments.equals(((Construct) other).arguments);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.constructedType.hashCode() * this.arguments.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Construct) {
			int cmp = this.constructedType.compareTo(((Construct) other).constructedType);
			if (cmp != 0)
				return cmp;
			return this.arguments.compareTo(((Construct) other).arguments);
		}
		return super.compareTo(other);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
	}
}
