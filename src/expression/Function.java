package expression;

import java.util.Comparator;
import java.util.Optional;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

/**
 * Expression for representation of interpreted function
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Function extends MetaFunction implements Comparable<Expression> {

	/**
	 * Type of the function arguments
	 */
	public final TypeTuple argsType;
	/**
	 * Function arguments
	 */
	public final Tuple args;
	/**
	 * Body of the fucntion
	 */
	public final Expression body;

	public Function(TypeTuple argsType, Tuple args, Expression body, Environment createdEnvironment) {
		super(createdEnvironment);
		this.argsType = argsType;
		this.args = args;
		this.body = body;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = new Environment(this.creationEnvironment);
			Type[] argsTypeArr = new Type[this.args.values.length];

			for (int i = 0; i < this.args.values.length; i++) {
				Expression e = this.args.values[i];
				if (!(e instanceof Variable)) {
					throw new AppendableException(e + " is not instance of " + Variable.class.getName());
				}
				TypeVariable tv = new TypeVariable(NameGenerator.next());
				childEnv.put((Variable) e, new TypeHolder(tv));
				argsTypeArr[i] = tv;
			}

			Type argsType = new TypeTuple(argsTypeArr);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			// Now check if body was typed correctly according to user defined types of
			// arguments
			Optional<Substitution> s = Type.unify(argsType, this.argsType);

			if (!s.isPresent()) {
				throw new TypesDoesNotUnifyException(argsType, this.argsType);
			}

			// Compose all substitutions in order to check if there are no collisions and
			// provide final substitution
			Substitution finalSubst = s.get().compose(bodyInfered.second);

			argsType = argsType.apply(finalSubst);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(finalSubst)),
					finalSubst);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Function getFunction() {
		return this;
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Function) {
			Function o = (Function) other;
			if (this.argsType == o.argsType) {
				return 0;
			}
			if (this.argsType == null) {
				return 1;
			}
			if (o.argsType == null) {
				return -1;
			}

			return this.argsType.compareTo(o.argsType);
		}
		return super.compareTo(other);
	}

	@Override
	public String toString() {
		return "(func " + this.args.toString() + " " + this.body.toString() + ")";
	}
}
