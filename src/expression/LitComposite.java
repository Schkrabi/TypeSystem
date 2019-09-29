package expression;

import java.util.stream.Collectors;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import util.ThrowingFunction;

/**
 * Literal for representing composed type literals
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitComposite extends Literal {

	/**
	 * Composed values of this literal
	 */
	public final Tuple value;
	/**
	 * Type of this literal
	 */
	public final TypeAtom composedType;

	public LitComposite(Tuple value, TypeAtom composedType) {
		super();
		this.value = value;
		this.composedType = composedType;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.composedType, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO Auto-generated method stub
		return this.value.toClojureCode();
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof LitComposite) {
			return this.composedType.equals(((LitComposite) other).composedType)
					&& this.value.equals(((LitComposite) other).value);
		}
		return false;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitComposite) {
			int cmp = this.composedType.compareTo(((LitComposite) other).composedType);
			if (cmp != 0)
				return cmp;
			return this.value.compareTo(((LitComposite) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.value.hashCode() * this.composedType.hashCode();
	}

	@Override
	public String toString() {
		return "<" + this.composedType.toString() + " "
				+ this.value.stream().map(x -> x.toString() + ",").reduce("", (x, y) -> x + y) + ">";
	}

	/**
	 * Creates constructor of LitComposite for given types and typeAtom
	 * 
	 * @param composedTypes types of which the new type is composed
	 * @param constructed   composed type
	 * @return Lambda expression
	 */
	public static Lambda makeConstructor(TypeTuple composedTypes, TypeAtom constructed) {
		Tuple args = new Tuple(
				composedTypes.stream().map(x -> new Variable(NameGenerator.next())).collect(Collectors.toList()));
		Expression body = new LitCompositeConstructor(args, constructed);
		return new Lambda(args, composedTypes, body);
	}

	/**
	 * Class for construction LitComposite in constructors
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class LitCompositeConstructor extends Expression {
		/**
		 * Names of arguments of construction lambda
		 */
		public final Tuple args;
		/**
		 * Created typeAtom
		 */
		public final TypeAtom createdType;

		public LitCompositeConstructor(Tuple args, TypeAtom createdType) {
			super();
			this.args = args;
			this.createdType = createdType;
		}

		@Override
		public Expression interpret(final Environment env) throws AppendableException {
			Tuple value = new Tuple(
					this.args.stream().map(ThrowingFunction.wrapper(x -> x.interpret(env))).collect(Collectors.toList()));
			return new LitComposite(value, this.createdType);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			return new Pair<Type, Substitution>(this.createdType, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode() throws AppendableException {
			// TODO Auto-generated method stub
			return "";
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof LitCompositeConstructor) {
				return this.args.equals(((LitCompositeConstructor) other).args)
						&& this.createdType.equals(((LitCompositeConstructor) other).createdType);
			}
			return false;
		}

		@Override
		public int compareTo(Expression other) {
			if (other instanceof LitCompositeConstructor) {
				int cmp = this.args.compareTo(((LitCompositeConstructor) other).args);
				if (cmp != 0)
					return cmp;
				return this.createdType.compareTo(((LitCompositeConstructor) other).createdType);
			}
			return super.compareTo(other);
		}

	}

}
