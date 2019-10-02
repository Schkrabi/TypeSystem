package expression;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
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
			Environment childEnv = Environment.create(this.creationEnvironment);

			List<Type> l = new LinkedList<Type>();

			for (Expression e : this.args) {
				if (!(e instanceof Variable)) {
					throw new AppendableException(e + " is not instance of " + Variable.class.getName());
				}
				TypeVariable tv = new TypeVariable(NameGenerator.next());
				childEnv.put((Variable) e, new TypeHolder(tv));
				l.add(tv);
			}

			Type argsType = new TypeTuple(l);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			// Now check if body was typed correctly according to user defined types of
			// arguments
			Substitution s = Type.unify(argsType, this.argsType);

			// Compose all substitutions in order to check if there are no collisions and
			// provide final substitution
			Substitution finalSubst = s.union(bodyInfered.second);

			argsType = argsType.apply(finalSubst);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(finalSubst)),
					finalSubst);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Function getFunction(TypeTuple realArgsType) {
		return this;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Function) {
			int cmp = this.argsType.compareTo(((Function) other).argsType);
			if (cmp != 0)
				return cmp;
			cmp = this.args.compareTo(((Function) other).args);
			if (cmp != 0)
				return cmp;
			cmp = this.body.compareTo(((Function) other).body);
			if (cmp != 0)
				return cmp;
			return this.creationEnvironment.compareTo(((Function) other).creationEnvironment);
		}
		return super.compareTo(other);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(FunctionInternal (");

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();

			if (t instanceof TypeVariable) {
				s.append(e.toString());
			} else {
				s.append('(');
				s.append(t.toString());
				s.append(' ');
				s.append(e.toString());
				s.append(')');
			}
		}

		s.append(") ");
		s.append(this.body.toString());
		s.append(' ');
		s.append(this.creationEnvironment);
		s.append(')');

		return s.toString();
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Function) {
			return this.args.equals(((Function) other).args) && this.body.equals(((Function) other).body)
					&& this.argsType.equals(((Function) other).argsType) && super.equals(other);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.argsType.hashCode() * this.args.hashCode() * this.body.hashCode();
	}

	public static Function IntNativeConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
	public static Function IntConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
	public static Function IntStringConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"))),
			new LitComposite(new Tuple(Arrays.asList(new LitString("_x"))), TypeAtom.TypeIntString), Environment.topLevelEnvironment);
	public static Function IntRomanConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
			new Tuple(Arrays.asList(new Variable("_x"))),
			new LitComposite(new Tuple(Arrays.asList(new LitString("_x"))), TypeAtom.TypeIntRoman), Environment.topLevelEnvironment);
	public static Function StringNativeConstructor = new Function(
			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Tuple(Arrays.asList(new Variable("_x"))),
			new Variable("_x"), Environment.topLevelEnvironment);
	public static Function StringConstructor = new Function(
			new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), new Tuple(Arrays.asList(new Variable("_x"))),
			new Variable("_x"), Environment.topLevelEnvironment);
	public static Function DoubleNativeConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
	public static Function DoubleConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
	public static Function BoolNativeConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
	public static Function BoolConstructor = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)),
			new Tuple(Arrays.asList(new Variable("_x"))), new Variable("_x"), Environment.topLevelEnvironment);
}
