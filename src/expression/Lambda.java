package expression;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import interpretation.Environment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends MetaLambda implements Comparable<Expression> {

	/**
	 * Formal arguments (names) of the lambda expression
	 */
	public final Tuple args;

	/**
	 * Body
	 */
	public final Expression body;

	/**
	 * Non mandatory type of the lambda arguments
	 */
	public final TypeTuple argsType;

	public Lambda(Variable arg, Expression body) {
		this.args = new Tuple(Arrays.asList(arg));
		this.body = body;
		this.argsType = new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()) ));
	}

	public Lambda(Tuple args, Expression body) {
		this.args = args;
		this.body = body;
		
		List<Type> types = new LinkedList<Type>();
		for(int i = 0; i < args.size(); i++)
			types.add(new TypeVariable(NameGenerator.next()));
		this.argsType = new TypeTuple(types);
	}

	public Lambda(Tuple args, TypeTuple argsType, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = argsType;
	}

	@Override
	public Expression interpret(Environment env) {
		return new Function(this.argsType, this.args, this.body, env);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(func (");
		
		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while(i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			
			if(t instanceof TypeVariable) {
				s.append(e.toString());
			}
			else {
				s.append('(');
				s.append(t.toString());
				s.append(' ');
				s.append(e.toString());
				s.append(')');
			}
		}
		
		s.append(") ");
		s.append(this.body.toString());
		s.append(')');
		
		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = new Environment(env);
			List<Type> argsTypeArr = new LinkedList<Type>();

			for (Expression e : this.args) {
				if (!(e instanceof Variable)) {
					//TODO change throwable
					throw new AppendableException(e + " is not instance of " + Variable.class.getName());
				}
				TypeVariable tv = new TypeVariable(NameGenerator.next());
				childEnv.put((Variable) e, new TypeHolder(tv));
				argsTypeArr.add(tv);
			}

			Type argsType = new TypeTuple(argsTypeArr);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			// Now check if body was typed correctly according to user defined types of
			// arguments
			Optional<Substitution> s = Type.unify(argsType, this.argsType);

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
	public String toClojureCode() throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append("(fn [");

		Iterator<Expression> i = this.args.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			if (!(e instanceof Variable)) {
				//TODO change throwable
				throw new AppendableException("Invalid expression in lambda variable list!");
			}
			Variable v = (Variable) e;
			s.append(v.toClojureCode());
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append("] ");
		s.append(this.body.toClojureCode());
		s.append(')');
		return s.toString();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Lambda) {
			int cmp = this.args.compareTo(((Lambda) other).args);
			if(cmp != 0) {
				return cmp;
			}
			
			cmp = this.argsType.compareTo(((Lambda) other).argsType);
			if(cmp != 0) {
				return cmp;
			}
			
			return this.body.compareTo(((Lambda) other).body);
		}
		return super.compareTo(other);
	}

	@Override
	public Lambda getLambda(Comparator<? super Lambda> c) {
		return this;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Lambda) {
			return this.args.equals(((Lambda) other).args) && this.body.equals(((Lambda) other).body)
					&& this.argsType.equals(((Lambda) other).argsType);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.args.hashCode() * this.argsType.hashCode() * this.body.hashCode();
	}
}
