package velka.core.abstraction;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.core.exceptions.InvalidArgumentsException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.TypeVariable;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends Abstraction implements Comparable<Expression> {

	/**
	 * Symbol for lambda special form
	 */
	public static final String LAMBDA = "lambda";

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

	/**
	 * General identity lambda
	 */
	public static final Lambda identity = Lambda.makeIdentity(new TypeVariable(NameGenerator.next()));

	public Lambda(Tuple args, TypeTuple argsType, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = argsType;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return new Function(this.argsType, this.args, this.body, env);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(lambda (");

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
		s.append(')');

		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Tuple typeHolderArgs = new Tuple(this.argsType.stream().map(x -> new TypeHolder(x)).collect(Collectors.toList()));
		return this.inferWithArgs(typeHolderArgs, env, typeEnv);
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		return this.doInferWithArgs(args, env, env, typeEnv);
	}
	
	/**
	 * Creates new environment, where each argument is binded to typeholder with its infered type and returns this environemnts along with substitution, which is union of all argument inference substitutions
	 * @param creationEnv environment where abstraction was created
	 * @param argsInfered Infered type of applied arguments
	 * @return pair of environment and substitution
	 * @throws AppendableException
	 */
	private Pair<Environment, Substitution> prepareInferenceEnvironment(Environment creationEnv, TypeTuple argsInfered) throws AppendableException{
		Environment childEnv = Environment.create(creationEnv);
		Substitution argsSubst = Substitution.EMPTY;
		
		Iterator<Expression> itFormalArg = this.args.iterator();
		Iterator<Type> itFormalArgType = this.argsType.iterator();
		Iterator<Type> itArgType = argsInfered.iterator();
		
		while (itFormalArg.hasNext()) {
			Expression sym = itFormalArg.next();
			Type fArgType = itFormalArgType.next();
			Type argType = itArgType.next();
			Type holderType;
			
			//If representation of this argument can be unified with representation of 
			//formal argument then use it.
			Optional<Substitution> uni = Type.unifyRepresentation(fArgType, argType);
			if(uni.isPresent()) {
				holderType = fArgType.apply(uni.get());
				Optional<Substitution> o = argsSubst.union(uni.get());
				if(!o.isPresent()) {
					throw new SubstitutionsCannotBeMergedException(argsSubst, uni.get());
				}
				argsSubst = o.get();
			}
			//Otherwise use representation of formal argument, since it will be converted
			else{
				holderType = fArgType;
			}

			if (!(sym instanceof Symbol)) {
				throw new AppendableException(sym + " is not instance of " + Symbol.class.getName());
			}
			childEnv.put((Symbol) sym, new TypeHolder(holderType));
		}
		
		return new Pair<Environment, Substitution>(childEnv, argsSubst);
	}
	
	/**
	 * Does the actual logic for inferWithArgs, since for functions creation and interpretation environment might differ.
	 * @param args arguments applied with
	 * @param creationEnv environment where abstraction was created
	 * @param applicationEnvironment environment where abstraction was applied
	 * @param typeEnv type environment
	 * @return pair of inferred type and substitution
	 * @throws AppendableException
	 */
	public Pair<Type, Substitution> doInferWithArgs(Tuple args, Environment creationEnv, Environment applicationEnvironment, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Pair<Type, Substitution> argsInfered = args.infer(applicationEnvironment, typeEnv);
			
			//First check if arguments are of valid types
			if(!Type.unifyTypes(argsInfered.first, this.argsType).isPresent()) {
				throw new InvalidArgumentsException(this, args);
			}
			
			Pair<Environment, Substitution> ceS = this.prepareInferenceEnvironment(creationEnv, (TypeTuple)argsInfered.first);
			
			Environment childEnv = ceS.first;
			Substitution argsSubst = ceS.second;		
			
			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv, typeEnv);
			Optional<Substitution> o = argsSubst.union(bodyInfered.second);
			if(!o.isPresent()) {
				throw new SubstitutionsCannotBeMergedException(argsSubst, bodyInfered.second);
			}			
			
			Type argsType = this.argsType.apply(o.get());
			Type bodyType = bodyInfered.first.apply(o.get());
			
			TypeArrow finalType = new TypeArrow(argsType, bodyType);
			
			Set<TypeVariable> scopeVariables = this.argsType.getVariables();
			scopeVariables.addAll(finalType.getVariables());
			
			Map<TypeVariable, TypeVariable> replacements = TypeVariable.makeRenameMap(scopeVariables);
			
			Substitution finalSubstitution = o.get().removeTypeVariables(replacements);
			
			finalType = (TypeArrow) finalType.replaceVariables(replacements);

			return new Pair<Type, Substitution>(finalType, finalSubstitution);
		} catch (AppendableException e) {
			e.appendMessage("\nin " + this.toString());
			throw e;
		}
	}

	/**
	 * Creates code of this lambda as simple lambda in Clojure (e.g. (fn [x] x))
	 * 
	 * @param env environment where lambda is evaluated
	 * @param typeEnv type environment
	 * @return string containing clojure code
	 * @throws AppendableException Thrown on unification error or when any argument
	 *                             is not a variable
	 */
	protected String toClojureFn(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<String> fnArgs = new LinkedList<String>();
		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		Environment child = Environment.create(env);
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			if (!(e instanceof Symbol)) {
				// TODO change throwable
				throw new AppendableException("Invalid expression in lambda variable list!");
			}
			Symbol v = (Symbol) e;
			child.put(v, new TypeHolder(t));
			
			fnArgs.add(v.toClojureCode(env, typeEnv));
		}
		
		String fn = ClojureHelper.fnHelper(fnArgs, this.body.toClojureCode(child, typeEnv));
		
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		
		return ClojureHelper.addTypeMetaInfo(fn, p.first);
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Lambda) {
			int cmp = this.args.compareTo(((Lambda) other).args);
			if (cmp != 0) {
				return cmp;
			}

			cmp = this.argsType.compareTo(((Lambda) other).argsType);
			if (cmp != 0) {
				return cmp;
			}

			return this.body.compareTo(((Lambda) other).body);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Lambda) {
			boolean argsEqual = this.args.equals(((Lambda) other).args);
			if(!argsEqual) {
				return false;
			}
			boolean bodyEqual = this.body.equals(((Lambda) other).body);
			if(!bodyEqual) {
				return false;
			}
			
			return Type.unifyRepresentation(this.argsType, ((Lambda) other).argsType).isPresent();
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.args.hashCode() * this.body.hashCode();
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Expression> rankingFunction) throws AppendableException {
		Function f = (Function) this.interpret(env, typeEnv);
		return f.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
	}

	/**
	 * Makes identity lambda with given type
	 * 
	 * @param argType type of the identity arg
	 * @return identity lambda
	 */
	public static Lambda makeIdentity(Type argType) {
		Symbol symbol = new Symbol(NameGenerator.next());
		return new Lambda(new Tuple(Arrays.asList(symbol)), new TypeTuple(Arrays.asList(argType)), symbol);
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return ClojureHelper.lambdaHelper(this.toClojureFn(env, typeEnv));
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}
}
