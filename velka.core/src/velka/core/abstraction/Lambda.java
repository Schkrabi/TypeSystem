package velka.core.abstraction;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.core.application.AbstractionApplication;
import velka.core.application.Convert;
import velka.core.exceptions.ConversionException;
import velka.core.exceptions.InvalidArgumentsException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.Operators;
import velka.types.Substitution;
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
	 * Creates new environment, where each argument is binded to typeholder with its inferred type and returns this environments along with substitution, which is union of all argument inference substitutions
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
			Substitution s = argsSubst.compose(bodyInfered.second);
			
			Type argsType = this.argsType.apply(s);
			Type bodyType = bodyInfered.first.apply(s);
			
			TypeArrow finalType = new TypeArrow(argsType, bodyType);
			
			return new Pair<Type, Substitution>(finalType, s);
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
		
		return fn;
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
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Function f = (Function) this.interpret(env, typeEnv);
		return f.doSubstituteAndEvaluate(args, env, typeEnv);
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
		String code = this.toClojureFn(env, typeEnv);
		return code;
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}
	
	/**
	 * Creates default cost function for this lambda
	 * @return a Lambda expression
	 * @throws AppendableException if inference fails
	 */
	public Lambda defaultCostFunction() throws AppendableException {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		TypeArrow type = (TypeArrow)p.first;
		TypeTuple argsType = (TypeTuple)type.ltype;
		
		Lambda costFunction = new Lambda(
				this.args,
				(TypeTuple)argsType.removeRepresentationInformation(),
				new AbstractionApplication(
						Operators.ConversionCost,
						new Tuple(this, this.args)));
		return costFunction;
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		//Cannot convert into more general type
		//	converting [Int:Native] -> Int:Native to [A] -> Int:Native is not possible
		//	to should not contain any type variables.
		//However type of this can contain type variables
		//	converting A -> B to [Int:Native Int:Native] -> String:Native is viable
		if(!(to instanceof TypeArrow)) {
			throw new ConversionException(to, this);
		}
		TypeArrow to_typeArrow = (TypeArrow)to;
		
		Optional<Substitution> o = Type.unifyTypes(this.argsType, to_typeArrow.ltype);
		if(o.isEmpty()) {
			throw new ConversionException(to, this);
		}
		
		Tuple formalArgs = null;
		Expression convertedArgs = null;
		
		TypeArrow from_typeArrow = (TypeArrow)from;
		if(from_typeArrow.ltype instanceof TypeVariable) {
			formalArgs = this.args;
			convertedArgs = this.args;
		}
		else {
			formalArgs = new Tuple(this.args.stream().map(x -> new Symbol(NameGenerator.next())));
			convertedArgs = new Convert(to_typeArrow.ltype, from_typeArrow.ltype, formalArgs);
		}
		
		Expression convertedBody = 
				new Convert(
						from_typeArrow.rtype,
						to_typeArrow.rtype,
						new AbstractionApplication(
								this,
								convertedArgs));
		
		TypeTuple convertedArgsType =
				(TypeTuple)to_typeArrow.ltype.apply(o.get());
								
		Lambda lambda = new Lambda(
				formalArgs,
				convertedArgsType,
				convertedBody);
		
		return lambda;
	}
}
