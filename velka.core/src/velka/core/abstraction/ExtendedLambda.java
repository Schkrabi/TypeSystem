package velka.core.abstraction;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.CostAggregation;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingFunction;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends Abstraction {
	
	/**
	 * Symbol for extended lambda-special form
	 */
	public static final String EXTENDED_LAMBDA = "extended-lambda";

	public final Type argsType;
	
	/**
	 * Implementations of this function.
	 * Implementations are keys of the map, cost functionas are values
	 */
	protected final Map<? extends Lambda, Expression> implementations;
	
	protected ExtendedLambda(Type argsType, Map<? extends Lambda, Expression> implementations) {
		this.implementations = new TreeMap<Lambda, Expression>(implementations);
		this.argsType = argsType;
	}
	
	public ExtendedLambda(Type argsType) {
		this.implementations = new TreeMap<Lambda, Expression>();
		this.argsType = argsType;
	}
	
	/**
	 * Gets shallow copy of implementation sets
	 * @return TreeSet
	 */
	public Map<Lambda, Expression> getImplementations(){
		return new TreeMap<Lambda, Expression>(this.implementations);
	}

	@Override
	public Expression interpret(final Environment env) throws AppendableException {
		Map<Function, Expression> m = new TreeMap<Function, Expression>();
		
		for(Map.Entry<? extends Lambda, Expression> e : this.implementations.entrySet()) {
			Function f = (Function)e.getKey().interpret(env);
			Expression r = e.getValue().interpret(env);
			
			m.put(f, r);
		}
		
		return new ExtendedFunction(this.argsType, m, env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		if(this.implementations.isEmpty()) {
			Type t =  new TypeArrow(this.argsType, new TypeVariable(NameGenerator.next()));
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}
		
		Lambda sample = this.implementations.keySet().stream().findAny().get();
		
		List<Set<Type>> argsTypeSets = sample.argsType.stream().map(x -> new TreeSet<Type>()).collect(Collectors.toList());
		
		for(Lambda l : this.implementations.keySet()) {
			Iterator<Set<Type>> iSets = argsTypeSets.iterator();
			Iterator<Type> iArgsType = l.argsType.iterator();
			while(iSets.hasNext()) {
				Set<Type> set = iSets.next();
				Type argType = iArgsType.next();
				set.add(argType);
			}
		}
				
		Tuple typeHolderArgs = null;
		
		try {
			typeHolderArgs = new Tuple(
					argsTypeSets.stream().map(ThrowingFunction.wrapper(x -> RepresentationOr.makeRepresentationOr(x)))
							.map(x -> new TypeHolder(x)).collect(Collectors.toList()));
		} catch (RuntimeException re) {
			if (re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException) re.getCause();
				throw e;
			}
			throw re;
		}	
		
		return this.inferWithArgs(typeHolderArgs, env);
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env)
			throws AppendableException {
		return this.doInferWithArgs(args, env, env);
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
	protected Pair<Type, Substitution> doInferWithArgs(Tuple args, Environment creationEnv,
			Environment applicationEnvironment) throws AppendableException {
		try {
			Set<Type> types = new TreeSet<Type>();
			Substitution s = Substitution.EMPTY;
			for (Lambda l : this.implementations.keySet()) {
				Pair<Type, Substitution> p = l.doInferWithArgs(args, creationEnv, applicationEnvironment);
				types.add(p.first);
				
				Optional<Substitution> opt = s.merge(p.second);
				if(opt.isEmpty()) {
					throw new SubstitutionsCannotBeMergedException(s, p.second);
				}
				s = opt.get();
			}

			Type type = RepresentationOr.makeRepresentationOr(types);
			return new Pair<Type, Substitution>(type, s);
		} catch (AppendableException e) {
			e.appendMessage("\nin " + this.toString());
			throw e;
		}
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(elambda (");

		Iterator<? extends Lambda> k = this.implementations.keySet().iterator();
		while (k.hasNext()) {
			Lambda l = k.next();
			s.append('(');
			s.append(l.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(l.body.toString());
			s.append(')');
			if (k.hasNext()) {
				s.append(' ');
			}
		}

		s.append(')');

		return s.toString();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedLambda) {
			ExtendedLambda o = (ExtendedLambda) other;
			
			int cmp = this.argsType.compareTo(o.argsType);
			if(cmp != 0) {
				return cmp;
			}
			
			for (Entry<? extends Lambda, Expression> e : this.implementations.entrySet()) {
				Lambda l = e.getKey();				
				if (!o.implementations.containsKey(l)) {
					return 1;
				}
				Expression v = e.getValue();
				cmp = o.implementations.get(l).compareTo(v);
				if(cmp != 0) {
					return cmp;
				}
			}
			for (Entry<? extends Lambda, Expression> e : o.implementations.entrySet()) {
				Lambda l = e.getKey();
				if (!this.implementations.containsKey(l)) {
					return -1;
				}
				Expression v = e.getValue();
				cmp = this.implementations.get(l).compareTo(v);
				if(cmp != 0) {
					return cmp;
				}
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedLambda) {
			return this.implementations.equals(((ExtendedLambda) other).implementations);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.implementations.hashCode();
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
		ExtendedFunction f = (ExtendedFunction) this.interpret(env);
		return f.doSubstituteAndEvaluate(args, env);
	}
	
	/**
	 * Convenience constructor for extended lambda
	 * @param impls implementations
	 * @return new extended lambda instance
	 * @throws AppendableException if implementations arguments does not unify
	 */
	public static ExtendedLambda makeExtendedLambda(Lambda... impls) throws AppendableException {
		return ExtendedLambda.makeExtendedLambda(Arrays.asList(impls));
	}

	/**
	 * Creates new Extended lambda expression
	 * 
	 * @param implementations implementations of extended lambda
	 * @return new ExtendedLambda object
	 * @throws AppendableException thrown if any of the argument types does not
	 *                             unify
	 */
	public static ExtendedLambda makeExtendedLambda(Collection<Lambda> implementations) throws AppendableException {
		Map<Lambda, Expression> m = new TreeMap<Lambda, Expression>();
		for(Lambda l : implementations) {
			var costF = Lambda.constFun(l.args.size(), new LitDouble(CostAggregation.instance().defaultImplementationRank()));
			m.put(l, costF);
		}
		
		return ExtendedLambda.makeExtendedLambda(m);
	}

	/**
	 * Creates new Extended lambda expression
	 * 
	 * @param implementations implementations of extended lambda
	 * @param rankingFunction ranking function used for selecting implementation
	 * @return new ExtendedLambda object
	 * @throws AppendableException thrown if any of the argument types does not
	 *                             unify
	 */	
	public static ExtendedLambda makeExtendedLambda(Map<Lambda, Expression> implementations)
			throws AppendableException{
		if(implementations.isEmpty()) {
			throw new AppendableException("Cannot create extended lambda from empty implementation set.");
		}
		
		Optional<Substitution> o = Type.unifyMany(implementations.keySet().stream().map(x -> x.argsType).collect(Collectors.toSet()));
		
		if(!o.isPresent()) {
			throw new AppendableException("Cannot create extended lambda from implementations: "
					+ implementations);
		}
		
		Type sample = implementations.keySet().stream().findAny().get().argsType;
		sample.apply(o.get());
		sample = sample.removeRepresentationInformation();
		
		return new ExtendedLambda(sample, implementations);
	}

	@Override
	protected String implementationsToClojure(Environment env) throws AppendableException {				
		List<String> implCodes = new LinkedList<String>();
		
		for(Map.Entry<? extends Lambda, Expression> e : this.implementations.entrySet()) {
			Lambda l = e.getKey();
			Expression ex = e.getValue();
			
			String implCode = l.toClojureFn(env);
			String costFnCode = ex.toClojureCode(env);
			var p = l.infer(env);
			
			String implWithCostCode = ClojureHelper.setCostAndType(
					implCode, 
					costFnCode, 
					p.first.clojureTypeRepresentation());
			
			implCodes.add(implWithCostCode);
		}
		
		return ClojureHelper.clojureSetHelper(implCodes);
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Environment env) throws AppendableException {		
		
		Lambda bestImplementation = null;
		var bestCost = 0.d;
		for(Entry<? extends Lambda, Expression> e : this.implementations.entrySet()) {
			var cost = this.implementationCost((Lambda)e.getKey(), (Abstraction)e.getValue(), args, env);
			if(cost.isPresent()) {
				if(cost.get() > bestCost || bestImplementation == null ) {
					bestImplementation = e.getKey();
					bestCost = cost.get();
				}
			}
		}
		
		return bestImplementation;
	}
	
	/** Computes the cost of using implementation with given arguments */
	private Optional<Double> implementationCost(Lambda impl, Abstraction cost, Tuple args, Environment env) {
		try {
			var costAppl = new AbstractionApplication(cost, args);
			var implCost = costAppl.interpret(env);
			if(!(implCost instanceof LitDouble)) {
				throw new RuntimeException();
			}
			
			var sum = ((LitDouble)implCost).value;
			
			var fpit = impl.argsType.iterator();
			var ait = args.iterator();
			while(fpit.hasNext()) {
				var formalArgType = fpit.next();
				var arg = ait.next();
				var argType = arg.infer(env).first;
				var ccost = env.getTypeSystem().conversionCost(argType, formalArgType, arg, env);
				
				if(ccost == null) return Optional.empty();
				
				sum = CostAggregation.instance().aggregate(sum, ccost); 
			}
			
			return Optional.of(sum);
		} catch(AppendableException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		if(!(to instanceof TypeArrow)) {
			throw new ConversionException(to, this);
		}
		if(Type.unifyTypes(from, to).isPresent()) {
			return this;
		}		
		
		throw new ConversionException(to, this);
	}
}
