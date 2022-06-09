package velka.core.abstraction;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.MalformedSelectionFunctionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.ListNative;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteropObject;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
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
	
	/**
	 * Symbol for extended-lambda-cost special form
	 */
	public static final String EXTENDED_LAMBDA_COST = "extended-lambda-selection";

	/**
	 * Various implementations of this function
	 */
	protected final Set<? extends Lambda> implementations;

	/**
	 * Ranking function for this extended lambda
	 */
	public final Expression selectionFunction;

	protected ExtendedLambda(Collection<? extends Lambda> implementations, Expression selectionFunction) {
		this.implementations = new TreeSet<Lambda>(implementations);
		this.selectionFunction = selectionFunction;
	}
	
	/**
	 * Gets shallow copy of implementation sets
	 * @return TreeSet
	 */
	public Set<? extends Lambda> getImplementations(){
		return new TreeSet<Lambda>(this.implementations);
	}
	
	/**
	 * Gets clojure code of the selection function
	 * @param env
	 * @param typeEnv
	 * @return
	 * @throws AppendableException
	 */
	public String selectionFunctionCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String selectionFunCode = "";
		if(this.selectionFunction instanceof Operator) {
			selectionFunCode = ((Operator)this.selectionFunction).getClojureSymbol().toClojureCode(env, typeEnv);
		}
		else{
			selectionFunCode = this.selectionFunction.toClojureCode(env, typeEnv);
		}
		return selectionFunCode;
	}

	@Override
	public Expression interpret(final Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Set<Function> fs = null;
		try {
			fs = this.implementations.stream().map(ThrowingFunction.wrapper(x -> (Function) x.interpret(env, typeEnv)))
					.collect(Collectors.toSet());
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}

		return ExtendedFunction.makeExtendedFunction(fs, env, this.selectionFunction);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Lambda sample = this.implementations.stream().findAny().get();
		
		List<Set<Type>> argsTypeSets = sample.argsType.stream().map(x -> new TreeSet<Type>()).collect(Collectors.toList());
		
		for(Lambda l : this.implementations) {
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
		
		return this.inferWithArgs(typeHolderArgs, env, typeEnv);
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		return this.doInferWithArgs(args, env, env, typeEnv);
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
			Environment applicationEnvironment, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Set<Type> types = new TreeSet<Type>();
			Substitution s = Substitution.EMPTY;
			for (Lambda l : this.implementations) {
				Pair<Type, Substitution> p = l.doInferWithArgs(args, creationEnv, applicationEnvironment, typeEnv);
				types.add(p.first);
				Optional<Substitution> o = s.union(p.second);
				if (!o.isPresent()) {
					throw new SubstitutionsCannotBeMergedException(s, p.second);
				}
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

		Iterator<? extends Lambda> k = this.implementations.iterator();
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

			for (Lambda l : this.implementations) {
				if (!o.implementations.contains(l))
					return 1;
			}
			for (Lambda l : o.implementations) {
				if (!this.implementations.contains(l))
					return -1;
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
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Expression> rankingFunction) throws AppendableException {
		ExtendedFunction f = (ExtendedFunction) this.interpret(env, typeEnv);
		return f.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
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
		return ExtendedLambda.makeExtendedLambda(implementations, ExtendedLambda.defaultSelectionFunction);
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
	public static ExtendedLambda makeExtendedLambda(Collection<Lambda> implementations, Expression rankingFunction)
			throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedLambda(implementations, rankingFunction);
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {		
		String implsValue = "";
		
		try {
			implsValue = ClojureHelper.listNativeClojure(this.implementations.stream()
					.map(ThrowingFunction.wrapper(x -> x.toClojureFn(env, typeEnv))).collect(Collectors.toList()));
		} catch (RuntimeException re) {
			if (re.getCause() instanceof AppendableException) {
				throw (AppendableException) re.getCause();
			}
			throw re;
		}
		
		String selectionFunCode = this.selectionFunctionCode(env, typeEnv);
		
		final String impls = "_impls";
		final String args = "_args";
		final String selectionFn = "_selection-function";
		
		String code = ClojureHelper.letHelper(
				ClojureHelper.fnHelper(
						new Pair<List<String>, String>(Arrays.asList(args),
								ClojureHelper.applyVelkaFunction(selectionFunCode,
										impls,
										ClojureHelper.applyClojureFunction(
												ClojureCoreSymbols.tuple2velkaListSymbol_full, args))),
						new Pair<List<String>, String>(Arrays.asList(args, selectionFn),
								ClojureHelper.applyVelkaFunction(selectionFn,
										impls,
										ClojureHelper.applyClojureFunction(
												ClojureCoreSymbols.tuple2velkaListSymbol_full, args)))),
				new Pair<String, String>(impls, implsValue));
		
		return code;
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> selectionFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		AbstractionApplication selectionApplication = 
				new AbstractionApplication(
						selectionFunction.isPresent() ? selectionFunction.get() : this.selectionFunction,
						new Tuple(
								ListNative.collectionToListNative(this.implementations),
								ListNative.tupleToListNative(args)
								));
		
		Expression impl = selectionApplication.interpret(env, typeEnv);
		if(!(impl instanceof Abstraction)) {
			throw new MalformedSelectionFunctionException(this, selectionApplication.fun, args, impl);
		}
		return (Abstraction)impl;
	}
	
	/**
	 * Default selection function
	 */
	public static final Operator defaultSelectionFunction = new Operator() {
		
		/**
		 * Type Variable for return type of the operator
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());
		/**
		 * Type Variable for return type of the operator
		 */
		private final TypeVariable B = new TypeVariable(NameGenerator.next());
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			final String implList = "_impl-list";
			final String argList = "_arg-list";
			final String argType = "_arg-type";
			
			final String pair1 = "_pair1";
			final String pair2 = "_pair2";
			
			final String impl = "_impl";
			
			//(fn [impl-list arg-list]
			//	(let [argType (GET-TYPE (LIST-NATIVE-2-VECTOR arg-list))]
			//		(second (reduce (fn [pair1 pair2] (if (< (first pair1) (first pair2)) pair1 pair2))
			//						(map (fn [impl] [(.tupleDistance (.ltype (GET-TYPE impl)) argsType)]) impl-list)))))
			final String code = ClojureHelper.fnHelper(Arrays.asList(implList, argList),
					ClojureHelper.letHelper(
							ClojureHelper.applyClojureFunction("second",
									ClojureHelper.applyClojureFunction("reduce",
											ClojureHelper.fnHelper(Arrays.asList(pair1, pair2),
													ClojureHelper.clojureIfHelper(
															ClojureHelper.applyClojureFunction("<",
																	ClojureHelper.applyClojureFunction(
																			"first", pair1),
																	ClojureHelper.applyClojureFunction("first", pair2)),
															pair1, pair2)),
											ClojureHelper.applyClojureFunction("map", 
													ClojureHelper.fnHelper(Arrays.asList(impl),
														ClojureHelper.clojureVectorHelper(
																ClojureHelper.applyClojureFunction(".tupleDistance",
																		ClojureHelper.applyClojureFunction(".ltype",
																				ClojureHelper.applyClojureFunction(
																						ClojureCoreSymbols.getTypeClojureSymbol_full,
																						impl)),
																		argType),
																impl)),
													ClojureHelper.getLiteralInnerValue(implList)))),
							new Pair<String, String>(argType,
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full,
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.listNativeToTuple_full, argList)))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("default-selection-function", Operators.NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite implList = (LitComposite)args.get(0);
			LitComposite argList = (LitComposite)args.get(1);
			
			TypeTuple argsTuple = null;
			
			try {
				argsTuple = new TypeTuple(ListNative.listNativeToTuple(argList).stream().map(ThrowingFunction.wrapper(e -> e.infer(env, typeEnv).first)).collect(Collectors.toList()));
			}catch(RuntimeException re) {
				if(re.getCause() instanceof AppendableException) {
					throw (AppendableException)re.getCause();
				}
				throw re;
			}
			
			int bestCost = Integer.MAX_VALUE;
			Lambda bestImplementation = null; 
			
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>)((LitInteropObject)implList.value).javaObject;
			for(Expression e : l) {
				Lambda current = (Lambda)e;
				
				int currentCost = argsTuple.tupleDistance(current.argsType);
				
				if(currentCost < bestCost || bestImplementation == null) {
					bestCost = currentCost;
					bestImplementation = current;
				}
			}
			
			return bestImplementation;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					new TypeArrow(A, B));
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
}
