package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ClojureHelper.ProxyImpl;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

/** Tree set operators */
@VelkaOperatorBank
@Description("Operators for working with java.util.TreeSet.") 
@Header("Tree Set")
public class TreeSet extends OperatorBank {

	/** Clojure namespace */
	public static final String NAMESPACE = "velka.clojure.treeSet";
	
	public static final Path PATH = Paths.get("velka", "clojure");
	public static final Path FILE = Paths.get("treeMap.clj");
	
	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	
	@VelkaConstructor
	@Description("Constructs Set:Tree.") 
	@Name("Constructs a new, empty tree set, ordered according to the given comparator function.") 
	@Syntax("(construct Set Tree <comparator function>)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String cmpFun = "_compare-function";
			String a1 = "_a1";
			String a2 = "_a2";
			String code;
			try {
				code = ClojureHelper.fnHelper(
						Arrays.asList(cmpFun),
						ClojureHelper.constructJavaClass(
								java.util.TreeSet.class,
								ClojureHelper.proxy(
										java.util.Comparator.class,
										Arrays.asList(),
										ProxyImpl.of(
												java.util.Comparator.class.getDeclaredMethod("compare", Object.class, Object.class),
												Arrays.asList(a1, a2), 
													ClojureHelper.applyVelkaFunction(
																cmpFun, a1, a2)))));
			} catch (NoSuchMethodException | SecurityException e) {
				throw new AppendableException(e.toString());
			}
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			final Expression cmpFun = args.get(0);
			
			java.util.Comparator<Object> comparator = new java.util.Comparator<Object>() {

				@Override
				public int compare(Object o1, Object o2) {
					var e1 = Literal.objectToLiteral(o1);
					var e2 = Literal.objectToLiteral(o2);
					
					Expression cmpEval = new AbstractionApplication(
												cmpFun,
												new Tuple(e1, e2));
					Expression cmp;
					try {
						cmp = cmpEval.interpret(env, typeEnv);
					} catch (AppendableException e) {
						throw new RuntimeException(e);
					}
					
					LitInteger cmpLI = (LitInteger)cmp;
					
					return (int)cmpLI.value;
				}
				
			};
			
			var set = new java.util.TreeSet<Expression>(comparator);
			return new LitInteropObject(set, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(
							new TypeArrow(new TypeTuple(A, A), TypeAtom.TypeIntNative)),
					TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Set:Tree ";
		}		
	};
	
	@VelkaOperator
	@Description("Adds the specified element to this set if it is not already present.") 
	@Example("(map-tree-ceiling-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-ceiling-entry <map>)")
	public static Operator add = Operator.wrapJavaMethod(java.util.TreeSet.class, "add", "set-tree-add", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Adds the specified element to this set if it is not already present.")
	public static Operator addAll = Operator.wrapJavaMethod(java.util.TreeSet.class, "addAll", "set-tree-add-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	@Description("Returns the least element in this set greater than or equal to the given element, or null if there is no such element.")
	public static Operator ceiling = Operator.wrapJavaMethod(java.util.TreeSet.class, "ceiling", "set-tree-ceiling", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Removes all of the elements from this set.")
	public static Operator clear = Operator.wrapJavaMethod(java.util.TreeSet.class, "clear", "set-tree-clear", NAMESPACE);
	
	@VelkaOperator
	@Description("Returns true if this set contains the specified element.")
	public static Operator contains = Operator.wrapJavaMethod(java.util.TreeSet.class, "contains", "set-tree-contains", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Returns the first (lowest) element currently in this set.")
	public static Operator first = Operator.wrapJavaMethod(java.util.TreeSet.class, "first", "set-tree-first", NAMESPACE);
	
	@VelkaOperator
	@Description("Returns the greatest element in this set less than or equal to the given element, or null if there is no such element.")
	public static Operator floor = Operator.wrapJavaMethod(java.util.TreeSet.class, "floor", "set-tree-floor", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Returns the least element in this set strictly greater than the given element, or null if there is no such element.")
	public static Operator higher = Operator.wrapJavaMethod(java.util.TreeSet.class, "higher", "set-tree-higher", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Returns the last (highest) element currently in this set.")
	public static Operator last = Operator.wrapJavaMethod(java.util.TreeSet.class, "last", "set-tree-last", NAMESPACE);
	
	@VelkaOperator
	@Description("Returns the greatest element in this set strictly less than the given element, or null if there is no such element.")
	public static Operator lower = Operator.wrapJavaMethod(java.util.TreeSet.class, "lower", "set-tree-lower", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Retrieves and removes the first (lowest) element, or returns null if this set is empty.")
	public static Operator pollFirst = Operator.wrapJavaMethod(java.util.TreeSet.class, "pollFirst", "set-tree-poll-first", NAMESPACE);
	
	@VelkaOperator
	@Description("Retrieves and removes the last (highest) element, or returns null if this set is empty.")
	public static Operator pollLast = Operator.wrapJavaMethod(java.util.TreeSet.class, "pollLast", "set-tree-poll-last", NAMESPACE);
	
	@VelkaOperator
	@Description("Removes the specified element from this set if it is present.")
	public static Operator remove = Operator.wrapJavaMethod(java.util.TreeSet.class, "remove", "set-tree-remove", NAMESPACE, Object.class);
	
	@VelkaOperator
	@Description("Returns the number of elements in this set (its cardinality).")
	public static Operator size = Operator.wrapJavaMethod(java.util.TreeSet.class, "size", "set-tree-size", NAMESPACE);
	
	@VelkaOperator
	@Description("Returns true if this collection contains all of the elements in the specified collection. ")
	public static Operator containsAll = Operator.wrapJavaMethod(java.util.TreeSet.class, "containsAll", "set-tree-contains-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	@Description("Retains only the elements in this collection that are contained in the specified collection (optional operation). In other words, removes from this collection all of its elements that are not contained in the specified collection. ")
	public static Operator retainAll = Operator.wrapJavaMethod(java.util.TreeSet.class, "retainAll", "set-tree-retain-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	@Description("Removes from this set all of its elements that are contained in the specified collection (optional operation). If the specified collection is also a set, this operation effectively modifies this set so that its value is the asymmetric set difference of the two sets.")
	public static Operator removeAll = Operator.wrapJavaMethod(java.util.TreeSet.class, "removeAll", "set-tree-remove-all", NAMESPACE, Collection.class);
	
	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return PATH;
	}

	@Override
	public Path getFileName() {
		return FILE;
	}

	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
	}
	
	private static TreeSet singleton = null;
	
	public static TreeSet instance() {
		if(singleton == null) {
			singleton = new TreeSet();
		}
		return singleton;
	}

}
