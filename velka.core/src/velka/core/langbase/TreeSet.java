package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
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
import velka.util.annotations.VelkaConversion;
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
	public static final Path FILE = Paths.get("treeSet.clj");
	
	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	
	@VelkaConstructor
	@Description("Constructs Set:Tree.") 
	@Name("Constructs a new, empty tree set, ordered according to the given comparator function.") 
	@Syntax("(construct Set Tree <comparator function>)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
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
						cmp = cmpEval.interpret(env);
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
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
	
	@VelkaConstructor
	@Description("Copies Set:Tree.") 
	@Name("Constructs a new, tree set copying an existing tree set.") 
	@Syntax("(construct Set Tree <comparator function>)")
	public static Constructor copyConstructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var arg = "_arg";
			var code = ClojureHelper.fnHelper(List.of(arg),
					ClojureHelper.applyClojureFunction("java.util.TreeSet.", 
							arg));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-construct-copy", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var ts = (java.util.TreeSet<Object>)lio.javaObject;
			
			var cts = new java.util.TreeSet<Object>(ts);
			
			return new LitInteropObject(cts, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
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
	
	@VelkaConversion
	@Description("Converts Set:BitSet into Set:Tree.") 
	@Example("(convert Set:BitSet Set:Tree (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))") 
	@Syntax("(convert Set:BitSet Set:Tree <arg>)")
	public static Conversion treeSetToBitSet = new Conversion() {

		@Override
		public Expression cost() {
			//TODO fine tune!
			return Lambda.constFun(1, new LitDouble(0.8d));
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final String arg = "_arg", bitSet = "_bs", tmp = "_tmp", x = "_x";
			var code = ClojureHelper.fnHelper(List.of(arg),
					ClojureHelper.letHelper(bitSet, 
							Pair.of(bitSet, ClojureHelper.constructJavaClass(java.util.BitSet.class)),
							Pair.of(tmp, ClojureHelper.applyClojureFunction("doall", 
									ClojureHelper.applyClojureFunction("map",
											ClojureHelper.fnHelper(List.of(x), ClojureHelper.applyClojureFunction(".set", bitSet, x)),
											arg)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("tree-set-2-bit-set", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var treeSet = (java.util.TreeSet<Object>)lio.javaObject;
			var bitSet = new BitSet();
			
			treeSet.stream().forEach(e -> {
				if(e instanceof Long l) {
					bitSet.set(l.intValue());
					return;
				}
				throw new RuntimeException("Can only convert integer sets to bit sets");
			});
			
			return new LitInteropObject(bitSet, TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
	};
	
	@VelkaOperator
	public static final Operator intersect = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final var s1 = "_s1";
			final var s2 = "_s2";
			final var tmp = "_tmp";
			
			var code = ClojureHelper.fnHelper(List.of(s1, s2),
					ClojureHelper.letHelper(s1, 
							Pair.of(tmp, ClojureHelper.constructJavaClass(java.util.TreeSet.class, s1)),
							Pair.of("_aux", ClojureHelper.applyClojureFunction(".retainAll", tmp, s2)),
							Pair.of("_aux", ClojureHelper.applyClojureFunction(".retainAll", s1, tmp))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-intersect", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var ls1 = (LitInteropObject)args.get(0);
			var ls2 = (LitInteropObject)args.get(1);
			
			@SuppressWarnings("unchecked")
			var s1 = (java.util.TreeSet<Object>)ls1.javaObject;
			@SuppressWarnings("unchecked")
			var s2 = (java.util.TreeSet<Object>)ls2.javaObject;
			
			var tmp = new java.util.TreeSet<Object>(s1);
			tmp.retainAll(s2);
			s1.retainAll(tmp);
			
			return new LitInteropObject(s1, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree, TypeAtom.TypeSetTree), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-tree-intersect";
		}
	};
	
	@VelkaOperator
	public static final Operator union = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final var s1 = "_s1";
			final var s2 = "_s2";
			
			var code = ClojureHelper.fnHelper(List.of(s1, s2),
					ClojureHelper.letHelper(s1, 
							Pair.of("_aux", ClojureHelper.applyClojureFunction(".addAll", s1, s2))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-union", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var ls1 = (LitInteropObject)args.get(0);
			var ls2 = (LitInteropObject)args.get(1);
			
			@SuppressWarnings("unchecked")
			var s1 = (java.util.TreeSet<Object>)ls1.javaObject;
			@SuppressWarnings("unchecked")
			var s2 = (java.util.TreeSet<Object>)ls2.javaObject;
			
			s1.addAll(s2);
			return new LitInteropObject(s1, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree, TypeAtom.TypeSetTree), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-tree-union";
		}
		
	};
	
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
	
	private static TreeSet singleton = null;
	
	public static TreeSet instance() {
		if(singleton == null) {
			singleton = new TreeSet();
		}
		return singleton;
	}

}
