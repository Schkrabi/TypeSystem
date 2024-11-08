package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

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
import velka.util.Functions;
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
	@Description("Returns true if this set contains no elements.")
	public static Operator isEmpty = Operator.wrapJavaMethod(java.util.TreeSet.class, "isEmpty", "set-tree-is-empty", NAMESPACE);
	
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
	
	public static final Symbol mapSymbol = new Symbol("velka-map", NAMESPACE);
	public static final Symbol mapSymbol_out = new Symbol("set-tree-map");
	
	@VelkaOperator
	@Description("Map int function")
	public static final Operator map = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var fun = "_fun";
			var set = "_set";
			var x = "_x";
			var ret = "_ret";
			var code = ClojureHelper.fnHelper(List.of(set, fun),
						ClojureHelper.letHelper(
								ret,
								Pair.of(ret, ClojureHelper.constructJavaClass(java.util.TreeSet.class, 
												ClojureHelper.applyClojureFunction(".comparator", set))),
								Pair.of("tmp", ClojureHelper.applyClojureFunction(".addAll", 
										ret,
										ClojureHelper.applyClojureFunction("map", 
												ClojureHelper.fnHelper(List.of(x), ClojureHelper.applyVelkaFunction(fun, x)),
												set)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return mapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var fun = args.get(1);
			@SuppressWarnings("unchecked")
			var tSet = (java.util.TreeSet<Object>)set.javaObject;
			
			var rSet = new java.util.TreeSet<Object>(tSet.comparator());
			
			tSet.stream().forEach(x -> {
				var ex = Literal.objectToLiteral(x);
				var app = new AbstractionApplication(fun, new Tuple(ex));
				try {
					var exp = app.interpret(env);
					var o = Literal.literalToObject(exp);
					rSet.add(o); 
				}catch(Exception e) {
					throw new RuntimeException(e);
				}
			});
			
			return new LitInteropObject(rSet, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var A = new TypeVariable(NameGenerator.next());
			var B = new TypeVariable(NameGenerator.next());
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree, new TypeArrow(new TypeTuple(A), B)),
					TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return mapSymbol_out.toString();
		}
	};
	
	@VelkaOperator
	public static final Operator toList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var code = ClojureHelper.fnHelper(List.of(set),
							ClojureHelper.applyClojureFunction("seq", set));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("to-list", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var tSet = (java.util.TreeSet<Object>)set.javaObject;
			
			var l = new ArrayList<Expression>();
			tSet.stream().forEach(o -> l.add(Literal.objectToLiteral(o)));
			
			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-tree-to-list";
		}
		
	};
	
	@VelkaConversion
	@Description("Converts Set:BitSet into Set:Tree.") 
	@Example("(convert Set:BitSet Set:Tree (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))") 
	@Syntax("(convert Set:BitSet Set:Tree <arg>)")
	public static Conversion treeSetToBitSet = new Conversion() {
		
		Double costX1 = 0d;
		Double costY1 = 0.8d;
		Double costX2 = 1000d;
		Double costY2 = 0.5d;

		@Override
		public Expression cost() {
			final var f = Functions.linearFunctionFromPoints(costX1, costY1, costX2, costY2);
			var l = new Operator() {

				@Override
				protected String toClojureOperator(Environment env) throws AppendableException {
					var arg = "_arg";
					var code = ClojureHelper.fnHelper(List.of(arg),
							ClojureHelper.applyClojureFunction("min", costY1.toString(),
									ClojureHelper.applyClojureFunction("max", costY2.toString(),
											ClojureHelper.applyClojureFunction(".apply",
													ClojureHelper.applyClojureFunction(
															"velka.util.Functions/linearFunctionFromPoints",
															costX1.toString(), costY1.toString(), costX2.toString(),
															costY2.toString()),
													ClojureHelper.applyClojureFunction("double", ClojureHelper.applyClojureFunction(".size", arg))))));
					return code;
				}

				@Override
				public Symbol getClojureSymbol() {
					return new Symbol(NameGenerator.next());
				}

				@Override
				protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
					var lio = (LitInteropObject)args.get(0);
					@SuppressWarnings("unchecked")
					var set = (java.util.TreeSet<Object>)lio.javaObject;
					
					var cost = Math.min(0.8d, Math.max(0.5d, f.apply((double)set.size())));
					
					return new LitDouble(cost);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree), TypeAtom.TypeDoubleNative);
					return Pair.of(type, Substitution.EMPTY);
				}
				
			};
			
			return l;
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
				if(e instanceof Integer i) {
					bitSet.set(i);
					return;
				}
				if(e instanceof LitInteger li) {
					bitSet.set((int)li.value);
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
					ClojureHelper.letHelper(tmp, 
							Pair.of(tmp, ClojureHelper.constructJavaClass(java.util.TreeSet.class, s1)),
							Pair.of("_aux", ClojureHelper.applyClojureFunction(".retainAll", tmp, s2))));
			
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
			
			return new LitInteropObject(tmp, TypeAtom.TypeSetTree);
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
	
	@VelkaOperator
	public static final Operator fromList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var lst = "_lst";
			var cmpFun = "_cmp-fun";
			var set = "_set";
			var a1 = "a1";
			var a2 = "a2";
			String code;
			try {
				code = ClojureHelper.fnHelper(List.of(lst, cmpFun),
							ClojureHelper.letHelper(set, 
									Pair.of(set, ClojureHelper.constructJavaClass(
											java.util.TreeSet.class,
											ClojureHelper.proxy(
													java.util.Comparator.class,
													Arrays.asList(),
													ProxyImpl.of(
															java.util.Comparator.class.getDeclaredMethod("compare", Object.class, Object.class),
															Arrays.asList(a1, a2), 
																ClojureHelper.applyVelkaFunction(
																			cmpFun, a1, a2))))),
									Pair.of("tmp", ClojureHelper.applyClojureFunction(".addAll", set, lst))));
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}		
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("from-list", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lst = (LitInteropObject)args.get(0);
			var cmp = args.get(1);
			@SuppressWarnings("unchecked")
			var l = ((java.util.List<Expression>) lst.javaObject).stream()
					.map(e -> Literal.literalToObject(e))
					.collect(Collectors.toList());
			
			var set = new java.util.TreeSet<Object>(
					new java.util.Comparator<Object>() {

						@Override
						public int compare(Object o1, Object o2) {
							try {
								var arg1 = Literal.objectToLiteral(o1);
								var arg2 = Literal.objectToLiteral(o2);
								var appl = new velka.core.application.AbstractionApplication(cmp,
										new Tuple(arg1, arg2));

								var ret = appl.interpret(env);

								if (ret instanceof LitInteger li) {
									return (int) li.value;
								}
								throw new RuntimeException("Invalid result of comparator " + ret);

							} catch (AppendableException ae) {
								throw new RuntimeException(ae);
							}
						}
					});
			
			set.addAll(l);
			
			return new LitInteropObject(set, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var A = new TypeVariable(NameGenerator.next());
			var type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(new TypeTuple(A, A), TypeAtom.TypeIntNative)),
					TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-tree-from-list";
		}
	};
	
	@VelkaConversion
	public static Conversion toHashSet = new Conversion() {

		@Override
		public Expression cost() {
			var hashSet = new Symbol(NameGenerator.next());
			
			var cost = new Lambda(
					new Tuple(hashSet), 
					new TypeTuple(TypeAtom.TypeSetHash),
					new AbstractionApplication(
							new AbstractionApplication(
									Operators.linFunPoints, 
									new Tuple(new LitDouble(0d), new LitDouble(0.8d), new LitDouble(1000d), new LitDouble(0.5d))), 
							new Tuple(
									new AbstractionApplication(Operators.IntToDouble,
											new Tuple(new AbstractionApplication(HashSet.size, new Tuple(hashSet)))))));
			return cost;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var ts = "_tree-set";
			var code = ClojureHelper.fnHelper(
					List.of(ts),
					ClojureHelper.constructJavaClass(java.util.HashSet.class, ts));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("treeset-2-hashset");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var ts = (java.util.TreeSet<Object>)lio.javaObject;
			
			var hs = new java.util.HashSet<Object>(ts);
			
			return new LitInteropObject(hs, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetTree), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "treeset-2-hashset";
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
