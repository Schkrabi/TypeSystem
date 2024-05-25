package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.ExceptionExpr;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.ClojureHelper.ProxyImpl;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains utilities to work with tree maps in Velka
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("Operators for working with java.util.TreeMap.") 
@Header("Tree Map")
public class TreeMap extends OperatorBank{
	/**
	 * Clojure namespace for TreeMap
	 */
	public static final String NAMESPACE = "velka.clojure.treeMap";
	
	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	
	@VelkaConstructor
	@Description("Constructs Map:Tree.") 
	@Name("Constructs a new, empty tree map, ordered according to the given comparator function.") 
	@Syntax("(construct Map Tree <comparator function>)")
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
								java.util.TreeMap.class,
								ClojureHelper.proxy(
										java.util.Comparator.class,
										Arrays.asList(),
										ProxyImpl.of(
												java.util.TreeMap.class.getDeclaredMethod("compare", Object.class, Object.class),
												Arrays.asList(a1, a2), 
													ClojureHelper.applyVelkaFunction(
																cmpFun, a1, a2)))));
			} catch (NoSuchMethodException e) {
				throw new AppendableException(e.toString());
			} catch (SecurityException e) {
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
			
			java.util.Comparator<Expression> comparator = new java.util.Comparator<Expression>() {

				@Override
				public int compare(Expression o1, Expression o2) {
					Expression cmpEval = new AbstractionApplication(
												cmpFun,
												new Tuple(o1, o2));
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
			
			java.util.TreeMap<Expression, Expression> map = new java.util.TreeMap<Expression, Expression>(comparator);
			return new LitInteropObject(map, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(
							new TypeArrow(new TypeTuple(A, A), TypeAtom.TypeIntNative)),
					TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Map Tree";
		}		
	};
	
	private static final Symbol ceilingEntrySymbol = new Symbol("ceiling-entry", NAMESPACE);
	public static final Symbol ceilingEntrySymbol_out = new Symbol("map-tree-ceiling-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key greater than or equal to the given key, or throws error if no such mapping exists.") 
	@Example("(map-tree-ceiling-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-ceiling-entry <map>)")
	public static final Operator ceilingEntry = new Operator() {

		private final String ERROR = "No key-value mapping found!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".ceilingEntry",
											map,
											key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return ceilingEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Map.Entry<Expression, Expression> e = map.ceilingEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return ceilingEntrySymbol_out.toString();
		}
		
	};
	
	private static final Symbol ceilingKeySymbol = new Symbol("ceiling-key", NAMESPACE);
	public static final Symbol ceilingKeySymbol_out = new Symbol("map-tree-ceiling-key");
	
	@VelkaOperator
	@Description("Returns the least key greater than or equal to the given key, or null if there is no such key.") 
	@Example("(map-tree-ceiling-key (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-ceiling-entry <map>)")
	public static final Operator ceilingKey = new Operator() {

		private final String ERROR = "No ceiling key exist!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String ceilKey = "_ceilKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", ceilKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ceilKey),
							Pair.of(ceilKey,
									ClojureHelper.applyClojureFunction(".ceilingKey",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return ceilingKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Expression e = map.ceilingKey(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return ceilingKeySymbol_out.toString();
		}
		
	};
	
	private static final Symbol containsKeySymbol = new Symbol("contains-key", NAMESPACE);
	public static final Symbol containsKeySymbol_out = new Symbol("map-tree-contains-key");
	
	@VelkaOperator
	@Description("Returns true if this map contains a mapping for the specified key.") 
	@Example("(map-tree-contains-key (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-contains-key <map> <key>)")
	public static final Operator containsKey = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".containsKey",
									map,
									key)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			if(map.containsKey(key)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return containsKeySymbol_out.toString();
		}
		
	};
	
	private static final Symbol containsValueSymbol = new Symbol("contains-value", NAMESPACE);
	public static final Symbol containsValueSymbol_out = new Symbol("map-tree-contains-value");
	
	@VelkaOperator
	@Description("Returns true if this map maps one or more keys to the specified value.") 
	@Example("(map-tree-contains-value (construct Map Tree (lambda (x y) -1)) \"foo\")") 
	@Syntax("(map-tree-contains-value <map> <value>)")
	public static final Operator containsValue = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String value = "_value";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, value),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".containsValue",
									map,
									value)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsValueSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression value = args.get(1);
			
			if(map.containsValue(value)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, V), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return containsValueSymbol_out.toString();
		}
		
	};
	
	private static final Symbol firstEntrySymbol = new Symbol("first-entry", NAMESPACE);
	public static final Symbol firstEntrySymbol_out = new Symbol("map-tree-first-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-first-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-first-entry <map>)")
	public static final Operator firstEntry = new Operator() {

		private final String ERROR = "map-tree-first-entry cannot retrieve first entry!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".firstEntry",
											map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return firstEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Map.Entry<Expression, Expression> e = map.firstEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return firstEntrySymbol_out.toString();
		}
	};
	
	private static final Symbol firstKeySymbol = new Symbol("first-key", NAMESPACE);
	public static final Symbol firstKeySymbol_out = new Symbol("map-tree-first-key");
	
	@VelkaOperator
	@Description("Returns the first (lowest) key currently in this map. Throws error if no such key exists.") 
	@Example("(map-tree-first-key (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-first-key <map>)")
	public static final Operator firstKey = new Operator() {

		private final String ERROR = "map-tree-first-key cannot retrieve first key!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String firstKey = "_firstKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", firstKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									firstKey),
							Pair.of(firstKey,
									ClojureHelper.applyClojureFunction(".firstKey",
									map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return firstKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = null;
			try {
				key = map.firstKey();
			} catch(NoSuchElementException e) {
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return key;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return firstKeySymbol_out.toString();
		}
		
	};
	
	private static final Symbol floorEntrySymbol = new Symbol("floor-entry", NAMESPACE);
	public static final Symbol floorEntrySymbol_out = new Symbol("map-tree-floor-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key less than or equal to the given key, or throws error if there is no such key.") 
	@Example("(map-tree-floor-entry (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-floor-entry <map> <key>)")
	public static final Operator floorEntry = new Operator() {

		private final String ERROR = "No key-value mapping found!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".floorEntry",
											map,
											key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return floorEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Map.Entry<Expression, Expression> e = map.floorEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return floorEntrySymbol_out.toString();
		}
		
	};
	
	private static final Symbol floorKeySymbol = new Symbol("floor-key", NAMESPACE);
	public static final Symbol floorKeySymbol_out = new Symbol("map-tree-floor-key");
	
	@VelkaOperator
	@Description("Returns the greatest key less than or equal to the given key, or throws error if there is no such key.") 
	@Example("(map-tree-floor-key (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-floor-key <map> <key>)")
	public static final Operator floorKey = new Operator() {

		private final String ERROR = "No ceiling key exist!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String floorKey = "_floorKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", floorKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									floorKey),
							Pair.of(floorKey,
									ClojureHelper.applyClojureFunction(".floorKey",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return floorKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Expression e = map.floorKey(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return floorKeySymbol_out.toString();
		}
	};
	
	private static final Symbol getSymbol = new Symbol("velka-get", NAMESPACE);
	public static final Symbol getSymbol_out = new Symbol("map-tree-get");
	
	@VelkaOperator
	@Description("Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.") 
	@Example("(map-tree-get (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-get <map> <key>)")
	public static final Operator get = new Operator() {

		private final String ERROR = "Key not found in map";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String value = "_value";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", value),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									value),
							Pair.of(value, ClojureHelper.applyClojureFunction(
									".get",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			Expression value = map.get(key);
			
			if(value == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return value;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), V);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return getSymbol_out.toString();
		}
		
	};
	
	private static final Symbol headMapSymbol = new Symbol("head-map", NAMESPACE);
	public static final Symbol headMapSymbol_out = new Symbol("map-tree-head");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive is true) to-key.") 
	@Example("(map-tree-head (construct Map Tree (lambda (x y) -1)) 1 \"foo\")") 
	@Syntax("(map-tree-head <map> <to-key> <value>)")
	public static final Operator headMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String toKey = "_to-key";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, toKey),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".headMap",
											map,
											toKey)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return headMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression toKey = args.get(1);
			
			java.util.TreeMap<Expression, Expression> res = new java.util.TreeMap<Expression, Expression>(map.headMap(toKey));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return headMapSymbol_out.toString();
		}
		
	};
	
	private static final Symbol headMapInclSymbol = new Symbol("head-map-incl", NAMESPACE);
	public static final Symbol headMapInclSymbol_out = new Symbol("map-tree-head-incl");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive? is true) to-key.") 
	@Example("(map-tree-head-incl (construct Map Tree (lambda (x y) -1)) 1 #f)") 
	@Syntax("(map-tree-head-incl <map> <to-key> <inclusive?>)")
	public static final Operator headMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String toKey = "_to-key";
			String inclusive = "_inclusive";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, toKey, inclusive),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".headMap",
											map,
											toKey,
											inclusive)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return headMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression toKey = args.get(1);
			LitBoolean inclusive = (LitBoolean)args.get(2);
			
			java.util.TreeMap<Expression, Expression> res = new java.util.TreeMap<Expression, Expression>(
					map.headMap(toKey, inclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return headMapInclSymbol_out.toString();
		}
	};
	
	private static final Symbol higherEntrySymbol = new Symbol("higher-entry", NAMESPACE);
	public static final Symbol higherEntrySymbol_out = new Symbol("map-tree-higher-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key strictly greater than the given key, or null if there is no such key.") 
	@Example("(map-tree-higher-entry (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-higher-entry <map> <key>)")
	public static final Operator higherEntry = new Operator() {

		private static final String ERROR = "map-tree-higher-entry cannot retrieve the entry.";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".higherEntry",
											map,
											key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return higherEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Map.Entry<Expression, Expression> e = map.higherEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return higherEntrySymbol_out.toString();
		}
	};
	
	private static final Symbol higherKeySymbol = new Symbol("higher-key", NAMESPACE);
	public static final Symbol higherKeySymbol_out = new Symbol("map-tree-higher-key");
	
	@VelkaOperator
	@Description("Returns the least key strictly greater than the given key, or throws error if there is no such key.") 
	@Example("(map-tree-higher-key (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-higher-key <map> <key>)")
	public static final Operator higherKey = new Operator() {

		private final String ERROR = "No higher key exist!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String retKey = "_retKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", retKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									retKey),
							Pair.of(retKey,
									ClojureHelper.applyClojureFunction(".higherKey",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return higherKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Expression e = map.higherKey(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return higherKeySymbol_out.toString();
		}
	};
	
	private static final Symbol keysSymbol = new Symbol("velka-keys", NAMESPACE);
	public static final Symbol keysSymbol_out = new Symbol("map-tree-keys");
	
	@VelkaOperator
	@Description("Returns a Set view of the keys contained in this map.") 
	@Example("(map-tree-keys (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-keys <map>)")
	public static final Operator keys = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					LitComposite
					.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.applyClojureFunction(
											".keySet",
											map)),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return keysSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			return ListNative.makeListNativeExpression(new LinkedList<Expression>(map.keySet()));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return keysSymbol_out.toString();
		}
		
	};
	
	private static final Symbol lastEntrySymbol = new Symbol("last-entry", NAMESPACE);
	public static final Symbol lastEntrySymbol_out = new Symbol("map-tree-last-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-last-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-last-entry <map>)")
	public static final Operator lastEntry = new Operator() {

		private final String ERROR = "map-tree-last-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".lastEntry",
											map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lastEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Map.Entry<Expression, Expression> e = map.lastEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lastEntrySymbol_out.toString();
		}
	};
	
	private static final Symbol lastKeySymbol = new Symbol("last-key", NAMESPACE);
	public static final Symbol lastKeySymbol_out = new Symbol("map-tree-last-key");
	
	@VelkaOperator
	@Description("Returns the last (highest) key currently in this map.") 
	@Example("(map-tree-last-key (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-last-key <map>)")
	public static final Operator lastKey = new Operator() {

		private final String ERROR = "map-tree-first-key cannot retrieve first key!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String firstKey = "_firstKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", firstKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									firstKey),
							Pair.of(firstKey,
									ClojureHelper.applyClojureFunction(".lastKey",
									map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lastKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = null;
			try {
				key = map.lastKey();
			} catch(NoSuchElementException e) {
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return key;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lastKeySymbol_out.toString();
		}
	};
	
	private static final Symbol lowerEntrySymbol = new Symbol("lower-entry", NAMESPACE);
	public static final Symbol lowerEntrySymbol_out = new Symbol("map-tree-lower-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key strictly less than the given key, or throws error if there is no such key.") 
	@Example("(map-tree-lower-entry (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-lower-entry <map> <key>)")
	public static final Operator lowerEntry = new Operator() {

		private static final String ERROR = "map-tree-lower-entry cannot retrieve the entry.";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".lowerEntry",
											map,
											key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lowerEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Map.Entry<Expression, Expression> e = map.lowerEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lowerEntrySymbol_out.toString();
		}
	};
	
	private static final Symbol lowerKeySymbol = new Symbol("lower-key", NAMESPACE);
	public static final Symbol lowerKeySymbol_out = new Symbol("map-tree-lower-key");
	
	@VelkaOperator
	@Description("Returns the greatest key strictly less than the given key, or throws an error if there is no such key.") 
	@Example("(map-tree-lower-key (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-lower-key <map> <key>)")
	public static final Operator lowerKey = new Operator() {

		private static final String ERROR = "map-tree-lower-key cannot retrieve the key.";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String retKey = "_retKey";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", retKey),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									retKey),
							Pair.of(retKey,
									ClojureHelper.applyClojureFunction(".lowerKey",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lowerKeySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Expression e = map.lowerKey(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), K);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lowerKeySymbol_out.toString();
		}
	};
	
	private static final Symbol pollFirstEntrySymbol = new Symbol("poll-first-entry", NAMESPACE);
	public static final Symbol pollFirstEntrySymbol_out = new Symbol("map-tree-poll-first-entry");
	
	@VelkaOperator
	@Description("Removes and returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-poll-first-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-poll-first-entry <map>)")
	public static final Operator pollFirstEntry = new Operator() {

		private final String ERROR = "map-tree-poll-first-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".pollFirstEntry",
											map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return pollFirstEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Map.Entry<Expression, Expression> e = map.pollFirstEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return pollFirstEntrySymbol_out.toString();
		}
		
	};
	
	private static final Symbol pollLastEntrySymbol = new Symbol("poll-last-entry", NAMESPACE);
	public static final Symbol pollLastEntrySymbol_out = new Symbol("map-tree-poll-last-entry");
	
	@VelkaOperator
	@Description("Removes and returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-poll-last-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-poll-last-entry <map>)")
	public static final Operator pollLastEntry = new Operator() {

		private final String ERROR = "map-tree-poll-last-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String entry = "_entry";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", entry),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									ClojureHelper.tupleHelper(
											ClojureHelper.applyClojureFunction(
													".getKey",
													entry),
											ClojureHelper.applyClojureFunction(
													".getValue",
													entry))),
							Pair.of(
									entry,
									ClojureHelper.applyClojureFunction(
											".pollLastEntry",
											map))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return pollLastEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Map.Entry<Expression, Expression> e = map.pollLastEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return new Tuple(e.getKey(), e.getValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return pollLastEntrySymbol_out.toString();
		}
		
	};
	
	private static final Symbol putSymbol = new Symbol("put", NAMESPACE);
	public static final Symbol putSymbol_out = new Symbol("map-tree-put");
	
	@VelkaOperator
	@Description("Associates the specified value with the specified key in the map.") 
	@Example("(map-tree-put (construct Map Tree (lambda (x y) -1)) 1 \"foo\")") 
	@Syntax("(map-tree-put <map> <key> <value>)")
	public static final Operator put = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String value = "_value";
			String tmp = "_tmp";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key, value),
					ClojureHelper.letHelper(
							map,
							Pair.of(tmp,
									ClojureHelper.applyClojureFunction(
											".put",
											map,
											key,
											value))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return putSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			Expression value = args.get(2);
			
			map.put(key, value);
			
			return lji;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, V), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return putSymbol_out.toString();
		}
		
	};
	
	private static final Symbol putAllSymbol = new Symbol("put-all", NAMESPACE);
	public static final Symbol putAllSymbol_out = new Symbol("map-tree-put-all");
	
	@VelkaOperator
	@Description("Copies all of the mappings from the specified map to this map.") 
	@Example("(map-tree-put-all (construct Map Tree (lambda (x y) -1)) (map-tree-put (construct Map Tree (lambda (x y) -1) 1 \"foo\")))") 
	@Syntax("(map-tree-put-all <receiving-map> <source-map>)")
	public static final Operator putAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String sourceMap = "_source-map";
			String tmp = "_tmp";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, sourceMap),
					ClojureHelper.letHelper(
							map,
							Pair.of(tmp, ClojureHelper.applyClojureFunction(
									".putAll",
									map,
									sourceMap))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return putAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			LitInteropObject lji2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> sourceMap = (java.util.TreeMap<Expression, Expression>)lji2.javaObject;
						
			map.putAll(sourceMap);
			
			return lji;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, TypeAtom.TypeMapTree), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return putAllSymbol_out.toString();
		}
	};
	
	private static final Symbol removeSymbol = new Symbol("velka-remove", NAMESPACE);
	public static final Symbol removeSymbol_out = new Symbol("map-tree-remove");
	
	@VelkaOperator
	@Description("Removes the mapping for this key from this TreeMap if present.") 
	@Example("(map-tree-remove (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-remove <map> <key>)")
	public static final Operator remove = new Operator() {

		private final String ERROR = "map-tree-remove removed key does not exist!";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String value = "_value";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
					ClojureHelper.letHelper(
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("nil?", value),
									ClojureHelper.errorHelper(ClojureHelper.stringHelper(ERROR)),
									value),
							Pair.of(value,
									ClojureHelper.applyClojureFunction(".remove",
									map,
									key))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			Expression val = map.remove(key);
			
			if(val == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env, typeEnv);
			}
			
			return val;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), V);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return removeSymbol_out.toString();
		}
	};
	
	private static final Symbol sizeSymbol = new Symbol("velka-size", NAMESPACE);
	public static final Symbol sizeSymbol_out = new Symbol("map-tree-size");
	
	@VelkaOperator
	@Description("Returns the number of key-value mappings in this map.") 
	@Example("(map-tree-size (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-size <map>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".size",
									map)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			int size = map.size();
			
			return new LitInteger((long)size);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return sizeSymbol_out.toString();
		}
		
	};
	
	private static final Symbol subMapInclSymbol = new Symbol("sub-map-incl", NAMESPACE);
	public static final Symbol subMapInclSymbol_out = new Symbol("map-tree-sub-map-inclusive");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys range from fromKey to toKey.") 
	@Example("(map-tree-sub-map-inclusive (construct Map Tree (lambda (x y) -1)) 1 #t 3 #f)") 
	@Syntax("(map-tree-sub-map-inclusive <map> <from-key> <from-inclusive> <to-key> <to-inclusive>)")
	public static final Operator subMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String fromKey = "_from-key";
			String fromInclusive = "_from-inclusive";
			String toKey = "_to-key";
			String toInclusive = "_to-inclusive";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, fromKey, fromInclusive, toKey, toInclusive),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".subMap",
											map,
											fromKey, 
											fromInclusive,
											toKey,
											toInclusive)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return subMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression fromKey = args.get(1);
			LitBoolean fromInclusive = (LitBoolean)args.get(2);
			Expression toKey = args.get(3);
			LitBoolean toInclusive = (LitBoolean)args.get(4);
			
			java.util.TreeMap<Expression, Expression> res = 
					new java.util.TreeMap<Expression, Expression>(
							map.subMap(
									fromKey, 
									fromInclusive == LitBoolean.TRUE,
									toKey,
									toInclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return subMapInclSymbol_out.toString();
		}
		
	};
	
	private static final Symbol subMapSymbol = new Symbol("velka-sub-map", NAMESPACE);
	public static final Symbol subMapSymbol_out = new Symbol("map-tree-sub-map");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys range from fromKey, inclusive, to toKey, exclusive.") 
	@Example("(map-tree-sub-map (construct Map Tree (lambda (x y) -1)) 1 3)") 
	@Syntax("(map-tree-sub-map <map> <from-key> <to-key>)")
	public static final Operator subMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String fromKey = "_from-key";
			String toKey = "_to-key";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, fromKey, toKey),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".subMap",
											map,
											fromKey,
											toKey)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return subMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression fromKey = args.get(1);
			Expression toKey = args.get(2);
			
			java.util.TreeMap<Expression, Expression> res = 
					new java.util.TreeMap<Expression, Expression>(
							map.subMap(
									fromKey, 
									toKey));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return subMapSymbol_out.toString();
		}
		
	};
	
	private static final Symbol tailMapSymbol = new Symbol("tail-map", NAMESPACE);
	public static final Symbol tailMapSymbol_out = new Symbol("map-tree-tail-map");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are greater than or equal to from-key.") 
	@Example("(map-tree-tail-map (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-tail-map <map> <from-key>)")
	public static final Operator tailMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".tailMap",
											map,
											key)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return tailMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			
			java.util.TreeMap<Expression, Expression> res = new java.util.TreeMap<Expression, Expression>(map.tailMap(key));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return tailMapSymbol_out.toString();
		}
	};
	
	private static final Symbol tailMapInclSymbol = new Symbol("tail-map-incl", NAMESPACE);
	public static final Symbol tailMapInclSymbol_out = new Symbol("map-tree-tail-map-incl");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are greater than (or equal to, if inclusive is true) fromKey.") 
	@Example("(map-tree-tail-map-incl (construct Map Tree (lambda (x y) -1)) 1 #t)") 
	@Syntax("(map-tree-tail-map-incl <map> <from-key> <inclusive?>)")
	public static final Operator tailMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String key = "_key";
			String inclusive = "_inclusive";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map, key, inclusive),
							ClojureHelper.constructJavaClass(
									java.util.TreeMap.class,
									ClojureHelper.applyClojureFunction(
											".headMap",
											map,
											key,
											inclusive)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return tailMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			Expression key = args.get(1);
			LitBoolean inclusive = (LitBoolean)args.get(2);
			
			java.util.TreeMap<Expression, Expression> res = new java.util.TreeMap<Expression, Expression>(
					map.tailMap(key, inclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return tailMapInclSymbol_out.toString();
		}
		
	};
	
	private static final Symbol valuesSymbol = new Symbol("velka-values", NAMESPACE);
	public static final Symbol valuesSymbol_out = new Symbol("map-tree-values");
	
	@VelkaOperator
	@Description("Returns a Collection view of the values contained in this map.") 
	@Example("(map-tree-values (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-values <map>)")
	public static final Operator values = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String map = "_map";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(map),
					LitComposite
					.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.applyClojureFunction(
											".values",
											map)),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return valuesSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Expression, Expression> map = (java.util.TreeMap<Expression, Expression>)lji.javaObject;
			
			return ListNative.makeListNativeExpression(new LinkedList<Expression>(map.values()));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return valuesSymbol_out.toString();
		}
		
	};
	
	public static final Path PATH = Paths.get("velka", "clojure");
	public static final Path FILE = Paths.get("treeMap.clj");
	
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
		typeEnv.addRepresentation(TypeAtom.TypeMapTree);
	}
	
	private TreeMap() {}
	private static TreeMap me = null;
	
	public static TreeMap singleton() {
		if(me == null) {
			me = new TreeMap();
		}
		return me;
	}

	
}
