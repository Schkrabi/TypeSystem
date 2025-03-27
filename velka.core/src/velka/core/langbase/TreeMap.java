package velka.core.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JStatement;
import com.sun.codemodel.JVar;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.ExceptionExpr;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaThrower;
import velka.java.runtime.VelkaTuple;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;
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
	
	public static final Symbol constructorSymbol = new Symbol("velka_construct", TreeMap.singleton().getNamespace());
	
	@VelkaConstructor
	@Description("Constructs Map:Tree.") 
	@Name("Constructs a new, empty tree map, ordered according to the given comparator function.") 
	@Syntax("(construct Map Tree <comparator function>)")
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
		public Symbol getInternalSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			final Expression cmpFun = args.get(0);
			
			java.util.Comparator<Object> comparator = new java.util.Comparator<Object>() {

				@Override
				public int compare(Object o1, Object o2) {
					Expression e1, e2;
					if(o1 instanceof Expression exp) {
						e1 = exp; 
					}
					else {
						e1 = Literal.objectToLiteral(o1);
					}
					
					if(o2 instanceof Expression exp) {
						e2 = exp;
					}
					else {
						e2 = Literal.objectToLiteral(o2);
					}
					
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
			
			java.util.TreeMap<Expression, Expression> map = new java.util.TreeMap<Expression, Expression>(comparator);
			return new LitInteropObject(map, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var cmpCl = CodeModelInstance.instance().anonymousClass(java.util.Comparator.class);
			
			var cmpM = cmpCl.method(JMod.PUBLIC, int.class, "compare");			
			var o1 = cmpM.param(Object.class, "_o1");
			var o2 = cmpM.param(Object.class, "o2");
			
			var vtCl = CodeModelInstance.instance().ref(VelkaTuple.class);
			
			var tuple = cmpM.body().decl(vtCl, "_tuple", 
					JExpr._new(vtCl)
						.arg(CodeModelInstance.instance().ref(java.util.List.class).staticInvoke("of").arg(o1).arg(o2))
						.arg(JExpr._new(TypeUtil.instance().typeTupleJType())
								.arg(JavaTypeSystem.codeInstance().invoke("getType").arg(o1))
								.arg(JavaTypeSystem.codeInstance().invoke("getType").arg(o2))));
			
			var numberCl = CodeModelInstance.instance()._ref(Number.class);
			var cmp = cmpM.body().decl(numberCl, "_cmp",
					JExpr.cast(numberCl, 
							JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_0")))
							.invoke("apply").arg(tuple)));
			
			cmpM.body()._return(cmp.invoke("intValue"));
			
			method.body()
				._return(JExpr._new(CodeModelInstance.instance().ref(java.util.TreeMap.class))
							.arg(JExpr._new(cmpCl)));
		}
	};
	
	private static final Symbol ceilingEntrySymbol = new Symbol("ceiling_entry", TreeMap.singleton().getNamespace());
	public static final Symbol ceilingEntrySymbol_out = new Symbol("map-tree-ceiling-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key greater than or equal to the given key, or throws error if no such mapping exists.") 
	@Example("(map-tree-ceiling-entry (construct Map Tree (lambda (x y) -1)) 42)") 
	@Syntax("(map-tree-ceiling-entry <map> <key>)")
	public static final Operator ceilingEntry  = new Operator() {

		private final String ERROR = "No key-value mapping found!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return ceilingEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Object key = args.get(1);
			
			var e = map.ceilingEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return ceilingEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("ceilingEntry").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol ceilingKeySymbol = new Symbol("ceiling_key", TreeMap.singleton().getNamespace());
	public static final Symbol ceilingKeySymbol_out = new Symbol("map-tree-ceiling-key");
	
	@VelkaOperator
	@Description("Returns the least key greater than or equal to the given key, or null if there is no such key.") 
	@Example("(map-tree-ceiling-key (construct Map Tree (lambda (x y) -1)) 42)") 
	@Syntax("(map-tree-ceiling-entry <map> <key>)")
	public static final Operator ceilingKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "ceilingKey",
			"map-tree-ceiling-key", TreeMap.singleton().getNamespace(), Object.class);
	
	private static final Symbol containsKeySymbol = new Symbol("contains_key", TreeMap.singleton().getNamespace());
	public static final Symbol containsKeySymbol_out = new Symbol("map-tree-contains-key");
	
	@VelkaOperator
	@Description("Returns true if this map contains a mapping for the specified key.") 
	@Example("(map-tree-contains-key (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-contains-key <map> <key>)")
	public static final Operator containsKey = Operator.wrapJavaMethod(java.util.TreeMap.class, "containsKey",
			"map-tree-contains-key", TreeMap.singleton().getNamespace(), Object.class);	
	
	private static final Symbol containsValueSymbol = new Symbol("contains_value", TreeMap.singleton().getNamespace());
	public static final Symbol containsValueSymbol_out = new Symbol("map-tree-contains-value");
	
	@VelkaOperator
	@Description("Returns true if this map maps one or more keys to the specified value.") 
	@Example("(map-tree-contains-value (construct Map Tree (lambda (x y) -1)) \"foo\")") 
	@Syntax("(map-tree-contains-value <map> <value>)")
	public static final Operator containsValue = Operator.wrapJavaMethod(java.util.TreeMap.class, "containsValue", 
			"map-tree-contains-value", TreeMap.singleton().getNamespace(), Object.class);
	
	private static final Symbol firstEntrySymbol = new Symbol("first_entry", TreeMap.singleton().getNamespace());
	public static final Symbol firstEntrySymbol_out = new Symbol("map-tree-first-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-first-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-first-entry <map>)")
	public static final Operator firstEntry = new Operator() {

		private final String ERROR = "map-tree-first-entry cannot retrieve first entry!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return firstEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var e = map.firstEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return firstEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("firstEntry"));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol firstKeySymbol = new Symbol("first_key", TreeMap.singleton().getNamespace());
	public static final Symbol firstKeySymbol_out = new Symbol("map-tree-first-key");
	
	@VelkaOperator
	@Description("Returns the first (lowest) key currently in this map. Throws error if no such key exists.") 
	@Example("(map-tree-first-key (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-first-key <map>)")
	public static final Operator firstKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "firstKey", 
			"map-tree-first-key", TreeMap.singleton().getNamespace()); 
	
	private static final Symbol floorEntrySymbol = new Symbol("floor_entry", TreeMap.singleton().getNamespace());
	public static final Symbol floorEntrySymbol_out = new Symbol("map-tree-floor-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key less than or equal to the given key, or throws error if there is no such key.") 
	@Example("(map-tree-floor-entry (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-floor-entry <map> <key>)")
	public static final Operator floorEntry = new Operator() {

		private final String ERROR = "No key-value mapping found!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return floorEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Object key;
			if(args.get(1) instanceof Literal lit) {
				key = Literal.literalToObject(lit);
			}
			else {
				key = args.get(1);
			}
			
			var e = map.floorEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return floorEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("floorEntry").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol floorKeySymbol = new Symbol("floor_key", TreeMap.singleton().getNamespace());
	public static final Symbol floorKeySymbol_out = new Symbol("map-tree-floor-key");
	
	@VelkaOperator
	@Description("Returns the greatest key less than or equal to the given key, or throws error if there is no such key.") 
	@Example("(map-tree-floor-key (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-floor-key <map> <key>)")
	public static final Operator floorKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "floorKey", 
			"map-tree-floor-key", TreeMap.singleton().getNamespace(), Object.class); 
	
	private static final Symbol getSymbol = new Symbol("velka_get", TreeMap.singleton().getNamespace());
	public static final Symbol getSymbol_out = new Symbol("map-tree-get");
	
	@VelkaOperator
	@Description("Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.") 
	@Example("(map-tree-get (construct Map Tree (lambda (x y) -1)) 1 )") 
	@Syntax("(map-tree-get <map> <key>)")
	public static final Operator get = Operator.wrapJavaMethod(java.util.TreeMap.class, "get", 
			"map-tree-get", TreeMap.singleton().getNamespace(), Object.class); 
	
	private static final Symbol headMapSymbol = new Symbol("head_map", TreeMap.singleton().getNamespace());
	public static final Symbol headMapSymbol_out = new Symbol("map-tree-head");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive is true) to-key.") 
	@Example("(map-tree-head (construct Map Tree (lambda (x y) -1)) 1 \"foo\")") 
	@Syntax("(map-tree-head <map> <to-key> <value>)")
	public static final Operator headMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return headMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Expression toKey = args.get(1);
			
			var res = new java.util.TreeMap<Object, Object>(map.headMap(toKey));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return headMapSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var head = _method.body().decl(CodeModelInstance.instance().ref(java.util.SortedMap.class), "head",
					mappedArgs.get(new Symbol("_0")).invoke("headMap").arg(mappedArgs.get(new Symbol("_1"))));
			var tmcl = CodeModelInstance.instance().ref(java.util.TreeMap.class);
			var ret = _method.body().decl(tmcl, "ret",
					JExpr._new(tmcl).arg(head));
			_method.body()._return(ret);
		}
		
	};
	
	private static final Symbol headMapInclSymbol = new Symbol("head_map_incl", TreeMap.singleton().getNamespace());
	public static final Symbol headMapInclSymbol_out = new Symbol("map-tree-head-incl");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive? is true) to-key.") 
	@Example("(map-tree-head-incl (construct Map Tree (lambda (x y) -1)) 1 #f)") 
	@Syntax("(map-tree-head-incl <map> <to-key> <inclusive?>)")
	public static final Operator headMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return headMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Expression toKey = args.get(1);
			LitBoolean inclusive = (LitBoolean)args.get(2);
			
			var res = new java.util.TreeMap<Object, Object>(
					map.headMap(toKey, inclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return headMapInclSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var head = _method.body().decl(CodeModelInstance.instance().ref(java.util.SortedMap.class), "head",
					mappedArgs.get(new Symbol("_0")).invoke("headMap")
						.arg(mappedArgs.get(new Symbol("_1")))
						.arg(mappedArgs.get(new Symbol("_2"))));
			var tmcl = CodeModelInstance.instance().ref(java.util.TreeMap.class);
			var ret = _method.body().decl(tmcl, "ret",
					JExpr._new(tmcl).arg(head));
			_method.body()._return(ret);
		}
	};
	
	private static final Symbol higherEntrySymbol = new Symbol("higher_entry", TreeMap.singleton().getNamespace());
	public static final Symbol higherEntrySymbol_out = new Symbol("map-tree-higher-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the least key strictly greater than the given key, or null if there is no such key.") 
	@Example("(map-tree-higher-entry (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-higher-entry <map> <key>)")
	public static final Operator higherEntry = new Operator() {

		private static final String ERROR = "map-tree-higher-entry cannot retrieve the entry.";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return higherEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Expression key = args.get(1);
			
			var e = map.higherEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return higherEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("higherEntry").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol higherKeySymbol = new Symbol("higher_key", TreeMap.singleton().getNamespace());
	public static final Symbol higherKeySymbol_out = new Symbol("map-tree-higher-key");
	
	@VelkaOperator
	@Description("Returns the least key strictly greater than the given key, or throws error if there is no such key.") 
	@Example("(map-tree-higher-key (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-higher-key <map> <key>)")
	public static final Operator higherKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "higherKey", 
			"map-tree-higher-key", TreeMap.singleton().getNamespace(), Object.class); 
	
	private static final Symbol keysSymbol = new Symbol("velka_keys", TreeMap.singleton().getNamespace());
	public static final Symbol keysSymbol_out = new Symbol("map-tree-keys");
	
	@VelkaOperator
	@Description("Returns a Set view of the keys contained in this map.") 
	@Example("(map-tree-keys (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-keys <map>)")
	public static final Operator keys = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String map = "_map";
			String code = ClojureHelper.fnHelper(
					List.of(map),
					ClojureHelper.constructJavaClass(ArrayList.class,
									ClojureHelper.applyClojureFunction(
											".keySet",
											map)));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return keysSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			return new LitInteropObject(new ArrayList<Object>(map.keySet()), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return keysSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var ks = method.body().decl(CodeModelInstance.instance()._ref(java.util.Set.class), "_ks",
					mappedArgs.get(new Symbol("_0")).invoke("keySet"));
			method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(java.util.ArrayList.class)).arg(ks));
		}
	};
	
	private static final Symbol lastEntrySymbol = new Symbol("last_entry", TreeMap.singleton().getNamespace());
	public static final Symbol lastEntrySymbol_out = new Symbol("map-tree-last-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-last-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-last-entry <map>)")
	public static final Operator lastEntry = new Operator() {

		private final String ERROR = "map-tree-last-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return lastEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var e = map.lastEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lastEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("lastEntry"));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol lastKeySymbol = new Symbol("last_key", TreeMap.singleton().getNamespace());
	public static final Symbol lastKeySymbol_out = new Symbol("map-tree-last-key");
	
	@VelkaOperator
	@Description("Returns the last (highest) key currently in this map.") 
	@Example("(map-tree-last-key (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-last-key <map>)")
	public static final Operator lastKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "lastKey", 
			"map-tree-last-key", TreeMap.singleton().getNamespace());
	
	private static final Symbol lowerEntrySymbol = new Symbol("lower_entry", TreeMap.singleton().getNamespace());
	public static final Symbol lowerEntrySymbol_out = new Symbol("map-tree-lower-entry");
	
	@VelkaOperator
	@Description("Returns a key-value mapping associated with the greatest key strictly less than the given key, or throws error if there is no such key.") 
	@Example("(map-tree-lower-entry (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-lower-entry <map> <key>)")
	public static final Operator lowerEntry = new Operator() {

		private static final String ERROR = "map-tree-lower-entry cannot retrieve the entry.";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return lowerEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var key = args.get(1);
			
			var e = map.lowerEntry(key);
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lowerEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("lowerEntry").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol lowerKeySymbol = new Symbol("lower_key", TreeMap.singleton().getNamespace());
	public static final Symbol lowerKeySymbol_out = new Symbol("map-tree-lower-key");
	
	@VelkaOperator
	@Description("Returns the greatest key strictly less than the given key, or throws an error if there is no such key.") 
	@Example("(map-tree-lower-key (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-lower-key <map> <key>)")
	public static final Operator lowerKey = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "lowerKey", 
			"map-tree-lower-key", TreeMap.singleton().getNamespace(), Object.class);
	
	private static final Symbol pollFirstEntrySymbol = new Symbol("poll_first_entry", TreeMap.singleton().getNamespace());
	public static final Symbol pollFirstEntrySymbol_out = new Symbol("map-tree-poll-first-entry");
	
	@VelkaOperator
	@Description("Removes and returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-poll-first-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-poll-first-entry <map>)")
	public static final Operator pollFirstEntry = new Operator() {

		private final String ERROR = "map-tree-poll-first-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return pollFirstEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var e = map.pollFirstEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return pollFirstEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("pollFirstEntry"));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol pollLastEntrySymbol = new Symbol("poll_last_entry", TreeMap.singleton().getNamespace());
	public static final Symbol pollLastEntrySymbol_out = new Symbol("map-tree-poll-last-entry");
	
	@VelkaOperator
	@Description("Removes and returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.") 
	@Example("(map-tree-poll-last-entry (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-poll-last-entry <map>)")
	public static final Operator pollLastEntry = new Operator() {

		private final String ERROR = "map-tree-poll-last-entry cannot retrieve last entry!";
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return pollLastEntrySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var e = map.pollLastEntry();
			if(e == null) {
				//Will throw error
				Expression err = new ExceptionExpr(new LitString(ERROR));
				err.interpret(env);
			}
			
			Expression ek;
			if(e.getKey() instanceof Expression expr) {
				ek = expr;
			}
			else {
				ek = Literal.objectToLiteral(e.getKey());
			}
			
			Expression ev;
			if(e.getValue() instanceof Expression expr) {
				ev = expr;
			}
			else {
				ev = Literal.objectToLiteral(e.getValue());
			}
			
			return new Tuple(ek, ev);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), new TypeTuple(K, V));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return pollLastEntrySymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var e = method.body().decl(CodeModelInstance.instance()._ref(Map.Entry.class), "_e",
					mappedArgs.get(new Symbol("_0")).invoke("pollLastEntry"));
			
			method.body()._if(e.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit(ERROR)));
			
			method.body()._return(VelkaTuple._velkaTuple(
					e.invoke("getKey"),
					e.invoke("getValue")));
		}
	};
	
	private static final Symbol putSymbol = new Symbol("put", TreeMap.singleton().getNamespace());
	public static final Symbol putSymbol_out = new Symbol("map-tree-put");
	
	@VelkaOperator
	@Description("Associates the specified value with the specified key in the map.") 
	@Example("(map-tree-put (construct Map Tree (lambda (x y) -1)) 1 \"foo\")") 
	@Syntax("(map-tree-put <map> <key> <value>)")
	public static final Operator put = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return putSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			java.util.TreeMap<Object, Object> map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var k = args.get(1);
			Object key = null;
			if(k instanceof Literal l) {
				key = Literal.literalToObject(l);
			}
			else {
				key = k;
			}
			
			var v = args.get(2);
			Object value = null;
			if(v instanceof Literal l) {
				value = Literal.literalToObject(l);
			}
			else {
				value = v;
			}
			
			map.put(key, value);
			
			return lji;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			TypeVariable V = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, V), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return putSymbol_out.toString();
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var tmcl = CodeModelInstance.instance().ref(java.util.TreeMap.class);
			var tm = method.body().decl(tmcl, "_tm", mappedArgs.get(new Symbol("_0")));
			
			method.body().add(tm.invoke("put")
					.arg(mappedArgs.get(new Symbol("_1")))
					.arg(mappedArgs.get(new Symbol("_2"))));
			method.body()._return(tm);
		}
		
	};
	
	private static final Symbol putAllSymbol = new Symbol("put_all", TreeMap.singleton().getNamespace());
	public static final Symbol putAllSymbol_out = new Symbol("map-tree-put-all");
	
	@VelkaOperator
	@Description("Copies all of the mappings from the specified map to this map.") 
	@Example("(map-tree-put-all (construct Map Tree (lambda (x y) -1)) (map-tree-put (construct Map Tree (lambda (x y) -1) 1 \"foo\")))") 
	@Syntax("(map-tree-put-all <receiving-map> <source-map>)")
	public static final Operator putAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return putAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, TypeAtom.TypeMapTree), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return putAllSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body().add(
					mappedArgs.get(new Symbol("_0")).invoke("putAll").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	};
	
	private static final Symbol removeSymbol = new Symbol("velka_remove", TreeMap.singleton().getNamespace());
	public static final Symbol removeSymbol_out = new Symbol("map-tree-remove");
	
	@VelkaOperator
	@Description("Removes the mapping for this key from this TreeMap if present.") 
	@Example("(map-tree-remove (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-remove <map> <key>)")
	public static final Operator remove = Operator.wrapNullableJavaMethod(java.util.TreeMap.class, "remove", 
			"map-tree-remove", TreeMap.singleton().getNamespace(), Object.class); 
	
	private static final Symbol sizeSymbol = new Symbol("velka_size", TreeMap.singleton().getNamespace());
	public static final Symbol sizeSymbol_out = new Symbol("map-tree-size");
	
	@VelkaOperator
	@Description("Returns the number of key-value mappings in this map.") 
	@Example("(map-tree-size (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-size <map>)")
	public static final Operator size = Operator.wrapJavaMethod(java.util.TreeMap.class, "size",
			"map-tree-size", TreeMap.singleton().getNamespace());
	
	private static final Symbol subMapInclSymbol = new Symbol("sub_map_incl", TreeMap.singleton().getNamespace());
	public static final Symbol subMapInclSymbol_out = new Symbol("map-tree-sub-map-inclusive");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys range from fromKey to toKey.") 
	@Example("(map-tree-sub-map-inclusive (construct Map Tree (lambda (x y) -1)) 1 #t 3 #f)") 
	@Syntax("(map-tree-sub-map-inclusive <map> <from-key> <from-inclusive> <to-key> <to-inclusive>)")
	public static final Operator subMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return subMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Object fromKey = args.get(1);
			LitBoolean fromInclusive = (LitBoolean)args.get(2);
			Object toKey = args.get(3);
			LitBoolean toInclusive = (LitBoolean)args.get(4);
			
			var res = 
					new java.util.TreeMap<Object, Object>(
							map.subMap(
									fromKey, 
									fromInclusive == LitBoolean.TRUE,
									toKey,
									toInclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return subMapInclSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(java.util.TreeMap.class))
					.arg(mappedArgs.get(new Symbol("_0")).invoke("subMap")
							.arg(mappedArgs.get(new Symbol("_1")))
							.arg(mappedArgs.get(new Symbol("_2")))
							.arg(mappedArgs.get(new Symbol("_3")))
							.arg(mappedArgs.get(new Symbol("_4")))));
		}
	};
	
	private static final Symbol subMapSymbol = new Symbol("velka_sub_map", TreeMap.singleton().getNamespace());
	public static final Symbol subMapSymbol_out = new Symbol("map-tree-sub-map");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys range from fromKey, inclusive, to toKey, exclusive.") 
	@Example("(map-tree-sub-map (construct Map Tree (lambda (x y) -1)) 1 3)") 
	@Syntax("(map-tree-sub-map <map> <from-key> <to-key>)")
	public static final Operator subMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return subMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var fromKey = args.get(1);
			var toKey = args.get(2);
			
			var res = 
					new java.util.TreeMap<Object, Object>(
							map.subMap(
									fromKey, 
									toKey));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return subMapSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(java.util.TreeMap.class))
					.arg(mappedArgs.get(new Symbol("_0")).invoke("subMap")
							.arg(mappedArgs.get(new Symbol("_1")))
							.arg(mappedArgs.get(new Symbol("_2")))));
		}
	};
	
	private static final Symbol tailMapSymbol = new Symbol("tail_map", TreeMap.singleton().getNamespace());
	public static final Symbol tailMapSymbol_out = new Symbol("map-tree-tail-map");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are greater than or equal to from-key.") 
	@Example("(map-tree-tail-map (construct Map Tree (lambda (x y) -1)) 1)") 
	@Syntax("(map-tree-tail-map <map> <from-key>)")
	public static final Operator tailMap = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return tailMapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			Expression key = args.get(1);
			
			var res = new java.util.TreeMap<Object, Object>(map.tailMap(key));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return tailMapSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(java.util.TreeMap.class))
					.arg(mappedArgs.get(new Symbol("_0")).invoke("tailMap")
							.arg(mappedArgs.get(new Symbol("_1")))));
		}
	};
	
	private static final Symbol tailMapInclSymbol = new Symbol("tail_map_incl", TreeMap.singleton().getNamespace());
	public static final Symbol tailMapInclSymbol_out = new Symbol("map-tree-tail-map-incl");
	
	@VelkaOperator
	@Description("Returns a view of the portion of this map whose keys are greater than (or equal to, if inclusive is true) fromKey.") 
	@Example("(map-tree-tail-map-incl (construct Map Tree (lambda (x y) -1)) 1 #t)") 
	@Syntax("(map-tree-tail-map-incl <map> <from-key> <inclusive?>)")
	public static final Operator tailMapIncl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return tailMapInclSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var key = args.get(1);
			var inclusive = (LitBoolean)args.get(2);
			
			var res = new java.util.TreeMap<Object, Object>(
					map.tailMap(key, inclusive == LitBoolean.TRUE));
			
			return new LitInteropObject(res, TypeAtom.TypeMapTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable K = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree, K, TypeAtom.TypeBoolNative), TypeAtom.TypeMapTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return tailMapInclSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(java.util.TreeMap.class))
					.arg(mappedArgs.get(new Symbol("_0")).invoke("tailMap")
							.arg(mappedArgs.get(new Symbol("_1")))
							.arg(mappedArgs.get(new Symbol("_2")))));
		}
	};
	
	private static final Symbol valuesSymbol = new Symbol("velka_values", TreeMap.singleton().getNamespace());
	public static final Symbol valuesSymbol_out = new Symbol("map-tree-values");
	
	@VelkaOperator
	@Description("Returns a Collection view of the values contained in this map.") 
	@Example("(map-tree-values (construct Map Tree (lambda (x y) -1)))") 
	@Syntax("(map-tree-values <map>)")
	public static final Operator values = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return valuesSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			var lji = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var map = (java.util.TreeMap<Object, Object>)lji.javaObject;
			
			var l = map.values();
			return new LitInteropObject(new ArrayList<Object>(l), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeMapTree), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return valuesSymbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var vls = method.body().decl(CodeModelInstance.instance()._ref(Collection.class), "_vls",
					mappedArgs.get(new Symbol("_0")).invoke("values"));
			
			var arrListCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ret = method.body().decl(arrListCl, "_ret",
					JExpr._new(arrListCl).arg(vls));
			method.body()._return(ret);
		}
	};
	
	private TreeMap() {}
	private static TreeMap me = null;
	
	public static TreeMap singleton() {
		if(me == null) {
			me = new TreeMap();
		}
		return me;
	}

	@Override
	protected String name() {
		return "treeMap";
	}

	
}
