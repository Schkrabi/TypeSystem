package velka.core.langbase;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Supplier;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JVar;

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
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.TypedObject;
import velka.java.runtime.VelkaTuple;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.BitSetHelper;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Header;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaConversion;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

@VelkaOperatorBank
@Description("Operators for working with java.util.HashSet.") 
@Header("Hash Set")
public class HashSet extends OperatorBank {
	
	@VelkaConstructor
	public static final Constructor constructor = Constructor.wrapJavaConstructor(java.util.HashSet.class,
			HashSet.instance().getNamespace());
	
	@VelkaOperator
	public static Operator fromList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var l = "_list";
			var code = ClojureHelper.fnHelper(List.of(l),
					ClojureHelper.constructJavaClass(java.util.HashSet.class, l));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("from_list", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (io.vavr.collection.Stream<Object>)lio.javaObject;
			
			var s = new java.util.HashSet<Object>();
			l.forEach(e -> s.add(e));
			
			return new LitInteropObject(s, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-from-list";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {			
			var hsCl = CodeModelInstance.instance()._ref(java.util.HashSet.class);
			var hs = method.body().decl(hsCl, "_hs", JExpr._new(hsCl));
			
			var aCl = CodeModelInstance.instance().anonymousClass(Consumer.class);
			var accept = aCl.method(JMod.PUBLIC, void.class, "accept");
			var o = accept.param(Object.class, "_o");
			
			accept.body().add(hs.invoke("add").arg(o));
			
			method.body().add(mappedArgs.get(new Symbol("_0")).invoke("forEach").arg(JExpr._new(aCl)));
			
			method.body()._return(hs);
		}
	};
	
	@VelkaOperator
	public static Operator add = Operator.wrapJavaMethod(java.util.HashSet.class, "add", "set-hash-add", HashSet.instance().getNamespace(), Object.class);
	
	@VelkaOperator
	public static Operator clear = Operator.wrapJavaMethod(java.util.HashSet.class, "clear", "set-hash-clear", HashSet.instance().getNamespace());
	
	@VelkaOperator
	public static Operator contains = Operator.wrapJavaMethod(java.util.HashSet.class, "contains", "set-hash-contains", HashSet.instance().getNamespace(), Object.class);
	
	@VelkaOperator
	public static Operator isEmpty = Operator.wrapJavaMethod(java.util.HashSet.class, "isEmpty", "set-hash-is-empty", HashSet.instance().getNamespace());
	
	@VelkaOperator
	public static Operator remove = Operator.wrapJavaMethod(java.util.HashSet.class, "remove", "set-hash-remove", HashSet.instance().getNamespace(), Object.class);
	
	@VelkaOperator
	public static Operator size = Operator.wrapJavaMethod(java.util.HashSet.class, "size", "set-hash-size", HashSet.instance().getNamespace());
	
	@VelkaOperator
	public static Operator addAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hs";
			var l = "_l";
			var code = ClojureHelper.fnHelper(List.of(hs, l),
					ClojureHelper.applyClojureFunction(".addAll", hs, l));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var hs = (java.util.HashSet<Object>)lio.javaObject;
			
			var lio1 = (LitInteropObject)args.get(1);
			var s = (io.vavr.collection.Stream<Object>)lio1.javaObject;
			
			s.forEach(e -> hs.add(e));
			
			return lio;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_set_hash_add_all", HashSet.instance().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(Consumer.class);
			var accept = aCl.method(JMod.PUBLIC, void.class, "accept");
			var o = accept.param(Object.class, "_o");
			
			accept.body().add(mappedArgs.get(new Symbol("_0")).invoke("add").arg(o));
			
			method.body().add(mappedArgs.get(new Symbol("_1")).invoke("forEach").arg(JExpr._new(aCl)));
			
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeListNative), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-add-all";
		}
	};

	@VelkaOperator
	public static Operator containsAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hs";
			var l = "_l";
			var code = ClojureHelper.fnHelper(List.of(hs, l),
					ClojureHelper.applyClojureFunction(".containsAll", hs, l));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var hs = (java.util.HashSet<Object>)lio.javaObject;
			
			var lio1 = (LitInteropObject)args.get(1);
			var s = (io.vavr.collection.Stream<Object>)lio1.javaObject;
			
			var r = s.foldLeft(true, (x, y) -> x && hs.contains(y));
			
			return Literal.objectToLiteral(r);
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_set_hash_contains_all", HashSet.instance().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(BiFunction.class);
			var apply = aCl.method(JMod.PUBLIC, Object.class, "apply");
			var o1 = apply.param(Object.class, "_o1");
			var o2 = apply.param(Object.class, "_o2");
			
			apply.body()._return(JExpr.cast(CodeModelInstance.instance().ref(Boolean.class), o1)
					.cand(mappedArgs.get(new Symbol("_0")).invoke("contains").arg(o2)));
			
			method.body()._return(
					mappedArgs.get(new Symbol("_1"))
						.invoke("foldLeft")
						.arg(JExpr.TRUE)
						.arg(JExpr._new(aCl)));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeListNative), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-contains-all";
		}
		
	};
		
	@VelkaOperator
	public static Operator intersect = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set1 = "_set1";
			var set2 = "_set2";
			var set = "_set";
			var code = ClojureHelper.fnHelper(List.of(set1, set2),
					ClojureHelper.letHelper(set, 
							Pair.of(set, ClojureHelper.constructJavaClass(java.util.HashSet.class, set1)),
							Pair.of("tmp", ClojureHelper.applyClojureFunction(".retainAll", set, set2))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka_intersect", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio1 = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s1 = (java.util.HashSet<Object>)lio1.javaObject;
			
			var lio2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			var s2 = (java.util.HashSet<Object>)lio2.javaObject;
			
			var s = new java.util.HashSet<Object>(s1);
			s.retainAll(s2);
			
			return new LitInteropObject(s, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeSetHash), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-intersect";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var hscl = CodeModelInstance.instance()._ref(java.util.HashSet.class);
			var set = method.body().decl(hscl, "set",
					JExpr._new(hscl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(set.invoke("retainAll").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._return(set);			
		}
	};
	
	@VelkaOperator
	public static Operator union = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set1 = "_set1";
			var set2 = "_set2";
			var set = "_set";
			var code = ClojureHelper.fnHelper(List.of(set1, set2),
					ClojureHelper.letHelper(set, 
							Pair.of(set, ClojureHelper.constructJavaClass(java.util.HashSet.class, set1)),
							Pair.of("tmp", ClojureHelper.applyClojureFunction(".addAll", set, set2))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka_union", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio1 = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s1 = (java.util.HashSet<Object>)lio1.javaObject;
			
			var lio2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			var s2 = (java.util.HashSet<Object>)lio2.javaObject;
			
			var s = new java.util.HashSet<Object>(s1);
			s.addAll(s2);
			
			return new LitInteropObject(s, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeSetHash), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-union";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var hscl = CodeModelInstance.instance()._ref(java.util.HashSet.class);
			var set = method.body().decl(hscl, "set",
					JExpr._new(hscl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(set.invoke("addAll").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._return(set);			
		}
	};
	
	@VelkaOperator
	public static Operator toList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hash-set";
			var code = ClojureHelper.fnHelper(List.of(hs),
					ClojureHelper.applyClojureFunction("lazy-seq", hs));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("to_list", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var hs = (java.util.HashSet<Object>)lio.javaObject;
			
			var l = io.vavr.collection.Stream.ofAll(hs.stream());
			
			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-to-list";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					CodeModelInstance.instance().ref(io.vavr.collection.Stream.class)
						.staticInvoke("ofAll")
						.arg(mappedArgs.get(new Symbol("_0")).invoke("stream")));
		}
	};
	
	@VelkaOperator
	public static Operator largest = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hash-set";
			var comparator = "_comparator";
			var x = "_x";
			var y = "_y";
			var code = ClojureHelper.fnHelper(List.of(hs, comparator),
						ClojureHelper.clojureIfHelper(
								ClojureHelper.applyClojureFunction(".isEmpty", hs), 
								Expression.EMPTY_EXPRESSION.toClojureCode(env),
								ClojureHelper.applyClojureFunction(
										"reduce", 
										ClojureHelper.fnHelper(
												List.of(x, y),
												ClojureHelper.clojureIfHelper(
														ClojureHelper.applyClojureFunction("<=", 
																ClojureHelper.applyVelkaFunction(comparator, x, y), 
																"0"), 
														y, x)),
										hs)));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka_largest", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var hs = (java.util.HashSet<Object>)lio.javaObject;
			
			if(hs.isEmpty()) {
				return Expression.EMPTY_EXPRESSION;
			}
			
			var comparator = args.get(1);
			
			var largest = hs.stream().reduce(
					(o1, o2) -> {
						var e1 = Literal.objectToLiteral(o1);
						var e2 = Literal.objectToLiteral(o2);
						
						var appl = new AbstractionApplication(comparator, new Tuple(e1, e2));
						Expression cmp;
						try {
							cmp = appl.interpret(env);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
						if(cmp instanceof LitInteger li) {
							if(li.value <= 0) {
								return o2;
							}
							return o1;
						}
						throw new RuntimeException("Invalid comparator function " + comparator);
					}).get();
			
			return Literal.objectToLiteral(largest);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var A = new TypeVariable(NameGenerator.next());
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, new TypeArrow(new TypeTuple(A, A), TypeAtom.TypeIntNative)), A);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-largest";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var _if = method.body()._if(mappedArgs.get(new Symbol("_0")).invoke("isEmpty"));
			_if._then()._return(CodeModelInstance.instance().ref(TypedObject.class).staticRef("VELKA_EMPTY"));
			
			var jcomparator = CodeModelInstance.instance().anonymousClass(BinaryOperator.class);
			var cmpApply = jcomparator.method(com.sun.codemodel.JMod.PUBLIC, Object.class, "apply");
			
			var o1 = cmpApply.param(Object.class, "_o1");
			var o2 = cmpApply.param(Object.class, "_o2");
			
			var t1 = cmpApply.body().decl(TypeUtil.instance().typeJType(), "_t1", JavaTypeSystem.codeInstance().invoke("getType").arg(o1));
			var t2 = cmpApply.body().decl(TypeUtil.instance().typeJType(), "_t2", JavaTypeSystem.codeInstance().invoke("getType").arg(o2));
			
			var numCl = CodeModelInstance.instance().ref(Number.class);
			
			var cmp = cmpApply.body().decl(CodeModelInstance.instance()._ref(Object.class), "_cmp",
					mappedArgs.get(new Symbol("_1")).invoke("apply").arg(JExpr._new(CodeModelInstance.instance()._ref(VelkaTuple.class))
							.arg(CodeModelInstance.instance().ref(java.util.List.class).staticInvoke("of").arg(o1).arg(o2))
							.arg(JExpr._new(TypeUtil.instance().typeTupleJType()).arg(t1).arg(t2))));
			
			var _cmpIf = cmpApply.body()._if(cmp._instanceof(numCl).not());
			_cmpIf._then()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(JExpr.lit("Invalid comparator function ").plus(mappedArgs.get(new Symbol("_1")))));
			
			var icmp = cmpApply.body().decl(CodeModelInstance.instance().ref(Integer.class), "_icmp", 
					JExpr.cast(numCl, cmp).invoke("intValue"));
			
			cmpApply.body()._if(icmp.invoke("compareTo").arg(JExpr.lit(0)).gt(JExpr.lit(0)))._then()._return(o1);
			cmpApply.body()._return(o2);
			
			var _r = method.body().decl(CodeModelInstance.instance().ref(Object.class), "_rslt",
					mappedArgs.get(new Symbol("_0")).invoke("stream").invoke("reduce").arg(JExpr._new(jcomparator)).invoke("get"));
			
			method.body()._return(_r);
		}
	};
	
	@VelkaConversion
	public static Conversion toTreeSet = new Conversion() {
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var arg = "_arg";
			final var code = ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/hashset2treeset", arg)); 
			
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("hash_set_2_tree_set", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var hashSet = (java.util.HashSet<Object>)lio.javaObject;
			
			var treeset = BitSetHelper.hashset2treeset(hashSet);
			
			return new LitInteropObject(treeset, TypeAtom.TypeSetTree);
		};

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(CodeModelInstance.instance().ref(BitSetHelper.class).staticInvoke("hashset2treeset")
					.arg(mappedArgs.get(new Symbol("_0"))));
		}
	};
	
	@VelkaConversion
	public static Conversion toBitSet = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var arg = "_arg";
			final var code = ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/hashset2bitset", arg)); 
			
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("hashset_to_bitset", HashSet.instance().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var hashSet = (java.util.HashSet<Object>)lio.javaObject;
			
			var bitset = BitSetHelper.hashset2bitset(hashSet);
			
			return new LitInteropObject(bitset, TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(CodeModelInstance.instance().ref(BitSetHelper.class).staticInvoke("hashset2bitset")
					.arg(mappedArgs.get(new Symbol("_0"))));
		}
	};
	
	@VelkaOperator
	public static Operator removeAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hs";
			var l = "_l";
			var code = ClojureHelper.fnHelper(List.of(hs, l),
					ClojureHelper.letHelper(hs, 
							Pair.of("_tmp", ClojureHelper.applyClojureFunction(".removeAll", hs, l))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var ll = (java.util.HashSet)lio.javaObject;
			
			var lio1 = (LitInteropObject)args.get(1);
			var l = (io.vavr.collection.Stream)lio1.javaObject;
			
			l.forEach(e -> ll.remove(e));
			
			return lio;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_set_hash_remove_all", HashSet.instance().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(Consumer.class);
			var accept = aCl.method(JMod.PUBLIC, void.class, "accept");
			var o = accept.param(Object.class, "_o");
			
			accept.body().add(mappedArgs.get(new Symbol("_0")).invoke("remove").arg(o));
			
			method.body().add(mappedArgs.get(new Symbol("_1")).invoke("forEach").arg(JExpr._new(aCl)));
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeListNative), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-remove-all";
		}
	};
	
	@VelkaOperator
	public static Operator retainAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var hs = "_hs";
			var l = "_l";
			var code = ClojureHelper.fnHelper(List.of(hs, l),
					ClojureHelper.letHelper(hs, 
							Pair.of("_tmp", ClojureHelper.applyClojureFunction(".retainAll", hs, l))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var ts = (java.util.HashSet)lio.javaObject;
			
			var lio1 = (LitInteropObject)args.get(1);
			var l = (io.vavr.collection.Stream)lio1.javaObject;
			
			ts.retainAll(l.asJava());
			
			return lio;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_set_hash_retain_all", HashSet.instance().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			method.body()
			.add(mappedArgs.get(new Symbol("_0")).invoke("retainAll")
					.arg(mappedArgs.get(new Symbol("_1")).invoke("asJava")));
		method.body()._return(mappedArgs.get(new Symbol("_0")));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash, TypeAtom.TypeListNative), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "set-hash-retain-all";
		}
	};
	
	private static HashSet singleton = null;
	
	public static HashSet instance() {
		if(singleton == null) {
			singleton = new HashSet();
		}
		return singleton;
	}

	@Override
	protected String name() {
		return "hashSet";
	}

}
