package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
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
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
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

	/** Clojure namespace */
	public static final String NAMESPACE = "velka.clojure.hashSet";
	
	public static final Path PATH = Paths.get("velka", "clojure");
	public static final Path FILE = Paths.get("hashSet.clj");
	
	@VelkaConstructor
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var code = ClojureHelper.fnHelper(List.of(),
					ClojureHelper.constructJavaClass(java.util.HashSet.class));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-construct", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var s = new java.util.HashSet<Object>();
			
			return new LitInteropObject(s, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Set:Hash ";
		}
	};
	
	@VelkaConstructor
	public static Constructor copyConstructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var code = ClojureHelper.fnHelper(List.of(set),
					ClojureHelper.constructJavaClass(java.util.HashSet.class, set));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-copy-construcotr", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var hs = (java.util.HashSet<Object>)lio.javaObject;
			
			var s = new java.util.HashSet<Object>(hs);
			
			return new LitInteropObject(s, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetHash), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Set:Hash ";
		}
	};
	
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
		public Symbol getClojureSymbol() {
			return new Symbol("from-list", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Expression>)lio.javaObject;
			
			var s = new java.util.HashSet<Object>();
			l.stream().forEach(e -> s.add(Literal.literalToObject(e)));
			
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
	};
	
	@VelkaOperator
	public static Operator add = Operator.wrapJavaMethod(java.util.HashSet.class, "add", "set-hash-add", NAMESPACE, Object.class);
	
	@VelkaOperator
	public static Operator clear = Operator.wrapJavaMethod(java.util.HashSet.class, "clear", "set-hash-clear", NAMESPACE);
	
	@VelkaOperator
	public static Operator contains = Operator.wrapJavaMethod(java.util.HashSet.class, "contains", "set-hash-contains", NAMESPACE, Object.class);
	
	@VelkaOperator
	public static Operator isEmpty = Operator.wrapJavaMethod(java.util.HashSet.class, "isEmpty", "set-hash-is-empty", NAMESPACE);
	
	@VelkaOperator
	public static Operator remove = Operator.wrapJavaMethod(java.util.HashSet.class, "remove", "set-hash-remove", NAMESPACE, Object.class);
	
	@VelkaOperator
	public static Operator size = Operator.wrapJavaMethod(java.util.HashSet.class, "size", "set-hash-size", NAMESPACE);
	
	@VelkaOperator
	public static Operator addAll = Operator.wrapJavaMethod(java.util.HashSet.class, "addAll", "set-hash-add-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	public static Operator containsAll = Operator.wrapJavaMethod(java.util.HashSet.class, "containsAll", "set-hash-contains-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	public static Operator removeAll = Operator.wrapJavaMethod(java.util.HashSet.class, "removeAll", "set-hash-remove-all", NAMESPACE, Collection.class);
	
	@VelkaOperator
	public static Operator retainAll = Operator.wrapJavaMethod(java.util.HashSet.class, "retainAll", "set-hash-retain-all", NAMESPACE, Collection.class);
	
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
		public Symbol getClojureSymbol() {
			return new Symbol("velka-intersect", NAMESPACE);
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
		public Symbol getClojureSymbol() {
			return new Symbol("velka-union", NAMESPACE);
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
	};
	
	@VelkaConversion
	public static Conversion toTreeSet = new Conversion() {

		@Override
		public Expression cost() {
			var hashSet = new Symbol(NameGenerator.next());
			
			var cost = new Lambda(
					new Tuple(hashSet), 
					new TypeTuple(TypeAtom.TypeSetHash),
					new AbstractionApplication(
							new AbstractionApplication(
									Operators.linFunPoints, 
									new Tuple(new LitDouble(0d), new LitDouble(0.6d), new LitDouble(1000d), new LitDouble(0.3d))), 
							new Tuple(
									new AbstractionApplication(Operators.IntToDouble,
											new Tuple(new AbstractionApplication(HashSet.size, new Tuple(hashSet)))))));
			return cost;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var arg = "_arg";
			final var code = ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/hashset2treeset", arg)); 
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("hash-set-2-tree-set", NAMESPACE);
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
		
	};
	
	@VelkaConversion
	public static Conversion toBitSet = new Conversion() {

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
			var arg = "_arg";
			final var code = ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/hashset2bitset", arg)); 
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("hashset-to-bitset", NAMESPACE);
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
	
	private static HashSet singleton = null;
	
	public static HashSet instance() {
		if(singleton == null) {
			singleton = new HashSet();
		}
		return singleton;
	}

}
