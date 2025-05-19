package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JVar;

import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;
import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaConversion;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

@VelkaOperatorBank
@Description("Operators for working with velka Lists") 
@Header("List")
public class ListNative extends OperatorBank{

	/**
	 * Empty list native
	 */
	public static final Expression EMPTY_LIST_NATIVE = 
			new LitInteropObject(io.vavr.collection.Stream.empty(), TypeAtom.TypeListNative);

	/**
	 * Clojure code for empty list
	 */
	public static final String EMPTY_LIST_NATIVE_CLOJURE = "'()";

	/**
	 * Construtor for empty list
	 */
	@VelkaConstructor
	@Description("Constructs Empty List:Native.")
	@Name("Construct Empty List") 
	@Syntax("(construct List:Native)")
	public static final Constructor constructorEmpty = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String code = ClojureHelper.fnHelper(Arrays.asList(),
					Type.addTypeMetaInfo("'()", TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-construct-empty-list-native", ListNative.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			return new LitInteropObject(io.vavr.collection.Stream.empty(), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var stCl = CodeModelInstance.instance().ref(io.vavr.collection.Stream.class);
			
			method.body()._return(stCl.staticInvoke("empty"));
		}

	};

	public static final Symbol constructorSymbol = new Symbol("velka_construct_list_native", ListNative.singleton().getNamespace());

	/**
	 * Constructor for non-empty list
	 */
	@VelkaConstructor
	@Description("Constructs new List:Native adding element as head to list.") 
	@Name("Construct by cons") 
	@Syntax("(construct List:Native <element> <list>)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String val = "_value";
			String rest = "_rest";
			String code = ClojureHelper.fnHelper(Arrays.asList(val, rest),
					ClojureHelper.applyClojureFunction("lazy-seq", Type.addTypeMetaInfo(
							ClojureHelper.applyClojureFunction("cons", val, rest),
							TypeAtom.TypeListNative)));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var val = args.get(0);			
			var interop = (LitInteropObject) args.get(1);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) interop.javaObject;

			Object o = null;
			if(val instanceof Literal lit) {
				o = Literal.literalToObject(lit);
			}
			else {
				o = val;
			}
			
			s = s.prepend(o);			

			return new LitInteropObject(s, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var stCl = CodeModelInstance.instance().ref(io.vavr.collection.Stream.class);
			
			method.body()._return(
					JExpr.cast(stCl, mappedArgs.get(new Symbol("_1")))
						.invoke("prepend")
						.arg(mappedArgs.get(new Symbol("_0"))));
		}
	};

	/**
	 * is-list-native-empty operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if list is empty. Returns _false_ otherwise.") 
	@Example("(is-list-native-empty (construct List:Native)) ;; = #t") 
	@Syntax("(is-list-native-empty <list>)")
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String code = ClojureHelper.wrapClojureOperatorToFn(1, "empty?");
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>)lio.javaObject;
			if(s.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_is_list_native_empty", ListNative.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			method.body()._return(
					mappedArgs.get(new Symbol("_0")).invoke("isEmpty"));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "is-list-native-empty";
		}
		
	};

	/**
	 * head-list-native symbol
	 */
	private static final Symbol headSymbol = new Symbol("head", ListNative.singleton().getNamespace());
	public static final Symbol headSymbol_out = new Symbol("list-native-head");

	/**
	 * head-list-native operator
	 */
	@VelkaOperator
	@Description("Returns first element in this list.") 
	@Example("(head-list-native (build-list-native 5 (lambda (x) x))) ;; = 0") 
	@Syntax("(head-list-native <list>)")
	public static final Operator headListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take head of empty list.";

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.clojureIfHelper(
							ClojureHelper.applyClojureFunction("empty?", list),
							ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
							ClojureHelper.applyClojureFunction("first", list)));
			return code;
		}

		@Override
		public String toString() {
			return headSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return headSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>)lio.javaObject;
			var e = s.head();
			
			if(e instanceof Expression expr) {
				return expr;
			}
			
			return Literal.objectToLiteral(e);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					mappedArgs.get(new Symbol("_0")).invoke("head"));
		}
	};

	/**
	 * tail-list-native symbol
	 */
	private static final Symbol tailSymbol = new Symbol("tail", ListNative.singleton().getNamespace());
	public static final Symbol tailSymbol_out = new Symbol("list-native-tail");

	@VelkaOperator
	@Description("Returns list consisting of all elements of original list, except the first element.") 
	@Example("(tail-list-native (build-list-native 5 (lambda (x) x))) ;; = (1 2 3 4)") 
	@Syntax("(tail-list-native <list>)")
	public static final Operator tailListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take tail of empty list.";

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("empty?",
													list),
											ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
											ClojureHelper.applyClojureFunction("rest",
													list))),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return tailSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return tailSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>)lio.javaObject;
			var l = s.tail();

			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var l = mappedArgs.get(new Symbol("_0"));
			method.body()._return(
					l.invoke("tail"));
		}
	};

	/**
	 * map-list-native symbol
	 */
	private static final Symbol mapSymbol = new Symbol("velka_map", ListNative.singleton().getNamespace());
	public static final Symbol mapSymbol_out = new Symbol("list-native-map");

	/**
	 * map-list-native operator
	 */
	@VelkaOperator
	@Description("Returns a List:Native consisting of the results of applying the given function to the elements of list.") 
	@Example("(map-list-native\n"
					+ "    (build-list-native 5 (lambda (x)))\n"
					+ "    (lambda (y) (* y 2))) ;; = (0 2 4 6 8)") 
	@Syntax("(map-list-native <list> <function>)")
	public static final Operator mapListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String fn = "_fn";
			String arg = "_arg";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, list),
					LitComposite
							.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("lazy-seq",
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg),
															ClojureHelper.applyVelkaFunction(fn, arg)),
													list)),
									TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.mapSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return ListNative.mapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression f = args.get(0);
			LitInteropObject interop = (LitInteropObject) args.get(1);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) interop.javaObject;

			var m = s.map(o -> {
				Expression a = null;
				if(o instanceof Expression e) {
					a = e;
				}
				else {
					a = Literal.objectToLiteral(o);
				}
				var app = new AbstractionApplication(f, new Tuple(a));
				Expression ret;
				try {
					ret = app.interpret(env);
				} catch (AppendableException e1) {
					throw new RuntimeException(e1);
				}
				
				if(ret instanceof Literal l) {
					return Literal.literalToObject(l);
				}
				
				return ret;
			});
			
			return new LitInteropObject(m, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(
							Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(A)), B), TypeAtom.TypeListNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {			
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var aCl = CodeModelInstance.instance().anonymousClass(java.util.function.Function.class);
			var app = aCl.method(JMod.PUBLIC, Object.class, "apply");
			var o = app.param(Object.class, "_o");
			var r = app.body().decl(oCl, "r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_0"))).invoke("apply").arg(VelkaTuple._of(o)));
			app.body()._return(r);
			
			method.body()._return(
					mappedArgs.get(new Symbol("_1")).invoke("map").arg(JExpr._new(aCl)));
		}		
	};

	/**
	 * Symbol for map2-list-native
	 */
	private static final Symbol map2Symbol = new Symbol("map2", ListNative.singleton().getNamespace());
	public static final Symbol map2Symbol_out = new Symbol("list-native-map2");

	/**
	 * map2-list-native operator
	 */
	@VelkaOperator
	@Description("Returns a List:Native consisting of the results of applying the given function to the elements of list1 and list2.") 
	@Example("(map2-list-native\n"
					+ "    (build-list-native 5 (lambda (x) x))\n"
					+ "    (build-list-native 5 (lambda (x) x))\n"
					+ "    +) ;; = (0 2 4 6 8)") 
	@Syntax("(map2-list-native <list1> <list2> <function>)")
	public static final Operator map2ListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";

			String code = ClojureHelper
					.fnHelper(Arrays.asList(fn, list1, list2),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("lazy-seq",
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
															ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
													list1,
													list2)),
									TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.map2Symbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return ListNative.map2Symbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression f = args.get(0);
			var iOp1 = (LitInteropObject) args.get(1);
			var iOp2 = (LitInteropObject) args.get(2);
			
			@SuppressWarnings("unchecked")
			var l1 = (io.vavr.collection.Stream<Object>) iOp1.javaObject;
			@SuppressWarnings("unchecked")
			var l2 = (io.vavr.collection.Stream<Object>) iOp2.javaObject;

			var l = l1.zip(l2).map(
					tuple -> {
						Function<Object, Expression> extr = (Object x) -> {
							if(x instanceof Expression e) {
								return e;
							}
							return Literal.objectToLiteral(x);
						};
						var t = tuple.map(extr, extr);
						var arg = new Tuple(t._1, t._2);
						
						var app = new AbstractionApplication(f, arg);
						Expression ret;
						try {
							ret = app.interpret(env);
						} catch (AppendableException e1) {
							throw new RuntimeException(e1);
						}
						
						if(ret instanceof Literal lit) {
							return Literal.literalToObject(lit);
						}
						
						return ret;
					});

			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeVariable C = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), C),
					TypeAtom.TypeListNative, TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var aCl = CodeModelInstance.instance().anonymousClass(java.util.function.Function.class);
			var app = aCl.method(JMod.PUBLIC, Object.class, "apply");
			
			var o = app.param(Object.class, "_o");
			
			var t2Cl = CodeModelInstance.instance().ref(io.vavr.Tuple2.class);
			var t = app.body().decl(t2Cl, "_t",
					JExpr.cast(t2Cl, o));
			
			var r = app.body().decl(oCl, "r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_0"))).invoke("apply").arg(VelkaTuple._of(t.invoke("_1"), t.invoke("_2"))));
			app.body()._return(r);
			
			method.body()._return(mappedArgs.get(new Symbol("_1")).invoke("zip").arg(mappedArgs.get(new Symbol("_2")))
					.invoke("map").arg(JExpr._new(aCl)));
		}
	};

	/**
	 * Symbol for foldl-list-native
	 */
	private static final Symbol foldlSymbol = new Symbol("foldl", ListNative.singleton().getNamespace());
	public static final Symbol foldlSymbol_out = new Symbol("list-native-foldl");

	/**
	 * foldl-list-native operator
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.") 
	@Example("(foldl-list-native / 0 (build-list-native 3 (lambda (x) (+ x 1)))) ;; = 0.16666666666666666666666666666667") 
	@Syntax("(foldl-list-native <function> <terminator> <list>)")
	public static final Operator foldlListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String terminator = "_term";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, terminator, list),
					ClojureHelper.applyClojureFunction("reduce",
							ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
									ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
							terminator, 
							ClojureHelper.applyClojureFunction("reverse", list)));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.foldlSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return ListNative.foldlSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var abst = args.get(0);
			var terminator = args.get(1);
			var io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) io.javaObject;
			
			var ret = s.foldRight(terminator, (o1, e2) -> {
				Expression e1 = null;
				if(o1 instanceof Expression e) {
					e1 = e;
				}
				else {
					e1 = Literal.objectToLiteral(o1);
				}
				
				var app = new AbstractionApplication(abst, new Tuple(e2, e1));
				Expression r;
				try {
					r = app.interpret(env);
				} catch (AppendableException e3) {
					throw new RuntimeException(e3);
				}
				return r;				
			});
			
			return ret;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(new TypeArrow(new TypeTuple(A, B), A), A, TypeAtom.TypeListNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {		
			var aCl = CodeModelInstance.instance().anonymousClass(BiFunction.class);
			var app = aCl.method(JMod.PUBLIC, Object.class, "apply");
			var o1 = app.param(Object.class, "_o1");
			var o2 = app.param(Object.class, "_o2");
			
			app.body()._return(
					JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_0")))
						.invoke("apply")
						.arg(VelkaTuple._of(o2, o1)));
			
			method.body()._return(
					mappedArgs.get(new Symbol("_2"))
						.invoke("foldRight")
							.arg(mappedArgs.get(new Symbol("_1")))
							.arg(JExpr._new(aCl)));
		}		
	};

	public static final Symbol ListNativeToLinkedListSymbol = new Symbol("to_linked_list", ListNative.singleton().getNamespace());
	public static final Symbol ListNativeToLinkedListSymbol_out = new Symbol("list-native-2-linked-list");

	@VelkaConversion
	@Description("Converts List:Native to List:JavaLinked.") 
	@Example("(list-native-2-linked-list (build-list-native 5 (lambda (x) x)))") 
	@Syntax("(list-native-2-linked-list <list native>)")
	public static final Conversion ListNativeToLinkedListOperator = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper
					.fnHelper(List.of(list),
							ClojureHelper.constructJavaClass(java.util.LinkedList.class, list));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.ListNativeToLinkedListSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return ListNativeToLinkedListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (io.vavr.collection.Stream<Object>) iOp.javaObject;
			
			var a = new java.util.LinkedList<Object>(l.asJava());

			return new LitInteropObject(a, TypeAtom.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			
			var lCl = CodeModelInstance.instance().ref(java.util.LinkedList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0")).invoke("asJava")));
			method.body()._return(ll);
		}
	};

	public static final Symbol filterSymbol = new Symbol("velka_filter", ListNative.singleton().getNamespace());
	public static final Symbol filterSymbol_out = new Symbol("list-native-filter");

	@VelkaOperator	
	@Description("Returns new List:Native containing only those elements of list, for which predicate returns true.") 
	@Example("(filter-list-native (build-list-native 5 (lambda (x) x)) (lambda (y) (= (mod y 2) 0))) ;; = (0 2 4)") 
	@Syntax("(filter-list-native <list> <predicate>)")
	public static final Operator filter = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list, pred),
							LitComposite
									.clojureValueToClojureLiteral(
											ClojureHelper.applyClojureFunction("lazy-seq",
													ClojureHelper.applyClojureFunction("filter",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper
																			.applyVelkaFunction(pred, arg)),
															list)),
											TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return filterSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.filterSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) iOp.javaObject;
			var pred = args.get(1);
			
			var r = s.filter(e -> {
				AbstractionApplication app;
				if(e instanceof Expression expr) {
					app = new AbstractionApplication(pred, new Tuple(expr));
				}
				else {
					app = new AbstractionApplication(pred, new Tuple(Literal.objectToLiteral(e)));
				}
				
				Expression rsl = null;
				try {
					rsl = app.interpret(env);
				} catch (AppendableException e1) {
					throw new RuntimeException(e1);
				}
				return rsl == LitBoolean.TRUE;
			});

			return new LitInteropObject(r, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			final TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(new TypeTuple(A), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(Predicate.class);
			var app = aCl.method(JMod.PUBLIC, boolean.class, "test");
			var o = app.param(Object.class, "_o");
			
			app.body()
					._return(JExpr
							.cast(CodeModelInstance.instance().ref(Boolean.class),
									JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
											mappedArgs.get(new Symbol("_1"))).invoke("apply").arg(VelkaTuple._of(o)))
							.invoke("booleanValue"));
			
			method.body()._return(
					mappedArgs.get(new Symbol("_0"))
					.invoke("filter")
					.arg(JExpr._new(aCl)));
		}
	};

	/**
	 * Symbol for get operator
	 */
	public static final Symbol getSymbol = new Symbol("velka_get", ListNative.singleton().getNamespace());
	/**
	 * Public symbol for get operator
	 */
	public static final Symbol getSymbol_out = new Symbol("list-native-get");

	/**
	 * Get operator
	 */
	@VelkaOperator
	@Description("Returns the element at the specified position in this list.") 
	@Example("(get-list-native (build-list-native 5 (lambda (x) (* 2 x))) 1) ;; = 2") 
	@Syntax("(get-list-native <list> <index>)")
	public static final Operator get = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, index), ClojureHelper.applyClojureFunction("nth",
					list, index));

			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return getSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.getSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) iOp.javaObject;
			LitInteger index = (LitInteger) args.get(1);

			var val = s.get(index.value);
			if(val instanceof Expression expr) {
				return expr;
			}
			return Literal.objectToLiteral(val);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeIntNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			method.body()
				._return(
						mappedArgs.get(new Symbol("_0"))
							.invoke("get")
							.arg(mappedArgs.get(new Symbol("_1"))));
		}

	};

	public static final Symbol buildListSymbol = new Symbol("build_list", ListNative.singleton().getNamespace());
	public static final Symbol buildListSymbol_out = new Symbol("list-native-build");

	@VelkaOperator
	@Description("Creates a List:Native of n elements by applying function to the integers from 0 to (- n 1) in order.\n"
					+ "If lst is the resulting list, then (get-list-native lst i) is the value produced by (function i).") 
	@Example("(build-list-native 5 (lambda (x) (* x x))) ;; = (0 1 4 9 16)") 
	@Syntax("(build-list-native <n> <function>)")
	public static final Operator buildList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String n = "_n";
			String fn = "_fn";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(n, fn),
							LitComposite
									.clojureValueToClojureLiteral(
											ClojureHelper.applyClojureFunction("lazy-seq",
													ClojureHelper.applyClojureFunction("map",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper.applyVelkaFunction(fn,
																			LitInteger.clojureLit(
																					arg))),
															ClojureHelper.applyClojureFunction("range",
																	n))),
											TypeAtom.TypeListNative));

			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return buildListSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.buildListSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger n = (LitInteger) args.get(0);
			Expression fn = args.get(1);
			
			var s = io.vavr.collection.Stream.range(0, n.value).map(i ->{
				var app = new AbstractionApplication(fn, new Tuple(new LitInteger(i)));
				Expression ret;
				try {
					ret = app.interpret(env);
				} catch (AppendableException e) {
					throw new RuntimeException(e);
				}
				if(ret instanceof Literal l) {
					return Literal.literalToObject(l);
				}
				return ret;
			});

			return new LitInteropObject(s, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable buildListNative_A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative,
							new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), buildListNative_A)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(Function.class);
			var app = aCl.method(JMod.PUBLIC, Object.class, "apply");
			var o = app.param(Object.class, "_o");
			
			app.body()._return(
					mappedArgs.get(new Symbol("_1"))
					.invoke("apply")
					.arg(VelkaTuple._of(o)));
			
			method.body()._return(
					CodeModelInstance.instance().ref(io.vavr.collection.Stream.class)
					.staticInvoke("range")
					.arg(JExpr.lit(0))
					.arg(mappedArgs.get(new Symbol("_0")))
						.invoke("map")
						.arg(JExpr._new(aCl)));
		}
	};

	@VelkaOperator
	@Description("Returns the number of elements in this list.") 
	@Example("(size-list-native (build-list-native 3 (lambda (x) x))) ;; = 3") 
	@Syntax("(size-list-native <list>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list), LitInteger.clojureLit(
					ClojureHelper.applyClojureFunction("count", list)));

			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_list_native_size", ListNative.singleton().getNamespace());
		}
		
		@Override
		public String toString() {
			return "list-native-size";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (io.vavr.collection.Stream<Object>) iOp.javaObject;

			return new LitInteger(l.size());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			method.body()._return(
					mappedArgs.get(new Symbol("_0")).invoke("size"));
		}
	};

	public static final Symbol appendSymbol = new Symbol("append", ListNative.singleton().getNamespace());
	public static final Symbol appendSymbol_out = new Symbol("list-native-append");

	@VelkaOperator
	@Description("Creates a new List:Native where contents of list2 are appended after contents of list1.") 
	@Example("(append-list-native (build-list-native 2 (lambda (x) x)) (build-list-native 3 (lambda (x) (+ x 2)))) ;; = (0 1 2 3 4)") 
	@Syntax("(append-list-native <list1> <list2>)")
	public static final Operator append = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String code = ClojureHelper.fnHelper(Arrays.asList(list1, list2), LitComposite.clojureValueToClojureLiteral(
					ClojureHelper.applyClojureFunction("lazy-seq", ClojureHelper.applyClojureFunction("concat",
							list1, list2)),
					TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return appendSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.appendSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp1 = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l0 = (io.vavr.collection.Stream<Object>) iOp1.javaObject;
			var iOp2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			var l1 = (io.vavr.collection.Stream<Object>) iOp2.javaObject;

			var l = l0.appendAll(l1);
			
			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					mappedArgs.get(new Symbol("_0"))
						.invoke("appendAll")
						.arg(mappedArgs.get(new Symbol("_1"))));
		}
	};

	public static final Symbol reverseSymbol = new Symbol("velka_reverse", ListNative.singleton().getNamespace());
	public static final Symbol reverseSymbol_out = new Symbol("list-native-reverse");

	@VelkaOperator
	@Description("Creates new List:Native with the same elements as list, but in reversed (last to first) order.") 
	@Example("(reverse-list-native (build-list-native 5 (lambda (x) x))) ;; = (4 3 2 1 0)") 
	@Syntax("(reverse-list-native <list>)")
	public static final Operator reverse = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("reverse", list),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return reverseSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.reverseSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (io.vavr.collection.Stream<Object>) iOp.javaObject;
			var r = l.reverse();
			return new LitInteropObject(r, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					mappedArgs.get(new Symbol("_0"))
						.invoke("reverse"));
		}
	};

	public static final Symbol everypSymbol = new Symbol("velka_everyp", ListNative.singleton().getNamespace());
	public static final Symbol everypSymbol_out = new Symbol("list-native-everyp");

	@VelkaOperator
	@Description("Returns true if every element of this list returns true for the predicate. Otherwise returns false.") 
	@Example("(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= (mod x 2) 0))) ;; = #t\n"
					+ "(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= x 1))) ;; = #f") 
	@Syntax("(everyp-list-native <list> <predicate>)")
	public static final Operator everyp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String pred_arg = "_arg";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, pred),
					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction("every?",
							ClojureHelper.fnHelper(Arrays.asList(pred_arg),
									ClojureHelper.applyVelkaFunction(pred, pred_arg)),
							list)));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return everypSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.everypSymbol_out.toString();
		}

		@SuppressWarnings("deprecation")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			Expression pred = args.get(1);
			@SuppressWarnings("unchecked")
			var l = (io.vavr.collection.Stream<Object>) iOp.javaObject;
			var r = l.forAll(e -> {
				Expression exp = null;
				if(e instanceof Expression expr) {
					exp = expr;
				}
				else {
					exp = Literal.objectToLiteral(e);
				}
				
				var app = new AbstractionApplication(pred, new Tuple(exp));
				Expression ret;
				try {
					ret = app.interpret(env);
				} catch (AppendableException e1) {
					throw new RuntimeException(e1);
				}
				
				return ret == LitBoolean.TRUE;
			});
			
			if(r) return LitBoolean.TRUE;
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var aCl = CodeModelInstance.instance().anonymousClass(Predicate.class);
			var tst = aCl.method(JMod.PUBLIC, boolean.class, "test");
			var o = tst.param(Object.class, "_o");
			
			tst.body()._return(
					JExpr.cast(CodeModelInstance.instance().ref(Boolean.class),
					mappedArgs.get(new Symbol("_1"))
					.invoke("apply")
					.arg(VelkaTuple._of(o))).invoke("booleanValue"));
			
			method.body()._return(
					mappedArgs.get(new Symbol("_0"))
						.invoke("forAll")
						.arg(JExpr._new(aCl)));
		}
	};

	/**
	 * Operator for T foldr(Function<T, E, T>)
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the end.") 
	@Example("(def l1 (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-array-list-foldr / 0 l) ;; = 1.5") 
	@Syntax("(java-array-list-foldr <function> <terminator> <list>)")
	public static final Operator foldr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String terminator = "_term";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, terminator, list),
					ClojureHelper.applyClojureFunction("reduce",
							ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
									ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
							terminator, list));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var abst = args.get(0);
			var terminator = args.get(1);
			var io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			var s = (io.vavr.collection.Stream<Object>) io.javaObject;
			
			var ret = s.foldLeft(terminator, (e1, o2) -> {
				Expression e2 = null;
				if(o2 instanceof Expression e) {
					e2 = e;
				}
				else {
					e2 = Literal.objectToLiteral(o2);
				}
				
				var app = new AbstractionApplication(abst, new Tuple(e1, e2));
				Expression r;
				try {
					r = app.interpret(env);
				} catch (AppendableException e3) {
					throw new RuntimeException(e3);
				}
				return r;				
			});
			
			return ret;
		}

		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());
		
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), A), A, TypeAtom.TypeListNative)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("foldr", ListNative.singleton().getNamespace());
		}
		
		@Override
		public String toString() {
			return "list-native-foldr";
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {			
			var aCl = CodeModelInstance.instance().anonymousClass(BiFunction.class);
			var app = aCl.method(JMod.PUBLIC, Object.class, "apply");
			var o1 = app.param(Object.class, "_o1");
			var o2 = app.param(Object.class, "_o2");
			
			app.body()._return(
					JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_0")))
						.invoke("apply")
						.arg(VelkaTuple._of(o1, o2)));
			
			method.body()._return(
					mappedArgs.get(new Symbol("_2"))
						.invoke("foldLeft")
							.arg(mappedArgs.get(new Symbol("_1")))
							.arg(JExpr._new(aCl)));
		}
	};
	
	/**
	 * Creates a native list from collection
	 * 
	 * @param col collection
	 * @return LitComposite with native list
	 */
	public static Expression collectionToListNative(Collection<? extends Expression> col) {
		List<Expression> l = col.stream().collect(Collectors.toList());
		return ListNative.makeListNativeExpression(l);
	}

	/**
	 * Converts tuple into equvivalent list
	 * 
	 * @param t converted tuple
	 * @return LitComposite object containing native list
	 */
	public static Expression tupleToListNative(Tuple t) {
		return 
				new LitInteropObject(io.vavr.collection.Stream.ofAll(t.stream()),
				TypeAtom.TypeListNative);
	}

	/**
	 * Converts java list into list native expression
	 * 
	 * @param l converted list
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression makeListNativeExpression(List<Expression> l) {
		var s = io.vavr.collection.Stream.ofAll(l.stream());
		return new LitInteropObject(s, TypeAtom.TypeListNative);
	}

	/**
	 * Converts expression into List Native expression
	 * 
	 * @param exprs expression
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression of(Expression... exprs) {
		return makeListNativeExpression(Arrays.asList(exprs));
	}

	/**
	 * Converts (at interpretation runtime) list into tuple
	 * 
	 * @param list converted list
	 * @return tuple
	 */
	public static Tuple listNativeToTuple(LitComposite list) {
		@SuppressWarnings("unchecked")
		var s = (io.vavr.collection.Stream<Expression>) ((LitInteropObject) list.value).javaObject;

		return new Tuple(s.asJava());
	}
	
	public static String listNativeClojure(String clojureCode) {
		String code = ClojureHelper.applyClojureFunction("lazy-seq", clojureCode);
		return LitComposite.clojureValueToClojureLiteral(code, TypeAtom.TypeListNative);
	}

	/**
	 * Creates code for ListNative value in clojure
	 * @param members code for members of list
	 * @return code for list native
	 */
	public static String listNativeClojure(Collection<String> members) {
		String code =  listNativeClojure(ClojureHelper.applyClojureFunction("list", members));
		return code;
	}
	
	/**
	 * Creates code for ListNative value in clojure
	 * @param members members of the list
	 * @return code for list
	 */
	public static String listNativeClojure(String ...members) {
		return listNativeClojure(Arrays.asList(members));
	}

	/**
	 * Relative path to velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_PATH = velka.core.util.Constants.LOCATION;

	/**
	 * Name of the velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_NAME = Paths.get("list");
	
	private ListNative() {}
	private static ListNative instance = null;
	public static ListNative singleton() {
		if(instance == null) {
			instance = new ListNative();
		}
		return instance;
	}

	@Override
	protected String name() {
		return "list";
	}
}
