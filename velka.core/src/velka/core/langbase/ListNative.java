package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.stream.Collectors;

import com.sun.codemodel.JExpr;

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
import velka.core.exceptions.UserException;
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
			new LitInteropObject(new ArrayList<Object>(), TypeAtom.TypeListNative);

	/**
	 * Clojure code for empty list
	 */
	public static final String EMPTY_LIST_NATIVE_CLOJURE = ClojureHelper.constructJavaClass(ArrayList.class);

	/**
	 * Construtor for empty list
	 */
	@VelkaConstructor
	@Description("Constructs Empty List:Native.")
	@Name("Construct Empty List") 
	@Syntax("(construct List:Native)")
	public static final Constructor constructorEmpty = Constructor.wrapJavaConstructor(java.util.ArrayList.class, ListNative.singleton().getNamespace());

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
			var ll = "_ll";
			var tmp = "_tmp";
			String code = ClojureHelper.fnHelper(List.of(val, rest),
					ClojureHelper.letHelper(ll,
							Pair.of(ll, ClojureHelper.constructJavaClass(ArrayList.class)),
							Pair.of(tmp, ClojureHelper.applyClojureFunction(".add", ll, val)),
							Pair.of(tmp, ClojureHelper.applyClojureFunction(".addAll", ll, rest))));
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
			var l = (List<Object>) interop.javaObject;

			var ll = new ArrayList<Object>();
			
			if(val instanceof Literal lit) {
				ll.add(Literal.literalToObject(lit));
			}
			else {
				ll.add(val);
			}
			
			ll.addAll(l);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
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
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl));
			method.body().add(ll.invoke("add").arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(ll.invoke("addAll").arg(mappedArgs.get(new Symbol("_1"))));
			method.body()._return(ll);
		}
	};
	
	/**
	 * Operator for contructor from list
	 */
	@VelkaConstructor
	@Description("Construct List:JavaArray from existing list inserting all its elements.") 
	@Name("Construct from list") 
	@Syntax("(construct List:JavaArray <list>)")
	public static Constructor constructorFromList = Constructor.wrapJavaConstructor(java.util.ArrayList.class,
			ListNative.singleton().getNamespace(), Collection.class);
	
	/**
	 * Operator for capacity constructor
	 */
	@VelkaConstructor
	@Description("Constructs List:JavaArray with specified pre-allocated capacity.") 
	@Name("Construct with capacity") 
	@Syntax("(construc List JavaArray <capacity>)")
	public static Constructor constructorCapacity = Constructor.wrapJavaConstructor(java.util.ArrayList.class, 
			ListNative.singleton().getNamespace(), int.class);

	/**
	 * is-list-native-empty operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if list is empty. Returns _false_ otherwise.") 
	@Example("(is-list-native-empty (construct List:Native)) ;; = #t") 
	@Syntax("(is-list-native-empty <list>)")
	public static final Operator isEmpty = Operator.wrapJavaMethod(ArrayList.class, "isEmpty", "is-list-native-empty",
			ListNative.singleton().getNamespace()); 

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
			
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			var l = (List<Object>) interop.javaObject;

			if (l.isEmpty()) {
				throw new RuntimeException(errorMsg);
			}
			
			var e = l.get(0);
			
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
			var l = mappedArgs.get(new Symbol("_0"));
			var _if = method.body()._if(l.invoke("isEmpty"));
			_if._then()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(JExpr.lit(errorMsg)));
			
			method.body()._return(l.invoke("get").arg(JExpr.lit(0)));
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
			String code = ClojureHelper.fnHelper(List.of(list),
									ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("empty?",
													list),
											ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
											ClojureHelper.constructJavaClass(ArrayList.class,
													ClojureHelper.applyClojureFunction("rest",
															list))));
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
			
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			List<Expression> l = (List<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				throw new RuntimeException(errorMsg);
			}

			List<Expression> ll = new ArrayList<Expression>(l.subList(1, l.size()));
			return new LitInteropObject(ll, TypeAtom.TypeListNative);
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
			var _if = method.body()._if(l.invoke("isEmpty"));
			_if._then()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(JExpr.lit(errorMsg)));
			
			method.body()._return(
					JExpr._new(CodeModelInstance.instance().ref(ArrayList.class))
						.arg(l.invoke("subList").arg(JExpr.lit(1))
								.arg(l.invoke("size"))));
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
									ClojureHelper.constructJavaClass(ArrayList.class, 
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg),
															ClojureHelper.applyVelkaFunction(fn, arg)),
													list)));
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
			var l = (List<Object>) interop.javaObject;

			var agg = new ArrayList<Object>();

			for (var e : l) {
				AbstractionApplication appl;
				
				if(e instanceof Expression expr) {
					appl = new AbstractionApplication(f, new Tuple(expr));
				}
				else {
					appl = new AbstractionApplication(f, new Tuple(Literal.objectToLiteral(e)));
				}
				Expression res = appl.interpret(env);
				if(res instanceof Literal lit) {
					agg.add(Literal.literalToObject(lit));
				}
				else {
					agg.add(res);
				}
			}

			return new LitInteropObject(agg, TypeAtom.TypeListNative);
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
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_1")).invoke("size")));
			
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var _forEach = method.body().forEach(oCl, "o", mappedArgs.get(new Symbol("_1")));
			var r = _forEach.body().decl(oCl, "r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_0"))).invoke("apply").arg(VelkaTuple._velkaTuple(_forEach.var())));
			
			_forEach.body().add(ll.invoke("add").arg(r));
			
			method.body()._return(ll);
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
									ClojureHelper.constructJavaClass(ArrayList.class,
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
															ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
													list1,
													list2)));
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
			var l1 = (List<Object>) iOp1.javaObject;
			@SuppressWarnings("unchecked")
			var l2 = (List<Object>) iOp2.javaObject;

			var agg = new ArrayList<Object>();

			var i1 = l1.iterator();
			var i2 = l2.iterator();
			while (i1.hasNext() && i2.hasNext()) {
				var o1 = i1.next();
				Expression e1;
				if(o1 instanceof Expression expr) {
					e1 = expr;
				}
				else {
					e1 = Literal.objectToLiteral(o1);
				}
				
				var o2 = i2.next();
				Expression e2;
				if(o2 instanceof Expression expr) {
					e2 = expr;
				}
				else {
					e2 = Literal.objectToLiteral(o2);
				}				
				
				var appl = new AbstractionApplication(f, new Tuple(e1, e2));
				var ret = appl.interpret(env);
				
				if(ret instanceof Literal lit) {
					agg.add(Literal.literalToObject(lit));
				}
				else {
					agg.add(ret);
				}
			}

			return new LitInteropObject(agg, TypeAtom.TypeListNative);
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
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_1")).invoke("size")));
			
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var itCl = CodeModelInstance.instance().ref(Iterator.class);
			
			var it1 = method.body().decl(itCl, "it1", mappedArgs.get(new Symbol("_1")).invoke("iterator"));
			var it2 = method.body().decl(itCl, "it2", mappedArgs.get(new Symbol("_2")).invoke("iterator"));
			
			var _while = method.body()._while(it1.invoke("hasNext").band(it2.invoke("hasNext")));
			
			var o1 = _while.body().decl(oCl, "o1", it1.invoke("next"));
			var o2 = _while.body().decl(oCl, "o2", it2.invoke("next"));
			
			var r = _while.body().decl(oCl, "r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_0"))).invoke("apply").arg(VelkaTuple._velkaTuple(o1, o2)));
			
			_while.body().add(ll.invoke("add").arg(r));
			
			method.body()._return(ll);
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
							terminator, list));
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
			Expression f = args.get(0);
			Expression term = args.get(1);
			var iOp = (LitInteropObject)args.get(2);
			@SuppressWarnings("unchecked")
			var l = (List<Object>) iOp.javaObject;

			for (var e : l) {
				AbstractionApplication appl;
				
				if(e instanceof Expression expr) {
					appl = new AbstractionApplication(f, new Tuple(term, expr));
				}
				else {
					appl = new AbstractionApplication(f, new Tuple(term, Literal.objectToLiteral(e)));
				}
				term = appl.interpret(env);
			}

			return term;
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
			var oCl = CodeModelInstance.instance().ref(Object.class);
			
			var agg = method.body().decl(oCl, "ret", mappedArgs.get(new Symbol("_1")));
			
			var _forEach = method.body().forEach(oCl, "o", mappedArgs.get(new Symbol("_2")));
			
			_forEach.body().assign(agg, JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_0"))).invoke("apply").arg(VelkaTuple._velkaTuple(agg, _forEach.var())));
			
			method.body()._return(agg);
		}		
	};

	/**
	 * Symbol for add-to-end function
	 */
	private static final Symbol addToEndSymbol = new Symbol("add_to_end", ListNative.singleton().getNamespace());
	public static final Symbol addToEndSymbol_out = new Symbol("list-native-add-to-end");

	/**
	 * add-to-end operator
	 */
	@VelkaOperator
	@Description("Creates new list with appended the specified element to the end of list.") 
	@Example("(add-to-end-list-native (construct List:Native) 42) ;; = (42)") 
	@Syntax("(add-to-end-list-native <list> <element>)")
	public static final Operator addToEndOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String element = "_element";
			var ll = "_ll";
			var tmp = "_tmp";

			String code = ClojureHelper.fnHelper(List.of(list, element),
							ClojureHelper.letHelper(ll,
									Pair.of(ll, ClojureHelper.constructJavaClass(ArrayList.class, list)),
									Pair.of(tmp, ClojureHelper.applyClojureFunction(".add", ll, element))));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.addToEndSymbol_out.toString();
		}

		@Override
		public Symbol getInternalSymbol() {
			return ListNative.addToEndSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (List<Object>) iOp.javaObject;
			Object e;
			
			if(args.get(1) instanceof Literal lit) {
				e = Literal.literalToObject(lit);
			}
			else {
				e = args.get(1);
			}			

			var ll = new ArrayList<Object>(l);
			ll.add(e);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(ll.invoke("add").arg(mappedArgs.get(new Symbol("_1"))));
			method.body()._return(ll);
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
							ClojureHelper.constructJavaClass(LinkedList.class, list));
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
			List<Expression> l = (List<Expression>) iOp.javaObject;
			List<Expression> a = new ArrayList<Expression>(l);

			return new LitInteropObject(a, TypeAtom.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new AbstractionApplication(ListNative.size, new Tuple(arg)),
					List.of(Pair.of(arg, TypeAtom.TypeListNative)));
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(LinkedList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body()._return(ll);
		}
	};

	/**
	 * contains operator
	 */
	@VelkaOperator
	@Description("Returns true if this list contains the specified element.") 
	@Example("(contains-list-native (build-list-native 3 (lambda (x) x)) 0) ; = #t\n"
					+ "(contains-list-native (build-list-native 3 (lambda (x) x)) 5) ; = #f") 
	@Syntax("(contains-list-native <list> <element>)")
	public static final Operator contains = Operator.wrapJavaMethod(ArrayList.class, "contains",
			"list-native-contains", ListNative.singleton().getNamespace(), Object.class);

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
					.fnHelper(List.of(list, pred),
							ClojureHelper.constructJavaClass(ArrayList.class,
													ClojureHelper.applyClojureFunction("filter",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper
																			.applyVelkaFunction(pred, arg)),
															list)));
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
			var l = (List<Object>) iOp.javaObject;
			var pred = args.get(1);

			var ll = new ArrayList<Object>();

			for (var e : l) {
				AbstractionApplication app;
				if(e instanceof Expression expr) {
					app = new AbstractionApplication(pred, new Tuple(expr));
				}
				else {
					app = new AbstractionApplication(pred, new Tuple(Literal.objectToLiteral(e)));
				}
				
				Expression rsl = app.interpret(env);
				if (rsl.equals(LitBoolean.TRUE)) {
					ll.add(e);
				}
			}

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
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
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl));
			
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var _forEach = method.body().forEach(oCl, "o", mappedArgs.get(new Symbol("_0")));
			var r = _forEach.body().decl(oCl, "r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class),
					mappedArgs.get(new Symbol("_1"))).invoke("apply").arg(VelkaTuple._velkaTuple(_forEach.var())));
			
			var _if = _forEach.body()._if(JExpr.cast(CodeModelInstance.instance().ref(Boolean.class), r));
			_if._then().add(ll.invoke("add").arg(r));
			
			method.body()._return(ll);
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
	public static final Operator get = Operator.wrapJavaMethod(ArrayList.class, "get", "list-native-get",
			ListNative.singleton().getNamespace(), int.class); 

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
					.fnHelper(List.of(n, fn),
							ClojureHelper.constructJavaClass(ArrayList.class,													
													ClojureHelper.applyClojureFunction("map",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper.applyVelkaFunction(fn,
																			LitInteger.clojureLit(
																					arg))),
															ClojureHelper.applyClojureFunction("range",
																	n))));
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

			List<Object> l = new ArrayList<Object>();

			for (int i = 0; i < n.value; i++) {
				AbstractionApplication appl = new AbstractionApplication(fn, new Tuple(new LitInteger(i)));

				Expression expr = appl.interpret(env);
				
				if(expr instanceof Literal lit)
					l.add(Literal.literalToObject(lit));
				else
					l.add(expr);
			}

			return new LitInteropObject(l, TypeAtom.TypeListNative);
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
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0"))));
			
			var oCl = CodeModelInstance.instance().ref(Object.class);
			
			var _for = method.body()._for();
			var _i = _for.init(CodeModelInstance.instance().INT, "_i", JExpr.lit(0));
			_for.test(_i.lt(mappedArgs.get(new Symbol("_0"))));
			_for.update(_i.incr());
			
			var r = _for.body().decl(oCl, "_r", JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_1")))
					.invoke("apply").arg(VelkaTuple._velkaTuple(_i)));
			
			_for.body().add(ll.invoke("add").arg(r));
			
			method.body()._return(ll);
		}
	};

	public static final Symbol removeSymbol = new Symbol("velka_remove", ListNative.singleton().getNamespace());
	public static final Symbol removeSymbol_out = new Symbol("list-native-remove");

	@VelkaOperator
	@Description("Removes the first occurrence of the specified element from this list, if it is present.") 
	@Example("(remove-list-native build-list-native 3 (lambda (x) x)) 1) ;; = (0 2)") 
	@Syntax("(remove-list-native <list> <element>)")
	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String element = "_element";
			var ll = "_ll";
			var tmp = "_tmp";
			String code = ClojureHelper
					.fnHelper(List.of(list, element),
							ClojureHelper.letHelper(ll,
									Pair.of(ll, ClojureHelper.constructJavaClass(ArrayList.class, list)),
									Pair.of(tmp, ClojureHelper.applyClojureFunction(".remove", ll, element))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return removeSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.removeSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			List<Expression> l = (List<Expression>) iOp.javaObject;
			Object e;
			
			if(args.get(1) instanceof Literal lit) {
				e = Literal.literalToObject(lit);
			}
			else {
				e = args.get(1);
			}

			List<Expression> ll = new ArrayList<Expression>(l);
			ll.remove(e);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(ll.invoke("remove").arg(mappedArgs.get(new Symbol("_1"))));
			method.body()._return(ll);
		}
	};

	@VelkaOperator
	@Description("Returns the number of elements in this list.") 
	@Example("(size-list-native (build-list-native 3 (lambda (x) x))) ;; = 3") 
	@Syntax("(size-list-native <list>)")
	public static final Operator size = Operator.wrapJavaMethod(ArrayList.class, "size", "list-native-size", 
			ListNative.singleton().getNamespace());

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
			var ll = "_ll";
			var tmp = "_tmp";
			String code = ClojureHelper.fnHelper(List.of(list1, list2),
					ClojureHelper.letHelper(ll,
							Pair.of(ll, ClojureHelper.constructJavaClass(ArrayList.class, list1)),
							Pair.of(tmp, ClojureHelper.applyClojureFunction(".addAll", ll, list2))));
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
			List<Expression> l0 = (List<Expression>) iOp1.javaObject;
			var iOp2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			List<Expression> l1 = (List<Expression>) iOp2.javaObject;

			List<Expression> aux = new ArrayList<Expression>(l0);
			aux.addAll(l1);

			return new LitInteropObject(aux, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var ll = method.body().decl(lCl, "ll", JExpr._new(lCl).arg(mappedArgs.get(new Symbol("_0"))));
			method.body().add(ll.invoke("addAll").arg(mappedArgs.get(new Symbol("_1"))));
			method.body()._return(ll);
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
			String code = ClojureHelper.fnHelper(List.of(list),
					ClojureHelper.constructJavaClass(ArrayList.class,
							ClojureHelper.applyClojureFunction("reverse", list)));
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
			List<Object> l = (List<Object>) iOp.javaObject;
			ListIterator<Object> li = l.listIterator(l.size());
			List<Object> r = new LinkedList<Object>();
			while (li.hasPrevious()) {
				r.add(li.previous());
			}

			return new LitInteropObject(r, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var itCl = CodeModelInstance.instance().ref(ListIterator.class);
			var l = mappedArgs.get(new Symbol("_0"));
			var _ll = method.body().decl(lCl, "_ll", JExpr._new(lCl).arg(l.invoke("size")));
			
			var _it = method.body().decl(itCl, "_it", l.invoke("listIterator").arg(l.invoke("size")));
			
			var _while = method.body()._while(_it.invoke("hasPrevious"));
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var _o = _while.body().decl(oCl, "_o", _it.invoke("previous"));
			_while.body().add(_ll.invoke("add").arg(_o));
			
			method.body()._return(_ll);
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
			String code = ClojureHelper.fnHelper(List.of(list, pred),
					ClojureHelper.applyClojureFunction("every?",
							ClojureHelper.fnHelper(Arrays.asList(pred_arg),
									ClojureHelper.applyVelkaFunction(pred, pred_arg)),
							list));
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

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (List<Object>) iOp.javaObject;
			Expression pred = args.get(1);

			for(var e : l) {
				Expression arg;
				if(e instanceof Expression expr) {
					arg = expr;
				}
				else {
					arg = Literal.objectToLiteral(e);
				}
				var appl = new AbstractionApplication(pred, new Tuple(arg));
				var rslt = appl.interpret(env);				
				
				if(rslt.equals(LitBoolean.FALSE)){
					return LitBoolean.FALSE;
				}
			}

			return LitBoolean.TRUE;
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
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var l = mappedArgs.get(new Symbol("_0"));
			var f = JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_1")));
			
			var _forEach = method.body().forEach(oCl, "_o", l);
			var bCl = CodeModelInstance.instance().ref(Boolean.class);
			var _r = _forEach.body().decl(bCl, "_r", JExpr.cast(bCl, f.invoke("apply").arg(VelkaTuple._velkaTuple(_forEach.var()))));
			
			var _if = _forEach.body()._if(_r.not());
			_if._then()._return(JExpr.FALSE);
			
			method.body()._return(JExpr.TRUE);
		}
	};
	
	/**
	 * Operator for boolean add(E e)
	 */
	@VelkaOperator
	@Description("Appends the specified element to the end of list.") 
	@Example("(list-native-add-to-end-in-place (construct List:JavaArray) 42)") 
	@Syntax("(list-native-add-to-end-in-place <list> <element>)")
	public static final Operator addToEndInPlace = Operator.wrapJavaMethod(ArrayList.class, "add",
			"list-native-add-to-end-in-place", ListNative.singleton().getNamespace(), Object.class);
	
	/**
	 * Operator for void add(int index, E element)
	 */
	@VelkaOperator
	@Description("Inserts the specified element at the specified position in list.") 
	@Example("(java-array-list-to-index (construct List:JavaArray) 0 42)") 
	@Syntax("(java-array-list-to-index <list> <index> <element>)")
	public static final Operator addToIndex = Operator.wrapJavaMethod(ArrayList.class, "add", "list-native-add-to-index",
			ListNative.singleton().getNamespace(), int.class, Object.class); 
	
	/**
	 * operator for boolean addAll(Collection<? extends E> c)
	 */
	@VelkaOperator
	@Description("Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's Iterator.")
	@Example("(def l (construct List:JavaArray))\n" + "(java-array-list-add l 42)\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n" + "(println l)\n" + ";;(42 0 1 2)")
	@Syntax("(java-array-list-add-all <list1> <list2>)")
	public static final Operator addAll = Operator.wrapJavaMethod(ArrayList.class, "addAll", "list-native-add-all",
			ListNative.singleton().getNamespace(), Collection.class);
	
	/**
	 * Operator for boolean containsAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains all of the elements in the specified list.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
			+ "(java-array-list-contains-all k (build-list-native 2 (lambda (x) x))) ;; = #t")
	@Syntax("(java-array-list-contains-all <list1> <list2>)")
	public static final Operator containsAll = Operator.wrapJavaMethod(ArrayList.class, "containsAll",
			"list-native-contains-all", ListNative.singleton().getNamespace(), Collection.class);
	
	/**
	 * Operator for int indexOf(Object o)
	 */
	@VelkaOperator
	@Description("Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
			+ "(list-native-index-of l 1) ;; = 1")
	@Syntax("(java-array-list-index-of <list> <element>)")
	public static final Operator indexOf = Operator.wrapJavaMethod(java.util.ArrayList.class, "indexOf",
			"list-native-index-of", ListNative.singleton().getNamespace(), Object.class);
	
	/**
	 * Operator for int lastIndexOf(E e)
	 */
	@VelkaOperator
	@Description("Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
			+ "(list-native-last-index-of l 1) ;; = 2")
	@Syntax("(java-array-list-last-index-of <list> <element>)")
	public static final Operator lastIndexOf = Operator.wrapJavaMethod(ArrayList.class, "lastIndexOf",
			"list-native-last-index-of", ListNative.singleton().getNamespace(), Object.class);

	/**
	 * Operator for boolean remove(Object o)
	 */
	@VelkaOperator
	@Description("Removes the first occurrence of the specified element from this list, if it is present.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n" + "(java-array-list-remove l 1)\n"
			+ "(println l)\n" + "(0 2)")
	@Syntax("(java-array-list-remove <list> <element>)")
	public static final Operator removeInPlace = Operator.wrapJavaMethod(ArrayList.class, "remove", "list-native-remove-in-place",
			ListNative.singleton().getNamespace(), Object.class);
	
	/**
	 * Operator for boolean removeAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Removes from this list all of its elements that are contained in the specified collection.")
	@Example("(let ((l (construct List:JavaArray (list 1 2 3)))"
			+ "(tmp (java-array-list-remove-all l (list 1 2))))"
			+ "l) ;;(3)")
	@Syntax("(java-array-list-remove-all <list> <removed-list>)")
	public static final Operator removeAll = Operator.wrapJavaMethod(ArrayList.class, "removeAll",
			"list-native-remove-all", ListNative.singleton().getNamespace(), Collection.class);
	
	/**
	 * Operator for boolean retainAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Retains only the elements in this list that are contained in the specified collection.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
					+ "(java-array-list-retain-all l (build-list-native 2 (lambda (x) (+ 1 x))))\n"
					+ "(println l)\n"
					+ "(2 3)") 
	@Syntax("(java-array-list-retain-all <retained-list> <retainee-list>)")
	public static final Operator retainAll = Operator.wrapJavaMethod(ArrayList.class, "retainAll",
			"list-native-retain-all", ListNative.singleton().getNamespace(), Collection.class);
	
	/**
	 * Operator for E set(int index, E element)
	 */
	@VelkaOperator
	@Description("Replaces the element at the specified position in this list with the specified element.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-set l 1 42)\n"
					+ "(println l)\n"
					+ "(0 42 2)") 
	@Syntax("(java-array-list-set <list> <index> <element>)")
	public static final Operator set = Operator.wrapJavaMethod(ArrayList.class, "set", "list-native-set",
			ListNative.singleton().getNamespace(), int.class, Object.class);
	
	/**
	 * Operator for List<E> subList(int fromIndex, int toIndex)
	 */
	@VelkaOperator
	@Description("Returns a view of the portion of this list between the specified fromIndex, inclusive, and toIndex, exclusive.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 10 (lambda (x) x)))\n"
					+ "(java-array-list-sublist l 3 7)\n"
					+ ";; = (2 3 4 5 6 7)")
	@Syntax("(java-array-list-sublist <list> <fromIndex> <toIndex>)")
	public static final Operator sublist = Operator.wrapJavaMethod(ArrayList.class, "subList", "list-native-sublist",
			ListNative.singleton().getNamespace(), int.class, int.class);

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
			String abst = "_abst";
			String term = "_term";
			String list = "_list";
			String agg = "_agg";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(abst, term, list),
					ClojureHelper.applyClojureFunction(
							"reduce",
							ClojureHelper.fnHelper(
									Arrays.asList(agg, element),
									ClojureHelper.applyVelkaFunction(
											abst,
											agg,
											element)),
							term,
							ClojureHelper.applyClojureFunction(
									"reverse",
									list)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var abst = args.get(0);
			var terminator = args.get(1);
			var io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			var list = (ArrayList<Object>) io.javaObject;

			Expression agg = terminator;
			var i = list.listIterator(list.size());
			while (i.hasPrevious()) {
				var element = i.previous();
				AbstractionApplication app = new AbstractionApplication(abst,
						new Tuple(agg, Literal.objectToLiteral(element)));
				agg = app.interpret(env);
			}

			return agg;
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
			var oCl = CodeModelInstance.instance().ref(Object.class);
			var itCl = CodeModelInstance.instance().ref(ListIterator.class);
			var l = mappedArgs.get(new Symbol("_2"));
			var f = JExpr.cast(CodeModelInstance.instance().ref(VelkaAbstraction.class), mappedArgs.get(new Symbol("_0")));
			
			var agg = method.body().decl(oCl, "ret", mappedArgs.get(new Symbol("_1")));
			var _i = method.body().decl(itCl, "_it", l.invoke("listIterator").arg(l.invoke("size")));
			
			var _while = method.body()._while(_i.invoke("hasPrevious"));
			var _o = _while.body().decl(oCl, "_o", _i.invoke("previous"));
			
			_while.body().assign(agg, f.invoke("apply").arg(VelkaTuple._velkaTuple(agg, _o)));
			
			method.body()._return(agg);
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
				new LitInteropObject(new LinkedList<Expression>(t.stream().collect(Collectors.toList())),
				TypeAtom.TypeListNative);
	}

	/**
	 * Converts java list into list native expression
	 * 
	 * @param l converted list
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression makeListNativeExpression(List<Expression> l) {
		List<Object> ll = new ArrayList<Object>();
		l.forEach(e -> {
			if(e instanceof Literal lit) {
				ll.add(Literal.literalToObject(lit));
			}
			else {
				ll.add(e);
			}
		});
		return new LitInteropObject(ll, TypeAtom.TypeListNative);
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
		List<Expression> l = (List<Expression>) ((LitInteropObject) list.value).javaObject;

		return new Tuple(l);
	}

	/**
	 * Prints list in clojure style
	 * 
	 * @param l printed list
	 * @return string with printed list
	 */
	public static String toStringListNative(List<Expression> l) {
		StringBuilder sb = new StringBuilder("(");
		Iterator<Expression> i = l.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toString());
			if (i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append(")");
		return sb.toString();
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
