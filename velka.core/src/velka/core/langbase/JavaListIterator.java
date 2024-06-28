package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ListIterator;

import velka.core.abstraction.Operator;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
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
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaOperator;

public class JavaListIterator extends OperatorBank {

	public static final String NAMESPACE = "velka.clojure.listIterator";
	
	/**
	 * Type name for list iterator
	 */
	public static final Symbol iteratorAddSymbol = new Symbol("iterator-add", NAMESPACE);
	public static final Symbol iteratorAddSymbol_out = new Symbol("list-iterator-add");
	@VelkaOperator
	@Description("Inserts the specified element into the list (optional operation).") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-add it 42)") 
	@Syntax("(list-iterator-add <list-iterator> <element>)")
	public static final Operator iteratorAdd = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String listIt = "_list-iterator";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(listIt, element),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".add",
											listIt,
											element),
									listIt)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorAddSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			Expression e = args.get(1);
			
			it.add(e);
			
			return it_io;
		}
		
		
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator, A),
					TypeAtom.TypeListIterator);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorAddSymbol_out.toString();
		}
	};
	public static final Symbol iteratorHasNextSymbol = new Symbol("has-next", NAMESPACE);
	public static final Symbol iteratorHasNextSymbol_out = new Symbol("list-iterator-has-next");
	@VelkaOperator
	@Description("Returns true if this list iterator has more elements when traversing the list in the forward direction.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-has-next it)") 
	@Syntax("(list-iterator-has-next <list iterator>)")
	public static final Operator iteratorHasNext = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".hasNext",
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorHasNextSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			return it.hasNext() ? LitBoolean.TRUE : LitBoolean.FALSE;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorHasNextSymbol_out.toString();
		}
	};
	public static final Symbol iteratorHasPreviousSymbol = new Symbol("has-previous", NAMESPACE);
	public static final Symbol iteratorHasPreviousSymbol_out = new Symbol("list-iterator-has-previous");
	@VelkaOperator
	@Description("Returns true if this list iterator has more elements when traversing the list in the reverse direction.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-has-previous it)") 
	@Syntax("list-iterator-has-previous <iterator>)")
	public static final Operator iteratorHasPrevious = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".hasPrevious",
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorHasPreviousSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			return it.hasPrevious() ? LitBoolean.TRUE : LitBoolean.FALSE;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorHasPreviousSymbol_out.toString();
		}
	};
	public static final Symbol iteratorNextSymbol = new Symbol("iterator-next", NAMESPACE);
	public static final Symbol iteratorNextSymbol_out = new Symbol("list-iterator-next");
	@VelkaOperator
	@Description("Returns the next element in the list and advances the cursor position.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-next it)") 
	@Syntax("(list-iterator-next <iterator>)")
	public static final Operator iteratorNext = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					ClojureHelper.applyClojureFunction(
							".next",
							it));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorNextSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			return it.next();
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					A);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorNextSymbol_out.toString();
		}
	};
	public static final Symbol iteratorNextIndexSymbol = new Symbol("next-index", NAMESPACE);
	public static final Symbol iteratorNextIndexSymbol_out = new Symbol("list-iterator-next-index");
	@VelkaOperator
	@Description("Returns the index of the element that would be returned by a subsequent call to list-iterator-next.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-next-index it)") 
	@Syntax("(list-iterator-next-index <iterator>)")
	public static final Operator iteratorNextIndex = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextIndex",
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorNextIndexSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			int index = it.nextIndex();
			return new LitInteger(index);			
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorNextIndexSymbol_out.toString();
		}
	};
	public static final Symbol iteratorPreviousSymbol = new Symbol("iterator-previous", NAMESPACE);
	public static final Symbol iteratorPreviousSymbol_out = new Symbol("list-iterator-previous");
	@VelkaOperator
	@Description("Returns the previous element in the list and moves the cursor position backwards.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-previous it)") 
	@Syntax("(list-iterator-previous <iterator>)")
	public static final Operator iteratorPrevious = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					ClojureHelper.applyClojureFunction(
							".previous",
							it));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorPreviousSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			return it.previous();
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					A);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorPreviousSymbol_out.toString();
		}
	};
	public static final Symbol iteratorPreviousIndexSymbol = new Symbol("previous-index", NAMESPACE);
	public static final Symbol iteratorPreviousIndexSymbol_out = new Symbol("list-iterator-previous-index");
	@VelkaOperator
	@Description("Returns the index of the element that would be returned by a subsequent call to list-iterator-previous.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-previous-index it)") 
	@Syntax("(list-iterator-previous-index <iterator>)")
	public static final Operator iteratorPreviousIndex = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".previousIndex",
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorPreviousIndexSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			int index = it.previousIndex();
			return new LitInteger(index);			
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorPreviousIndexSymbol_out.toString();
		}
	};
	public static final Symbol iteratorRemoveSymbol = new Symbol("iterator-remove", NAMESPACE);
	public static final Symbol iteratorRemoveSymbol_out = new Symbol("list-iterator-remove");
	@VelkaOperator
	@Description("Removes from the list the last element that was returned by list-iterator-next or list-iterator-next (optional operation).") 
	@Example("(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
			+ "(define it (java-list-iterator l 3))\n"
			+ "(list-iterator-next it)"
			+ "(list-iterator-next (list-iterator-remove it))") 
	@Syntax("(list-iterator-remove <iterator>)")
	public static final Operator iteratorRemove = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".remove",
											it),
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorRemoveSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			it.remove();
			
			return it_io;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator),
					TypeAtom.TypeListIterator);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorRemoveSymbol_out.toString();
		}
	};
	public static final Symbol iteratorSetSymbol = new Symbol("iterator-set", NAMESPACE);
	public static final Symbol iteratorSetSymbol_out = new Symbol("list-iterator-set");
	@VelkaOperator
	@Description("Replaces the last element returned by list-iterator-next or list-iterator-previous() with the specified element (optional operation).") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-next it)"
					+ "(list-iterator-set it 42)") 
	@Syntax("(list-iterator-remove <iterator> <element>)")
	public static final Operator iteratorSet = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String it = "_iterator";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(it, element),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											it,
											element),
									it)));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return iteratorSetSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject it_io = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ListIterator<Expression> it = (ListIterator<Expression>)it_io.javaObject;
			
			Expression e = args.get(1);
			
			it.set(e);
			
			return it_io;
		}		
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListIterator, A),
					TypeAtom.TypeListIterator);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return iteratorSetSymbol_out.toString();
		}
	};

	public static final Path PATH = Paths.get("velka", "clojure");

	public static final Path FILE_NAME = Paths.get("listIterator.clj");

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
		return FILE_NAME;
	}
	
	private static JavaListIterator instance = null;
	private JavaListIterator() {}
	public static JavaListIterator singleton() {
		if(instance == null) {
			instance = new JavaListIterator();
		}
		return instance;
	}

}
