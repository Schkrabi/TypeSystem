package velka.core.langbase;

import java.util.ListIterator;

import velka.core.abstraction.Operator;
import velka.core.expression.Symbol;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaOperator;

public class JavaListIterator extends OperatorBank {
	
	/**
	 * Type name for list iterator
	 */
	public static final Symbol iteratorAddSymbol = new Symbol("iterator_add", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorAddSymbol_out = new Symbol("list-iterator-add");
	@VelkaOperator
	@Description("Inserts the specified element into the list (optional operation).") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-add it 42)") 
	@Syntax("(list-iterator-add <list-iterator> <element>)")
	public static final Operator iteratorAdd = Operator.wrapJavaMethod(ListIterator.class, "add", "list-iterator-add",
			JavaListIterator.singleton().getNamespace(), Object.class);
	
	
	public static final Symbol iteratorHasNextSymbol = new Symbol("has_next", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorHasNextSymbol_out = new Symbol("list-iterator-has-next");
	@VelkaOperator
	@Description("Returns true if this list iterator has more elements when traversing the list in the forward direction.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-has-next it)") 
	@Syntax("(list-iterator-has-next <list iterator>)")
	public static final Operator iteratorHasNext = Operator.wrapJavaMethod(ListIterator.class, "hasNext", "list-iterator-has-next",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorHasPreviousSymbol = new Symbol("has_previous", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorHasPreviousSymbol_out = new Symbol("list-iterator-has-previous");
	@VelkaOperator
	@Description("Returns true if this list iterator has more elements when traversing the list in the reverse direction.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-has-previous it)") 
	@Syntax("list-iterator-has-previous <iterator>)")
	public static final Operator iteratorHasPrevious = Operator.wrapJavaMethod(ListIterator.class, "hasPrevious", "list-iterator-has-previous",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorNextSymbol = new Symbol("iterator_next", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorNextSymbol_out = new Symbol("list-iterator-next");
	@VelkaOperator
	@Description("Returns the next element in the list and advances the cursor position.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-next it)") 
	@Syntax("(list-iterator-next <iterator>)")
	public static final Operator iteratorNext = Operator.wrapNullableJavaMethod(ListIterator.class, "next", "list-iterator-next",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorNextIndexSymbol = new Symbol("next_index", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorNextIndexSymbol_out = new Symbol("list-iterator-next-index");
	@VelkaOperator
	@Description("Returns the index of the element that would be returned by a subsequent call to list-iterator-next.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 0))\n"
					+ "(list-iterator-next-index it)") 
	@Syntax("(list-iterator-next-index <iterator>)")
	public static final Operator iteratorNextIndex = Operator.wrapJavaMethod(ListIterator.class, "nextIndex", "list-iterator-next-index",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorPreviousSymbol = new Symbol("iterator_previous", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorPreviousSymbol_out = new Symbol("list-iterator-previous");
	@VelkaOperator
	@Description("Returns the previous element in the list and moves the cursor position backwards.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-previous it)") 
	@Syntax("(list-iterator-previous <iterator>)")
	public static final Operator iteratorPrevious = Operator.wrapNullableJavaMethod(ListIterator.class, "previous", "list-iterator-previous",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorPreviousIndexSymbol = new Symbol("previous_index", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorPreviousIndexSymbol_out = new Symbol("list-iterator-previous-index");
	@VelkaOperator
	@Description("Returns the index of the element that would be returned by a subsequent call to list-iterator-previous.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-previous-index it)") 
	@Syntax("(list-iterator-previous-index <iterator>)")
	public static final Operator iteratorPreviousIndex = Operator.wrapJavaMethod(ListIterator.class, "previousIndex", "list-iterator-previous-index",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorRemoveSymbol = new Symbol("iterator_remove", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorRemoveSymbol_out = new Symbol("list-iterator-remove");
	@VelkaOperator
	@Description("Removes from the list the last element that was returned by list-iterator-next or list-iterator-next (optional operation).") 
	@Example("(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
			+ "(define it (java-list-iterator l 3))\n"
			+ "(list-iterator-next it)"
			+ "(list-iterator-next (list-iterator-remove it))") 
	@Syntax("(list-iterator-remove <iterator>)")
	public static final Operator iteratorRemove = Operator.wrapJavaMethod(ListIterator.class, "remove", "list-iterator-remove",
			JavaListIterator.singleton().getNamespace());
	
	public static final Symbol iteratorSetSymbol = new Symbol("iterator_set", JavaListIterator.singleton().getNamespace());
	public static final Symbol iteratorSetSymbol_out = new Symbol("list-iterator-set");
	@VelkaOperator
	@Description("Replaces the last element returned by list-iterator-next or list-iterator-previous() with the specified element (optional operation).") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(define it (java-list-iterator l 3))\n"
					+ "(list-iterator-next it)"
					+ "(list-iterator-set it 42)") 
	@Syntax("(list-iterator-remove <iterator> <element>)")
	public static final Operator iteratorSet = Operator.wrapJavaMethod(ListIterator.class, "set", "list-iterator-set",
			JavaListIterator.singleton().getNamespace(), Object.class);
	
	private static JavaListIterator instance = null;
	private JavaListIterator() {}
	public static JavaListIterator singleton() {
		if(instance == null) {
			instance = new JavaListIterator();
		}
		return instance;
	}

	@Override
	protected String name() {
		return "listIterator";
	}

}
