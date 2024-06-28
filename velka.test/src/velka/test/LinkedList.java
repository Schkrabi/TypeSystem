 package velka.test;

class LinkedList extends VelkaTest {
	
//	@Test
//	void constructor() throws Exception {
//		this.assertInterpretationEquals("(construct List:JavaLinked)",
//				new LitInteropObject(new java.util.LinkedList<Object>(), JavaLinkedList.TypeListJavaLinked));
//		this.assertIntprtAndCompPrintSameValues("(construct List:JavaLinked)");
//	}
//
//	@Test
//	void addToEnd() throws Exception {
//		this.assertInterpretationEquals(
//				"(" + JavaLinkedList.addToEndSymbol_out.toString() + " (construct List:JavaLinked) 42)", LitBoolean.TRUE);
//		
//		this.assertIntprtAndCompPrintSameValues("(println (" + JavaLinkedList.addToEndSymbol_out.toString() + " (construct List:JavaLinked) 42))");
//	}
//	
//	@Test
//	void addToIndex() throws Exception {
//		this.assertInterpretationEquals(
//				"(" + JavaLinkedList.addToIndexSymbol_out.toString() + " (construct List:JavaLinked) 0 42)",
//				Expression.EMPTY_EXPRESSION);
//		this.assertIntprtAndCompPrintSameValues("(println (" + JavaLinkedList.addToIndexSymbol_out.toString() + " (construct List:JavaLinked) 0 42))");
//	}
//	
//	@Test
//	void addAll() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.addAllSymbol_out + " l1 l2)",
//				LitBoolean.TRUE);
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(println (" + JavaLinkedList.addAllSymbol_out + " l1 l2))");
//	}
//	
//	@Test
//	void toStr() throws Exception {
//		assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.toStrSymbol_out + " l1)",
//				new LitString("[42, 42]"));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//				+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//				+ "(println (java-linked-list-to-str l))");	
//	}
//	
//	@Test 
//	void contains() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.containsSymbol_out + " l1 42)",
//				LitBoolean.TRUE);
//		
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.containsSymbol_out + " l1 84)",
//				LitBoolean.FALSE);
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)"
//				+ "(println (" + JavaLinkedList.containsSymbol_out + " l1 42))"
//				+ "(println (" + JavaLinkedList.containsSymbol_out + " l1 84))");
//	}
//	
//	@Test
//	void containsAll() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
//				LitBoolean.TRUE);
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 42)"
//				+ "(" + JavaLinkedList.containsAllSymbol_out + " l1 l2)",
//				LitBoolean.FALSE);
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(println (" + JavaLinkedList.containsAllSymbol_out + " l1 l2))"
//				+ "(println (" + JavaLinkedList.containsAllSymbol_out + " l2 l1))");
//	}
//	
//	@Test
//	void get() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.getSymbol_out + " l1 0)",
//				new LitInteger(1));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(println (" + JavaLinkedList.getSymbol_out + " l1 0))");
//	}
//	
//	@Test
//	void indexOf() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 1)",
//				new LitInteger(0));
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.indexOfSymbol_out + " l1 42)",
//				new LitInteger(-1));
//		
//		this.assertIntprtAndCompPrintSameValues(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.indexOfSymbol_out + " l1 1))"
//				+ "(println (" + JavaLinkedList.indexOfSymbol_out + " l1 42))");
//	}
//	
//	@Test 
//	void isEmpty() throws Exception {
//		this.assertInterpretationEquals(
//				"(" + JavaLinkedList.isEmptySymbol_out + " (construct List:JavaLinked))", 
//				LitBoolean.TRUE);
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.isEmptySymbol_out + " l1)", 
//				LitBoolean.FALSE);
//		
//		this.assertIntprtAndCompPrintSameValues(
//				"(println (" + JavaLinkedList.isEmptySymbol_out + " (construct List:JavaLinked)))");
//		this.assertIntprtAndCompPrintSameValues(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.isEmptySymbol_out + " l1))");
//	}
//	
//	@Test
//	void lastIndexOf() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 1)",
//				new LitInteger(0));
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.lastIndexOfSymbol_out + " l1 42)",
//				new LitInteger(-1));
//		
//		this.assertIntprtAndCompPrintSameValues(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.lastIndexOfSymbol_out + " l1 1))"
//				+ "(println (" + JavaLinkedList.lastIndexOfSymbol_out + " l1 42))");
//	}
//	
//	@Test
//	void remove() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.removeSymbol_out + " l1 2)", 
//				LitBoolean.TRUE);
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.removeSymbol_out + " l1 2))");
//	}
//	
//	@Test
//	void removeAll() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.removeAllSymbol_out + " l1 l2)",
//				LitBoolean.TRUE);
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 4)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 5)"
//				+ "(" + JavaLinkedList.removeAllSymbol_out + " l2 l1)",
//				LitBoolean.FALSE);
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(println (" + JavaLinkedList.removeAllSymbol_out + " l1 l2))"
//				+ "(println (" + JavaLinkedList.removeAllSymbol_out + " l2 l1))");
//	}
//	
//	@Test
//	void retainAll() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.retainAllSymbol_out + " l1 l2)",
//				LitBoolean.TRUE);
//		
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.retainAllSymbol_out + " l2 l1)",
//				LitBoolean.FALSE);
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(println (" + JavaLinkedList.retainAllSymbol_out + " l1 l2))"
//				+ "(println (" + JavaLinkedList.retainAllSymbol_out + " l2 l1))");
//	}
//	
//	@Test
//	void set() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.setSymbol_out + " l1 0 2)", 
//				new LitInteger(1));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.setSymbol_out + " l1 0 2))");
//	}
//	
//	@Test
//	void size() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.sizeSymbol_out + " l1)", 
//				new LitInteger(2));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(println (" + JavaLinkedList.sizeSymbol_out + " l1))");
//	}
//	
//	@Test
//	void sublist() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 84)" 
//				+ "(" + JavaLinkedList.sublistSymbol_out + " l1 0 1)", 
//				new LitInteropObject(new java.util.LinkedList<Object>(List.of(new LitInteger(42))), JavaLinkedList.TypeListJavaLinked));
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 84)" 
//				+ "(define l2 (" + JavaLinkedList.sublistSymbol_out + " l1 0 1))"
//				+ "(println (" + JavaLinkedList.getSymbol_out + " l2 0))");
//	}
//	
//	@Test
//	void map() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"  
//				+ "(" + JavaLinkedList.mapSymbol_out + " l1 (lambda (x) (* 2 x)))", 
//				new LitInteropObject(new java.util.LinkedList<Object>(List.of(new LitInteger(42))), JavaLinkedList.TypeListJavaLinked));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 84)" 
//				+ "(define l2 (" + JavaLinkedList.mapSymbol_out + " l1 (lambda (x) (* x 2))))"
//				+ "(println (" + JavaLinkedList.getSymbol_out + " l2 0))");
//	}
//	
//	@Test
//	void map2() throws Exception {
//		this.assertInterpretationEquals("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 21)"
//				+ "(" + JavaLinkedList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y)))",
//				JavaLinkedList.of(new LitInteger(42)));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n"
//				+ "(define l2 (construct List:JavaLinked))"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 3)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l2 1)"
//				+ "(define l3 (" + JavaLinkedList.map2Symbol_out + " l1 l2 (lambda (x y) (+ x y))))"
//				+ "(println (" + JavaLinkedList.getSymbol_out + " l3 0))");
//	}
//	
//	@Test 
//	void foldl() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
//				+ "(" + JavaLinkedList.foldlSymbol_out + " + 0 l1)", 
//				new LitInteger(63));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 21)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 42)" 
//				+ "(println (" + JavaLinkedList.foldlSymbol_out + " + 0 l1))");
//	}
//	
//	@Test
//	void foldr() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 4)"
//				+ "(" + JavaLinkedList.foldrSymbol_out + " / 16 l1)", 
//				new LitInteger(2));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l1 (construct List:JavaLinked))\n" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 1)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 2)" 
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l1 4)"
//				+ "(println (" + JavaLinkedList.foldrSymbol_out + " / 16 l1))");
//	}
//	
//	@Test
//	void convertToArrayList() throws Exception {
//		this.assertInterpretationEquals("(define l (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
//				+ "(convert List:JavaLinked List:JavaArray l)", 
//				JavaArrayList.of(new LitInteger(42), new LitInteger(21)));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
//				+ "(println (" + JavaArrayList.getSymbol_out + " (convert List:JavaLinked List:JavaArray l) 0))");
//	}
//	
//	@Test
//	void convertToListNative() throws Exception {
//		this.assertInterpretationEquals("(define l (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
//				+ "(convert List:JavaLinked List:Native l)", 
//				ListNative.of(new LitInteger(42), new LitInteger(21)));
//		
//		this.assertIntprtAndCompPrintSameValues("(define l (construct List:JavaLinked))\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 42)\n"
//				+ "(" + JavaLinkedList.addToEndSymbol_out + " l 21)\n"
//				+ "(println (" + JavaLinkedList.getSymbol_out + " l 0))");
//	}
//	
//	@Test
//	void everyp() throws Exception {
//		this.assertInterpretationEquals(
//				"(java-linked-list-everyp (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))",
//				LitBoolean.FALSE);
//		this.assertInterpretationEquals(
//				"(java-linked-list-everyp (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))",
//				LitBoolean.TRUE);
//		
//		this.assertIntprtAndCompPrintSameValues("(java-linked-list-everyp (construct List:Native #t (construct List:Native #t (construct List:Native))) (lambda (x) x))");
//		this.assertIntprtAndCompPrintSameValues("(java-linked-list-everyp (construct List:Native #t (construct List:Native #f (construct List:Native))) (lambda (x) x))");
//	}
//	
//	@Test
//	void itNext() throws Exception {
//		var ll = Stream.iterate(new LitInteger(0), x -> new LitInteger(x.value + 2)).limit(10).collect(Collectors.toList());
//		var li = ll.iterator();
//		this.assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))"
//						+ "(list-iterator-next (java-linked-list-iterator l 0))",
//				li.next());
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))"
//						+ "(println (list-iterator-next (java-linked-list-iterator l 0)))");
//	}
//	
//	@Test
//	void itAdd() throws Exception {
//		this.assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(list-iterator-next (list-iterator-add it 42))"
//						+ "(java-linked-list-to-str l)",
//				new LitString("[42, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18]"));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(println (list-iterator-next (list-iterator-add it 42)))"
//						+ "(println (java-linked-list-to-str l))");
//	}
//	
//	@Test
//	void itHasNext() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(list-iterator-has-next it)",
//				LitBoolean.TRUE);
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(println (list-iterator-has-next it))");
//	}
//	
//	@Test
//	void itHasPrevious() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(list-iterator-has-previous it)",
//				LitBoolean.FALSE);
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(println (list-iterator-has-previous it))");
//	}
//	
//	@Test
//	void itNextIndex() throws Exception {
//		var ll = Stream.iterate(new LitInteger(0), x -> new LitInteger(x.value + 2)).limit(10).collect(Collectors.toList());
//		var li = ll.listIterator(0);
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(list-iterator-next-index it)",
//				new LitInteger(li.nextIndex()));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 0))\n"
//						+ "(println (list-iterator-next-index it))");
//	}
//	
//	@Test
//	void itPrev() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-previous it)",
//				new LitInteger(4));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(println (list-iterator-previous it))");
//	}
//	
//	@Test
//	void itPrevIndex() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-previous-index it)",
//				new LitInteger(2));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(println (list-iterator-previous-index it))");
//	}
//	
//	@Test
//	void itRemove() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-next it)"
//						+ "(list-iterator-next (list-iterator-remove it))"
//						+ "(java-linked-list-to-str l)",
//				new LitString("[0, 2, 4, 8, 10, 12, 14, 16, 18]"));
//		
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-next it)"
//						+ "(println (list-iterator-next (list-iterator-remove it)))"
//						+ "(println (java-linked-list-to-str l))");
//	}
//	
//	void itSet() throws Exception {
//		assertInterpretationEquals(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-next it)"
//						+ "(list-iterator-next (list-iterator-set it 42))",
//				new LitInteger(42));
//		assertIntprtAndCompPrintSameValues(
//				"(define l (construct List:JavaLinked))\n"
//						+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
//						+ "(define it (java-linked-list-iterator l 3))\n"
//						+ "(list-iterator-next it)"
//						+ "(println (list-iterator-next (list-iterator-set it 42)))"
//						+ "(println (java-linked-list-to-str l))");
//	}
}
