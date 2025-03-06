package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.sun.codemodel.JExpr;

import velka.core.abstraction.Constructor;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.TypedObject;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.RomanNumbers;
import velka.util.annotations.Description;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains contructor operators
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("General constructors for basic Velka representations.") 
@Header("General Constructors")
public final class ConstructorOperators extends OperatorBank {
	
	/**
	 * Int:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor IntNativeConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg = (LitInteger) args.get(0);
			return arg;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "Int:Native";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return "identity";
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-int-native", ConstructorOperators.singleton().getNamespace());
		}
		
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeIntNative, 
					new TypeTuple(TypeAtom.TypeIntNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.stream().limit(1).findAny().get();
							if(o instanceof LitInteger li) {
								throw new RuntimeException("Invalid constructor");
							}
							return o;
						}
						
					});
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	
	};	
	
	/**
	 * Int:String constructor
	 */
	@VelkaConstructor
	@Description("Constructs Int:String from string.") 
	@Name("Construct Int String") 
	@Syntax("(construct Int String <string>)")
	public static Constructor IntStringConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return new LitComposite(arg, TypeAtom.TypeIntString);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntString);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "Int:String";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String arg = "_arg";
			return ClojureHelper.fnHelper(List.of(arg), LitComposite.clojureLit(TypeAtom.TypeIntString, arg));
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-int-string", ConstructorOperators.singleton().getNamespace());
		}

		@Override
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeIntString, 
					new TypeTuple(TypeAtom.TypeStringNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.iterator().next();
							if(o instanceof Expression e) {
							
								try {
									e = e.interpret((Environment) env);
								} catch (AppendableException e1) {
									throw new RuntimeException(e1);
								}
								
								if(e instanceof LitString ls) {
									return new LitComposite(ls, TypeAtom.TypeIntString);
								}
							}
							
							throw new RuntimeException("Invalid ctor argument");
						}
						
					});
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var o = mappedArgs.get(new Symbol("_0"));
			
			var _if = method.body()._if(o._instanceof(CodeModelInstance.instance()._ref(String.class)));
			_if._then()._return(JExpr._new(CodeModelInstance.instance().ref(TypedObject.class))
									.arg(o)
									.arg(TypeUtil.instance().type2java(TypeAtom.TypeIntString)));
			
			method.body()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(JExpr.lit("Invalid ctor argument")));
		}
	};
	/**
	 * Int:Roman constructor
	 */
	@VelkaConstructor
	@Description("Construct Int Roman from string.") 
	@Name("Construct Int Roman") 
	@Syntax("(construct Int Roman <string>)")
	public static Constructor IntRomanConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			
			if(!RomanNumbers.check(arg.value)) {
				throw new AppendableException(arg.value + " is not a valid roman number.");
			}
			
			return new LitComposite(arg, TypeAtom.TypeIntRoman);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeAtom.TypeIntRoman);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "Int:Roman";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String arg = "_arg";
			return ClojureHelper.fnHelper(List.of(arg),
					ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("velka.util.RomanNumbers/check", arg), 
							LitComposite.clojureLit(TypeAtom.TypeIntRoman, arg), 
							ClojureHelper.errorHelper(
									ClojureHelper.applyClojureFunction("str", arg, ClojureHelper.stringHelper(" is not a valid roman number.")))));
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-int-roman", ConstructorOperators.singleton().getNamespace());
		}

		@Override
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeIntRoman, 
					new TypeTuple(TypeAtom.TypeStringNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.stream().limit(1).findAny().get();
							
							if(o instanceof LitString ls) {
								if(!RomanNumbers.check(ls.value)) {
									throw new RuntimeException(ls.value + " is not a valid roman number.");
								}
								return new LitComposite(ls, TypeAtom.TypeIntRoman);
							}
							
							throw new RuntimeException("Invalid ctor argument");
						}
						
					});
			
		}
	
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var o = mappedArgs.get(new Symbol("_0"));
			
			var _if = method.body()._if(o._instanceof(CodeModelInstance.instance()._ref(String.class)));
			
			var _if2 = _if._then()._if(CodeModelInstance.instance().ref(RomanNumbers.class).staticInvoke("check").arg(o).not());
			_if2._then()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(o.plus(JExpr.lit(" is not a valid roman number."))));
			
			_if._then()._return(JExpr._new(CodeModelInstance.instance().ref(TypedObject.class))
									.arg(o)
									.arg(TypeUtil.instance().type2java(TypeAtom.TypeIntRoman)));
			
			method.body()._throw(JExpr._new(CodeModelInstance.instance().ref(RuntimeException.class)).arg(JExpr.lit("Invalid ctor argument")));
		}
	};
	
	/**
	 * String:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor StringNativeConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg = (LitString) args.get(0);
			return arg;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "String:Native";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return "identity";
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-string-native", ConstructorOperators.singleton().getNamespace());
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeStringNative, 
					new TypeTuple(TypeAtom.TypeStringNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.stream().limit(1).findAny().get();
							
							if(o instanceof LitString ls) {
								return new LitComposite(ls, TypeAtom.TypeStringNative);
							}
							
							throw new RuntimeException("Invalid ctor argument");
						}
						
					});
			
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	};
	
	/**
	 * Double:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor DoubleNativeConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitDouble arg = (LitDouble) args.get(0);
			return arg;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeDoubleNative)),
					TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "Double:Native";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return "identity";
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-double-native", ConstructorOperators.singleton().getNamespace());
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeDoubleNative, 
					new TypeTuple(TypeAtom.TypeDoubleNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.stream().limit(1).findAny().get();
							if(o instanceof LitDouble ld) {
								return ld;
							}
							throw new RuntimeException("Invalid ctor argument");
						}
						
					});
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	};
	
	/**
	 * Bool:Native constructor
	 */
	@VelkaConstructor(showInDoc = false)
	public static Constructor BoolNativeConstructor = new Constructor() {
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);
			return arg;
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	
		@Override
		public String toString() {
			return "Bool:Native";
		}
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return "identity";
		}
	
		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("velka-bool-native", ConstructorOperators.singleton().getNamespace());
		}
		
		@Override
		public void declareInTypeEnvironment(Environment env) throws AppendableException {
			env.getTypeSystem().addConstructor(
					TypeAtom.TypeBoolNative, 
					new TypeTuple(TypeAtom.TypeBoolNative), 
					new velka.util.IEvalueable() {

						@Override
						public Object evaluate(Collection<? extends Object> args, Object env) {
							var o = args.stream().limit(1).findAny().get();
							
							if(o instanceof LitBoolean lb) {
								return lb;
							}
							
							throw new RuntimeException("Invalid ctor argument");
						}
						
					});
			
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	};

	public static final Path VELKA_CLOJURE_CONSTRUCTORS_PATH = velka.core.util.Constants.LOCATION;

	public static final Path VELKA_CLOJURE_CONSTRUCTORS_NAME = Paths.get("constructors");
	
	private ConstructorOperators() {}
	private static ConstructorOperators instance = null;
	public static ConstructorOperators singleton() {
		if(instance == null) {
			instance = new ConstructorOperators();
		}
		return instance;
	}
	@Override
	protected String name() {
		return "constructors";
	}

}
