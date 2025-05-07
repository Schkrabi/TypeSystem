package velka.core.abstraction;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;
import com.sun.codemodel.JMod;

import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.VelkaThrower;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Expression for meta-language operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Operator extends Expression implements CompileableToJava, VelkaAbstraction {

	/** Creates clojure function for the operator */
	protected abstract String toClojureOperator(Environment env) throws AppendableException;
	
	/** Interprets the operator application */
	protected abstract Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException;
	
	/** Symbol used for operator in clojure */
	public abstract Symbol getInternalSymbol();
	
	/** Implements java code for the operator */
	protected abstract void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs);
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var _this = "_this";
		var _arg = "_arg";
		var _carg = "_carg";
		
		var type = (TypeArrow)this.getType();
		
		var bindings = new ArrayList<Pair<String, String>>();
		
		bindings.add(
				Pair.of(_carg,
						ClojureHelper.applyClojureFunction(
								".convert",
								ClojureCoreSymbols.typeSystem_full,
								ClojureHelper.applyClojureFunction(".getType", ClojureCoreSymbols.typeSystem_full, _arg),
								type.ltype.clojureTypeRepresentation(),
								_arg,
								"nil")));
		
		var applyCode = ClojureHelper.letHelper(
				ClojureHelper.applyClojureFunction("apply", this.toClojureOperator(env), _carg),
				bindings);
		
		var code = 
				ClojureHelper.reify(
					velka.types.typeSystem.VelkaAbstraction.class,
					Pair.of("apply", Pair.of(List.of(_this, _arg), applyCode)),
					Pair.of("getType", Pair.of(List.of(_this), type.clojureTypeRepresentation())));
		return code;
	}	
	
	@Override
	public Object apply(Collection<? extends Object> args) {
		var t = new Tuple(args.stream().map(o -> (Expression)o).toList());
		var env = TopLevelEnvironment.instantiate();
		Pair<Type, Substitution> tinf;
		try {
			tinf = t.infer(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var conv = env.getTypeSystem().convert(tinf.first, ((TypeArrow)this.getType()).ltype, t, env);
		var targ = (Tuple)conv;
		try {
			return this.doSubstituteAndEvaluate(targ, env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
	}
	
	@Override
	public Type getType() {
		try {
			return this.infer(null).first;
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
	}	
	
	/**
	 * Makes code for defining conversion in clojure header
	 * @return code
	 */
	public String clojureDef() {
		Environment env = TopLevelEnvironment.instantiate();
		try {
			return this.toClojureCode(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	public static String makeOperatorDeclaration(Operator operator) {
		return ClojureHelper.makeDeclaration(operator.getInternalSymbol().name);
	}

	/**
	 * Creates definitions for operator 
	 * 
	 * @param operator defined operator
	 * @param env environment
	 * @param typeEnvtype environment
	 * @return string with code
	 * @throws AppendableException
	 */
	public static String makeOperatorDef(Operator operator, Environment env) 
		throws AppendableException {
		var code = ClojureHelper.applyClojureFunction("def", operator.getInternalSymbol().name,
				operator.toClojureCode(env));
		
		return code;
	}
	
	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		throw new RuntimeException("doConvert not implemented in Operator");
	}
	
	@Override
	public JExpression toJavaExpr(Environment env) {
		TypeArrow type;
		try {
			type = (TypeArrow)this.infer(env).first;
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var argType = (TypeTuple)type.ltype;
		var retType = type.rtype;
		
		var aClass = CodeModelInstance.instance().anonymousClass(VelkaAbstraction.class);
		var fType = aClass.field(JMod.PRIVATE | JMod.FINAL, Type.class, "_type", 
				TypeUtil.instance().type2java(type));
		
		aClass.method(JMod.PUBLIC, Type.class, "getType").body()
			._return(fType);
		
		var apply = aClass.method(JMod.PUBLIC, Object.class, "apply");
		var argmap = Abstraction.convertAndDeclareParms(
				Stream.iterate(0, x -> x + 1).map(x -> Pair.of(new Symbol("_" + Integer.toString(x)), argType.get(x)))
						.limit(argType.size()).toList(), 
				apply);
		
		this.modifyJavaMethod(apply, argmap);
		
		return JExpr._new(aClass);
	}
	
	public static Operator wrapJavaMethod(Class<?> clazz, String methodName, String velkaName, String namespace, Class<?> ...parameters) {
		try {
			var mthd = clazz.getMethod(methodName, parameters);
			return wrapJavaMethod(clazz, mthd, velkaName, namespace);
		} catch (NoSuchMethodException | SecurityException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static Operator wrapJavaMethod(Class<?> clazz, Method method, String velkaName, String namespace) {
		var op = new Operator() {

			@Override
			protected String toClojureOperator(Environment env) throws AppendableException {
				if(method.getReturnType().equals(void.class)) {
					return ClojureHelper.wrapVoidClojureOperatorToFn(method.getParameterCount() + 1, "." + method.getName());
				}
				return ClojureHelper.wrapClojureOperatorToFn(method.getParameterCount() + 1, "." + method.getName());
			}

			@Override
			public Symbol getInternalSymbol() {
				return new Symbol("velka_" + velkaName.replace('-', '_'), namespace);
			}

			@Override
			protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
					throws AppendableException {
				var jargs = new Object[args.size() - 1];
				Object instance = null;
				int i = 0;
				for(var a : args) {
					if(instance == null) {
						instance = Literal.literalToObject(a);
					}
					else {
						if (a instanceof Literal) {
							jargs[i] = Literal.literalToObject(a);
						}
						else {
							jargs[i] = a;
						}
						i++;
					}
				}
				
				Object jrslt = null;
				try {
					jrslt = method.invoke(instance, jargs);
				} catch (IllegalAccessException | InvocationTargetException | RuntimeException e ) {
					throw new RuntimeException(e);
				}
				
				if(jrslt instanceof Expression expr) {
					return expr;
				}
				var rslt = Literal.objectToLiteral(jrslt);
				return rslt;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				var as = new java.util.LinkedList<Type>();
				as.add(TypeAtom.javaClassToType(clazz));
				
				for(var c : method.getParameterTypes()) {
					as.add(TypeAtom.javaClassToType(c));
				}
				
				var ret = TypeAtom.javaClassToType(method.getReturnType());
				
				var t = new TypeArrow(new TypeTuple(as), ret);
				
				return Pair.of(t, Substitution.EMPTY);
			}
			
			@Override
			public String toString() {
				return velkaName;
			}
			
			@Override
			protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
				this.wrapNaryMethod(_method, method, mappedArgs, method.getParameterCount());
			}
		};
		
		return op;
	}
	
	public static Operator wrapNullableJavaMethod(Class<?> clazz, String methodName, String velkaName, String namespace, Class<?> ...parameters) {
		try {
			var mthd = clazz.getMethod(methodName, parameters);
			return wrapNullableJavaMethod(clazz, mthd, velkaName, namespace);
		} catch (NoSuchMethodException | SecurityException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static Operator wrapNullableJavaMethod(Class<?> clazz, Method method, String velkaName, String namespace) {
		var op = new Operator() {

			@Override
			protected String toClojureOperator(Environment env) throws AppendableException {
				if(method.getReturnType().equals(void.class)) {
					throw new RuntimeException("Method returning void cannot be nullable!");
				}
				return ClojureHelper.wrapNullableClojureOperatorToFn(method.getParameterCount() + 1, "." + method.getName());
			}

			@Override
			public Symbol getInternalSymbol() {
				return new Symbol("velka_" + velkaName.replace('-', '_'), namespace);
			}

			@Override
			protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
					throws AppendableException {
				var jargs = new Object[args.size() - 1];
				Object instance = null;
				int i = 0;
				for(var a : args) {
					if(instance == null) {
						instance = Literal.literalToObject(a);
					}
					else {
						if (a instanceof Literal) {
							jargs[i] = Literal.literalToObject(a);
						}
						else {
							jargs[i] = a;
						}
						i++;
					}
				}
				
				Object jrslt = null;
				try {
					jrslt = method.invoke(instance, jargs);
					
					if(		jrslt == null
						&& 	!method.getReturnType().equals(void.class)) {
						throw new RuntimeException("Wrapped null exception");
					}
				} catch (IllegalAccessException | InvocationTargetException | RuntimeException e ) {
					throw new UserException(e.getLocalizedMessage());
				}
				
				var rslt = Literal.objectToLiteral(jrslt);
				return rslt;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				var as = new java.util.LinkedList<Type>();
				as.add(TypeAtom.javaClassToType(clazz));
				
				for(var c : method.getParameterTypes()) {
					as.add(TypeAtom.javaClassToType(c));
				}
				
				var ret = TypeAtom.javaClassToType(method.getReturnType());
				
				var t = new TypeArrow(new TypeTuple(as), ret);
				
				return Pair.of(t, Substitution.EMPTY);
			}
			
			@Override
			public String toString() {
				return velkaName;
			}
			
			@Override
			protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
				this.wrapNullableNaryMethod(_method, method, mappedArgs, method.getParameterCount());
			}
		};
		
		return op;
	}
	
	protected void wrapNaryMethod(com.sun.codemodel.JMethod method, Method called, Map<Symbol, com.sun.codemodel.JVar> mappedArgs, int numOfArgs) {
		var invocation = mappedArgs.get(new Symbol("_0")).invoke(called.getName());
		
		TypeArrow type = null;
		try {
			type = ((TypeArrow)this.infer(null).first);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var argType = (TypeTuple)type.ltype;
		
		for(int i = 1; i < numOfArgs + 1; i++) {			
			if(argType.get(i).equals(TypeAtom.TypeIntNative)) {
				invocation.arg(mappedArgs.get(new Symbol("_" + i)).invoke("intValue"));
			}
			else {
				invocation.arg(mappedArgs.get(new Symbol("_" + i)));
			}
		}		
		
		if(type.rtype.equals(TypeAtom.TypeListNative)
				&& called.getReturnType().isArray()) {
			invocation = CodeModelInstance.instance().ref(List.class).staticInvoke("of").arg(invocation);
		}
		
		if(called.getReturnType().equals(void.class)) {
			method.body().add(invocation);
			method.body()._return(CodeModelInstance.emptyExpression());
		}
		else {
			var ret = method.body().decl(CodeModelInstance.instance().ref(Object.class), "ret", invocation);
			method.body()._return(ret);
		}
	}
	
	protected void wrapNullableNaryMethod(com.sun.codemodel.JMethod method, Method called, Map<Symbol, com.sun.codemodel.JVar> mappedArgs, int numOfArgs) {
		var invocation = mappedArgs.get(new Symbol("_0")).invoke(called.getName());
		
		TypeArrow type = null;
		try {
			type = ((TypeArrow)this.infer(null).first);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var argType = (TypeTuple)type.ltype;
		
		for(int i = 1; i < numOfArgs + 1; i++) {			
			if(argType.get(i).equals(TypeAtom.TypeIntNative)) {
				invocation.arg(mappedArgs.get(new Symbol("_" + i)).invoke("intValue"));
			}
			else {
				invocation.arg(mappedArgs.get(new Symbol("_" + i)));
			}
		}		
		
		if(type.rtype.equals(TypeAtom.TypeIntNative)) {
			invocation = CodeModelInstance.instance().ref(Integer.class).staticInvoke("valueOf").arg(invocation);
		}
		else if(type.rtype.equals(TypeAtom.TypeListNative)) {
			invocation = CodeModelInstance.instance().ref(List.class).staticInvoke("of").arg(invocation);
		}
		
		if(called.getReturnType().equals(void.class)) {
			method.body().add(invocation);
			method.body()._return(CodeModelInstance.emptyExpression());
		}
		else {
			var ret = method.body().decl(CodeModelInstance.instance().ref(Object.class), "ret", invocation);
			method.body()._if(ret.eq(JExpr._null()))
				._then().add(VelkaThrower._throw(JExpr.lit("Wrapped null exception")));
			method.body()._return(ret);
		}
	}
}
