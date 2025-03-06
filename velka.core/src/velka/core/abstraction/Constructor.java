package velka.core.abstraction;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;
import com.sun.codemodel.JMod;

import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.core.util.DeclarableInTypeEnvironment;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Class for constructors
 * 
 * @author r.skrabal
 *
 */
public abstract class Constructor extends Operator implements DeclarableInTypeEnvironment {

	/**
	 * Declares this constructor in TypeEnvironment
	 * 
	 * @remark Most operators use trivial infer function, returing a constant type
	 *         and empty substituion. This is exploited by default implementation of
	 *         Constructor, counting on fact that env and typeEnv will never be used
	 *         on infer call. If inference of constructor is not trivial, this
	 *         method should be overriden.
	 * @param typeEnv where is delcared
	 * @throws AppendableException
	 */
	public void declareInTypeEnvironment(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env);
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom constructed = (TypeAtom)ta.rtype;
		final var me = this;
		
		env.getTypeSystem().addConstructor(
				constructed, 
				(TypeTuple)ta.ltype,
				new velka.util.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>();
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var apl = new AbstractionApplication(me, new Tuple(eargs));
						try {
							var eenv = (Environment)env;
							return apl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
					
		});
	}

	@Override
	public String toClojureCode(Environment env) {
		Pair<Type, Substitution> p;
		try {
			p = this.infer(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom constructed = (TypeAtom)ta.rtype;
		
		final var arg = "_arg";
		final var rhis = "_this";
		final var cenv = "_env";
		String code;
		try {
			code = ClojureHelper.applyClojureFunction(".addConstructor", 
					ClojureCoreSymbols.typeSystem_full,
					constructed.clojureTypeRepresentation(),
					ta.ltype.clojureTypeRepresentation(),
					ClojureHelper.reify(velka.util.IEvalueable.class, 
							Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), 
									ClojureHelper.applyVelkaFunction_argsTuple(super.toClojureCode(env), 
											arg)))));
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}				
		
		return code;
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
		
		var ieval = CodeModelInstance.instance().anonymousClass(velka.util.IEvalueable.class);
		var eval = ieval.method(JMod.PUBLIC, Object.class, "evaluate");
		
		var argmap = Abstraction.convertAndDeclareParms(
				Stream.iterate(0, x -> x + 1).map(x -> Pair.of(new Symbol("_" + Integer.toString(x)), argType.get(x)))
						.limit(argType.size()).toList(),
				eval);
		
		eval.param(Object.class, "env");
		
		this.modifyJavaMethod(eval, argmap);
		
		var jexpr = 
				JavaTypeSystem.codeInstance().invoke("addConstructor")
					.arg(TypeUtil.instance().type2java(retType))
					.arg(TypeUtil.instance().type2java(argType))
					.arg(JExpr._new(ieval));
		return jexpr;
	}
	
	public static Constructor wrapJavaContructorToType(Class<?> clazz, String namespace, Type ctype, Class<?> ...parameters) {
		java.lang.reflect.Constructor<?> ctor;
		try {
			ctor = clazz.getConstructor(parameters);
		} catch (NoSuchMethodException | SecurityException e) {
			throw new RuntimeException(e);
		}
		return wrapJavaConstructorCtorToType(clazz, ctor, namespace, ctype);
	}
	
	public static Constructor wrapJavaConstructor(Class<?> clazz, String namespace, Class<?> ...parameters) {
		java.lang.reflect.Constructor<?> ctor;
		try {
			ctor = clazz.getConstructor(parameters);
		} catch (NoSuchMethodException | SecurityException e) {
			throw new RuntimeException(e);
		}
		return wrapJavaConstructorCtor(clazz, ctor, namespace);
	}
	
	public static Constructor wrapJavaConstructorCtor(Class<?> clazz, java.lang.reflect.Constructor<?> ctor, String namespace) {
		var ctype = TypeAtom.javaClassToType(clazz);
		return wrapJavaConstructorCtorToType(clazz, ctor, namespace, ctype);
	}
	
	public static Constructor wrapJavaConstructorCtorToType(Class<?> clazz, java.lang.reflect.Constructor<?> ctor, String namespace, 
			Type ctype) {		
		var op = new Constructor() {

			@Override
			protected String toClojureOperator(Environment env) throws AppendableException {
				return ClojureHelper.wrapClojureOperatorToFn(ctor.getParameterCount(), clazz.getName() + ".");
			}

			@Override
			public Symbol getInternalSymbol() {
				var sb = new StringBuilder("velka_ctor")
						.append(clazz.getName())
						.append("_");
				
				for(var c : ctor.getParameterTypes()) {
					sb.append(c.getName());
				}
				
				return new Symbol(sb.toString(), namespace);
			}

			@Override
			protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
				var jargs = new Object[args.size()];
				
				int i = 0;
				for(var a : args) {
					if (a instanceof Literal) {
						jargs[i] = Literal.literalToObject(a);
					}
					else {
						jargs[i] = a;
					}
					i++;
				}
				
				Object jrslt = null;
				
				try {
					jrslt = ctor.newInstance(jargs);
				} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
						| InvocationTargetException e) {
					throw new RuntimeException(e);
				}
				
				return new LitInteropObject(jrslt, ctype);
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				var as = new java.util.LinkedList<Type>();
				
				for(var c : ctor.getParameterTypes()) {
					as.add(TypeAtom.javaClassToType(c));
				}
				
				
				var t = new TypeArrow(new TypeTuple(as), ctype);
				
				return Pair.of(t, Substitution.EMPTY);
			}
		
			@Override
			protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
				var _new = JExpr._new(CodeModelInstance.instance().ref(clazz));
				
				int i = 0;
				for(var p : ctor.getParameterTypes()) {
					if(p.equals(int.class)) {
						_new.arg(mappedArgs.get(new Symbol("_" + i)).invoke("intValue"));
					}
					else {
						_new.arg(mappedArgs.get(new Symbol("_" + i)));
					}
					
					i++;
				}
				
				_method.body()._return(_new);
			}
		};
		
		return op;
	}
}
