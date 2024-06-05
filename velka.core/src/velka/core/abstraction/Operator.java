package velka.core.abstraction;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Optional;
import java.util.stream.Collectors;

import velka.core.application.AbstractionApplication;
import velka.core.application.Convert;
import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.core.literal.Literal;
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

/**
 * Expression for meta-language operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Operator extends Abstraction {

	/**
	 * Creates clojure function for the operator
	 * @param env environment
	 * @param typeEnv type environment
	 * @return clojure code
	 * @throws AppendableException
	 */
	protected abstract String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	/**
	 * Symbol used for operator in clojure
	 * @return fully qualified symbol
	 */
	public abstract Symbol getClojureSymbol();
	
	/**
	 * Makes code for defining conversion in clojure header
	 * @return code
	 */
	public String clojureDef() {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv;
		try {
			typeEnv = TypeEnvironment.initBasicTypes(env);
			return this.toClojureCode(env, typeEnv);
		} catch (AppendableException e) {
			e.printStackTrace();
		}
		return "";
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		return this.toClojureOperator(env, typeEnv);
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException{
		return this.infer(env, typeEnv);
	}

	public static String makeOperatorDeclaration(Operator operator) {
		return ClojureHelper.makeDeclaration(operator.getClojureSymbol().name);
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
	public static String makeOperatorDef(Operator operator, Environment env, TypeEnvironment typeEnv) 
		throws AppendableException {
		return Abstraction.makeLambdaDef(operator.getClojureSymbol().name, operator, env, typeEnv);
	}
	
	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		if(!(to instanceof TypeArrow)) {
			throw new ConversionException(to, this);
		}
		TypeArrow to_typeArrow = (TypeArrow)to;
		TypeArrow from_typeArrow = (TypeArrow)from;
		
		Optional<Substitution> o = Type.unifyTypes(from_typeArrow.ltype, to_typeArrow.ltype);
		if(o.isEmpty()) {
			throw new ConversionException(to, this);
		}
		
		TypeTuple to_ltype_typeTuple = (TypeTuple)to_typeArrow.ltype;
		Tuple formalArgs = new Tuple(to_ltype_typeTuple.stream().map(x -> new Symbol(NameGenerator.next())));
		Expression convertedArgs = null;
		
		if(from_typeArrow.ltype instanceof TypeVariable) {			
			convertedArgs = formalArgs;
		}
		else {
			convertedArgs = new Convert(to_typeArrow.ltype, from_typeArrow.ltype, formalArgs);
		}
		
		Expression convertedBody = 
				new Convert(
						from_typeArrow.rtype,
						to_typeArrow.rtype,
						new AbstractionApplication(
								this,
								convertedArgs));
		
		TypeTuple convertedArgsType =
				(TypeTuple)to_typeArrow.ltype.apply(o.get());
								
		Lambda lambda = new Lambda(
				formalArgs,
				convertedArgsType,
				convertedBody);
		
		return lambda;
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
			protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
				return ClojureHelper.wrapClojureOperatorToFn(method.getParameterCount() + 1, "." + method.getName());
			}

			@Override
			public Symbol getClojureSymbol() {
				return new Symbol("velka-" + method.getName(), namespace);
			}

			@Override
			protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
					throws AppendableException {
				var jargs = new Object[args.size() - 1];
				Object instance = null;
				int i = 0;
				for(var a : args) {
					if(instance == null) {
						instance = Literal.literalToObject(a);
					}
					else {
						jargs[i] = Literal.literalToObject(a);
						i++;
					}
				}
				
				Object jrslt = null;
				try {
					jrslt = method.invoke(instance, jargs);
				} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
					throw new RuntimeException(e);
				}
				
				var rslt = Literal.objectToLiteral(jrslt);
				return rslt;
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		};
		
		return op;
	}
}
