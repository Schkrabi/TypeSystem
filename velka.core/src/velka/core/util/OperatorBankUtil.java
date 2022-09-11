package velka.core.util;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import velka.core.abstraction.Operator;
import velka.core.expression.Symbol;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaConversion;
import velka.util.annotations.VelkaOperator;

/**
 * Utility methods to manage operator banks
 * @author Mgr. Radomir Skrabal
 *
 */
public class OperatorBankUtil {

	/**
	 * Puts operator to environment
	 * @param operator operator
	 * @param env environment
	 */
	public static void putOperatorToEnvironment(Operator operator, Environment env) {
		env.put(new Symbol(operator.toString()), operator);
	}
	
	/**
	 * Gets operators with specfic annotation from operator bank
	 * @param operatorBank Class containing annotated operator fields
	 * @param annotation Annotation we are filtering upon
	 * @return list of operators
	 * @throws IllegalArgumentException if operator is badly annotated
	 * @throws IllegalAccessException if operator is badly annotated
	 */
	private static List<Operator> getAnnotatedOperators(Class<?> operatorBank, Class<? extends Annotation> annotation) throws IllegalArgumentException, IllegalAccessException {
		List<Field> fields = 
				Arrays.asList(operatorBank.getFields())
				.stream()
				.filter(f -> f.getAnnotation(annotation) != null)
				.collect(Collectors.toList());
		
		List<Operator> l = new ArrayList<Operator>(fields.size());
		for(Field f : fields) {
			Operator operator = (Operator) f.get(null);
			l.add(operator);
		}
		return l;
	}

	/**
	 * Gets operators from operator bank
	 * @param clazz operator bank class
	 * @return list of Operators
	 * @throws IllegalArgumentException if operator is badly annotated
	 * @throws IllegalAccessException if operator is badly annotated
	 */
	public static List<Operator> getOperators(Class<?> clazz) throws IllegalArgumentException, IllegalAccessException {
		return getAnnotatedOperators(clazz, VelkaOperator.class);
	}
	
	/**
	 * Gets Constructors from operator bank
	 * @param clazz operator bank class
	 * @return list of Operators
	 * @throws IllegalArgumentException if operator is badly annotated
	 * @throws IllegalAccessException if operator is badly annotated
	 */
	public static List<Operator> getConstructors(Class<?> clazz) throws IllegalArgumentException, IllegalAccessException {
		return getAnnotatedOperators(clazz, VelkaConstructor.class);
	}
	
	/**
	 * Gets Conversions from operator bank
	 * @param clazz operator bank class
	 * @return list of Operators
	 * @throws IllegalArgumentException if operator is badly annotated
	 * @throws IllegalAccessException if operator is badly annotated
	 */
	public static List<Operator> getConversions(Class<?> clazz) throws IllegalArgumentException, IllegalAccessException {
		return getAnnotatedOperators(clazz, VelkaConversion.class);
	}
	
	/**
	 * Initializes operator bank operators in environment
	 * @param clazz operator bank class
	 * @param env environment
	 */
	public static void initializeInEnvironment(Class<?> clazz, Environment env) {
		try {
			for(Operator operator : OperatorBankUtil.getOperators(clazz)) {
				OperatorBankUtil.putOperatorToEnvironment(operator, env);
			}
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Creates clojure code to add conversion from VelkaConversion operator
	 * @param conversion conversion operator
	 * @param env environment
	 * @param typeEnv typeEnvironment
	 * @return Clojure Code
	 * @throws AppendableException If conversion is badly defined
	 */
	private static String conversionDefinition(Operator conversion, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = conversion.infer(env, typeEnv);
		
		if(!(p.first instanceof TypeArrow)) {
			throw new AppendableException("Conversion " + conversion.toString() + " does not infer to TypeArrow.");
		}
		TypeArrow ta = (TypeArrow)p.first;
		
		if(!(ta.rtype instanceof TypeAtom)) {
			throw new AppendableException("Conversion " + conversion.toString() + " must convert to typeAtom, got " + ta.rtype.toString());
		}
		
		if(!(ta.ltype instanceof TypeTuple)) {
			throw new AppendableException("Conversion " + conversion.toString() + " ltype of abstraction must always be a type tuple, got " + ta.ltype.toString()); 
		}
		
		TypeTuple tt = (TypeTuple)ta.ltype;
		if(tt.size() != 1) {
			throw new AppendableException("Conversion " + conversion.toString() + " only one argument allowed on conversion, got " + tt.toString());
		}
		
		if(!(tt.get(0) instanceof TypeAtom)) {
			throw new AppendableException("Conversion " + conversion.toString() + " must convert from typeAtom, got " + tt.get(0).toString());
		}
		
		TypeAtom from = (TypeAtom)tt.get(0);
		TypeAtom to = (TypeAtom)ta.rtype;
		
		return TypeAtom.addConversionToGlobalTable(from, to, conversion.toClojureCode(env, typeEnv));
	}
	
	/**
	 * Writes definitions of operator bank file
	 * @return
	 */
	public static String writeDefinitions(Class<?> clazz, String Namespace) {
		StringBuilder sb = new StringBuilder();
		
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		sb.append(ClojureHelper.declareNamespace(Namespace));
		
		try {
			List<Operator> operators = OperatorBankUtil.getOperators(clazz);
		
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDeclaration(operator));
			}
			
			Environment env = Environment.initTopLevelEnvironment();
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDef(operator, env, typeEnv));
			}
			
			List<Operator> conversions = OperatorBankUtil.getConversions(clazz);
			for(Operator conversion : conversions) {
				sb.append(conversionDefinition(conversion, env, typeEnv));
			}
			
		} catch (Exception e) {
			System.err.println("Error generating file for " + clazz.getName() + " :" + e.getMessage());
			return "";
		}
		
		return sb.toString();
	}
}
