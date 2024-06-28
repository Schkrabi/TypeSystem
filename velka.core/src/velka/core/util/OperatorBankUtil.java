package velka.core.util;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Operator;
import velka.core.expression.Symbol;
import velka.core.interpretation.Environment;
import velka.util.AppendableException;
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
	@SuppressWarnings("unchecked")
	private static <T extends Operator> List<T> getAnnotatedOperators(Class<?> operatorBank, Class<? extends Annotation> annotation) throws IllegalArgumentException, IllegalAccessException {
		List<Field> fields = 
				Arrays.asList(operatorBank.getFields())
				.stream()
				.filter(f -> f.getAnnotation(annotation) != null)
				.collect(Collectors.toList());
		
		List<T> l = new ArrayList<T>(fields.size());
		for(Field f : fields) {
			var operator = (T) f.get(null);
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
	public static List<Constructor> getConstructors(Class<?> clazz) throws IllegalArgumentException, IllegalAccessException {
		return getAnnotatedOperators(clazz, VelkaConstructor.class);
	}
	
	/**
	 * Gets Conversions from operator bank
	 * @param clazz operator bank class
	 * @return list of Operators
	 * @throws IllegalArgumentException if operator is badly annotated
	 * @throws IllegalAccessException if operator is badly annotated
	 */
	public static List<Conversion> getConversions(Class<?> clazz) throws IllegalArgumentException, IllegalAccessException {
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
	public static String conversionDefinition(Conversion conversion, Environment env) throws AppendableException {		
		return conversion.toClojureCode(env);
	}
}
