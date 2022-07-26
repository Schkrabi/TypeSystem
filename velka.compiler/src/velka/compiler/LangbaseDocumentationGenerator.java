package velka.compiler;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import velka.core.abstraction.Operator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;
import velka.types.Type;

/**
 * This class is generating Markdown documentation for langbase operators
 * @author Mgr. Radomir Skrabal
 *
 */
public class LangbaseDocumentationGenerator {
	
	/**
	 * Writes documentation to files in folder.
	 * @param operatorBanks list of classes with static operator definitons in
	 *                      fields, must be annotated with VelkaOperatorBank
	 *                      annotation
	 * @param folder folder where to spit the documentation
	 * @throws AppendableException
	 */
	public static void spitDocStatic(Collection<Class<?>> operatorBanks, Path folder) throws AppendableException {
		LangbaseDocumentationGenerator me = new LangbaseDocumentationGenerator();
		me.spitDocumentation(operatorBanks, folder);
	}
	
	/**
	 * Writes documentation to files in folder.
	 * 
	 * @param operatorBanks list of classes with static operator definitons in
	 *                      fields, must be annotated with VelkaOperatorBank
	 *                      annotation
	 * @return Map with filename - Documentation String pairs
	 * @throws AppendableException
	 */
	public static void spitDocStatic(Collection<Class<?>> operatorBanks) throws AppendableException {
		spitDocStatic(operatorBanks, Paths.get("."));
	}
	
	/**
	 * Writes documentation to files in folder.
	 * 
	 * @param operatorBanks list of classes with static operator definitons in
	 *                      fields, must be annotated with VelkaOperatorBank
	 *                      annotation
	 * @param path          folder where documentation will be written
	 * @throws AppendableException
	 */
	public void spitDocumentation(Collection<Class<?>> operatorBanks, Path path) throws AppendableException {
		for(Map.Entry<Path, String> e : this.generate(operatorBanks).entrySet()) {
			Path name = e.getKey();
			String doc = e.getValue();
			try {
				Files.write(
						path.resolve(name),
						doc.getBytes());
			} catch (IOException e1) {
				throw new AppendableException(e1.toString());
			}
		}
	}

	/**
	 * Generates documentation for list of classes containing operators
	 * 
	 * @param operatorBanks list of classes with static operator definitons in
	 *                      fields, must be annotated with VelkaOperatorBank
	 *                      annotation
	 * @return Map with filename - Documentation String pairs
	 * @throws AppendableException
	 */
	public Map<Path, String> generate(Collection<Class<?>> operatorBanks) throws AppendableException {		
		Map<Path, String> m = new TreeMap<Path, String>();
		for(Class<?> clazz : operatorBanks) {
			m.put(Paths.get(clazz.getName() + ".md"),
				  this.operatorBankDoc(clazz));
		}		
		return m;
	}
	
	/**
	 * Generates documentation for all operators in a class
	 * 
	 * @param operatorBank class containing static fields with operators, must be
	 *                     annotated with VelkaOperatorBank annotation
	 * @return string with Markdown documentation code
	 * @throws AppendableException
	 */
	private String operatorBankDoc(Class<?> operatorBank) throws AppendableException {
		if(operatorBank.getAnnotation(VelkaOperatorBank.class) == null) {
			throw new AppendableException(operatorBank.toString() + " must be annotated with "
					+ VelkaOperatorBank.class.getName() + "to generate documentation!");
		}
		
		VelkaOperatorBank annotation = operatorBank.getAnnotation(VelkaOperatorBank.class);
		
		return new StringBuilder()
				.append("# ")
				.append(annotation.header())
				.append("\n")
				.append(annotation.description())
				.append("\n\n")
				.append(this.operatorBankToc(operatorBank))
				.append("\n\n")
				.append(this.operatorBankConstructors(operatorBank))
				.append("\n\n")
				.append(this.operatorBankOperators(operatorBank))
				.toString();
	}
	
	/**
	 * Creates documentation for operators in operation bank
	 * 
	 * @param operatorBank operator bank class
	 * @return Markdown documentation
	 * @throws AppendableException if bank contains badly annotated field
	 */
	private String operatorBankOperators(Class<?> operatorBank) throws AppendableException {
		List<Field> fields = 
				Arrays.asList(operatorBank.getFields())
				.stream()
				.filter(f -> f.getAnnotation(VelkaOperator.class) != null)
				.collect(Collectors.toList());
		
		if(fields.isEmpty()) {
			return "";
		}
		
		StringBuilder sb = new StringBuilder();
		sb.append("## Operators\n");		
		
		Iterator<Field> i = fields.iterator();
		while(i.hasNext()) {
			Field operatorField = i.next();
			Operator operator = this.getOperator(operatorField);
			sb.append(this.operatorDoc(operatorField, operator));
			if(i.hasNext()) {
				sb.append("\n");
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * Creates documentation for constructors in operation bank
	 * 
	 * @param operatorBank operator bank class
	 * @return Markdown documentation
	 * @throws AppendableException if bank contains badly annotated field
	 */
	private String operatorBankConstructors(Class<?> operatorBank) 
		throws AppendableException {
		List<Field> fields = 
				Arrays.asList(operatorBank.getFields())
				.stream()
				.filter(f -> f.getAnnotation(VelkaConstructor.class) != null)
				.collect(Collectors.toList());
		
		if(fields.isEmpty()) {
			return "";
		}
		
		StringBuilder sb = new StringBuilder();
		sb.append("## Constructors\n");		
		
		Iterator<Field> i = fields.iterator();
		while(i.hasNext()) {
			Field operatorField = i.next();
			Operator operator = this.getOperator(operatorField);
			sb.append(this.constructorDoc(operatorField, operator));
			if(i.hasNext()) {
				sb.append("\n");
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * Gets operator from operator field.
	 * 
	 * @param operatorField class field object
	 * @return Operator object
	 * @throws AppendableException if field is not valid static operator field
	 */
	private Operator getOperator(Field operatorField) throws AppendableException {
		Object o = null;
		
		try {
			o = operatorField.get(null);
		} catch (IllegalArgumentException e) {
			throw new AppendableException(operatorField.toString() + " must be static to generate documentation");
		} catch (IllegalAccessException e) {
			throw new AppendableException(operatorField.toString() + " must be public to generate documentation");
		}
		if (!(o instanceof Operator)) {
			throw new AppendableException(operatorField.toString() + " must be Operator to generate documentation");
		}
		
		Operator operator = (Operator)o;
		return operator;
	}
	
	/**
	 * Creates table of contents for Operator Bank
	 * 
	 * @param operatorBank operator bank class
	 * @return Markdown code
	 * @throws AppendableException unlikely
	 */
	private String operatorBankToc(Class<?> operatorBank) throws AppendableException {
		List<Field> fields = 
				Arrays.asList(operatorBank.getFields())
				.stream()
				.filter(f -> (f.getAnnotation(VelkaOperator.class) != null)
						|| (f.getAnnotation(VelkaConstructor.class) != null))
				.collect(Collectors.toList());
		
		if(fields.isEmpty()) {
			return "";
		}
		
		StringBuilder sb = new StringBuilder();
		Iterator<Field> i = fields.iterator();
		
		sb.append("## Table of Contents\n");
		
		while(i.hasNext()) {
			Field operatorField = i.next();
			Operator operator = this.getOperator(operatorField);			
			sb.append(this.operatorTocEntry(operatorField, operator));
			if(i.hasNext()) {
				sb.append("\n");
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * Creates header name for operator
	 * 
	 * @param operatorField Operator field object
	 * @param operator      operator object
	 * @return String with header name in form of "fieldName(operatorSymbol)"
	 */
	private String operatorHeaderName(Field operatorField, Operator operator) {
		return new StringBuilder()
				.append(operatorField.getName())
				.append("(")
				.append(operator.toString())
				.append(")")
				.toString();	
	}
	
	/**
	 * Returns unique identifier of operator to label the operator in the document
	 * 
	 * @param operatorField field object
	 * @param operator      operator object
	 * @return string with identifier
	 */
	private String operatorUniqueId(Field operatorField, Operator operator) {
		return operator.getClass().getName();
	}
	
	/**
	 * Creates link to operator
	 * 
	 * @param linkText      text of the link
	 * @param operatorField Operator field object
	 * @param operator      operator object
	 * @return string with Markdown documentation code
	 */
	private String operatorMakeLink(String linkText, Field operatorField, Operator operator) {
		return new StringBuilder()
				.append("[")
				.append(linkText)
				.append("](#")
				.append(this.operatorUniqueId(operatorField, operator))
				.append(")")
				.toString();
	}
	
	/**
	 * Generates Table of Contents entry for operator
	 * 
	 * @param operatorField Operator field object
	 * @param operator      operator object
	 * @return string with Markdown documentation code
	 */
	private String operatorTocEntry(Field operatorField, Operator operator) {
		return new StringBuilder()
				.append("* ")
				.append(this.operatorMakeLink(
						this.operatorHeaderName(operatorField, operator),
						operatorField,
						operator))
				.toString();		
	}

	/**
	 * Returns .md documentation for Velka operator
	 * 
	 * @param operatorField Operator field object, must be annotated
	 *                      with @VelkaOperator annotation
	 * @param operator      operator object
	 * @return string with Markdown documentation code
	 * @throws AppendableException unlikely
	 */
	private String operatorDoc(Field operatorField, Operator operator) throws AppendableException {
		VelkaOperator operatorAnnotation = operatorField.getAnnotation(VelkaOperator.class);
		String description 	= operatorAnnotation.description();	
		
		return new StringBuilder()
			.append(this.operatorDocHeader(operatorField, operator))		
			.append(this.operatorDocSyntax(operatorAnnotation))
			.append(this.operatorDocType(operator))
			.append(description)
			.append("\n\n")
			.append(this.operatorDocExample(operatorAnnotation))
			.toString();	
	}
	
	/**
	 * Returns .md documentation for Velka Constructor
	 * @param operatorFieldOperator field object, must be annotated
	 *                      with @VelkaConstructor annotation
	 * @param operator operator object
	 * @return string with Markdown documentation code
	 * @throws AppendableException unlikely
	 */
	private String constructorDoc(Field operatorField, Operator operator) throws AppendableException {
		VelkaConstructor constructorAnnotation = operatorField.getAnnotation(VelkaConstructor.class);
		String description = constructorAnnotation.description();
		
		return new StringBuilder()
				.append(this.operatorDocHeader(operatorField, operator))
				.append(this.constructorDocSyntax(constructorAnnotation))
				.append(this.operatorDocType(operator))
				.append(description)
				.toString();
	}
	
	/**
	 * Generates header of the operator Markdown documentation
	 * 
	 * @param operatorName   operator name
	 * @param operatorSymbol operator symbol used in Velka
	 * @return header Markdown code
	 */
	private String operatorDocHeader(Field operatorField, Operator operator) {
		return new StringBuilder()
			.append("### ")
			.append("<a name=\"")
			.append(this.operatorUniqueId(operatorField, operator))
			.append("\"> ")
			.append(this.operatorHeaderName(operatorField, operator))
			.append("</a>\n")
			.toString();
	}
	
	/**
	 * Generates syntax md documentation
	 * 
	 * @param syntax syntax string
	 * @return Markdown code
	 */
	private String docSyntax(String syntax) {
		return new StringBuilder()
				.append("Syntax:\n\n")
				.append("~~~\n")
				.append(syntax)
				.append("\n")
				.append("~~~\n\n")
				.toString();
	}
	
	/**
	 * Generates syntax of the operator Markdown documentation
	 * 
	 * @param operatorAnnotation operator VelkaOperator annotation
	 * @return Markdown code
	 */
	private String operatorDocSyntax(VelkaOperator operatorAnnotation) {
		return this.docSyntax(operatorAnnotation.syntax());
	}
	
	/**
	 * Generates syntax of the operator Markdown documentation
	 * @param constructorAnnotation constructors VelkaConstructor annotation
	 * @return Markdown code
	 */
	private String constructorDocSyntax(VelkaConstructor constructorAnnotation) {
		return this.docSyntax(constructorAnnotation.syntax());
	}
	
	/**
	 * Generates Type signature for Markdown documentation
	 * 
	 * @param type operator type
	 * @return Markdown code
	 */
	private String operatorDocType(Operator operator) throws AppendableException {
		Environment env 		= Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
		Type type = operator.infer(env, typeEnv).first;
		
		return new StringBuilder()
				.append("Type Signature:\n\n")
				.append("~~~\n")
				.append(type.toString())
				.append("\n")
				.append("~~~\n\n")
				.toString();
	}
	
	/**
	 * Generates example for Markdown documentation
	 * 
	 * @param example operator example
	 * @return Markdown code
	 */
	private String operatorDocExample(VelkaOperator operatorAnnotation) {
		return new StringBuilder()
				.append("Example:\n\n")
				.append("~~~\n")
				.append(operatorAnnotation.example())
				.append("\n")
				.append("~~~")
				.toString();
	}
}
