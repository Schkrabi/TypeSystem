package semantic;

import java.util.Set;
import java.util.TreeSet;

public final class SemanticParserConstants {
	public static final String DEFTYPE = "deftype";
	public static final String DEFREP = "defrep";
	public static final String LAMBDA = "lambda";
	public static final String ELAMBDA = "elambda";
	public static final String IF = "if";
	
	public static final Set<String> specialForms;
	
	static{
		specialForms = new TreeSet<String>();
		specialForms.add(DEFREP);
		specialForms.add(DEFTYPE);
		specialForms.add(ELAMBDA);
		specialForms.add(IF);
		specialForms.add(LAMBDA);
	}
	
	public static boolean isSpecialForm(String symbol){
		return specialForms.contains(symbol);
	}
}
