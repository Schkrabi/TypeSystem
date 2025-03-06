package velka.core.util;

import java.nio.file.Path;
import java.nio.file.Paths;

public class Constants {

	/** Default package for velka compiled source */
	public static final String PACKAGE = "velka.runtime";
	
	/** Default location of compiled velka source */
	public static final Path LOCATION = Paths.get("velka", "runtime");
	
	public static final Path DEFAULT_JAVA_FILE = LOCATION.resolve("user.java");
	
	/**
	 * Name of default namespace if no namespace was declared by user
	 */
	public static final String DEFAULT_NAMESPACE = PACKAGE + ".user";

}
