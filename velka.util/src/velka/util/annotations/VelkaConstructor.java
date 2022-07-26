package velka.util.annotations;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Retention(RUNTIME)
@Target(FIELD)
public @interface VelkaConstructor {
	public String name();
	public String description();
	public String syntax();
	public boolean showInDoc() default true;
}
