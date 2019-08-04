/**
 * 
 */
package util;

/**
 * @author r.skrabal
 *
 */
@FunctionalInterface
public interface ThrowingFunction<T, R> {
	R apply(T o) throws AppendableException;
}
