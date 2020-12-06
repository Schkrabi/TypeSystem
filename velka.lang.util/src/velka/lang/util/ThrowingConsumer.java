package velka.lang.util;

import java.util.function.Consumer;

/**
 * 
 * @author Mgr. Radomir Skrabal
 *
 * @param <T>
 * @param <E>
 */
@FunctionalInterface
public interface ThrowingConsumer<T, E extends Exception> {
	void accept(T t) throws E;

	public static <T> Consumer<T> wrapper(ThrowingConsumer<T, Exception> throwingConsumer) {

		return i -> {
			try {
				throwingConsumer.accept(i);
			} catch (Exception ex) {
				throw new RuntimeException(ex);
			}
		};
	}
}
