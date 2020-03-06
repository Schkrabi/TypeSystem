package util;

import java.util.function.Function;

@FunctionalInterface
public interface ThrowingFunction<T, R, E extends Exception> {
	R apply(T o) throws E;

	public static <T, R> Function<T, R> wrapper(ThrowingFunction<T, R, AppendableException> function) {
		return x -> {
			try {
				return function.apply(x);
			} catch (AppendableException e) {
				throw new RuntimeException(e);
			}
		};
	}
}
