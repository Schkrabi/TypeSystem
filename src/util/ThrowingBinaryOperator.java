package util;

import java.util.function.BinaryOperator;

@FunctionalInterface
public interface ThrowingBinaryOperator<T, E extends Exception> {
	T apply(T x, T y) throws E;

	public static <T> BinaryOperator<T> wrapper(ThrowingBinaryOperator<T, AppendableException> function) {
		return (x, y) -> {
			try {
				return (function.apply(x, y));
			} catch (AppendableException e) {
				throw new RuntimeException(e);
			}
		};
	}
}
