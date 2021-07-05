package velka.util;

import java.util.function.Predicate;

@FunctionalInterface
public interface ThrowingPredicate<T, E extends Exception> {
	boolean apply(T x) throws E;
	
	public static <T> Predicate<T> wrapper(ThrowingPredicate<T, AppendableException> function){
		return x -> {
			try {
				return (function.apply(x));
			} catch(AppendableException e) {
				throw new RuntimeException(e);
			}
		};
	}
}
