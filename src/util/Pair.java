package util;

public class Pair<T1, T2> {
	public final T1 first;
	public final T2 second;

	public Pair(T1 first, T2 second) {
		this.first = first;
		this.second = second;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Pair<?, ?>) {
			return this.first.equals(((Pair<?, ?>) other).first) && this.second.equals(((Pair<?, ?>) other).second);
		}
		return false;
	}
	
	@Override
	public String toString() {
		return "(" + first.toString() + "," + second.toString() + ")";
	}
}
