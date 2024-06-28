package velka.util;

import java.util.function.BinaryOperator;

public class CompleteLattices {

	public static BinaryOperator<Double> GoguenTConorm = (Double x, Double y) -> x + y - (x * y);
	
	public static BinaryOperator<Double> GoguenTNorm = (Double x, Double y) -> x * y;
}
