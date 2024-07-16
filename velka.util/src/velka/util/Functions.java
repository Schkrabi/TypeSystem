package velka.util;

import java.util.function.Function;

public class Functions {

	/** Creates a new linear function */
	public static Function<Double, Double> linearFunction(double k, double q){
		return (Double x) -> k * x + q;
	}
	
	/** Creates a new linear function from two points */
	public static Function<Double, Double> linearFunctionFromPoints(double x1, double y1, double x2, double y2){
		var k = (y2 - y1)/(x2 - x1);
		var q = y2 - k * x2;
		return linearFunction(k, q);
	}
}
