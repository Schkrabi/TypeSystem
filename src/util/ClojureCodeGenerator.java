package util;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;

import expression.Expression;

public class ClojureCodeGenerator {
	public static void toClojureCode(List<Expression> exprs, Writer target) throws IOException, Exception {
		ClojureCodeGenerator.writeHeaders(target);

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			target.write(e.toClojureCode());
			if (i.hasNext() && e != Expression.EMPTY_EXPRESSION) {
				target.write('\n');
			}
		}
	}

	private static void writeHeaders(Writer target) throws IOException {
	}
}
