package velka.truffle.compiler;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {

	public static void main(String[] args) {
		Context context = Context.create();
		Value result = context.eval("vlk", "42");
		assert result.asInt() == 42;
		context.close();
	}

}
