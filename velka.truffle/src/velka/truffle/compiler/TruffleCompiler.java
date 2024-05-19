package velka.truffle.compiler;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;

import velka.truffle.node.VelkaNode;
import velka.truffle.wrapper.ClassExtension;
import velka.truffle.wrapper.IToVelkaNode;

/** Converts internal Velka representation to AST*/
public class TruffleCompiler {
	
	private TruffleCompiler() {}
	
	private static TruffleCompiler singleton = null;
	public static TruffleCompiler instance() {
		if(singleton == null) {
			singleton = new TruffleCompiler();
		}
		return singleton;
	}

	public Collection<VelkaNode> compile(Collection<velka.core.expression.Expression> exprs){
		return exprs.stream().map(x -> this.compile(x)).toList();
	}
	
	public VelkaNode compile(velka.core.expression.Expression e) {
		var wrap = wrap(e);
		return wrap.toVelkaNode();
	}

	public IToVelkaNode wrap(velka.core.expression.Expression e) {
		var c = e.getClass();
		for (Class<?> clazz : wrapperClasses) {
			var a = clazz.getAnnotation(ClassExtension.class);
			if (a != null) {
				if (c.equals(a.value())) {
					try {
						var con = clazz.getDeclaredConstructor(c);
						var wrap = (IToVelkaNode) con.newInstance(e);
						return wrap;
					} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
							| IllegalArgumentException | InvocationTargetException e1) {
						throw new RuntimeException(e1);
					}
				}
			}
		}
		return null;
	}

	public final List<Class<? extends IToVelkaNode>> wrapperClasses = List
			.of(velka.truffle.wrapper.LitIntegerTruffle.class);
}
