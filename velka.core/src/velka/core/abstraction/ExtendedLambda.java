package velka.core.abstraction;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.application.AbstractionApplication;
import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.ImplementationSelector;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.RankAggregation;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingFunction;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends Expression implements CompileableToJava {

	public final TypeTuple argType;
	
	public ExtendedLambda(TypeTuple argType) {
		this.argType = argType;
	}
	
	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return new ExtendedFunction(env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		var tv = new TypeVariable(NameGenerator.next());
		var t = new TypeArrow(this.argType, tv);
		return Pair.of(t, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var inf = this.infer(env);
		
		var code =
				ClojureHelper.constructJavaClass(
						velka.types.typeSystem.VelkaExtendedFunction.class,
						ClojureCoreSymbols.typeSystem_full,
						inf.first.clojureTypeRepresentation());
		
		return code;
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		throw new RuntimeException("Not Implemented");
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		Pair<Type, Substitution> inf;
		try {
			inf = this.infer(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		var _new =
		JExpr._new(CodeModelInstance.instance().ref(velka.types.typeSystem.VelkaExtendedFunction.class))
			.arg(JavaTypeSystem.codeInstance())
			.arg(TypeUtil.instance().type2java(inf.first));
		return _new;
	}
	
	@Override
	public boolean equals(Object o) {
		if(o instanceof ExtendedLambda el) {
			return this.argType.equals(el.argType);
		}
		return false;
	}
}
