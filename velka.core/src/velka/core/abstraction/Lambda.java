package velka.core.abstraction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;
import com.sun.codemodel.JMod;

import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends Expression implements CompileableToJava {

	/**
	 * Symbol for lambda special form
	 */
	public static final String LAMBDA = "lambda";

	/**
	 * Body
	 */
	public final Expression body;

	public final List<Pair<Symbol, Type>> parms;

	/**
	 * General identity lambda
	 */
	public static final Lambda identity = Lambda.makeIdentity(new TypeVariable(NameGenerator.next()));

	public Lambda(Expression body, Collection<Pair<Symbol, Type>> parms) {
		this.body = body;
		this.parms = new java.util.ArrayList<Pair<Symbol, Type>>(parms);
	}
	
	public TypeTuple getParmType() {
		return new TypeTuple(this.parms.stream().map(p -> p.second).toList());
	}
	
	public Tuple getParmSymbols() {
		return new Tuple(this.parms.stream().map(p -> p.first).toList());
	}

	@Override
	public Expression interpret(Environment env) {
		return new Function(env, this.body, this.parms);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(lambda (");

		for(var p : this.parms) {
			s.append('(').append(p.second).append(' ').append(p.first).append(')');
		}

		s.append(") ");
		s.append(this.body.toString());
		s.append(')');

		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		var clj = Environment.create(env);
		
		for(var p : this.parms) {
			clj.put(p.first, new TypeHolder(p.second));
		}		
		
		var ret = this.body.infer(clj);
		
		var t = new TypeArrow(
				(new TypeTuple(this.parms.stream().map(p -> p.second).toList())).apply(ret.second), 
				ret.first);
		
		return Pair.of(t, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Lambda l) {
			if(this.parms.size() != l.parms.size()) return false;
			
			var i = this.parms.iterator();
			var j = l.parms.iterator();
			while(i.hasNext()) {
				var p = i.next();
				var q = j.next();
				
				if(!p.first.equals(q.first)) return false;
				if(Type.unifyRepresentation(p.second, q.second).isEmpty()) return false;
			}
			
			
			return this.body.equals(l.body);			
		}
		return false;
	}

	@Override
	public int hashCode() {
		return new StringBuilder()
				.append(this.parms.hashCode())
				.append(this.body.hashCode())
				.toString().hashCode();
	}

	/**
	 * Makes identity lambda with given type
	 * 
	 * @param argType type of the identity arg
	 * @return identity lambda
	 */
	public static Lambda makeIdentity(Type argType) {
		Symbol symbol = new Symbol(NameGenerator.next());
		return new Lambda(symbol, List.of(Pair.of(symbol, new TypeVariable(NameGenerator.next()))));
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		throw new RuntimeException("doConvert not implemented for lambda");
	}
	
	/** Creates a const function with given number of arguments */
	public static  Lambda constFun(int numOfArgs, Expression constExp) {
		var parms = Stream
				.generate(() -> Pair.of(new Symbol(NameGenerator.next()), (Type)(new TypeVariable(NameGenerator.next()))))
				.limit(numOfArgs)
				.toList();
		return new Lambda(constExp, parms);
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		Pair<Type, Substitution> inf;
		Environment clj = null;
		try {
			inf = this.infer(env);
			clj = Environment.create(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		
		var aClass = CodeModelInstance.instance().anonymousClass(VelkaAbstraction.class);
		var fType = aClass.field(JMod.PRIVATE | JMod.FINAL, Type.class, "_type", 
				TypeUtil.instance().type2java(inf.first));
		
		aClass.method(JMod.PUBLIC, Type.class, "getType").body()
			._return(fType);
		
		var apply = aClass.method(JMod.PUBLIC, Object.class, "apply");
		
		var parmTypeTuple = (TypeTuple)((TypeArrow)inf.first).ltype;
		var i = parmTypeTuple.iterator();
		var parmList = new ArrayList<Pair<Symbol, Type>>();
		for(var p : this.parms) {
			var t = i.next();
			clj.put(p.first, new TypeHolder(t));
			parmList.add(Pair.of(p.first, t));
		}
		
		Abstraction.convertAndDeclareParms(
				parmList, apply);
		
		var compileablebody = (CompileableToJava)this.body;
		apply.body()._return(compileablebody.toJavaExpr(clj));
		
		return JExpr._new(aClass);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var _this = "_this";
		var _arg = "_arg";
		var _carg = "_carg";
		
		var inf = this.infer(env);
		var ta = (TypeArrow)inf.first;
		
		var bindings = new ArrayList<Pair<String, String>>();
		
		bindings.add(
				Pair.of(_carg,
						ClojureHelper.applyClojureFunction(
								".convert",
								ClojureCoreSymbols.typeSystem_full,
								ClojureHelper.applyClojureFunction(".getType", ClojureCoreSymbols.typeSystem_full, _arg),
								ta.ltype.clojureTypeRepresentation(),
								_arg,
								"nil")));
		
		var clj = Environment.create(env);
		int i = 0;
		
		for(var p : this.parms) {
			clj.put(p.first, new TypeHolder(p.second));
			bindings.add(Pair.of(p.first.toClojureCode(env), 
					ClojureHelper.applyClojureFunction("get", _carg, Integer.toString(i))));
			i++;
		}
		
		var applyCode =
				ClojureHelper.letHelper(
						this.body.toClojureCode(clj),
						bindings);		
		
		var code = 
				ClojureHelper.reify(
					velka.types.typeSystem.VelkaAbstraction.class,
					Pair.of("apply", Pair.of(List.of(_this, _arg), applyCode)),
					Pair.of("getType", Pair.of(List.of(_this), inf.first.clojureTypeRepresentation())));
		return code;
	}
}
