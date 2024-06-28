package velka.core.interpretation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.application.Convert;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.langbase.OperatorBank;
import velka.core.util.OperatorBankUtil;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.typeSystem.TypeSystem;
import velka.util.AppendableException;

/** Top level environment with associated type system */
public class TopLevelEnvironment extends Environment {

	private final TypeSystem typeSystem;
	
	private TopLevelEnvironment(TypeSystem typeSystem) {
		super(null);
		if(typeSystem == null) {
			throw new RuntimeException("Type system must not be null");
		}
		this.typeSystem = typeSystem;
		
		for(OperatorBank bank : OperatorBank.operatorBanks) {
			OperatorBankUtil.initializeInEnvironment(bank.getClass(), this);
		}
	}
	
	@Override
	public TypeSystem getTypeSystem() {
		return this.typeSystem;
	}
	
	public static TopLevelEnvironment instantiate() {
		var ts = new TypeSystem(
				new velka.types.typeSystem.IConversionEngine() {

					@Override
					public Object convertTuple(TypeSystem typeSystem, TypeTuple from, TypeTuple to, Object o, Object env) {
						var tuple = (Tuple)o;
						var it = tuple.iterator();
						var ift = from.iterator();
						var itt = to.iterator();
						
						var l = new ArrayList<Expression>(tuple.size());
						while(it.hasNext()) {
							var e = it.next();
							var fr = ift.next();
							var tt = itt.next();
							
							var c = typeSystem.convert(fr, tt, List.of(e), env);
							
							l.add((Expression)c);
						}
						
						return new Tuple(l);
					}

					@Override
					public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env) {
						var abst = (Abstraction)o;
						var argTuple = (TypeTuple)to.ltype;
						var cargs = new Tuple(Symbol.uniqueSymbolList(argTuple.size()));
						
						Expression exp = new Lambda(cargs,
								argTuple,
								new Convert(from.rtype,
										to.rtype,
										new AbstractionApplication(abst, cargs)));
						
						var eenv = (Environment)env;
						
						try {
							exp = exp.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
						
						return exp;
					}

					@Override
					public Collection<? extends Object> instantiateCollection(Object o) {
						return List.of(o);
					}
					
				});
		
		var tle = new TopLevelEnvironment(ts);
		
		for(var ob : OperatorBank.operatorBanks) {
			try {
				ob.initInEnvironment(tle);
			} catch (AppendableException e) {
				throw new RuntimeException(e);
			}
		}
		
		return tle;
	}
}
