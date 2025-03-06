package velka.core.interpretation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Function;
import velka.core.application.AbstractionApplication;
import velka.core.application.Convert;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.langbase.OperatorBank;
import velka.core.literal.LitDouble;
import velka.core.util.OperatorBankUtil;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.typeSystem.TypeSystem;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

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
							
							var c = typeSystem.convert(fr, tt, e, env);
							
							l.add((Expression)c);
						}
						
						return new Tuple(l);
					}

					@Override
					public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env) {
						var f = (Expression)o;
						List<Pair<Symbol, Type>> tparms = ((TypeTuple) to.ltype).stream()
								.map(t -> Pair.of(new Symbol(NameGenerator.next()), t)).toList();
						
						var parms = new Tuple(tparms.stream().map(p -> p.first).toList());
						
						var eenv = (Environment)env;
						
						var exp = new Function(eenv,
								new Convert(from.rtype, to.rtype,
								new AbstractionApplication(f, 
										new Convert(to.ltype, from.ltype,
										parms))),
								tparms);
						
						return exp;
					}

					@Override
					public Collection<? extends Object> instantiateCollection(Object o) {
						return List.of(o);
					}
					
				}) {

					@Override
					public Type getType(Object object) {
						if(object instanceof velka.core.literal.LitInteger) {
							return TypeAtom.TypeIntNative;
						}
						else if(object instanceof velka.core.literal.LitDouble) {
							return TypeAtom.TypeDoubleNative;
						}
						else if(object instanceof velka.core.literal.LitString) {
							return TypeAtom.TypeStringNative;
						}
						else if(object instanceof velka.core.literal.LitBoolean) {
							return TypeAtom.TypeBoolNative;
						}
						else if(object instanceof velka.core.literal.LitInteropObject lio) {
							return lio.type;
						}
						else if(object instanceof velka.core.literal.LitComposite lc) {
							return lc.composedType;
						}
						else if(object instanceof velka.core.expression.Tuple t) {
							var tt = new TypeTuple(t.stream().map(e -> this.getType(e)).toList());
							return tt;
						}
						else if(object instanceof VelkaAbstraction va) {
							return va.getType();
						}
						//Other expressions should not be found here, since this should only discern evaluated object
						
						throw new RuntimeException("Unrecognized type!");
					}
					
					@Override
					public double extractRank(Object rank) {
						if(rank instanceof LitDouble lt) {
							return lt.value;
						}
						throw new RuntimeException("Rank is not an LitDouble!");
					}
			
		};
		
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
