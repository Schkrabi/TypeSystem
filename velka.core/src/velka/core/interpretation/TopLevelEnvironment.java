package velka.core.interpretation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Function;
import velka.core.abstraction.Operator;
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
					
					private Expression convertOp(
							TypeSystem typeSystem, 
							TypeArrow from, 
							TypeArrow to,
							Operator op,
							Environment env) {
						if(Type.unifyRepresentation(from.ltype, to.ltype)
								.isEmpty()) {
							List<Pair<Symbol, Type>> tparms = ((TypeTuple) to.ltype).stream()
									.map(t -> Pair.of(new Symbol(NameGenerator.next()), t)).toList();
							Tuple parms = new Tuple(tparms.stream().map(p -> p.first).toList());
							
							if(Type.unifyRepresentation(from.rtype, to.rtype)
									.isEmpty()) {								
								return new Function(env,
										new Convert(from.rtype, to.rtype,
												new AbstractionApplication(op, 
														new Convert(to.ltype, from.ltype,
														parms))),
												tparms);
							}
							
							return new Function(env,
											new AbstractionApplication(op, 
													new Convert(to.ltype, from.ltype,
													parms)),
											tparms);
						}
						
						if(Type.unifyRepresentation(from.rtype, to.rtype)
								.isEmpty()) {
							List<Pair<Symbol, Type>> tparms = ((TypeTuple) to.ltype).stream()
									.map(t -> Pair.of(new Symbol(NameGenerator.next()), t)).toList();
							Tuple parms = new Tuple(tparms.stream().map(p -> p.first).toList());
							
							return new Function(env,
									new Convert(from.rtype, to.rtype,
											new AbstractionApplication(op, 
													parms)),
											tparms);
						}
						
						return op;
					}
					
					private Expression convertF(
							TypeSystem typeSystem,
							TypeArrow from,
							TypeArrow to,
							Function f,
							Environment env) {
						Expression body = null;
						List<Pair<Symbol, Type>> tparms = null;
						Tuple parms = null;
						
						if(Type.unifyRepresentation(from.ltype, to.ltype)
								.isEmpty()) {
							tparms = ((TypeTuple) to.ltype).stream()
									.map(t -> Pair.of(new Symbol(NameGenerator.next()), t)).toList();
							parms = new Tuple(tparms.stream().map(p -> p.first).toList());
							body = new AbstractionApplication(f, new Convert(to.ltype, from.ltype, parms));
						} else {
							tparms = f.parms;
							body = f.body;
						}
						
						if(Type.unifyRepresentation(from.rtype, to.rtype)
								.isEmpty()) {
							body = new Convert(from.rtype, to.rtype, body);
						}
						else {
							body = f.body; 
						}
						
						Function exp = null;
						if(body != f.body) {
							exp = new Function(f.env, body, tparms);
						}
						else {
							exp = f;
						}
						
						return exp;
					}

					@Override
					public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env) {
						var eenv = (Environment)env;
						if(o instanceof Function fun) {
							return this.convertF(typeSystem, from, to, fun, eenv);
						}
						else if(o instanceof Operator op) {
							return this.convertOp(typeSystem, from, to, op, eenv);
						}
						throw new RuntimeException("Unrecognized abstraction");
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
						else if(object.equals(Expression.EMPTY_EXPRESSION)) {
							return TypeTuple.EMPTY_TUPLE;
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
