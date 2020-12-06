package velka.lang.conversions;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import velka.lang.abstraction.Lambda;
import velka.lang.application.AbstractionApplication;
import velka.lang.exceptions.ConversionException;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

public class Conversions {
	
	public static Expression convert(Type from, Expression converted, Type to) throws AppendableException {
		//TODO find a better solution
		if(from instanceof RepresentationOr) {
			return Conversions.convertRepresentationOr((RepresentationOr)from, converted, to);
		}
		else if(from instanceof TypeArrow) {
			return Conversions.convertTypeArrow((TypeArrow)from, converted, to);
		} else if(from instanceof TypeAtom) {
			return Conversions.convertTypeAtom((TypeAtom)from, converted, to);
		} else if(from instanceof TypeTuple) {
			return Conversions.convertTypeTuple((TypeTuple)from, converted, to);
		} else if(from instanceof TypeVariable) {
			return Conversions.convertTypeVariable((TypeVariable)from, converted, to);
		}
		throw new AppendableException("Unrecognized type " + from + " for conversion");
	}

	private static Expression convertRepresentationOr(RepresentationOr from, Expression converted, Type to) throws AppendableException{
		throw new AppendableException(
				"Cannot directly convert RepresentationOr type. Select specific representation in order to create conversion.");
	}
	
	private static Expression convertTypeArrow(TypeArrow from, Expression converted, Type to) throws AppendableException {
		if (to instanceof TypeVariable) {
			return converted;
		}
		if (!(to instanceof TypeArrow)) {
			throw new ConversionException(from, to, converted);
		}
		TypeArrow t = (TypeArrow) to;

		Tuple formalArgs = new Tuple(
				((TypeTuple) t.ltype).stream().map(x -> new Symbol(NameGenerator.next())).collect(Collectors.toList()));
		TypeTuple argsType = (TypeTuple) t.ltype;
		Tuple realArgs = (Tuple) Conversions.convert(t.ltype, formalArgs, from.ltype);
		Expression body = Conversions.convert(from.rtype, new AbstractionApplication(converted, realArgs), t.rtype);

		Lambda l = new Lambda(formalArgs, argsType, body);
		return l;
	}
	
	private static Expression convertTypeAtom(TypeAtom from, Expression converted, Type to) throws AppendableException {
		if (to instanceof TypeVariable || to.equals(from)) {
			return converted;
		}
		if (!(to instanceof TypeAtom) || !TypeAtom.isSameBasicType(from, (TypeAtom) to)) {
			throw new ConversionException(from, to, converted);
		}
		if (((TypeAtom) to).representation.equals(TypeRepresentation.WILDCARD)
				|| from.representation.equals(TypeRepresentation.WILDCARD)) {
			return converted;
		}

		Expression e = TypeEnvironment.singleton.convertTo(converted, from, (TypeAtom) to);

		return e;
	}
	
	private static Expression convertTypeTuple(TypeTuple from, Expression converted, Type to) throws AppendableException {
		if (to instanceof TypeVariable || from.equals(to)) {
			return converted;
		}
		if (!(to instanceof TypeTuple) || (from.size() != ((TypeTuple) to).size())) {
			throw new ConversionException(from, to, converted);
		}

		final Symbol symbol = new Symbol(NameGenerator.next());
		List<Expression> l = new LinkedList<Expression>();
		TypeTuple toTuple = (TypeTuple) to;
		Iterator<Type> i = from.iterator();
		Iterator<Type> j = toTuple.iterator();
		int k = 0;
		boolean anyConverted = false;
		while (i.hasNext()) {
			Type subfrom = i.next();
			Type subto = j.next();
			final int constK = k;

			Expression e = new Expression() {

				@Override
				public Expression interpret(Environment env) throws AppendableException {
					Expression e = symbol.interpret(env);
					if(!(e instanceof Tuple)) {
						throw new AppendableException("Expected tuple got " + e.toString());
					}
					Tuple t = (Tuple) e;
					// Should not interpret converted value again as it will already be bound
					// interpreted
					return t.get(constK);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					return new Pair<Type, Substitution>(subfrom, Substitution.EMPTY);
				}

				@Override
				public String toClojureCode(Environment env) throws AppendableException {
					return "(get " + symbol.toClojureCode(env) + " " + constK + ")";
				}
				
				@Override
				public String toString() {
					return "(get " + symbol.toString() + " " + constK + ")";
				}

			};
			
			Expression subconverted = Conversions.convert(subfrom, e, subto);
			if(!anyConverted && !subconverted.equals(e)) {
				anyConverted = true;
			}

			l.add(subconverted);
			k++;
		}
		
		if(!anyConverted) {
			return converted;
		}

		Tuple conversionTuple = new Tuple(l) {
			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression e = converted.interpret(env);
				Environment bound = Environment.create(env);
				bound.put(symbol, e);
				return super.interpret(bound);
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				Pair<Type, Substitution> p = converted.infer(env);
				return new Pair<Type, Substitution>(to, p.second);
			}

			@Override
			public String toClojureCode(Environment env) throws AppendableException {
				return "((fn [" + symbol.toClojureCode(env) + "] " + super.toClojureCode(env) + ") "
						+ converted.toClojureCode(env) + ")";
			}
		};

		return conversionTuple;
	}
	
	private static Expression convertTypeVariable(TypeVariable from, Expression converted, Type to) {
		return converted;
	}
}
