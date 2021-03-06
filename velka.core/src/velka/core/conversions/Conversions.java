package velka.core.conversions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.TypeEnvironment;
import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.NameGenerator;

public class Conversions {
	
	/**
	 * Clojure namespace
	 */
	public static final String NAMESPACE = "velka.clojure.conversions";
	
	public static Expression convert(Type from, Expression converted, Type to, TypeEnvironment typeEnv) throws AppendableException {
		if(to instanceof TypeVariable) {
			return Conversions.convertToTypeVariable(from, converted, (TypeVariable)to, typeEnv);
		}
		else if(to instanceof RepresentationOr) {
			return Conversions.convertToRepresentationOr(from, converted, (RepresentationOr)to, typeEnv);
		}
		else if(from instanceof RepresentationOr) {
			return Conversions.convertRepresentationOr((RepresentationOr)from, converted, to, typeEnv);
		}
		else if(from instanceof TypeArrow) {
			return Conversions.convertTypeArrow((TypeArrow)from, converted, to, typeEnv);
		} else if(from instanceof TypeAtom) {
			return Conversions.convertTypeAtom((TypeAtom)from, converted, to, typeEnv);
		} else if(from instanceof TypeTuple) {
			return Conversions.convertTypeTuple((TypeTuple)from, converted, to, typeEnv);
		} else if(from instanceof TypeVariable) {
			return Conversions.convertTypeVariable((TypeVariable)from, converted, to, typeEnv);
		}
		throw new AppendableException("Unrecognized type " + from + " for conversion");
	}
	
	private static Expression convertToTypeVariable(Type from, Expression converted, TypeVariable to, TypeEnvironment typeEnv) {
		return converted;
	}
	
	private static Expression convertToRepresentationOr(Type from, Expression converted, RepresentationOr to, TypeEnvironment typeEnv) throws AppendableException{
		for(Type t : to.getRepresentations()) {
			if(Type.unifyTypes(from, t).isPresent()) {
				return converted;
			}
		}
		
		throw new ConversionException(from, to, converted);
	}

	private static Expression convertRepresentationOr(RepresentationOr from, Expression converted, Type to, TypeEnvironment typeEnv) throws AppendableException{
		for(Type t : from.getRepresentations()) {
			if(Type.unifyTypes(t, to).isPresent()) {
				return converted;
			}
		}
		
		throw new ConversionException(from, to, converted);
	}
	
	private static Expression convertTypeArrow(TypeArrow from, Expression converted, Type to, TypeEnvironment typeEnv) throws AppendableException {
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
		Tuple realArgs = (Tuple) Conversions.convert(t.ltype, formalArgs, from.ltype, typeEnv);
		Expression body = Conversions.convert(from.rtype, new AbstractionApplication(converted, realArgs), t.rtype, typeEnv);

		Lambda l = new Lambda(formalArgs, argsType, body);
		return l;
	}
	
	private static Expression convertTypeAtom(TypeAtom from, Expression converted, Type to, TypeEnvironment typeEnv) throws AppendableException {
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

		Expression e = typeEnv.convertTo(converted, from, (TypeAtom) to);

		return e;
	}
	
	private static Expression convertTypeTuple(TypeTuple from, Expression converted, Type to, TypeEnvironment typeEnv) throws AppendableException {
		if (to instanceof TypeVariable || from.equals(to)) {
			return converted;
		}
		
		int size = from.size();
		
		if (!(to instanceof TypeTuple) || (((TypeTuple) to).size() != size)
				|| !(converted instanceof Tuple)) {
			throw new ConversionException(from, to, converted);
		}

		Iterator<Type> itFrom = from.iterator();
		Iterator<Type> itTo = ((TypeTuple)to).iterator();
		Iterator<Expression> itExpr = ((Tuple)converted).iterator();
		
		List<Expression> l = new ArrayList<Expression>(size);
		
		while(itFrom.hasNext()) {
			Type eFrom = itFrom.next();
			Type eTo = itTo.next();
			Expression e = itExpr.next();
			
			l.add(Conversions.convert(eFrom, e, eTo, typeEnv));
		}
		
		return new Tuple(l);
	}
	
	private static Expression convertTypeVariable(TypeVariable from, Expression converted, Type to, TypeEnvironment typeEnv) {
		return converted;
	}
}
