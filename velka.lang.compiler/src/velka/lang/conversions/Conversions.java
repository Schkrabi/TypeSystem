package velka.lang.conversions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import velka.lang.abstraction.Lambda;
import velka.lang.application.AbstractionApplication;
import velka.lang.exceptions.ConversionException;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;

public class Conversions {
	
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
