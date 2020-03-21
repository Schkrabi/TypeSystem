package application;

import java.util.Arrays;

import expression.Expression;
import expression.TypeHolder;
import expression.Symbol;
import interpretation.Environment;
import abstraction.Abstraction;
import abstraction.Lambda;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

/**
 * Expression for defconversion special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefConversionExpression extends Expression {

	/**
	 * Type from which we are converting
	 */
	TypeAtom fromType;
	/**
	 * Type to which expression is converting
	 */
	TypeAtom toType;
	/**
	 * Conversion lambda
	 */
	Lambda conversion;

	public DefConversionExpression(TypeAtom fromType, TypeAtom toType, Lambda conversion) {
		super();
		this.fromType = fromType;
		this.toType = toType;
		this.conversion = conversion;
	}

	@Override
	public String toString() {
		return "(defconversion " + fromType.toString() + " " + toType.toString() + " " + this.conversion + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefConversionExpression) {
			return this.fromType.equals(((DefConversionExpression) other).fromType)
					&& this.toType.equals(((DefConversionExpression) other).toType)
					&& this.conversion.equals(((DefConversionExpression) other).conversion);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.fromType.hashCode() * this.toType.hashCode() * this.conversion.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefConversionExpression) {
			int cmp = this.fromType.compareTo(((DefConversionExpression) other).fromType);
			if (cmp != 0)
				return cmp;
			cmp = this.toType.compareTo(((DefConversionExpression) other).toType);
			if (cmp != 0)
				return cmp;
			return this.conversion.compareTo(((DefConversionExpression) other).conversion);
		}
		return super.compareTo(other);
	}

	/**
	 * Creates converison operator
	 * 
	 * @param env environment where constructor is evaluated
	 * 
	 * @return conversion operator for this defconversion
	 */
	private Abstraction makeConverisionOperator(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.conversion.infer(env);
		TypeArrow expected = new TypeArrow(new TypeTuple(Arrays.asList(this.fromType)), this.toType);
		if (!p.first.equals(expected)) {
			throw new AppendableException("Invalid conversion expected " + expected + " got " + p.first);
		}

		return (Abstraction)this.conversion.interpret(env);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Abstraction conversion = (Abstraction)this.makeConverisionOperator(env);

		TypeEnvironment.singleton.addConversion(this.fromType, this.toType, conversion);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.conversion.infer(env);
		TypeArrow type = (TypeArrow) p.first;
		Type.unify(type.ltype, new TypeTuple(Arrays.asList(this.fromType)));
		Type.unify(type.rtype, this.toType);
		return Expression.EMPTY_EXPRESSION.infer(env);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		Symbol convName = new Symbol(TypeEnvironment.makeConversionName(this.fromType, this.toType));
		TypeEnvironment.singleton.addConversion(this.fromType, this.toType, convName);
		Environment.topLevelEnvironment.put(convName,
				new TypeHolder(new TypeArrow(new TypeTuple(Arrays.asList(this.fromType)), this.toType)));
		return "(def " + TypeEnvironment.makeConversionName(this.fromType, this.toType) + " "
				+ this.conversion.toClojureCode(env) + ")";
	}

}
