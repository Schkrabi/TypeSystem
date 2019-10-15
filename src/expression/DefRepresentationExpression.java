package expression;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import interpretation.Environment;
import operators.Operator;
import parser.SemanticNode;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import util.ThrowingFunction;

/**
 * Expression for defrepresentation special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefRepresentationExpression extends Expression {

	/**
	 * Name of type for which representation is defined
	 */
	public final TypeName typeName;
	/**
	 * Name of representation which is defined
	 */
	public final TypeRepresentation representation;
	/**
	 * Members of composite representation that is defined
	 */
	private final List<SemanticNode> members;

	public DefRepresentationExpression(TypeName typeName, TypeRepresentation representation,
			List<SemanticNode> members) {
		super();
		this.typeName = typeName;
		this.representation = representation;
		this.members = new ArrayList<SemanticNode>(members);
	}

	@Override
	public String toString() {
		return "(defrepresentaion " + this.typeName.toString() + " " + this.representation.toString() + " ("
				+ this.members.stream().map(x -> x.toString()).reduce("", (x, y) -> x + " " + y) + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefRepresentationExpression) {
			return this.typeName.equals(((DefRepresentationExpression) other).typeName)
					&& this.representation.equals(((DefRepresentationExpression) other).representation)
					&& this.members.equals(((DefRepresentationExpression) other).members);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.typeName.hashCode() * this.representation.hashCode() * this.members.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefRepresentationExpression) {
			int cmp = this.typeName.compareTo(((DefRepresentationExpression) other).typeName);
			if (cmp != 0)
				return cmp;
			cmp = this.representation.compareTo(((DefRepresentationExpression) other).representation);
			if (cmp != 0)
				return cmp;

			cmp = this.members.size() - ((DefRepresentationExpression) other).members.size();
			if (cmp != 0)
				return cmp;

			Iterator<SemanticNode> i = this.members.iterator();
			Iterator<SemanticNode> j = ((DefRepresentationExpression) other).members.iterator();

			while (i.hasNext()) {
				String t = i.next().toString();
				String u = j.next().toString();
				cmp = t.compareTo(u);
				if (cmp != 0)
					return cmp;
			}

			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		TypeAtom type = new TypeAtom(this.typeName, this.representation);
		Tuple args = new Tuple(
				this.members.stream().map(x -> new Variable(NameGenerator.next())).collect(Collectors.toList()));

		TypeTuple memberTypes = new TypeTuple(this.members.stream()
				.map(ThrowingFunction
						.wrapper(x -> TypeEnvironment.singleton.isType(x) ? TypeEnvironment.singleton.getType(x).get()
								: new TypeVariable(x.asSymbol())))
				.collect(Collectors.toList()));

		Function constructor = new Function(memberTypes, args, new LitComposite(args, type),
				Environment.topLevelEnvironment);

		TypeEnvironment.singleton.addRepresentation(type, constructor);
		env.put(new Variable(type.toString()), constructor);

		Operator.makeGetters(type, memberTypes).stream().forEach(x -> env.put(new Variable(x.symbol), x));

		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO Auto-generated method stub
		throw new AppendableException("Not Implemented");
	}

}
