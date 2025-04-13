package velka.parser.antlr;

import java.util.List;

import velka.util.AppendableException;
import velka.util.Pair;

public class SemanticNode {
	public final NodeType type;
	private final Object value;

	private SemanticNode(NodeType type, Object value) {
		this.type = type;
		this.value = value;
	}

	/**
	 * Makes new SemanticNode
	 * 
	 * @param type
	 * @param value
	 * @return
	 * @throws AppendableException
	 */
	public static SemanticNode make(NodeType type, Object value) throws AppendableException {
		if (!isAdequateType(type, value)) {
			throw new AppendableException(type.toString() + value + " is not an adequate type-value combination");
		}
		return new SemanticNode(type, value);
	}

	public static boolean isAdequateType(NodeType type, Object value) {
		switch (type) {
		case SYMBOL:
			return value instanceof String;
		case PAIR:
			return value instanceof SemanticPair;
		case INT:
			return value instanceof Integer;
		case DOUBLE:
			return value instanceof Double;
		case STRING:
			return value instanceof String;
		case BOOL:
			return value instanceof Boolean;
		case LIST:
			return value instanceof List<?>;
		case ARROW:
			if (!(value instanceof Pair)) {
				return false;
			}
			@SuppressWarnings("unchecked")
			Pair<Object, Object> v = (Pair<Object, Object>) value;
			return (v.first instanceof SemanticNode) && (v.second instanceof SemanticNode);
		default:
			break;
		}
		return false;
	}

	public String asSymbol() throws AppendableException {
		if (this.type != NodeType.SYMBOL) {
			throw new AppendableException("" + this + " is not a symbol");
		}
		return (String) this.value;
	}

	public SemanticPair asPair() throws AppendableException {
		if (this.type != NodeType.PAIR) {
			throw new AppendableException("" + this + " is not a pair");
		}
		return (SemanticPair) this.value;
	}

	public Integer asInt() throws AppendableException {
		if (this.type != NodeType.INT) {
			throw new AppendableException("" + this + " is not an integer");
		}
		return (Integer) this.value;
	}

	public Double asDouble() throws AppendableException {
		if (this.type != NodeType.DOUBLE) {
			throw new AppendableException("" + this + " is not a double");
		}
		return (Double) this.value;
	}

	public String asString() throws AppendableException {
		if (this.type != NodeType.STRING) {
			throw new AppendableException("" + this + " is not a string");
		}
		return (String) this.value;
	}

	public Boolean asBool() throws AppendableException {
		if (this.type != NodeType.BOOL) {
			throw new AppendableException("" + this + " is not a bool");
		}
		return (Boolean) this.value;
	}

	@SuppressWarnings("unchecked")
	public Pair<SemanticNode, SemanticNode> asArrow() throws AppendableException {
		if (this.type != NodeType.ARROW) {
			throw new AppendableException(this.toString() + " is not an arrow");
		}
		return (Pair<SemanticNode, SemanticNode>) this.value;
	}

	@SuppressWarnings("unchecked")
	public List<SemanticNode> asList() throws AppendableException {
		if (this.type != NodeType.LIST) {
			throw new AppendableException("" + this + " is not a list");
		}
		return (List<SemanticNode>) this.value;
	}

	@Override
	public String toString() {
		return this.value.toString();
	}

	public enum NodeType {
		SYMBOL, PAIR, INT, DOUBLE, STRING, BOOL, LIST, ARROW,
		/** Unused NodeType for testing purposes onyl */
		UNUSED
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof SemanticNode) {
			return this.type == ((SemanticNode) other).type && this.value.equals(((SemanticNode) other).value);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.type.hashCode() * this.value.hashCode();
	}
}
