package semantic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import expression.Application;
import expression.Constructor;
import expression.Expression;
import expression.ExtendedLambda;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Sequence;
import expression.Tuple;
import expression.Variable;

import parser.SemanticNode;
import parser.SemanticNode.Pair;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;

public class SemanticParser {
	private Set<TypeConcrete> types = new HashSet<TypeConcrete>();
	private Map<TypeConcrete, Constructor> constructorMap = new HashMap<TypeConcrete, Constructor>();

	

	public SemanticParser() {
		// Int
		types.add(TypeConcrete.TypeInt);
		constructorMap.put(TypeConcrete.TypeInt,
				Constructor.IntPrimitiveConstructor);
		types.add(TypeRepresentation.TypeIntRoman);
		constructorMap.put(TypeRepresentation.TypeIntRoman,
				Constructor.IntRomanConstructor);
		types.add(TypeRepresentation.TypeIntString);
		constructorMap.put(TypeRepresentation.TypeIntString,
				Constructor.IntStringConstructor);

		// Bool
		types.add(TypeConcrete.TypeBool);
		constructorMap.put(TypeConcrete.TypeBool,
				Constructor.BoolPrimitiveConstructor);

		// String
		types.add(TypeConcrete.TypeString);
		constructorMap.put(TypeConcrete.TypeString,
				Constructor.StringPrimitiveConstructor);

		// Double
		types.add(TypeConcrete.TypeDouble);
		constructorMap.put(TypeConcrete.TypeDouble,
				Constructor.DoublePrimitiveConstructor);
	}

	public Expression parseTokenList(List<SemanticNode> l) throws Exception {
		if (l.isEmpty()) {
			return Expression.EMPTY_EXPRESSION;
		}

		SemanticNode head = l.get(0);

		if (head.type == SemanticNode.NodeType.SYMBOL) {
			// Special forms
			switch (head.asSymbol()) {
			case DEFTYPE:
				this.parseDeftype(l);
				return Expression.EMPTY_EXPRESSION;
			case DEFREP:
				this.parseDefrep(l);
				return Expression.EMPTY_EXPRESSION;
			case LAMBDA:
				return this.parseLambda(l);
			case ELAMBDA:
				return this.parseElambda(l);
			case IF:
				return this.parseIf(l);
			default:
				// Type construction - Base Types
				Optional<TypeConcrete> o = this.getType(head.asSymbol());
				if (o.isPresent()) {
					return this.constructType(o.get(), l);
				}
			}
		}

		// Type construction - Type Representations
		if (head.type == SemanticNode.NodeType.PAIR) {
			Optional<TypeConcrete> o = this.getType(head.asPair().lvalue,
					head.asPair().rvalue);
			if (!o.isPresent()) {
				throw new Exception("Unknown type constructor " + head.asPair()
						+ " in " + l);
			}
			return this.constructType(o.get(), l);
		}

		List<Expression> tmp = new ArrayList<Expression>();

		for (SemanticNode t : l) {
			Expression e = this.parseAtom(t);
			tmp.add(e);
		}

		return new Sequence(tmp);
	}

	public Expression parseAtom(SemanticNode token) throws Exception {
		Literal l;
		switch (token.type) {
		case SYMBOL:
			return new Variable(token.asSymbol());
		case PAIR:
			throw new Exception("Unexpected pair " + token);
		case INT:
			l = new LitInteger(token.asInt());
			l.setLiteralType(TypeConcrete.TypeInt);
			return l;
		case DOUBLE:
			l = new LitDouble(token.asDouble());
			l.setLiteralType(TypeConcrete.TypeDouble);
			return l;
		case STRING:
			l = new LitString(token.asString());
			l.setLiteralType(TypeConcrete.TypeString);
			return l;
		case BOOL:
			return token.asBool() ? LitBoolean.TRUE : LitBoolean.FALSE;
		case LIST:
			return this.parseTokenList(token.asList());
		}
		throw new Exception("Unrecognized token type!");
	}

	private void parseDeftype(List<SemanticNode> l) throws Exception {
		// Deftype has form:
		// (deftype TYPENAME TYPELIST CONSTRUCTOR)
		if (l.size() != 3) {
			throw new Exception("Badly formed deftype " + l);
		}

		// Type Name
		SemanticNode typeNameToken = l.get(1);
		if (typeNameToken.type != SemanticNode.NodeType.SYMBOL) {
			throw new Exception(
					"Expected symbol in 2nd position in deftype in " + l);
		}
		String typeName = typeNameToken.asSymbol();

		// Constructor
		SemanticNode constructorToken = l.get(2);
		if (constructorToken.type != SemanticNode.NodeType.LIST) {
			throw new Exception(
					"Expected constructor on 4th position in deftype in " + l
							+ " got " + constructorToken);
		}
		List<SemanticNode> constructorList = constructorToken.asList();
		if (constructorList.get(0).type != SemanticNode.NodeType.SYMBOL
				&& constructorList.get(0).asSymbol() != LAMBDA) {
			throw new Exception(constructorToken.toString()
					+ " is not a valid constructor in " + l);
		}
		Lambda constructor = this.parseLambda(constructorList);

		this.defineType(typeName, constructor);
	}

	private void parseDefrep(List<SemanticNode> l) throws Exception {
		// Defrep has form:
		// (defrep REPNAME BASETYPE TYPELIST CONSTRUCTOR)
		if (l.size() != 4) {
			throw new Exception("Badly formed defrep: " + l);
		}

		// Representation Name
		SemanticNode repNameToken = l.get(1);
		if (repNameToken.type != SemanticNode.NodeType.SYMBOL) {
			throw new Exception(
					"Expected symbol on 2nd position of defrep got "
							+ repNameToken);
		}
		String repName = repNameToken.asSymbol();

		// BaseType
		SemanticNode baseTypeNameToken = l.get(2);
		if (baseTypeNameToken.type != SemanticNode.NodeType.SYMBOL) {
			throw new Exception("In" + l
					+ "Expected symvol on 3rd position of defrep got "
					+ baseTypeNameToken);
		}
		String baseTypeName = baseTypeNameToken.asSymbol();

		// Constructor
		SemanticNode constructorToken = l.get(3);
		if (constructorToken.type != SemanticNode.NodeType.LIST) {
			throw new Exception(
					"Expected constructor on 5th position in defrep in " + l
							+ " got " + constructorToken);
		}
		List<SemanticNode> constructorList = constructorToken.asList();
		if (constructorList.get(0).type != SemanticNode.NodeType.SYMBOL
				&& constructorList.get(0).asSymbol() != LAMBDA) {
			throw new Exception(constructorToken.toString()
					+ " is not a valid constructor in " + l);
		}
		Lambda constructor = this.parseLambda(constructorList);

		this.defineRepresentation(baseTypeName, repName, constructor);
	}

	private Lambda parseLambda(List<SemanticNode> l) throws Exception {
		// Lambda has form:
		// (lambda ARGS BODY)
		if (l.size() != 3) {
			throw new Exception("Badly formed lambda " + l);
		}

		// Arg list
		SemanticNode argsListToken = l.get(1);
		if (argsListToken.type != SemanticNode.NodeType.LIST) {
			throw new Exception("Expected argument list in " + l + " got "
					+ argsListToken);
		}
		List<VariableTypePair> typedArgs = this.parseTypedArgList(argsListToken
				.asList());
		boolean isTyped = false;

		for (VariableTypePair vtp : typedArgs) {
			if (vtp.type != null) {
				isTyped = true;
			}
			if (vtp.type == null && isTyped) {
				// TODO přidat podporu částečně typovaných argument listů?
				throw new Exception(
						"Paritaly typed lambda argument list found in"
								+ l
								+ " only fully typed or untiped argument lists are currently supported");
			}
		}

		List<Expression> variables = new ArrayList<Expression>();
		for (VariableTypePair vtp : typedArgs) {
			variables.add(vtp.variable);
		}

		Tuple argsTuple = new Tuple(variables.toArray(new Expression[variables
				.size()]));

		// Body
		Expression e = this.parseAtom(l.get(2));

		if (isTyped) {
			List<Type> typeList = new ArrayList<Type>();
			for (VariableTypePair vtp : typedArgs) {
				typeList.add(vtp.type);
			}
			TypeTuple typeTuple = new TypeTuple(
					typeList.toArray(new Type[typeList.size()]));

			return new Lambda(argsTuple, typeTuple, e);
		}

		return new Lambda(argsTuple, e);
	}

	private ExtendedLambda parseElambda(List<SemanticNode> l) throws Exception {
		// Extended lambda has form:
		// (elambda ARGS DEFAULTIMPL (TYPELIST IMPL)*)
		if (l.size() < 3) {
			throw new Exception("Badly formed extended lambda " + l);
		}

		// args list
		SemanticNode argsListToken = l.get(1);
		if (argsListToken.type != SemanticNode.NodeType.LIST) {
			throw new Exception("Expected argument list in " + l + " got "
					+ argsListToken);
		}
		Tuple argsTuple = this.parseArgsList(argsListToken.asList());

		// Default implementation
		Expression defaultImplementation = this.parseAtom(l.get(2));

		// Implementations
		Set<Lambda> implementations = new TreeSet<Lambda>();
		int i = 0;
		for (SemanticNode t : l) {
			if (i < 3) {
				i++;
				continue;
			}

			if (t.type != SemanticNode.NodeType.LIST || t.asList().size() != 2) {
				throw new Exception("Expected implementaion in " + l + " got "
						+ t);
			}
			SemanticNode typeListToken = t.asList().get(0);
			SemanticNode implToken = t.asList().get(1);

			if (typeListToken.type != SemanticNode.NodeType.LIST) {
				throw new Exception("Expected type list in implementation " + t
						+ " got " + typeListToken);
			}
			TypeTuple typeList = this.parseTypeList(typeListToken.asList());
			Expression impl = this.parseAtom(implToken);
			implementations.add(new Lambda(argsTuple, typeList, impl));
		}

		return new ExtendedLambda(argsTuple, defaultImplementation,
				implementations);
	}

	private Expression parseIf(List<SemanticNode> l) throws Exception {
		// If has form:
		// (if PRED TRUEBRANCH FALSEBRANCH)
		if (l.size() != 4) {
			throw new Exception("Badly formed if " + l);
		}
		Expression pred = this.parseAtom(l.get(1));
		Expression trueBranch = this.parseAtom(l.get(2));
		Expression falseBranch = this.parseAtom(l.get(3));

		return new IfExpression(pred, trueBranch, falseBranch);
	}

	private Optional<TypeConcrete> getType(final String typeName) {
		Optional<TypeConcrete> o = types.stream()
				.filter(new java.util.function.Predicate<TypeConcrete>() {

					@Override
					public boolean test(TypeConcrete x) {
						return (!(x instanceof TypeRepresentation))
								&& x.name.equals(typeName);
					}
				}).findAny();
		return o;
	}

	private Optional<TypeConcrete> getType(final String typeName,
			final String representationName) {
		Optional<TypeConcrete> o = types.stream()
				.filter(new java.util.function.Predicate<TypeConcrete>() {
					@Override
					public boolean test(TypeConcrete x) {
						return (x instanceof TypeRepresentation)
								&& x.name.equals(representationName)
								&& ((TypeRepresentation) x).baseType.name
										.equals(typeName);
					}
				}).findAny();
		return o;
	}

	private void defineType(String typeName, Lambda lConstructor)
			throws Exception {
		TypeConcrete newType = new TypeConcrete(typeName);

		if (lConstructor.argsType == null) {
			throw new Exception(
					"Untiped constructor arguments found in definiton of "
							+ typeName);
		}
		Constructor constructor = new Constructor(newType, lConstructor.args,
				lConstructor.argsType, lConstructor.body);

		this.addType(newType);
		this.addConstructor(newType, constructor);
	}

	private void defineRepresentation(final String typeName, String repName,
			Lambda lConstructor) throws Exception {
		Optional<TypeConcrete> o = types.stream()
				.filter(new java.util.function.Predicate<TypeConcrete>() {
					@Override
					public boolean test(TypeConcrete x) {
						return (!(x instanceof TypeRepresentation))
								&& x.name.equals(typeName);
					}
				}).findAny();
		if (!o.isPresent()) {
			throw new Exception("Unknown base type: " + typeName);
		}

		TypeConcrete baseType = o.get();
		TypeRepresentation newType = new TypeRepresentation(repName, baseType);

		Constructor constructor = new Constructor(newType, lConstructor.args,
				lConstructor.argsType, lConstructor.body);

		if (lConstructor.argsType == null) {
			throw new Exception(
					"Untiped constructor arguments found in definiton of "
							+ typeName + ":" + repName);
		}

		this.addType(newType);
		this.addConstructor(newType, constructor);
	}

	private void addType(TypeConcrete newType) throws Exception {
		if (types.contains(newType)) {
			throw new Exception("Type " + newType + " is already defined");
		}
		types.add(newType);
	}

	private void addConstructor(TypeConcrete newType, Constructor constructor)
			throws Exception {

		if (constructorMap.containsKey(newType)) {
			throw new Exception("Constructor for " + newType
					+ " is already defined");
		}
		constructorMap.put(newType, constructor);
	}

	private TypeTuple parseTypeList(List<SemanticNode> l) throws Exception {
		Type[] realTypes = new Type[l.size()];
		int i = 0;
		for (SemanticNode t : l) {
			Optional<TypeConcrete> o;
			if (t.type == SemanticNode.NodeType.SYMBOL) {
				o = this.getType(t.asSymbol());
			} else if (t.type == SemanticNode.NodeType.PAIR) {
				Pair p = t.asPair();
				o = this.getType(p.lvalue, p.rvalue);
			} else {
				throw new Exception("Invalid token " + t + " in type list");
			}

			if (!o.isPresent()) {
				throw new Exception("Unknown type " + t);
			}
			realTypes[i] = o.get();
			i++;
		}
		return new TypeTuple(realTypes);
	}

	private Tuple parseArgsList(List<SemanticNode> l) throws Exception {
		Expression[] args = new Expression[l.size()];
		int i = 0;
		for (SemanticNode t : l) {
			if (t.type != SemanticNode.NodeType.SYMBOL) {
				throw new Exception("Invalid token in argument list " + t);
			}
			args[i] = new Variable(t.asSymbol());
			i++;
		}
		return new Tuple(args);
	}

	private static class VariableTypePair {
		public final Type type;
		public final Variable variable;

		public VariableTypePair(Type type, Variable variable) {
			this.type = type;
			this.variable = variable;
		}
	}

	private List<VariableTypePair> parseTypedArgList(List<SemanticNode> l)
			throws Exception {
		List<VariableTypePair> parsed = new ArrayList<VariableTypePair>();

		for (SemanticNode n : l) {
			VariableTypePair p;
			Type t;
			Variable v;

			if (n.type == SemanticNode.NodeType.SYMBOL) {
				p = new VariableTypePair(null, new Variable(n.asSymbol()));
			} else if (n.type == SemanticNode.NodeType.LIST) {
				List<SemanticNode> inner = n.asList();
				if (inner.size() != 2) {
					throw new Exception("Invalid token " + inner
							+ " in typed argument list " + l);
				}
				SemanticNode typeToken = n.asList().get(0);
				SemanticNode variableToken = n.asList().get(1);

				Optional<TypeConcrete> o;
				if (typeToken.type == SemanticNode.NodeType.PAIR) {
					Pair typePair = typeToken.asPair();
					o = this.getType(typePair.lvalue, typePair.rvalue);

				} else if (typeToken.type == SemanticNode.NodeType.SYMBOL) {
					o = this.getType(typeToken.asSymbol());
				} else {
					throw new Exception("Unexpected token " + typeToken
							+ " in " + n + " in " + l + " expected type token");
				}
				if (!o.isPresent()) {
					throw new Exception("Unknown type " + typeToken + " in "
							+ l);
				}
				t = o.get();

				if (variableToken.type == SemanticNode.NodeType.SYMBOL) {
					v = new Variable(variableToken.asSymbol());
				} else {
					throw new Exception("Unexpected token " + variableToken
							+ " in " + l);
				}

				p = new VariableTypePair(t, v);
			} else {
				throw new Exception("Invalid token " + n
						+ " in typed argument list " + l);
			}
			parsed.add(p);
		}

		return parsed;
	}
	
	/**
	 * Checks if given semantic node is special form reserved word
	 * @param node inspected node
	 * @return true if node is a symbol node containing special form, false otherwise
	 */
	private boolean isSpecialForm(SemanticNode node){
		if(node.type != SemanticNode.NodeType.SYMBOL){
			return false;
		}
		try {
			return SemanticParserConstants.isSpecialForm(node.asSymbol());
		} catch (Exception e) {
			return false;
		}
	}
	
	/**
	 * Checks if given semantic node is previously defined type
	 * @param node inspected node
	 * @return true if node is a node containing type identificator
	 */
	private boolean isType(SemanticNode node){
		if(node.type == SemanticNode.NodeType.SYMBOL){
			try {
				return this.getType(node.asSymbol()).isPresent();
			} catch (Exception e) {
				return false;
			}
		}
		if(node.type == SemanticNode.NodeType.PAIR){
			Pair p;
			try {
				p = node.asPair();
			} catch (Exception e) {
				return false;
			}
			return this.getType(p.lvalue, p.rvalue).isPresent();
		}
		return false;
	}
	
	private Expression parseSpecialForm(String specialForm, List<SemanticNode> args){
		Expression e;
		switch(specialForm){
		case SemanticParserConstants.DEFREP:
			//TODO
			e = Expression.EMPTY_EXPRESSION;
			break;
		case SemanticParserConstants.DEFTYPE:
			//TODO
			e = Expression.EMPTY_EXPRESSION;
			break;
		case SemanticParserConstants.ELAMBDA:
			//TODO;
			e = null;
			break;
		case SemanticParserConstants.IF:
			//TODO;
			e = null;
			break;
		case SemanticParserConstants.LAMBDA:
			e = null;
			break;
		default:
			//TODO exception	
			e = null;
		}
		
		return e;
	}
	
	private Expression parseTypeConstruction(TypeConcrete type,
			List<SemanticNode> argsList) throws Exception {
		Constructor c = this.constructorMap.get(type);
		Expression[] args = new Expression[argsList.size()];
		int i = 0;
		for (SemanticNode t : argsList) {
			Expression e = this.parseAtom(t);
			args[i] = e;
			i++;
		}
		Tuple argsTuple = new Tuple(args);

		return new Application(c, argsTuple);
	}
	
	private Expression parseList(List<SemanticNode> list) throws Exception{
		List<Expression> l = new ArrayList<Expression>();
		
		for(SemanticNode n : list){
			try{
				l.add(this.parseAtom(n));
			}catch(Exception e){
				throw new Exception(e.getMessage() + ": in " +  list);
			}
			
		}
		
		return new Sequence(l);	
	}
}
