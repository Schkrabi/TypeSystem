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
import util.AppendableException;

public class SemanticParser {
	private Set<TypeConcrete> types = new HashSet<TypeConcrete>();
	private Map<TypeConcrete, Constructor> constructorMap = new HashMap<TypeConcrete, Constructor>();

	public SemanticParser() {
		// Int
		types.add(TypeConcrete.TypeInt);
		constructorMap.put(TypeConcrete.TypeInt, Constructor.IntPrimitiveConstructor);
		types.add(TypeRepresentation.TypeIntRoman);
		constructorMap.put(TypeRepresentation.TypeIntRoman, Constructor.IntRomanConstructor);
		types.add(TypeRepresentation.TypeIntString);
		constructorMap.put(TypeRepresentation.TypeIntString, Constructor.IntStringConstructor);

		// Bool
		types.add(TypeConcrete.TypeBool);
		constructorMap.put(TypeConcrete.TypeBool, Constructor.BoolPrimitiveConstructor);

		// String
		types.add(TypeConcrete.TypeString);
		constructorMap.put(TypeConcrete.TypeString, Constructor.StringPrimitiveConstructor);

		// Double
		types.add(TypeConcrete.TypeDouble);
		constructorMap.put(TypeConcrete.TypeDouble, Constructor.DoublePrimitiveConstructor);
	}

	/**
	 * Parses list of nodes
	 * @param l
	 * @return
	 * @throws Exception
	 */
	public Expression parseNodelist(List<SemanticNode> l) throws AppendableException {
		if (l.isEmpty()) {
			return Expression.EMPTY_EXPRESSION;
		}
		
		SemanticNode head = SemanticParser.listHead(l);

		if(SemanticParser.isSpecialForm(head)) {
			return this.parseSpecialForm(head.asSymbol(), l);
		}
		if(this.isType(head)) {
			return this.parseTypeConstruction(this.parseType(head), l);
		}
		
		return this.parseList(l); 
	}

	/**
	 * Parses SemanticNode to expression
	 * @param token
	 * @return an Expression
	 * @throws AppendableException
	 */
	public Expression parseNode(SemanticNode token) throws AppendableException {
		Literal l;
		switch (token.type) {
		case SYMBOL:
			return new Variable(token.asSymbol());
		case PAIR:
			throw new AppendableException("Unexpected pair " + token);
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
			return this.parseNodelist(token.asList());
		}
		throw new AppendableException("Unrecognized token type!");
	}

	private ExtendedLambda parseElambda(List<SemanticNode> l) throws AppendableException {
		try {
			SemanticParser.validateElambdaList(l);
		}catch(AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		Tuple argsTuple = SemanticParser.parseArgsList(l.get(1).asList());

		Expression defaultImplementation = this.parseNode(l.get(2));

		Set<Lambda> implementations = new TreeSet<Lambda>();
		int i = 0;
		for (SemanticNode t : l) {
			if (i < 3) {
				i++;
				continue;
			}

			SemanticNode typeListToken = t.asList().get(0);
			SemanticNode implToken = t.asList().get(1);

			if (typeListToken.type != SemanticNode.NodeType.LIST) {
				throw new Exception("Expected type list in implementation " + t + " got " + typeListToken);
			}
			TypeTuple typeList = this.parseTypeList(typeListToken.asList());
			Expression impl = this.parseNode(implToken);
			implementations.add(new Lambda(argsTuple, typeList, impl));
		}

		return new ExtendedLambda(argsTuple, defaultImplementation, implementations);
	}

	/**
	 * Parses the if special form list
	 * @param l parsed list
	 * @return Expression representitng the if
	 * @throws AppendableException
	 */
	private Expression parseIf(List<SemanticNode> l) throws AppendableException {
		try {
			SemanticParser.validateIfList(l);
		}catch(AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		Expression pred = this.parseNode(l.get(1));
		Expression trueBranch = this.parseNode(l.get(2));
		Expression falseBranch = this.parseNode(l.get(3));

		return new IfExpression(pred, trueBranch, falseBranch);
	}

	private Optional<TypeConcrete> getType(final String typeName) {
		Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>() {

			@Override
			public boolean test(TypeConcrete x) {
				return (!(x instanceof TypeRepresentation)) && x.name.equals(typeName);
			}
		}).findAny();
		return o;
	}

	private Optional<TypeConcrete> getType(final String typeName, final String representationName) {
		Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>() {
			@Override
			public boolean test(TypeConcrete x) {
				return (x instanceof TypeRepresentation) && x.name.equals(representationName)
						&& ((TypeRepresentation) x).baseType.name.equals(typeName);
			}
		}).findAny();
		return o;
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

	private static class VariableTypePair {
		public final Type type;
		public final Variable variable;

		public VariableTypePair(Type type, Variable variable) {
			this.type = type;
			this.variable = variable;
		}
	}

	/**
	 * Checks if given semantic node is special form reserved word
	 * 
	 * @param node
	 *            inspected node
	 * @return true if node is a symbol node containing special form, false
	 *         otherwise
	 */
	private static boolean isSpecialForm(SemanticNode node) {
		if (node.type != SemanticNode.NodeType.SYMBOL) {
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
	 * 
	 * @param node
	 *            inspected node
	 * @return true if node is a node containing type identificator
	 */
	private boolean isType(SemanticNode node) {
		if (node.type == SemanticNode.NodeType.SYMBOL) {
			try {
				return this.getType(node.asSymbol()).isPresent();
			} catch (Exception e) {
				return false;
			}
		}
		if (node.type == SemanticNode.NodeType.PAIR) {
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

	/**
	 * Parses list containing special form
	 * 
	 * @param specialForm
	 *            special form reserved word
	 * @param specialFormList
	 *            arguments of the spedcial form
	 * @return
	 * @throws Exception
	 */
	private Expression parseSpecialForm(String specialForm, List<SemanticNode> specialFormList) throws AppendableException {
		Expression e;
		switch (specialForm) {
		case SemanticParserConstants.DEFREP:
			e = this.parseDefrep(specialFormList);
			break;
		case SemanticParserConstants.DEFTYPE:
			e = this.parseDeftype(specialFormList);
			break;
		case SemanticParserConstants.ELAMBDA:
			e = this.parseElambda(specialFormList);
			e = null;
			break;
		case SemanticParserConstants.IF:
			e = this.parseIf(specialFormList);
			break;
		case SemanticParserConstants.LAMBDA:
			e = this.parseLambda(specialFormList);
			break;
		default:
			// TODO exception
			e = null;
		}

		return e;
	}

	/**
	 * Parses list that constructs type
	 * 
	 * @param type
	 *            constructed type
	 * @param typeConstructionList
	 *            arguments for the constructor
	 * @return expression that constructs required type
	 * @throws Exception
	 */
	private Expression parseTypeConstruction(TypeConcrete type, List<SemanticNode> typeConstructionList)
			throws AppendableException {
		Constructor c = this.constructorMap.get(type);
		Expression[] args = new Expression[typeConstructionList.size() - 1];
		int i = 0;
		for (SemanticNode t : SemanticParser.listTail(typeConstructionList)) {
			Expression e = this.parseNode(t);
			args[i] = e;
			i++;
		}
		Tuple argsTuple = new Tuple(args);

		return new Application(c, argsTuple);
	}

	/**
	 * Parses simple list that does not represents special form application or type
	 * construction
	 * 
	 * @param list
	 * @return
	 * @throws Exception
	 */
	private Expression parseList(List<SemanticNode> list) throws AppendableException {
		List<Expression> l = new ArrayList<Expression>();

		for (SemanticNode n : list) {
			try {
				l.add(this.parseNode(n));
			} catch (Exception e) {
				throw new AppendableException(e.getMessage() + ": in " + list);
			}

		}

		return new Sequence(l);
	}

	/**
	 * Parses lambda special form
	 * 
	 * @param lambdaList
	 *            arguments of the special form
	 * @return expresion representing the lambda special form
	 * @throws Exception
	 */
	private Lambda parseLambda(List<SemanticNode> lambdaList) throws AppendableException {
		try {
			SemanticParser.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage(" in Lambda " + lambdaList);
			throw e;
		}

		List<VariableTypePair> typedArgs = this.parseTypedArgList(lambdaList.get(1).asList());

		TypeTuple argsTypes = null;
		if (SemanticParser.isArgListFullyTyped(typedArgs)) {
			List<Type> tmp = SemanticParser.filterTypesFromTypedArgsList(typedArgs);
			argsTypes = new TypeTuple(tmp.toArray(new Type[tmp.size()]));
		} else {
			if (!SemanticParser.isArgListUntypped(typedArgs)) {
				throw new AppendableException("Partially typed argument lists are not supported");
			}
		}

		List<Variable> tmp = SemanticParser.filterVariablesFromTypedArgsList(typedArgs);
		Tuple lambdaArgs = new Tuple(tmp.toArray(new Expression[tmp.size()]));

		Expression body = this.parseNode(lambdaList.get(2));

		return new Lambda(lambdaArgs, argsTypes, body);
	}

	/**
	 * Parses deftype special form and defines given type and constructor
	 * @param deftypeList list representing the special form
	 * @return expression representing special form
	 * @throws AppendableException
	 */
	private Expression parseDeftype(List<SemanticNode> deftypeList) throws AppendableException {
		try {
			SemanticParser.validateDefTypeList(deftypeList);
		} catch (AppendableException e) {
			e.appendMessage(" in deftype " + deftypeList);
		}

		String typeName = deftypeList.get(1).asSymbol();
				
		this.defineType(typeName, deftypeList.get(2).asList());
		
		return Expression.EMPTY_EXPRESSION;
	}
	
	private Expression parseDefrep(List<SemanticNode> defrepList) throws AppendableException {
		try {
			SemanticParser.validateDefRepList(defrepList);
		}catch(AppendableException e) {
			e.appendMessage("in " + defrepList);
			throw e;
		}
		
		String repName = defrepList.get(1).asSymbol();
		String typeName = defrepList.get(2).asSymbol();
		List<SemanticNode> constructorList = defrepList.get(3).asList();
		
		this.defineRepresentation(typeName, repName, constructorList);
		
		return Expression.EMPTY_EXPRESSION;
	}
	
	/**
	 * Creates 
	 * @param type
	 * @param lambdaList
	 * @return
	 * @throws AppendableException
	 */
	private Constructor parseConstructor(TypeConcrete type, List<SemanticNode> lambdaList) throws AppendableException {
		try {
			SemanticParser.validateLambdaList(lambdaList);
		}catch(AppendableException e) {
			e.appendMessage("in " + lambdaList);
			throw e;
		}
		
		Lambda lambda = this.parseLambda(lambdaList);
		
		if(lambda.argsType == null) {
			throw new AppendableException("Constructor argument list must be fully typed");
		}
		
		return new Constructor(type, lambda.args, lambda.argsType, lambda.body);
	}
	
	/**
	 * Parses typed argument list 
	 * @param l parsed list
	 * @return list of variable type pairs
	 * @throws AppendableException
	 */
	private List<VariableTypePair> parseTypedArgList(List<SemanticNode> l) throws AppendableException {
		List<VariableTypePair> parsed = new ArrayList<VariableTypePair>();

		for (SemanticNode n : l) {
			parsed.add(this.parseVariableTypePair(n));
		}

		return parsed;
	}
	
	/**
	 * Defines type in current global environment
	 * @param typeName name of the type
	 * @param lambdaList constructor of the type
	 * @throws AppendableException
	 */
	private void defineType(String typeName, List<SemanticNode> lambdaList) throws AppendableException {
		TypeConcrete type = new TypeConcrete(typeName);

		Constructor constructor = this.parseConstructor(type, lambdaList);

		this.addType(type);
		this.addConstructor(type, constructor);
	}

	/**
	 * Defines type representation in current global environment
	 * @param typeName name of the base type
	 * @param repName name of the representation
	 * @param lambdaList constructor of the representation
	 * @throws AppendableException
	 */
	private void defineRepresentation(final String typeName, String repName,List<SemanticNode> lambdaList) throws AppendableException {
		Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>() {
			@Override
			public boolean test(TypeConcrete x) {
				return (!(x instanceof TypeRepresentation)) && x.name.equals(typeName);
			}
		}).findAny();
		if (!o.isPresent()) {
			throw new AppendableException("Unknown base type: " + typeName);
		}

		TypeConcrete baseType = o.get();
		TypeRepresentation type = new TypeRepresentation(repName, baseType);

		Constructor constructor = this.parseConstructor(type, lambdaList);

		this.addType(type);
		this.addConstructor(type, constructor);
	}

	/**
	 * Adds the type to type set
	 * @param newType
	 * @throws AppendableException
	 */
	private void addType(TypeConcrete newType) throws AppendableException {
		if (types.contains(newType)) {
			throw new AppendableException("Type " + newType + " is already defined");
		}
		types.add(newType);
	}

	/**
	 * Maps the construcotr to given type
	 * @param newType
	 * @param constructor
	 * @throws AppendableException
	 */
	private void addConstructor(TypeConcrete newType, Constructor constructor) throws AppendableException {

		if (constructorMap.containsKey(newType)) {
			throw new AppendableException("Constructor for " + newType + " is already defined");
		}
		constructorMap.put(newType, constructor);
	}
	
	/**
	 * Parses types
	 * @param typeNode node with type anotation
	 * @return Representation of the type
	 * @throws AppendableException
	 */
	private TypeConcrete parseType(SemanticNode typeNode) throws AppendableException{
		Optional<TypeConcrete> o;
		if(typeNode.type == SemanticNode.NodeType.PAIR) {
			Pair p = typeNode.asPair();
			o = this.getType(p.lvalue, p.rvalue);
		}
		else if(typeNode.type == SemanticNode.NodeType.SYMBOL) {
			o = this.getType(typeNode.asSymbol());
		}
		else {
			throw new AppendableException("Invalid type token " + typeNode);
		}
		
		if(!o.isPresent()) {
			throw new UndefinedTypeException(typeNode.toString());
		}
		return o.get();
	}
	
	/**
	 * Parses argument to variable type pair
	 * @param pair
	 * @return
	 * @throws AppendableException
	 */
	private VariableTypePair parseVariableTypePair(SemanticNode pair) throws AppendableException {
		try {
			SemanticParser.validateVariableTypePair(pair);
		}catch(AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}
		
		if(SemanticParser.isSimpleSymbol(pair)) {
			return new VariableTypePair(null, new Variable(pair.asSymbol()));
		}
		
		TypeConcrete type = this.parseType(pair.asList().get(0));
		Variable variable = new Variable(pair.asList().get(0).asSymbol());
		return new VariableTypePair(type, variable);
	}

	/**
	 * Checks the arguments of the Lambda special form
	 * 
	 * @param lambdaList
	 *            checked arguments
	 * @return true if the arguments are valid lambda arguments, false otherwivse
	 * @throws Exception
	 */
	private static boolean validateLambdaList(List<SemanticNode> lambdaList) throws AppendableException {
		if (lambdaList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, lambdaList.size());
		}
		SemanticNode lambdaSymbol = lambdaList.get(0);
		if (lambdaSymbol.type != SemanticNode.NodeType.SYMBOL
				|| lambdaSymbol.asSymbol() != SemanticParserConstants.LAMBDA) {
			throw new UnexpectedExpressionException(lambdaSymbol);
		}

		SemanticNode lambdaArgs = lambdaList.get(1);

		if (lambdaArgs.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(lambdaArgs);
		}

		return true;
	}

	/**
	 * Checks the arguments of the Deftype special form
	 * 
	 * @param defTypeList
	 *            checked arguments
	 * @return true if the arguments are valid otherwise throws Exception
	 * @throws Exception
	 */
	private static boolean validateDefTypeList(List<SemanticNode> defTypeList) throws AppendableException {
		if (defTypeList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, defTypeList.size());
		}
		SemanticNode deftypeSymbol = defTypeList.get(0);
		if (deftypeSymbol.type != SemanticNode.NodeType.SYMBOL
				|| deftypeSymbol.asSymbol() != SemanticParserConstants.DEFTYPE) {
			throw new UnexpectedExpressionException(deftypeSymbol);
		}

		SemanticNode name = defTypeList.get(1);

		if (name.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(name);
		}

		SemanticNode constructor = defTypeList.get(2);
		if (constructor.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(constructor);
		}

		return true;
	}
	
	/**
	 * Checks the arguments of the Defrep special form
	 * @param defRepList checked arguments
	 * @return true if the arguments are valid, otherwise throws exception
	 * @throws AppendableException
	 */
	private static boolean validateDefRepList(List<SemanticNode> defRepList) throws AppendableException{
		if(defRepList.size() != 4) {
			throw new InvalidNumberOfArgsException(3, defRepList.size() - 1);
		}
		SemanticNode defRepSymbol = defRepList.get(0);
		if(defRepSymbol.type != SemanticNode.NodeType.SYMBOL
				|| defRepSymbol.asSymbol() != SemanticParserConstants.DEFREP) {
			throw new UnexpectedExpressionException(defRepSymbol);
		}
		
		SemanticNode rName = defRepList.get(1);
		if(rName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(rName);
		}
		
		SemanticNode tName = defRepList.get(2);
		if(tName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(tName);
		}
		
		SemanticNode constructor = defRepList.get(3);
		if(constructor.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(constructor);
		}
		
		return true;
	}
	
	/**
	 * Checks the arguments of the if special form
	 * @param ifList validated list of id special form 
	 * @return true if it is valid if special form list, throws exception otherwise
	 * @throws AppendableException
	 */
	private static boolean validateIfList(List<SemanticNode> ifList) throws AppendableException{
		if(ifList.size() != 4) {
			throw new InvalidNumberOfArgsException(3, ifList.size() - 1);
		}
		
		return true;
	}
	
	/**
	 * Validates extended lambda special form list
	 * @param l validated list
	 * @return true if list validates, throws exception otherwise
	 * @throws AppendableException
	 */
	private static boolean validateElambdaList(List<SemanticNode> l) throws AppendableException{
		if(l.size() < 3) {
			throw new AppendableException("Too few arguments (" + l.size() + ")");
		}
		
		SemanticNode argsList = l.get(1);
		if (argsList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(argsList);
		}

		int i = 0;
		for (SemanticNode t : l) {
			if (i < 3) {
				i++;
				continue;
			}
			
			SemanticParser.validateVariableTypePair(t);
		}
		
		return true;
	}
	
	private static boolean validateImplementations(List<SemanticNode> l) throws AppendableException{
		for(SemanticNode n : l) {
			if(n.type != SemanticNode.NodeType.LIST) {
				throw new UnexpectedExpressionException(n);
			}
			SemanticParser.validateImplementation(n.asList());
		}
		return true;
	}
	
	/**
	 * Validates
	 * @param l
	 * @return
	 * @throws AppendableException
	 */
	private static boolean validateImplementation(List<SemanticNode> l) throws AppendableException{
		if(l.size() != 2) {
			throw new AppendableException("Badly formed implementation" + l);
		}
		
		SemanticNode typeList = l.get(0);
		
		if(typeList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(typeList);
		}
		return true;
	}

	/**
	 * Checks if the variable-type list is fully typed
	 * 
	 * @param l
	 * @return false if some VariableTypePair has type of null, true otherwise
	 */
	private static boolean isArgListFullyTyped(List<VariableTypePair> l) {
		for (VariableTypePair p : l) {
			if (p.type == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Returns true if the VariableTypePair list is completely untyped - every pair
	 * has type of null. Returns false otherwise
	 * 
	 * @param l
	 *            checked list
	 * @return true or false
	 */
	private static boolean isArgListUntypped(List<VariableTypePair> l) {
		for (VariableTypePair p : l) {
			if (p.type != null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Filters the variables from the variableTypePair list
	 * 
	 * @param l
	 *            VariableTypePair list
	 * @return list of variables
	 */
	private static List<Variable> filterVariablesFromTypedArgsList(List<VariableTypePair> l) {
		List<Variable> r = new ArrayList<Variable>();
		for (VariableTypePair p : l) {
			r.add(p.variable);
		}
		return r;
	}

	/**
	 * Filters the types from the variableTypePair list
	 * 
	 * @param l
	 *            VariableTypePair list
	 * @return list of types
	 */
	private static List<Type> filterTypesFromTypedArgsList(List<VariableTypePair> l) {
		List<Type> r = new ArrayList<Type>();
		for (VariableTypePair p : l) {
			r.add(p.type);
		}
		return r;
	}

	/**
	 * Gets the tail of a list
	 * 
	 * @param l
	 * @return
	 */
	public static <E> List<E> listTail(List<E> l) {
		return l.subList(1, l.size() - 1);
	}
	
	/**
	 * Gets the head of a list
	 * @param l
	 * @return
	 */
	public static <E> E listHead(List<E> l) {
		return l.get(0);
	}
	
	/**
	 * Validates if semantic node is valid variable type pair
	 * @param pair validated semantic node
	 * @return true if node is valid variable type pair, throws exception otherwise
	 * @throws AppendableException
	 */
	private static boolean validateVariableTypePair(SemanticNode pair) throws AppendableException{
		if(SemanticParser.isSimpleSymbol(pair)) {
			return true;
		}
		
		if(pair.type != SemanticNode.NodeType.LIST
				|| pair.asList().size() != 2) {
			throw new UnexpectedExpressionException(pair);
		}
		List<SemanticNode> l = pair.asList();
		
		SemanticNode type = l.get(0);
		if(type.type != SemanticNode.NodeType.SYMBOL
				&& type.type != SemanticNode.NodeType.PAIR) {
			throw new UnexpectedExpressionException(type);
		}
		
		SemanticNode variable = l.get(1);
		if(variable.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(variable);
		}
			
		return true;
	}
	
	/**
	 * Returns true if the semantic node is simple symbol
	 * @param s
	 * @return
	 */
	private static boolean isSimpleSymbol(SemanticNode s) {
		return s.type == SemanticNode.NodeType.SYMBOL;
	}
	
	/**
	 * Parses simple list of formal arguments
	 * @param l parsed argument list
	 * @return tuple of formal arguments
	 * @throws AppendableException
	 */
	private static Tuple parseArgsList(List<SemanticNode> l) throws AppendableException {
		Expression[] args = new Expression[l.size()];
		int i = 0;
		for (SemanticNode t : l) {
			if (t.type != SemanticNode.NodeType.SYMBOL) {
				throw new UnexpectedExpressionException(t);
			}
			args[i] = new Variable(t.asSymbol());
			i++;
		}
		return new Tuple(args);
	}
}
