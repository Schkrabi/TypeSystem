package semantic;

import java.util.ArrayList;
import java.util.List;
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
import types.TypeTuple;
import util.AppendableException;

/**
 * Class for parsing the program in first inner form (list of "tokens") into
 * second inner form (Expression objects)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class SemanticParser {
	
	/**
	 * Environment managing types of this parser
	 */
	public final TypeEnvironment typeEnvironment = new TypeEnvironment();
	
	/**
	 * Parses list of nodes
	 * 
	 * @param l
	 * @return
	 * @throws Exception
	 */
	public Expression parseNodelist(List<SemanticNode> l) throws AppendableException {
		if (l.isEmpty()) {
			return Expression.EMPTY_EXPRESSION;
		}

		SemanticNode head = SemanticParserStatic.listHead(l);

		if (SemanticParserStatic.isSpecialForm(head)) {
			return this.parseSpecialForm(head.asSymbol(), l);
		}
		if (this.typeEnvironment.isType(head)) {
			return this.parseTypeConstruction(this.parseType(head), l);
		}

		return this.parseList(l);
	}

	/**
	 * Parses SemanticNode to expression
	 * 
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

	/**
	 * Parses the extended lambda special form
	 * 
	 * @param l
	 *            list containing the extendedLambda special form
	 * @return Expression represeting the parsed extended lambda
	 * @throws AppendableException
	 */
	private ExtendedLambda parseElambda(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateElambdaList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		Tuple argsTuple = SemanticParserStatic.parseArgsList(l.get(1).asList());
		Expression defaultImplementation = this.parseNode(l.get(2));
		List<SemanticNode> impls = l.subList(3, l.size());
		Set<Lambda> implementations = this.parseImplementations(argsTuple, impls);

		return new ExtendedLambda(argsTuple, defaultImplementation, implementations);
	}

	/**
	 * Parses the if special form list
	 * 
	 * @param l
	 *            parsed list
	 * @return Expression representitng the if
	 * @throws AppendableException
	 */
	private Expression parseIf(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateIfList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		Expression pred = this.parseNode(l.get(1));
		Expression trueBranch = this.parseNode(l.get(2));
		Expression falseBranch = this.parseNode(l.get(3));

		return new IfExpression(pred, trueBranch, falseBranch);
	}

	/**
	 * Parses list containing special form
	 * 
	 * @param specialForm
	 *            special form reserved word
	 * @param specialFormList
	 *            arguments of the special form
	 * @return
	 * @throws Exception
	 */
	private Expression parseSpecialForm(String specialForm, List<SemanticNode> specialFormList)
			throws AppendableException {
		Expression e;
		switch (specialForm) {
		case SemanticParserStatic.DEFREP:
			e = this.parseDefrep(specialFormList);
			break;
		case SemanticParserStatic.DEFTYPE:
			e = this.parseDeftype(specialFormList);
			break;
		case SemanticParserStatic.ELAMBDA:
			e = this.parseElambda(specialFormList);
			break;
		case SemanticParserStatic.IF:
			e = this.parseIf(specialFormList);
			break;
		case SemanticParserStatic.LAMBDA:
			e = this.parseLambda(specialFormList);
			break;
		case SemanticParserStatic.DEFCONVERSION:
			e = this.parseDefconversion(specialFormList);
			break;
		case SemanticParserStatic.DEFCONSTRUCTOR:
			e = this.parseDefConstructor(specialFormList);
			break;
		default:
			throw new AppendableException("Unrecognized special form " + specialForm);
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
		int argsCount = typeConstructionList.size() - 1;
		Constructor c = this.typeEnvironment.getConstructor(type, argsCount);
		Expression[] args = new Expression[argsCount];
		int i = 0;
		for (SemanticNode t : SemanticParserStatic.listTail(typeConstructionList)) {
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
			} catch (AppendableException e) {
				e.appendMessage(" in " + list);
				throw e;
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
			Validations.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage(" in Lambda " + lambdaList);
			throw e;
		}

		List<VariableTypePair> typedArgs = this.parseTypedArgList(lambdaList.get(1).asList());

		TypeTuple argsTypes = null;
		if (!SemanticParserStatic.isArgListUntypped(typedArgs)
				|| typedArgs.isEmpty()) {
			List<Type> tmp = SemanticParserStatic.filterTypesFromTypedArgsList(typedArgs);
			argsTypes = new TypeTuple(tmp.toArray(new Type[tmp.size()]));
		}

		List<Variable> tmp = SemanticParserStatic.filterVariablesFromTypedArgsList(typedArgs);
		Tuple lambdaArgs = new Tuple(tmp.toArray(new Expression[tmp.size()]));

		Expression body = this.parseNode(lambdaList.get(2));

		return new Lambda(lambdaArgs, argsTypes, body);
	}

	/**
	 * Parses deftype special form and defines given type and constructor
	 * 
	 * @param deftypeList
	 *            list representing the special form
	 * @return expression representing special form
	 * @throws AppendableException
	 */
	private Expression parseDeftype(List<SemanticNode> deftypeList) throws AppendableException {
		try {
			Validations.validateDefTypeList(deftypeList);
		} catch (AppendableException e) {
			e.appendMessage(" in deftype " + deftypeList);
			throw e;
		}

		String typeName = deftypeList.get(1).asSymbol();

		TypeConcrete type = this.typeEnvironment.addType(typeName);
		Constructor c = this.parseConstructor(type, deftypeList.get(2).asList());
		this.typeEnvironment.addConstructor(type, c);

		return Expression.EMPTY_EXPRESSION;
	}

	/**
	 * Parses defrep speial form and defines given type representastion and
	 * constructor
	 * 
	 * @param defrepList
	 * @return
	 * @throws AppendableException
	 */
	private Expression parseDefrep(List<SemanticNode> defrepList) throws AppendableException {
		try {
			Validations.validateDefRepList(defrepList);
		} catch (AppendableException e) {
			e.appendMessage("in " + defrepList);
			throw e;
		}

		String repName = defrepList.get(1).asSymbol();
		String typeName = defrepList.get(2).asSymbol();
		List<SemanticNode> constructorList = defrepList.get(3).asList();

		TypeConcrete type = this.typeEnvironment.addRepresentation(typeName, repName);
		Constructor c = this.parseConstructor(type, constructorList);
		this.typeEnvironment.addConstructor(type, c);

		return Expression.EMPTY_EXPRESSION;
	}

	/**
	 * Parses constructor from given lambda list
	 * 
	 * @param type
	 *            type that constructor constructs
	 * @param lambdaList
	 *            lambda list containing the definition of constructor
	 * @return Constructor represented by the lamdbaList, that constructs given type
	 * @throws AppendableException
	 */
	private Constructor parseConstructor(TypeConcrete type, List<SemanticNode> lambdaList) throws AppendableException {
		try {
			Validations.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage("in " + lambdaList);
			throw e;
		}

		Lambda lambda = this.parseLambda(lambdaList);

		if (lambda.argsType == null) {
			throw new AppendableException("Constructor argument list must be fully typed");
		}

		return new Constructor(type, lambda.args, lambda.argsType, lambda.body);
	}

	/**
	 * Parses typed argument list
	 * 
	 * @param l
	 *            parsed list
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
	 * Parses types
	 * 
	 * @param typeNode
	 *            node with type annotation
	 * @return Representation of the type
	 * @throws AppendableException
	 */
	private TypeConcrete parseType(SemanticNode typeNode) throws AppendableException {
		Optional<TypeConcrete> o;
		if (typeNode.type == SemanticNode.NodeType.PAIR) {
			Pair p = typeNode.asPair();
			o = this.typeEnvironment.getType(p.lvalue, p.rvalue);
		} else if (typeNode.type == SemanticNode.NodeType.SYMBOL) {
			o = this.typeEnvironment.getType(typeNode.asSymbol());
		} else {
			throw new AppendableException("Invalid type token " + typeNode);
		}

		if (!o.isPresent()) {
			throw new UndefinedTypeException(typeNode.toString());
		}
		return o.get();
	}

	/**
	 * Parses argument to variable type pair
	 * 
	 * @param pair
	 * @return
	 * @throws AppendableException
	 */
	private VariableTypePair parseVariableTypePair(SemanticNode pair) throws AppendableException {
		try {
			Validations.validateVariableTypePair(pair);
		} catch (AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}

		if (SemanticParserStatic.isSimpleSymbol(pair)) {
			return new VariableTypePair(null, new Variable(pair.asSymbol()));
		}

		TypeConcrete type = this.parseType(pair.asList().get(0));
		Variable variable = new Variable(pair.asList().get(1).asSymbol());
		return new VariableTypePair(type, variable);
	}

	/**
	 * Parses list of types
	 * 
	 * @param l
	 *            parsed list
	 * @return Tuple of types that list represents
	 * @throws AppendableException
	 */
	private TypeTuple parseTypeList(List<SemanticNode> l) throws AppendableException {
		Type[] types = new Type[l.size()];
		int i = 0;
		for (SemanticNode t : l) {
			types[i] = this.parseType(t);
			i++;
		}
		return new TypeTuple(types);
	}

	/**
	 * Parses list containing the implementations of extended lambda
	 * 
	 * @param lambdaArgs
	 *            extended lambda formal arguments
	 * @param l
	 *            list of implementations
	 * @return Set of Lambdas containing the implementations
	 * @throws AppendableException
	 */
	private Set<Lambda> parseImplementations(Tuple lambdaArgs, List<SemanticNode> l) throws AppendableException {
		Validations.validateImplementations(l);

		Set<Lambda> s = new TreeSet<Lambda>();

		for (SemanticNode n : l) {
			s.add(this.parseImplementation(lambdaArgs, n.asList()));
		}

		return s;
	}

	/**
	 * Parses single implementation of extended lambda
	 * 
	 * @param args
	 *            formal arguments of the implementation
	 * @param l
	 *            list containing unparsed implementation
	 * @return Lambda representing parsed implementation
	 * @throws AppendableException
	 */
	private Lambda parseImplementation(Tuple args, List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateImplementation(l);
		} catch (AppendableException e) {
			e.appendMessage("in implementation " + l);
			throw e;
		}
		TypeTuple argsType = this.parseTypeList(l.get(0).asList());
		Expression body = this.parseNode(l.get(1));
		return new Lambda(args, argsType, body);
	}
	
	/**
	 * Parses defconversion special form list
	 * @param l arguments of the defconversion special form
	 * @throws AppendableException
	 */
	private Expression parseDefconversion(List<SemanticNode> l) throws AppendableException{
		try {
			Validations.validateDefconversionList(l);
		}catch(AppendableException e) {
			e.appendMessage("in" + l);
			throw e;
		}
		TypeConcrete fromType = this.parseType(l.get(1));
		TypeConcrete toType = this.parseType(l.get(2));
		Constructor constructor = this.parseConstructor(toType, l.get(3).asList());
		
		if(constructor.argsType.values.length != 1
				|| constructor.argsType.values[0] != fromType)
		{
			AppendableException e = new AppendableException("Conversion must convert from defined type " + fromType + " got " + constructor.argsType);
			e.appendMessage("in" + l);
		}
		
		this.typeEnvironment.addConversion(fromType, toType, constructor);
		
		return Expression.EMPTY_EXPRESSION;
	}
	
	/**
	 * Parses defconstructor special form list
	 * @param l arguments of the defconversion special form
	 * @return Empty Expression (side effect adds constructor to type environment)
	 * @throws AppendableException
	 */
	private Expression parseDefConstructor(List<SemanticNode> l) throws AppendableException{
		try{
			Validations.validateDefConstructorList(l);
		}catch(AppendableException e){
			e.appendMessage(" in " + l);
			throw e;
		}
		
		TypeConcrete constructedType = this.parseType(l.get(1));
		Constructor constructor = this.parseConstructor(constructedType, l.get(2).asList());
		
		this.typeEnvironment.addConstructor(constructedType, constructor);
		
		return Expression.EMPTY_EXPRESSION;
	}
}
