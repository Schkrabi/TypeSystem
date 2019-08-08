package semantic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Application;
import expression.TypeConstructionLambda;
import expression.DefExpression;
import expression.ExceptionExpr;
import expression.Expression;
import expression.ExtendedLambda;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Variable;

import parser.SemanticNode;
import parser.SemanticPair;
import types.Type;
import types.TypeConcrete;
import types.TypeNotRecognizedException;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;

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
		/*
		 * if (this.typeEnvironment.isType(head)) { return
		 * this.parseTypeConstruction(this.parseType(head), l); }
		 */

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
		Variable v;

		switch (token.type) {
		case SYMBOL:
			v = new Variable(token.asSymbol());
			return v;
		case PAIR:
			v = token.asPair().asVariable();
			Optional<TypeConcrete> o = this.typeEnvironment.getType(token.asPair().first, token.asPair().second);
			if (!o.isPresent()) {
				throw new TypeNotRecognizedException(token.asPair().first + ":" + token.asPair().second);
			}
			return v;
		case INT:
			l = new LitInteger(token.asInt());
			return l;
		case DOUBLE:
			l = new LitDouble(token.asDouble());
			return l;
		case STRING:
			l = new LitString(token.asString());
			return l;
		case BOOL:
			return token.asBool() ? LitBoolean.TRUE : LitBoolean.FALSE;
		case LIST:
			return this.parseNodelist(token.asList());
		default:
			break;
		}
		throw new AppendableException("Unrecognized token type!");
	}

	/**
	 * Parses the extended lambda special form
	 * 
	 * @param l list containing the extendedLambda special form
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
	 * @param l parsed list
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

		return new IfExpression(new Tuple(Arrays.asList(pred, trueBranch, falseBranch)));
	}

	/**
	 * Parses list containing special form
	 * 
	 * @param specialForm     special form reserved word
	 * @param specialFormList arguments of the special form
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
		case SemanticParserStatic.DEFINE:
			e = this.parseDefine(specialFormList);
			break;
		case SemanticParserStatic.CONS:
			e = this.parseCons(specialFormList);
			break;
		case SemanticParserStatic.ERROR:
			e = this.parseError(specialFormList);
			break;
		default:
			throw new AppendableException("Unrecognized special form " + specialForm);
		}

		return e;
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
		Expression fun = null;
		Tuple args = null;

		for (SemanticNode n : list) {
			try {
				if (fun == null) {
					fun = this.parseNode(n);
					continue;
				}
				l.add(this.parseNode(n));
			} catch (AppendableException e) {
				e.appendMessage(" in " + list);
				throw e;
			}
		}

		args = new Tuple(l);

		return new Application(fun, args);
	}

	/**
	 * Parses lambda special form
	 * 
	 * @param lambdaList arguments of the special form
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

		List<TypeVariablePair> typedArgs = this.parseTypedArgList(lambdaList.get(1).asList());

		TypeTuple argsTypes = null;

		List<Type> tmp = typedArgs.stream().map(x -> x.first).collect(Collectors.toList());
		argsTypes = new TypeTuple(tmp);

		List<Variable> vtmp = SemanticParserStatic.filterVariablesFromTypedArgsList(typedArgs);
		Tuple lambdaArgs = new Tuple(vtmp);

		Expression body = this.parseNode(lambdaList.get(2));

		return new Lambda(lambdaArgs, argsTypes, body);
	}

	/**
	 * Parses deftype special form and defines given type and constructor
	 * 
	 * @param deftypeList list representing the special form
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

		this.typeEnvironment.addType(typeName);

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

		this.typeEnvironment.addRepresentation(typeName, repName);

		return Expression.EMPTY_EXPRESSION;
	}

	/**
	 * Parses constructor from given lambda list
	 * 
	 * @param typeName   type that constructor constructs
	 * @param lambdaList lambda list containing the definition of constructor
	 * @return Constructor represented by the lamdbaList, that constructs given type
	 * @throws AppendableException
	 */
	private TypeConstructionLambda parseConstructor(TypeConcrete type, List<SemanticNode> lambdaList)
			throws AppendableException {
		try {
			Validations.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage("in " + lambdaList);
			throw e;
		}

		Lambda lambda = this.parseLambda(lambdaList);

		return new TypeConstructionLambda(type, lambda.args, lambda.argsType, lambda.body);
	}

	/**
	 * Parses typed argument list
	 * 
	 * @param l parsed list
	 * @return list of variable type pairs
	 * @throws AppendableException
	 */
	private List<TypeVariablePair> parseTypedArgList(List<SemanticNode> l) throws AppendableException {
		List<TypeVariablePair> parsed = new ArrayList<TypeVariablePair>();

		for (SemanticNode n : l) {
			parsed.add(this.parseUntypedVariableOrVariableTypePair(n));
		}

		return parsed;
	}
	
	/**
	 * Parses token that is either variable type pair or simple untyped variable symbol
	 * @param n parsed token
	 * @return variable type pair
	 * @throws AppendableException
	 */
	private TypeVariablePair parseUntypedVariableOrVariableTypePair(SemanticNode n) throws AppendableException {
		if(SemanticParserStatic.isSimpleSymbol(n)) {
			return new TypeVariablePair(new TypeVariable(NameGenerator.next()), new Variable(n.asSymbol()));
		}
		return this.parseVariableTypePair(n);
	}

	/**
	 * Parses types
	 * 
	 * @param typeNode node with type annotation
	 * @return Representation of the type
	 * @throws AppendableException
	 */
	private TypeConcrete parseType(SemanticNode typeNode) throws AppendableException {
		Optional<TypeConcrete> o;
		if (typeNode.type == SemanticNode.NodeType.PAIR) {
			SemanticPair p = typeNode.asPair();
			o = this.typeEnvironment.getType(p.first, p.second);
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
	private TypeVariablePair parseVariableTypePair(SemanticNode pair) throws AppendableException {
		try {
			Validations.validateVariableTypePair(pair);
		} catch (AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}

		TypeConcrete type = this.parseType(pair.asList().get(0));
		Variable variable = new Variable(pair.asList().get(1).asSymbol());
		return new TypeVariablePair(type, variable);
	}

	/**
	 * Parses list of types
	 * 
	 * @param l parsed list
	 * @return Tuple of types that list represents
	 * @throws AppendableException
	 */
	private TypeTuple parseTypeList(List<SemanticNode> l) throws AppendableException {
		List<Type> types = new LinkedList<Type>();
		for (SemanticNode t : l) {
			types.add(this.parseType(t));
		}
		return new TypeTuple(types);
	}

	/**
	 * Parses list containing the implementations of extended lambda
	 * 
	 * @param lambdaArgs extended lambda formal arguments
	 * @param l          list of implementations
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
	 * @param args formal arguments of the implementation
	 * @param l    list containing unparsed implementation
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
	 * 
	 * @param l arguments of the defconversion special form
	 * @throws AppendableException
	 */
	private Expression parseDefconversion(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefconversionList(l);
		} catch (AppendableException e) {
			e.appendMessage("in" + l);
			throw e;
		}
		TypeConcrete fromType = this.parseType(l.get(1));
		TypeConcrete toType = this.parseType(l.get(2));
		TypeConstructionLambda constructor = this.parseConstructor(toType, l.get(3).asList());

		if (constructor.argsType.size() != 1 || constructor.argsType.get(0) != fromType) {
			AppendableException e = new AppendableException(
					"Conversion must convert from defined type " + fromType + " got " + constructor.argsType);
			e.appendMessage("in" + l);
			throw e;
		}

		this.typeEnvironment.addConversion(fromType, toType, constructor);

		return Expression.EMPTY_EXPRESSION;
	}

	/**
	 * Parses defconstructor special form list
	 * 
	 * @param l arguments of the defconversion special form
	 * @return Empty Expression (side effect adds constructor to type environment)
	 * @throws AppendableException
	 */
	private Expression parseDefConstructor(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefConstructorList(l);
		} catch (AppendableException e) {
			e.appendMessage(" in " + l);
			throw e;
		}

		SemanticNode type = l.get(1);
		TypeConcrete constructedType = this.parseType(type);
		TypeConstructionLambda constructor = this.parseConstructor(constructedType, l.get(2).asList());

		// this.typeEnvironment.addConstructor(constructedType, constructor);

		return new DefExpression(new Variable(type.toString()), constructor);// TODO improve semantics
	}

	/**
	 * Parses define special form list
	 * 
	 * @param l arguments of the define special form
	 * @return DefExpression expression
	 * @throws AppendableException
	 */
	private Expression parseDefine(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefineList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		Variable v = new Variable(l.get(1).asSymbol());
		Expression e = this.parseNode(l.get(2));
		return new DefExpression(v, e);
	}

	/**
	 * Parses cons special form list
	 * 
	 * @param l arguments of the cons special form
	 * @return Tuple Expression
	 * @throws AppendableException
	 */
	private Expression parseCons(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateConsList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new Tuple(Arrays.asList(this.parseNode(l.get(1)), this.parseNode(l.get(2))));
	}

	/**
	 * Parses error special form list
	 * 
	 * @param l error special form list
	 * @return ExceptionExpr expression
	 * @throws AppendableException
	 */
	private Expression parseError(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateErrorList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new ExceptionExpr(this.parseNode(l.get(1)));
	}
}
