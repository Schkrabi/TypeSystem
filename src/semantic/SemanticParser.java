package semantic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Application;
import expression.DefConversionExpression;
import expression.DefExpression;
import expression.DefRepresentationExpression;
import expression.DefTypeExpression;
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
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
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
	 * Parses list of nodes
	 * 
	 * @param l
	 * @return
	 * @throws Exception
	 */
	public static Expression parseNodelist(List<SemanticNode> l) throws AppendableException {
		if (l.isEmpty()) {
			return Expression.EMPTY_EXPRESSION;
		}

		SemanticNode head = SemanticParserStatic.listHead(l);

		if (SemanticParserStatic.isSpecialForm(head)) {
			return SemanticParser.parseSpecialForm(head.asSymbol(), l);
		}

		return SemanticParser.parseList(l);
	}

	/**
	 * Parses SemanticNode to expression
	 * 
	 * @param token
	 * @return an Expression
	 * @throws AppendableException
	 */
	public static Expression parseNode(SemanticNode token) throws AppendableException {
		Literal l;
		Variable v;

		switch (token.type) {
		case SYMBOL:
			v = new Variable(token.asSymbol());
			return v;
		case PAIR:
			v = token.asPair().asVariable();
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
			return SemanticParser.parseNodelist(token.asList());
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
	private static ExtendedLambda parseElambda(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateElambdaList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		List<TypeVariablePair> args = SemanticParser.parseTypedArgList(l.get(1).asList());
		Tuple argsTuple = new Tuple(args.stream().map(x -> x.second).collect(Collectors.toList()));
		List<SemanticNode> impls = l.subList(2, l.size());
		Set<Lambda> implementations = SemanticParser.parseImplementations(argsTuple, impls);

		return new ExtendedLambda(new TypeTuple(args.stream().map(x -> x.first).collect(Collectors.toList())),
				implementations);
	}

	/**
	 * Parses the if special form list
	 * 
	 * @param l parsed list
	 * @return Expression representitng the if
	 * @throws AppendableException
	 */
	private static Expression parseIf(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateIfList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		Expression pred = SemanticParser.parseNode(l.get(1));
		Expression trueBranch = SemanticParser.parseNode(l.get(2));
		Expression falseBranch = SemanticParser.parseNode(l.get(3));

		return new IfExpression(pred, trueBranch, falseBranch);
	}

	/**
	 * Parses list containing special form
	 * 
	 * @param specialForm     special form reserved word
	 * @param specialFormList arguments of the special form
	 * @return
	 * @throws Exception
	 */
	private static Expression parseSpecialForm(String specialForm, List<SemanticNode> specialFormList)
			throws AppendableException {
		Expression e;
		switch (specialForm) {
		case SemanticParserStatic.DEFREP:
			e = SemanticParser.parseDefrep(specialFormList);
			break;
		case SemanticParserStatic.DEFTYPE:
			e = SemanticParser.parseDeftype(specialFormList);
			break;
		case SemanticParserStatic.ELAMBDA:
			e = SemanticParser.parseElambda(specialFormList);
			break;
		case SemanticParserStatic.IF:
			e = SemanticParser.parseIf(specialFormList);
			break;
		case SemanticParserStatic.LAMBDA:
			e = SemanticParser.parseLambda(specialFormList);
			break;
		case SemanticParserStatic.DEFCONVERSION:
			e = SemanticParser.parseDefconversion(specialFormList);
			break;
		case SemanticParserStatic.DEFINE:
			e = SemanticParser.parseDefine(specialFormList);
			break;
		case SemanticParserStatic.CONS:
			e = SemanticParser.parseCons(specialFormList);
			break;
		case SemanticParserStatic.ERROR:
			e = SemanticParser.parseError(specialFormList);
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
	private static Expression parseList(List<SemanticNode> list) throws AppendableException {
		List<Expression> l = new ArrayList<Expression>();
		Expression fun = null;
		Tuple args = null;

		for (SemanticNode n : list) {
			try {
				if (fun == null) {
					fun = SemanticParser.parseNode(n);
					continue;
				}
				l.add(SemanticParser.parseNode(n));
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
	private static Lambda parseLambda(List<SemanticNode> lambdaList) throws AppendableException {
		try {
			Validations.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage(" in Lambda " + lambdaList);
			throw e;
		}
		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(lambdaList.get(1).asList());

		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));

		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));

		Expression body = SemanticParser.parseNode(lambdaList.get(2));

		return new Lambda(lambdaArgs, argsTypes, body);
	}

	/**
	 * Parses deftype special form and defines given type and constructor
	 * 
	 * @param deftypeList list representing the special form
	 * @return expression representing special form
	 * @throws AppendableException
	 */
	private static Expression parseDeftype(List<SemanticNode> deftypeList) throws AppendableException {
		try {
			Validations.validateDefTypeList(deftypeList);
		} catch (AppendableException e) {
			e.appendMessage(" in deftype " + deftypeList);
			throw e;
		}

		TypeName typeName = new TypeName(deftypeList.get(1).asSymbol());

		return new DefTypeExpression(typeName);
	}

	/**
	 * Parses defrep speial form and defines given type representastion and
	 * constructor
	 * 
	 * @param defrepList
	 * @return
	 * @throws AppendableException
	 */
	private static Expression parseDefrep(List<SemanticNode> defrepList) throws AppendableException {
		try {
			Validations.validateDefRepList(defrepList);
		} catch (AppendableException e) {
			e.appendMessage("in " + defrepList);
			throw e;
		}

		TypeRepresentation repName = new TypeRepresentation(defrepList.get(1).asSymbol());
		TypeName typeName = new TypeName(defrepList.get(2).asSymbol());
		List<SemanticNode> members = defrepList.get(3).asList();

		return new DefRepresentationExpression(typeName, repName, members);
	}

	/**
	 * Parses typed argument list
	 * 
	 * @param l parsed list
	 * @return list of variable type pairs
	 * @throws AppendableException
	 */
	private static List<TypeVariablePair> parseTypedArgList(List<SemanticNode> l) throws AppendableException {
		List<TypeVariablePair> parsed = new ArrayList<TypeVariablePair>();

		for (SemanticNode n : l) {
			parsed.add(SemanticParser.parseUntypedVariableOrVariableTypePair(n));
		}

		return parsed;
	}

	/**
	 * Parses token that is either variable type pair or simple untyped variable
	 * symbol
	 * 
	 * @param n parsed token
	 * @return variable type pair
	 * @throws AppendableException
	 */
	private static TypeVariablePair parseUntypedVariableOrVariableTypePair(SemanticNode n) throws AppendableException {
		if (SemanticParserStatic.isSimpleSymbol(n)) {
			return new TypeVariablePair(new TypeVariable(NameGenerator.next()), new Variable(n.asSymbol()));
		}
		return SemanticParser.parseVariableTypePair(n);
	}

	/**
	 * Parses types
	 * 
	 * @param typeNode node with type annotation
	 * @return Representation of the type
	 * @throws AppendableException
	 */
	private static TypeAtom parseType(SemanticNode typeNode) throws AppendableException {
		if (typeNode.type == SemanticNode.NodeType.PAIR) {
			return new TypeAtom(new TypeName(typeNode.asPair().first), new TypeRepresentation(typeNode.asPair().second));
		}
		
		return new TypeAtom(new TypeName(typeNode.asSymbol()), TypeRepresentation.WILDCARD);
	}

	/**
	 * Parses argument to variable type pair
	 * 
	 * @param pair
	 * @return
	 * @throws AppendableException
	 */
	private static TypeVariablePair parseVariableTypePair(SemanticNode pair) throws AppendableException {
		try {
			Validations.validateVariableTypePair(pair);
		} catch (AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}

		TypeAtom type = SemanticParser.parseType(pair.asList().get(0));
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
	private static TypeTuple parseTypeList(List<SemanticNode> l) throws AppendableException {
		List<Type> types = new LinkedList<Type>();
		for (SemanticNode t : l) {
			types.add(SemanticParser.parseType(t));
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
	private static Set<Lambda> parseImplementations(Tuple lambdaArgs, List<SemanticNode> l) throws AppendableException {
		Validations.validateImplementations(l);

		Set<Lambda> s = new TreeSet<Lambda>();

		for (SemanticNode n : l) {
			s.add(SemanticParser.parseImplementation(lambdaArgs, n.asList()));
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
	private static Lambda parseImplementation(Tuple args, List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateImplementation(l);
		} catch (AppendableException e) {
			e.appendMessage("in implementation " + l);
			throw e;
		}
		TypeTuple argsType = SemanticParser.parseTypeList(l.get(0).asList());
		Expression body = SemanticParser.parseNode(l.get(1));
		return new Lambda(args, argsType, body);
	}

	/**
	 * Parses defconversion special form list
	 * 
	 * @param l arguments of the defconversion special form
	 * @throws AppendableException
	 */
	private static Expression parseDefconversion(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefconversionList(l);
		} catch (AppendableException e) {
			e.appendMessage("in" + l);
			throw e;
		}
		TypeAtom fromType = SemanticParser.parseType(l.get(1));
		TypeAtom toType = SemanticParser.parseType(l.get(2));
		Lambda constructor = SemanticParser.parseLambda(l.get(3).asList());

		if (constructor.argsType.size() != 1) {
			AppendableException e = new AppendableException(
					"Conversion must convert from defined type " + fromType + " got " + constructor.argsType);
			e.appendMessage("in" + l);
			throw e;
		}

		return new DefConversionExpression(fromType, toType, constructor);
	}

	/**
	 * Parses define special form list
	 * 
	 * @param l arguments of the define special form
	 * @return DefExpression expression
	 * @throws AppendableException
	 */
	private static Expression parseDefine(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefineList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		Variable v = new Variable(l.get(1).asSymbol());
		Expression e = SemanticParser.parseNode(l.get(2));
		return new DefExpression(v, e);
	}

	/**
	 * Parses cons special form list
	 * 
	 * @param l arguments of the cons special form
	 * @return Tuple Expression
	 * @throws AppendableException
	 */
	private static Expression parseCons(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateConsList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new Tuple(Arrays.asList(SemanticParser.parseNode(l.get(1)), SemanticParser.parseNode(l.get(2))));
	}

	/**
	 * Parses error special form list
	 * 
	 * @param l error special form list
	 * @return ExceptionExpr expression
	 * @throws AppendableException
	 */
	private static Expression parseError(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateErrorList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new ExceptionExpr(SemanticParser.parseNode(l.get(1)));
	}
}
