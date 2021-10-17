package velka.parser.semantic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Lambda;
import velka.core.application.AbstractionApplication;
import velka.core.application.AndExpression;
import velka.core.application.CanDeconstructAs;
import velka.core.application.Construct;
import velka.core.application.Convert;
import velka.core.application.Deconstruct;
import velka.core.application.DefineConstructor;
import velka.core.application.DefineConversion;
import velka.core.application.DefineRepresentation;
import velka.core.application.DefineSymbol;
import velka.core.application.DefineType;
import velka.core.application.ExceptionExpr;
import velka.core.application.Get;
import velka.core.application.IfExpression;
import velka.core.application.InstanceOf;
import velka.core.application.InstanceOfRepresentation;
import velka.core.application.Loop;
import velka.core.application.Recur;
import velka.core.exceptions.UndefinedTypeException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitString;
import velka.core.literal.Literal;
import velka.parser.antlr.SemanticNode;
import velka.parser.antlr.SemanticPair;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingFunction;

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
		return SemanticParser.parseNodelist(l, new TreeMap<TypeVariable, TypeVariable>());
	}

	/**
	 * Parses list of nodes
	 * 
	 * @param l
	 * @param typeLet
	 * @return
	 * @throws AppendableException
	 */
	public static Expression parseNodelist(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (l.isEmpty()) {
			return Expression.EMPTY_EXPRESSION;
		}

		SemanticNode head = SemanticParserStatic.listHead(l);

		if (SemanticParserStatic.isSpecialForm(head)) {
			return SemanticParser.parseSpecialForm(head.asSymbol(), l, typeLet);
		}

		return SemanticParser.parseList(l, typeLet);
	}

	/**
	 * Parses SemanticNode to expression
	 * 
	 * @param token
	 * @return an Expression
	 * @throws AppendableException
	 */
	public static Expression parseNode(SemanticNode token) throws AppendableException {
		return SemanticParser.parseNode(token, new TreeMap<TypeVariable, TypeVariable>());
	}

	/**
	 * Parses SemanticNode to expression
	 * 
	 * @param token
	 * @param typeLet mapping of user defined typevariables
	 * @return an Expression
	 * @throws AppendableException
	 */
	public static Expression parseNode(SemanticNode token, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		Literal l;
		Symbol v;

		switch (token.type) {
		case SYMBOL:
			v = new Symbol(token.asSymbol());
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
			return SemanticParser.parseNodelist(token.asList(), typeLet);
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
	private static ExtendedLambda parseElambda(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateElambdaList(l, typeLet);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		List<TypeVariablePair> args = SemanticParser.parseTypedArgList(l.get(1).asList(), typeLet);
		Tuple argsTuple = new Tuple(args.stream().map(x -> x.second).collect(Collectors.toList()));
		List<SemanticNode> impls = l.subList(2, l.size());
		Set<Lambda> implementations = SemanticParser.parseImplementations(argsTuple, impls, typeLet);

		return ExtendedLambda.makeExtendedLambda(implementations);
	}

	/**
	 * Parses the if special form list
	 * 
	 * @param l parsed list
	 * @return Expression representitng the if
	 * @throws AppendableException
	 */
	private static Expression parseIf(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateIfList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		Expression pred = SemanticParser.parseNode(l.get(1), typeLet);
		Expression trueBranch = SemanticParser.parseNode(l.get(2), typeLet);
		Expression falseBranch = SemanticParser.parseNode(l.get(3), typeLet);

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
	private static Expression parseSpecialForm(String specialForm, List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		Expression e;
		switch (specialForm) {
		case SemanticParserStatic.DEFINE_REPRESENTATION:
			e = SemanticParser.parseDefrep(specialFormList, typeLet);
			break;
		case SemanticParserStatic.DEFINE_TYPE:
			e = SemanticParser.parseDeftype(specialFormList, typeLet);
			break;
		case SemanticParserStatic.EXTENDED_LAMBDA:
			e = SemanticParser.parseElambda(specialFormList, typeLet);
			break;
		case SemanticParserStatic.IF:
			e = SemanticParser.parseIf(specialFormList, typeLet);
			break;
		case SemanticParserStatic.LAMBDA:
			e = SemanticParser.parseLambda(specialFormList, typeLet);
			break;
		case SemanticParserStatic.DEFINE_CONVERSION:
			e = SemanticParser.parseDefconversion(specialFormList, typeLet);
			break;
		case SemanticParserStatic.DEFINE:
			e = SemanticParser.parseDefine(specialFormList, typeLet);
			break;
		case SemanticParserStatic.CONS:
			e = SemanticParser.parseCons(specialFormList, typeLet);
			break;
		case SemanticParserStatic.ERROR:
			e = SemanticParser.parseError(specialFormList, typeLet);
			break;
		case SemanticParserStatic.AND:
			e = SemanticParser.parseAnd(specialFormList, typeLet);
			break;
		case SemanticParserStatic.OR:
			e = SemanticParser.parseOr(specialFormList, typeLet);
			break;
		case SemanticParserStatic.DEFINE_CONSTRUCTOR:
			e = SemanticParser.parseDefineConstructor(specialFormList, typeLet);
			break;
		case SemanticParserStatic.CONSTRUCT:
			e = SemanticParser.parseConstruct(specialFormList, typeLet);
			break;
		case SemanticParserStatic.CONVERT:
			e = SemanticParser.parseConvert(specialFormList, typeLet);
			break;
		case SemanticParserStatic.DECONSTRUCT:
			e = SemanticParser.parseDeconstruct(specialFormList, typeLet);
			break;
		case SemanticParserStatic.CAN_DECONSTRUCT_AS:
			e = SemanticParser.parseCanDeconstructAs(specialFormList, typeLet);
			break;
		case SemanticParserStatic.LET_TYPE:
			e = SemanticParser.parseLetType(specialFormList, typeLet);
			break;
		case SemanticParserStatic.INSTANCE_OF:
			e = SemanticParser.parseInstanceOf(specialFormList, typeLet);
			break;
		case SemanticParserStatic.INSTANCE_OF_REPRESENTATION:
			e = SemanticParser.parseInstanceOfRepresentation(specialFormList, typeLet);
			break;
		case SemanticParserStatic.EAPPLY:
			e = SemanticParser.parseEapply(specialFormList, typeLet);
			break;
		case SemanticParserStatic.EXTENDED_LAMBDA_RANKING:
			e = SemanticParser.parseExtendedLambdaRanking(specialFormList, typeLet);
			break;
		case SemanticParserStatic.GET:
			e = SemanticParser.parseGet(specialFormList, typeLet);
			break;
		case SemanticParserStatic.TUPLE:
			e = SemanticParser.parseTuple(specialFormList, typeLet);
			break;
		case SemanticParserStatic.LET:
			e = SemanticParser.parseLet(specialFormList, typeLet);
			break;
		case SemanticParserStatic.LET_AST:
			e = SemanticParser.parseLetAst(specialFormList, typeLet);
			break;
		case SemanticParserStatic.RECUR:
			e = SemanticParser.parseRecur(specialFormList, typeLet);
			break;
		case SemanticParserStatic.LOOP:
			e = SemanticParser.parseLoop(specialFormList, typeLet);
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
	private static Expression parseList(List<SemanticNode> list, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		List<Expression> l = new ArrayList<Expression>();
		Expression fun = null;
		Tuple args = null;

		for (SemanticNode n : list) {
			try {
				if (fun == null) {
					fun = SemanticParser.parseNode(n, typeLet);
					continue;
				}
				l.add(SemanticParser.parseNode(n, typeLet));
			} catch (AppendableException e) {
				e.appendMessage(" in " + list);
				throw e;
			}
		}

		args = new Tuple(l);

		return new AbstractionApplication(fun, args);
	}

	/**
	 * Parses And special form
	 * 
	 * @param specialFormList list of arguments of special form
	 * @return new AndExpression instance
	 * @throws AppendableException if list does not validate
	 */
	private static Expression parseAnd(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateAndList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in AndExpression " + specialFormList);
			throw e;
		}

		try {
			Tuple args = new Tuple(specialFormList.subList(1, specialFormList.size()).stream()
					.map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x, typeLet)))
					.collect(Collectors.toList()));
			return new AndExpression(args);
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}
	}

	/**
	 * Parses Or special form
	 * 
	 * @param specialFormList list of the special form
	 * @return OrExpression object
	 * @throws AppendableException if specialFormList does not validate as
	 *                             OrExpression
	 */
	private static Expression parseOr(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateOrList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in OrExpression " + specialFormList);
			throw e;
		}

		try {
			Tuple args = new Tuple(specialFormList.subList(1, specialFormList.size()).stream()
					.map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x, typeLet)))
					.collect(Collectors.toList()));
			return new velka.core.application.OrExpression(args);
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}
	}

	/**
	 * Parses lambda special form
	 * 
	 * @param lambdaList arguments of the special form
	 * @return expresion representing the lambda special form
	 * @throws Exception
	 */
	private static Lambda parseLambda(List<SemanticNode> lambdaList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateLambdaList(lambdaList);
		} catch (AppendableException e) {
			e.appendMessage(" in Lambda " + lambdaList);
			throw e;
		}
		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(lambdaList.get(1).asList(), typeLet);

		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));

		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));

		Expression body = SemanticParser.parseNode(lambdaList.get(2), typeLet);

		return new Lambda(lambdaArgs, argsTypes, body);
	}

	/**
	 * Parses deftype special form and defines given type and constructor
	 * 
	 * @param deftypeList list representing the special form
	 * @return expression representing special form
	 * @throws AppendableException
	 */
	private static Expression parseDeftype(List<SemanticNode> deftypeList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateDefTypeList(deftypeList);
		} catch (AppendableException e) {
			e.appendMessage(" in deftype " + deftypeList);
			throw e;
		}

		return new DefineType(new TypeName(deftypeList.get(1).asSymbol()));
	}

	/**
	 * Parses defrep speial form and defines given type representastion and
	 * constructor
	 * 
	 * @param defrepList
	 * @return
	 * @throws AppendableException
	 */
	private static Expression parseDefrep(List<SemanticNode> defrepList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateDefRepList(defrepList);
		} catch (AppendableException e) {
			e.appendMessage("in " + defrepList);
			throw e;
		}

		TypeRepresentation repName = new TypeRepresentation(defrepList.get(1).asSymbol());
		TypeName typeName = new TypeName(defrepList.get(2).asSymbol());

		return new DefineRepresentation(typeName, repName);
	}

	/**
	 * Parses typed argument list
	 * 
	 * @param l parsed list
	 * @return list of variable type pairs
	 * @throws AppendableException
	 */
	private static List<TypeVariablePair> parseTypedArgList(List<SemanticNode> l,
			Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		List<TypeVariablePair> parsed = new ArrayList<TypeVariablePair>();

		for (SemanticNode n : l) {
			parsed.add(SemanticParser.parseUntypedVariableOrVariableTypePair(n, typeLet));
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
	private static TypeVariablePair parseUntypedVariableOrVariableTypePair(SemanticNode n,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		if (SemanticParserStatic.isSimpleSymbol(n)) {
			return new TypeVariablePair(new TypeVariable(NameGenerator.next()), new Symbol(n.asSymbol()));
		}
		return SemanticParser.parseVariableTypePair(n, letType);
	}

	/**
	 * Parses Wildcard type
	 * 
	 * @param typeSymbol name of the type
	 * @param letType
	 * @return TypeAtom
	 * @throws AppendableException
	 */
	private static Type parseTypeSymbol(String typeSymbol, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		TypeVariable tv = new TypeVariable(typeSymbol);

		if (letType.containsKey(tv)) {
			return letType.get(tv);
		}

		return SemanticParser.parseTypeAtom(typeSymbol, TypeRepresentation.WILDCARD.toString());
	}

	/**
	 * Parses type atom by its typeName and reprsentation name
	 * 
	 * @param typeName           name of type
	 * @param typeRepresentation name of representation
	 * @return parsed TypeAtom
	 * @throws UndefinedTypeException if Type or representation was not defined
	 */
	private static TypeAtom parseTypeAtom(String typeName, String typeRepresentation) throws UndefinedTypeException {
		TypeAtom typeAtom = new TypeAtom(new TypeName(typeName), new TypeRepresentation(typeRepresentation));
		return typeAtom;
	}

	/**
	 * Parses type arrow
	 * 
	 * @param ltypeNode SemanticNode with left type
	 * @param rtypeNode SemanticNode with right type
	 * @return typeArrow
	 * @throws AppendableException
	 */
	private static TypeArrow parseTypeArrow(SemanticNode ltypeNode, SemanticNode rtypeNode,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		Type ltype = SemanticParser.parseType(ltypeNode, letType);
		Type rtype = SemanticParser.parseType(rtypeNode, letType);
		return new TypeArrow(ltype, rtype);
	}

	/**
	 * Parses TypeTuple
	 * 
	 * @param l list of semanticNodes representing subtypes
	 * @return TypeTuple
	 * @throws AppendableException if atomic type is not recognized or fault
	 *                             semanticNode is detected
	 */
	private static TypeTuple parseTypeTuple(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			List<Type> parsed = l.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseType(x, letType)))
					.collect(Collectors.toList());
			return new TypeTuple(parsed);
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}
	}

	/**
	 * Parses types
	 * 
	 * @param typeNode node with type annotation
	 * @return Representation of the type
	 * @throws AppendableException
	 */
	private static Type parseType(SemanticNode typeNode, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		switch (typeNode.type) {
		case PAIR:
			SemanticPair semanticPair = typeNode.asPair();
			return SemanticParser.parseTypeAtom(semanticPair.first, semanticPair.second);
		case ARROW:
			Pair<SemanticNode, SemanticNode> arrowPair = typeNode.asArrow();
			return SemanticParser.parseTypeArrow(arrowPair.first, arrowPair.second, letType);
		case LIST:
			List<SemanticNode> l = typeNode.asList();
			return SemanticParser.parseTypeTuple(l, letType);
		case SYMBOL:
			return SemanticParser.parseTypeSymbol(typeNode.asSymbol(), letType);
		default:
			break;
		}

		throw new UndefinedTypeException(typeNode.toString());
	}

	/**
	 * Parses argument to variable type pair
	 * 
	 * @param pair
	 * @return
	 * @throws AppendableException
	 */
	private static TypeVariablePair parseVariableTypePair(SemanticNode pair, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateVariableTypePair(pair);
		} catch (AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}

		Type type = SemanticParser.parseType(pair.asList().get(0), letType);
		Symbol variable = new Symbol(pair.asList().get(1).asSymbol());
		return new TypeVariablePair(type, variable);
	}

	/**
	 * Parses list of types
	 * 
	 * @param l parsed list
	 * @return Tuple of types that list represents
	 * @throws AppendableException
	 */
	private static TypeTuple parseTypeList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		List<Type> types = new LinkedList<Type>();
		for (SemanticNode t : l) {
			types.add(SemanticParser.parseType(t, letType));
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
	private static Set<Lambda> parseImplementations(Tuple lambdaArgs, List<SemanticNode> l,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		Validations.validateImplementations(l, letType);

		Set<Lambda> s = new TreeSet<Lambda>();

		for (SemanticNode n : l) {
			s.add(SemanticParser.parseImplementation(lambdaArgs, n.asList(), letType));
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
	private static Lambda parseImplementation(Tuple args, List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateImplementation(l, letType);
		} catch (AppendableException e) {
			e.appendMessage("in implementation " + l);
			throw e;
		}
		TypeTuple argsType = SemanticParser.parseTypeList(l.get(0).asList(), letType);
		Expression body = SemanticParser.parseNode(l.get(1), letType);
		return new Lambda(args, argsType, body);
	}

	private static TypeAtom parseConversionType(SemanticNode typeNode, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		Type parsed = SemanticParser.parseType(typeNode, letType);
		if (!(parsed instanceof TypeAtom)) {
			throw new AppendableException("Only atomic types are allowed, got " + parsed);
		}
		return (TypeAtom) parsed;
	}

	/**
	 * Parses defconversion special form list
	 * 
	 * @param l arguments of the defconversion special form
	 * @throws AppendableException
	 */
	private static Expression parseDefconversion(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateDefconversionList(l);
		} catch (AppendableException e) {
			e.appendMessage("in" + l);
			throw e;
		}
		TypeAtom fromType = SemanticParser.parseConversionType(l.get(1), letType);
		TypeAtom toType = SemanticParser.parseConversionType(l.get(2), letType);

		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(l.get(3).asList(), letType);
		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));
		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));
		Expression body = SemanticParser.parseNode(l.get(4), letType);

		if (argsTypes.size() != 1) {
			AppendableException e = new AppendableException(
					"Conversion must convert from defined type " + fromType + " got " + argsTypes);
			e.appendMessage("in" + l);
			throw e;
		}

		return new DefineConversion(fromType, toType, lambdaArgs, body);
	}

	/**
	 * Parses define special form list
	 * 
	 * @param l arguments of the define special form
	 * @return DefExpression expression
	 * @throws AppendableException
	 */
	private static Expression parseDefine(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateDefineList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		Symbol v = new Symbol(l.get(1).asSymbol());
		Expression e = SemanticParser.parseNode(l.get(2), letType);
		return new DefineSymbol(v, e);
	}

	/**
	 * Parses cons special form list
	 * 
	 * @param l arguments of the cons special form
	 * @return Tuple Expression
	 * @throws AppendableException
	 */
	private static Expression parseCons(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateConsList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new Tuple(Arrays.asList(SemanticParser.parseNode(l.get(1), letType),
				SemanticParser.parseNode(l.get(2), letType)));
	}

	/**
	 * Parses error special form list
	 * 
	 * @param l error special form list
	 * @return ExceptionExpr expression
	 * @throws AppendableException
	 */
	private static Expression parseError(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateErrorList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}

		return new ExceptionExpr(SemanticParser.parseNode(l.get(1), letType));
	}

	/**
	 * Parses define constructor special form list
	 * 
	 * @param l special form list
	 * @return DefineConstructor expression
	 * @throws AppendableException
	 */
	private static Expression parseDefineConstructor(List<SemanticNode> l, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateDefineConstructorList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		TypeName typeName = new TypeName(l.get(1).asSymbol());
		TypeRepresentation typeRepresentation = new TypeRepresentation(l.get(2).asSymbol());

		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(l.get(3).asList(), letType);
		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));
		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));
		Expression body = SemanticParser.parseNode(l.get(4), letType);

		Lambda lambda = new Lambda(lambdaArgs, argsTypes, body);

		return new DefineConstructor(new TypeAtom(typeName, typeRepresentation), lambda);
	}

	/**
	 * Parses convert special form list
	 * 
	 * @param specialFormList list of the special form
	 * @return application.Convert instance
	 * @throws AppendableException if there is a validation error
	 */
	private static Expression parseConvert(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> letType)
			throws AppendableException {
		try {
			Validations.validateConvertList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList);
			throw e;
		}

		SemanticNode fromNode = specialFormList.get(1);
		TypeAtom from = SemanticParser.parseConversionType(fromNode, letType);

		SemanticNode toNode = specialFormList.get(2);
		TypeAtom to = SemanticParser.parseConversionType(toNode, letType);

		SemanticNode exprNode = specialFormList.get(3);
		Expression expression = SemanticParser.parseNode(exprNode, letType);

		return new Convert(from, to, expression);
	}

	/**
	 * Parses construct special from list
	 * 
	 * @param specialFormList parsed list
	 * @return application.Construct instance
	 * @throws AppendableException if there is validation error
	 */
	private static Expression parseConstruct(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		try {
			Validations.validateConstructList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList);
			throw e;
		}

		TypeName name = new TypeName(specialFormList.get(1).asSymbol());
		TypeRepresentation representation = new TypeRepresentation(specialFormList.get(2).asSymbol());
		TypeAtom type = new TypeAtom(name, representation);

		Tuple arguments = Tuple.EMPTY_TUPLE;

		List<SemanticNode> l = specialFormList.subList(3, specialFormList.size());
		try {
			arguments = new Tuple(l.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x, letType)))
					.collect(Collectors.toList()));
		} catch (RuntimeException e) {
			AppendableException ae = (AppendableException) e.getCause();
			throw ae;
		}

		return new Construct(type, arguments);
	}

	/**
	 * Parses Deconstruct special form
	 * 
	 * @param specialFormList parsed list
	 * @return Deconstruct instance
	 * @throws AppendableException if validations fails
	 */
	private static Deconstruct parseDeconstruct(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		try {
			Validations.validateDeconstructList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		Expression e = SemanticParser.parseNode(specialFormList.get(1), letType);
		Type t = SemanticParser.parseType(specialFormList.get(2), letType);

		return new Deconstruct(e, t);
	}

	/**
	 * Parses can deconstruct as special from list
	 * 
	 * @param specialFormList parsed list
	 * @return CanDeconstructAs instance
	 * @throws AppendableException if validation fails
	 */
	private static CanDeconstructAs parseCanDeconstructAs(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> letType) throws AppendableException {
		try {
			Validations.validateCanDeconstructAsList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		Expression e = SemanticParser.parseNode(specialFormList.get(1), letType);
		Type t = SemanticParser.parseType(specialFormList.get(2), letType);

		return new CanDeconstructAs(e, t);
	}

	/**
	 * Parses let-type special form list
	 * 
	 * @param specialFormList parsed token list
	 * @param typeLet         current type-let
	 * @return parsed expression
	 * @throws AppendableException if validations fails
	 */
	private static Expression parseLetType(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateLetTypeList(specialFormList, typeLet);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		SemanticNode typeVariableList = specialFormList.get(1);
		List<TypeVariable> typeVariables = SemanticParser.parseTypeVariableList(typeVariableList.asList());

		Map<TypeVariable, TypeVariable> newTypeLet = new TreeMap<TypeVariable, TypeVariable>(typeLet);
		for (TypeVariable t : typeVariables) {
			if (!(t instanceof TypeVariable)) {
				throw new AppendableException("Only typevariables are allowed in let-type found: " + t);
			}

			TypeVariable tv = (TypeVariable) t;

			newTypeLet.put(tv, new TypeVariable(NameGenerator.next()));
		}

		SemanticNode bodyNode = specialFormList.get(2);

		return SemanticParser.parseNode(bodyNode, newTypeLet);
	}

	/**
	 * Parses typevariable list for let-type
	 * 
	 * @param list parsed list
	 * @return Typetuple of parsed typevariables
	 * @throws AppendableException if validation fails
	 */
	private static List<TypeVariable> parseTypeVariableList(List<SemanticNode> list) throws AppendableException {
		try {
			Validations.validateTypeVariableList(list);
		} catch (AppendableException e) {
			e.appendMessage("in " + list.toString());
			throw e;
		}

		List<TypeVariable> l = new LinkedList<TypeVariable>();
		for (SemanticNode n : list) {
			l.add(new TypeVariable(n.asSymbol()));
		}
		return l;
	}

	/**
	 * parses instance-of special form list
	 * 
	 * @param specialFormList list of the special form
	 * @param typeLet         used typelet
	 * @return InstanceOf expression
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseInstanceOfRepresentation(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		try {
			Validations.validateInstanceOfRepresentationList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		Expression e = SemanticParser.parseNode(specialFormList.get(1), typeLet);
		Type t = SemanticParser.parseType(specialFormList.get(2), typeLet);

		return new InstanceOfRepresentation(e, t);
	}

	/**
	 * parses instance-of-representation special form list
	 * 
	 * @param specialFormList list of the special form
	 * @param typeLet         used typelet
	 * @return InstanceOfRepresentation expression
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseInstanceOf(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		try {
			Validations.validateInstanceOfList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		Expression e = SemanticParser.parseNode(specialFormList.get(1), typeLet);
		Type t = SemanticParser.parseType(specialFormList.get(2), typeLet);

		return new InstanceOf(e, t);
	}

	/**
	 * parses eapply special form list
	 * 
	 * @param specialFormList list of the special form
	 * @param typeLet         used typelet
	 * @return AbstractionApplication expression
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseEapply(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateEapplyList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		Expression abstraction = SemanticParser.parseNode(specialFormList.get(1), typeLet);
		Expression args = SemanticParser.parseNode(specialFormList.get(2), typeLet);

		if (specialFormList.size() == 4) {
			Expression ranking = SemanticParser.parseNode(specialFormList.get(3), typeLet);
			return new AbstractionApplication(abstraction, args, ranking);
		}

		return new AbstractionApplication(abstraction, args);
	}

	/**
	 * parses extended-lambda-ranking special form list
	 * 
	 * @param specialFormList list of the special form
	 * @param typeLet         used typelet
	 * @return ExtendedLambda expression
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseExtendedLambdaRanking(List<SemanticNode> specialFormList,
			Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		try {
			Validations.validateExtendedLambdaRankingList(specialFormList, typeLet);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}

		List<TypeVariablePair> args = SemanticParser.parseTypedArgList(specialFormList.get(1).asList(), typeLet);
		Tuple argsTuple = new Tuple(args.stream().map(x -> x.second).collect(Collectors.toList()));

		Expression ranking = SemanticParser.parseNode(specialFormList.get(2), typeLet);

		List<SemanticNode> impls = specialFormList.subList(3, specialFormList.size());
		Set<Lambda> implementations = SemanticParser.parseImplementations(argsTuple, impls, typeLet);

		return ExtendedLambda.makeExtendedLambda(implementations, ranking);
	}
	
	/**
	 * Parses Get special form
	 * @param specialFormList parsed token list
	 * @param typeLet used type let
	 * @return Get expression
	 * @throws AppendableException if validation fails
	 */
	private static Get parseGet(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet) throws AppendableException {
		try {
			Validations.validateGetList(specialFormList, typeLet);
		}catch(AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}
		
		Expression tuple = SemanticParser.parseNode(specialFormList.get(1), typeLet);
		Expression index = SemanticParser.parseNode(specialFormList.get(2), typeLet);
		
		return Get.makeGet(tuple, index);
	}
	
	/**
	 * Parses Tuple special form
	 * @param specialFormList parsed token list
	 * @param typeLet used type let
	 * @return parsed tuple
	 * @throws AppendableException if validation fails
	 */
	private static Tuple parseTuple(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		try {
			Validations.validateTupleList(specialFormList, typeLet);
		}catch(AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}
		
		List<Expression> l = new LinkedList<Expression>();
		try {
			l = specialFormList.subList(1, specialFormList.size()).stream()
					.map(ThrowingFunction.wrapper(node -> SemanticParser.parseNode(node, typeLet)))
					.collect(Collectors.toList());
		}catch(RuntimeException re) {
			if(re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException)re.getCause();
				e.appendMessage(" in " + specialFormList);
				throw e;
			}
			throw re;
		}
		
		return new Tuple(l);
	}
	
	/**
	 * Parses single let like binding
	 * @param bindingList parsed list
	 * @param typeLet used type let
	 * @return pair with parsed symbol and binded expression
	 * @throws AppendableException if validation fails
	 */
	private static Pair<Symbol, Expression> parseLetBinding(List<SemanticNode> bindingList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		Validations.validateLetBinding(bindingList, typeLet);
		
		Symbol symbol = (Symbol) SemanticParser.parseNode(bindingList.get(0), typeLet);
		Expression binded = SemanticParser.parseNode(bindingList.get(1), typeLet);
		
		return new Pair<Symbol, Expression>(symbol, binded);
	}
	
	/**
	 * Parses bindings of let like special form
	 * @param bindingsList list of bindings
	 * @param typeLet used type let
	 * @return list of Symbol - Expression pairs
	 * @throws AppendableException if validation fails
	 */
	private static List<Pair<Symbol, Expression>> parseLetBindings(List<SemanticNode> bindingsList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		List<Pair<Symbol, Expression>> bindings = null;
		try {
			bindings = bindingsList.stream()
					.map(ThrowingFunction.wrapper(binding -> SemanticParser.parseLetBinding(binding.asList(), typeLet))).collect(Collectors.toList());
		}catch(RuntimeException re) {
			if(re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException)re.getCause();
				e.appendMessage(" in " + bindingsList + "\n");
				throw e;
			}
		}
		return bindings;
	}
	
	/**
	 * Parses let special form list
	 * @param specialFormList parsed special form list
	 * @param typeLet used typelet
	 * @return application representing let
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseLet(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		try {
			Validations.validateLetList(specialFormList, typeLet);;
		}catch(AppendableException e) {
			e.appendMessage(" in " + specialFormList);
			throw e;
		}
		
		SemanticNode bindingNode = specialFormList.get(1);
		List<Pair<Symbol, Expression>> bindings = SemanticParser.parseLetBindings(bindingNode.asList(), typeLet);
		
		Expression body = SemanticParser.parseNode(specialFormList.get(2), typeLet);
		
		Lambda l = new Lambda(new Tuple(bindings.stream().map(p -> p.first).collect(Collectors.toList())),
				new TypeTuple(bindings.stream().map(p -> new TypeVariable(NameGenerator.next()))
						.collect(Collectors.toList())),
				body);
		
		AbstractionApplication apl = new AbstractionApplication(l,
					new Tuple(bindings.stream().map(p -> p.second).collect(Collectors.toList())));
		
		return apl;
	}
	
	/**
	 * Parses let* special form
	 * @param specialFormList parsed list
	 * @param typeLet used type let
	 * @return expression representing let*
	 * @throws AppendableException if validation fails
	 */
	private static Expression parseLetAst(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		try {
			Validations.validateLetAstList(specialFormList, typeLet);;
		}catch(AppendableException e) {
			e.appendMessage(" in " + specialFormList);
			throw e;
		}
		
		SemanticNode bindingNode = specialFormList.get(1);
		List<Pair<Symbol, Expression>> bindings = SemanticParser.parseLetBindings(bindingNode.asList(), typeLet);
		
		Expression body = SemanticParser.parseNode(specialFormList.get(2), typeLet);
		
		ListIterator<Pair<Symbol, Expression>> i = bindings.listIterator(bindings.size());
		
		while(i.hasPrevious()) {
			Pair<Symbol, Expression> p = i.previous();
			Lambda l = new Lambda(
						new Tuple(p.first),
						new TypeTuple(new TypeVariable(NameGenerator.next())),
						body);
			body = new AbstractionApplication(l, new Tuple(p.second));
		}
		
		return body;
	}
	
	/**
	 * Parses Loop special forms
	 * 
	 * @param specialFormList parsed list
	 * @param typeLet used typelet
	 * @return parsed Loop expression
	 * @throws AppendableException if parsing is not succesfull
	 */
	private static Expression parseLoop(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		try {
			Validations.validateLoop(specialFormList, typeLet);
		} catch (AppendableException e) {
			e.appendMessage(" in " + specialFormList);
			throw e;
		}

		SemanticNode bindingNode = specialFormList.get(1);
		List<Pair<Symbol, Expression>> bindings = SemanticParser.parseLetBindings(bindingNode.asList(), typeLet);

		Expression body = SemanticParser.parseNode(specialFormList.get(2), typeLet);

		Tuple argSymbols = new Tuple(bindings.stream().map(x -> x.first).collect(Collectors.toList()));
		Tuple args = new Tuple(bindings.stream().map(x -> x.second).collect(Collectors.toList()));

		return new Loop(argSymbols, body, args);
	}
	
	/**
	 * Parses recur special form
	 * @param specialFormList parsed special form list
	 * @param typeLet used typelet
	 * @return parsed Recur expression
	 * @throws AppendableException if parsing fails
	 */
	private static Expression parseRecur(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		try {
			Validations.validateRecur(specialFormList, typeLet);
		}catch(AppendableException e) {
			e.appendMessage(" in " + specialFormList);
			throw e;
		}
		
		List<SemanticNode> argsList = specialFormList.subList(1, specialFormList.size());
		List<Expression> l = new LinkedList<Expression>();
		
		for(SemanticNode n : argsList) {
			l.add(SemanticParser.parseNode(n));
		}		
		
		Tuple args = new Tuple(l);
		
		return new Recur(args);
	}
}
