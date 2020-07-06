package semantic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import abstraction.ExtendedLambda;
import abstraction.Lambda;
import application.AndExpression;
import application.Construct;
import application.Convert;
import application.Deconstruct;
import application.DefineConstructor;
import application.AbstractionApplication;
import application.DefineConversion;
import application.DefineSymbol;
import application.ExceptionExpr;
import application.IfExpression;
import expression.Expression;
import expression.Tuple;
import expression.Symbol;
import literal.LitBoolean;
import literal.LitDouble;
import literal.LitInteger;
import literal.LitString;
import literal.Literal;
import parser.SemanticNode;
import parser.SemanticPair;
import types.Type;
import types.TypeArrow;
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

		return ExtendedLambda.makeExtendedLambda(implementations);
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
		case SemanticParserStatic.DEFINE_REPRESENTATION:
			e = SemanticParser.parseDefrep(specialFormList);
			break;
		case SemanticParserStatic.DEFINE_TYPE:
			e = SemanticParser.parseDeftype(specialFormList);
			break;
		case SemanticParserStatic.EXTENDED_LAMBDA:
			e = SemanticParser.parseElambda(specialFormList);
			break;
		case SemanticParserStatic.IF:
			e = SemanticParser.parseIf(specialFormList);
			break;
		case SemanticParserStatic.LAMBDA:
			e = SemanticParser.parseLambda(specialFormList);
			break;
		case SemanticParserStatic.DEFINE_CONVERSION:
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
		case SemanticParserStatic.AND:
			e = SemanticParser.parseAnd(specialFormList);
			break;
		case SemanticParserStatic.OR:
			e = SemanticParser.parseOr(specialFormList);
			break;
		case SemanticParserStatic.DEFINE_CONSTRUCTOR:
			e = SemanticParser.parseDefineConstructor(specialFormList);
			break;
		case SemanticParserStatic.CONSTRUCT:
			e = SemanticParser.parseConstruct(specialFormList);
			break;
		case SemanticParserStatic.CONVERT:
			e = SemanticParser.parseConvert(specialFormList);
			break;
		case SemanticParserStatic.DECONSTRUCT:
			e = SemanticParser.parseDeconstruct(specialFormList);
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

		return new AbstractionApplication(fun, args);
	}

	/**
	 * Parses And special form
	 * 
	 * @param specialFormList list of arguments of special form
	 * @return new AndExpression instance
	 * @throws AppendableException if list does not validate
	 */
	private static Expression parseAnd(List<SemanticNode> specialFormList) throws AppendableException {
		try {
			Validations.validateAndList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in AndExpression " + specialFormList);
			throw e;
		}

		try {
			Tuple args = new Tuple(specialFormList.subList(1, specialFormList.size()).stream()
					.map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x))).collect(Collectors.toList()));
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
	private static Expression parseOr(List<SemanticNode> specialFormList) throws AppendableException {
		try {
			Validations.validateOrList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in OrExpression " + specialFormList);
			throw e;
		}

		try {
			Tuple args = new Tuple(specialFormList.subList(1, specialFormList.size()).stream()
					.map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x))).collect(Collectors.toList()));
			return new application.OrExpression(args);
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
		
		TypeEnvironment.singleton.addType(new TypeName(deftypeList.get(1).asSymbol()));
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
	private static Expression parseDefrep(List<SemanticNode> defrepList) throws AppendableException {
		try {
			Validations.validateDefRepList(defrepList);
		} catch (AppendableException e) {
			e.appendMessage("in " + defrepList);
			throw e;
		}

		TypeRepresentation repName = new TypeRepresentation(defrepList.get(1).asSymbol());
		TypeName typeName = new TypeName(defrepList.get(2).asSymbol());

		TypeEnvironment.singleton.addRepresentation(new TypeAtom(typeName, repName));
		return Expression.EMPTY_EXPRESSION;
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
			return new TypeVariablePair(new TypeVariable(NameGenerator.next()), new Symbol(n.asSymbol()));
		}
		return SemanticParser.parseVariableTypePair(n);
	}
	
	/**
	 * Parses  Wildcard type
	 * @param typeSymbol name of the type
	 * @return TypeAtom
	 * @throws UndefinedTypeException if Type was not defined
	 */
	private static Type parseTypeSymbol(String typeSymbol) throws UndefinedTypeException {
		if(TypeEnvironment.singleton.existType(new TypeName(typeSymbol))) {
			return SemanticParser.parseTypeAtom(typeSymbol, TypeRepresentation.WILDCARD.toString());
		}
		return new TypeVariable(typeSymbol);
	}
	
	/**
	 * Parses type atom by its typeName and reprsentation name
	 * @param typeName name of type
	 * @param typeRepresentation name of representation
	 * @return parsed TypeAtom
	 * @throws UndefinedTypeException if Type or representation was not defined
	 */
	private static TypeAtom parseTypeAtom(String typeName, String typeRepresentation) throws UndefinedTypeException {
		TypeAtom typeAtom = new TypeAtom(new TypeName(typeName), new TypeRepresentation(typeRepresentation));
		if(!TypeEnvironment.singleton.existsTypeAtom(typeAtom)) {
			throw new UndefinedTypeException(typeAtom.toString());
		}
		return typeAtom;
	}
	
	/**
	 * Parses type arrow
	 * @param ltypeNode SemanticNode with left type 
	 * @param rtypeNode SemanticNode with right type
	 * @return typeArrow
	 * @throws AppendableException 
	 */
	private static TypeArrow parseTypeArrow(SemanticNode ltypeNode, SemanticNode rtypeNode) throws AppendableException{
		Type ltype = SemanticParser.parseType(ltypeNode);
		Type rtype = SemanticParser.parseType(rtypeNode);
		return new TypeArrow(ltype, rtype);
	}
	
	/**
	 * Parses TypeTuple
	 * @param l list of semanticNodes representing subtypes
	 * @return TypeTuple
	 * @throws AppendableException if atomic type is not recognized or fault semanticNode is detected
	 */
	private static TypeTuple parseTypeTuple(List<SemanticNode> l) throws AppendableException{
		try {
			List<Type> parsed = l.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseType(x))).collect(Collectors.toList());
			return new TypeTuple(parsed);
		}catch(RuntimeException re) {
			AppendableException e = (AppendableException)re.getCause();
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
	private static Type parseType(SemanticNode typeNode) throws AppendableException {
		switch(typeNode.type) {
		case PAIR:
			SemanticPair semanticPair = typeNode.asPair();
			return SemanticParser.parseTypeAtom(semanticPair.first, semanticPair.second);
		case ARROW:
			Pair<SemanticNode, SemanticNode> arrowPair = typeNode.asArrow();
			return SemanticParser.parseTypeArrow(arrowPair.first, arrowPair.second);
		case LIST:
			List<SemanticNode> l = typeNode.asList();
			return SemanticParser.parseTypeTuple(l);
		case SYMBOL:
			return SemanticParser.parseTypeSymbol(typeNode.asSymbol());
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
	private static TypeVariablePair parseVariableTypePair(SemanticNode pair) throws AppendableException {
		try {
			Validations.validateVariableTypePair(pair);
		} catch (AppendableException e) {
			e.appendMessage("in " + pair);
			throw e;
		}

		Type type = SemanticParser.parseType(pair.asList().get(0));
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
	
	private static TypeAtom parseConversionType(SemanticNode typeNode) throws AppendableException {
		Type parsed = SemanticParser.parseType(typeNode);
		if(!(parsed instanceof TypeAtom)) {
			throw new AppendableException("Only atomic types are allowed, got " + parsed);
		}
		return (TypeAtom)parsed;
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
		TypeAtom fromType = SemanticParser.parseConversionType(l.get(1));
		TypeAtom toType = SemanticParser.parseConversionType(l.get(2));

		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(l.get(3).asList());
		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));
		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));
		Expression body = SemanticParser.parseNode(l.get(4));

		if (argsTypes.size() != 1) {
			AppendableException e = new AppendableException(
					"Conversion must convert from defined type " + fromType + " got " + argsTypes);
			e.appendMessage("in" + l);
			throw e;
		}

		Lambda lambda = new Lambda(lambdaArgs, new TypeTuple(Arrays.asList(fromType)), body);

		return DefineConversion.makeDefineConversion(fromType, toType, lambda);
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

		Symbol v = new Symbol(l.get(1).asSymbol());
		Expression e = SemanticParser.parseNode(l.get(2));
		return new DefineSymbol(v, e);
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

	/**
	 * Parses define constructor special form list
	 * 
	 * @param l special form list
	 * @return DefineConstructor expression
	 * @throws AppendableException
	 */
	private static Expression parseDefineConstructor(List<SemanticNode> l) throws AppendableException {
		try {
			Validations.validateDefineConstructorList(l);
		} catch (AppendableException e) {
			e.appendMessage("in " + l);
			throw e;
		}
		TypeName typeName = new TypeName(l.get(1).asSymbol());
		TypeRepresentation typeRepresentation = new TypeRepresentation(l.get(2).asSymbol());

		List<TypeVariablePair> typedArgs = SemanticParser.parseTypedArgList(l.get(3).asList());
		TypeTuple argsTypes = new TypeTuple(typedArgs.stream().map(x -> x.first).collect(Collectors.toList()));
		Tuple lambdaArgs = new Tuple(typedArgs.stream().map(x -> x.second).collect(Collectors.toList()));
		Expression body = SemanticParser.parseNode(l.get(4));

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
	private static Expression parseConvert(List<SemanticNode> specialFormList) throws AppendableException {
		try {
			Validations.validateConvertList(specialFormList);
		} catch (AppendableException e) {
			e.appendMessage("in " + specialFormList);
			throw e;
		}

		SemanticNode fromNode = specialFormList.get(1);
		TypeAtom from = SemanticParser.parseConversionType(fromNode);

		SemanticNode toNode = specialFormList.get(2);
		TypeAtom to = SemanticParser.parseConversionType(toNode);

		SemanticNode exprNode = specialFormList.get(3);
		Expression expression = SemanticParser.parseNode(exprNode);

		return new Convert(from, to, expression);
	}

	/**
	 * Parses construct special from list
	 * 
	 * @param specialFormList parsed list
	 * @return application.Construct instance
	 * @throws AppendableException if there is validation error
	 */
	private static Expression parseConstruct(List<SemanticNode> specialFormList) throws AppendableException {
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
			arguments = new Tuple(l.stream().map(ThrowingFunction.wrapper(x -> SemanticParser.parseNode(x)))
					.collect(Collectors.toList()));
		} catch (RuntimeException e) {
			AppendableException ae = (AppendableException) e.getCause();
			throw ae;
		}

		return new Construct(type, arguments);
	}
	
	/**
	 * Parses Deconstruct special form
	 * @param specialFormList
	 * @return
	 * @throws AppendableException
	 */
	private static Deconstruct parseDeconstruct(List<SemanticNode> specialFormList) throws AppendableException{
		try {
			Validations.validateDeconstructList(specialFormList);
		}catch(AppendableException e) {
			e.appendMessage("in " + specialFormList.toString());
			throw e;
		}
		
		Expression e = SemanticParser.parseNode(specialFormList.get(1));
		
	}
}
