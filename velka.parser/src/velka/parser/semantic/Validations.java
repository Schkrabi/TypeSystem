package velka.parser.semantic;

import java.util.List;
import java.util.Map;

import velka.parser.antlr.SemanticNode;
import velka.parser.exceptions.InvalidNumberOfArgsException;
import velka.parser.exceptions.UnexpectedExpressionException;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.Pair;
import velka.util.ThrowingConsumer;
import velka.util.ThrowingFunction;

/**
 * This class contains static validation methods used by SemanticParser
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Validations {

	/**
	 * Checks the arguments of the Lambda special form
	 * 
	 * @param lambdaList checked arguments
	 * @return true if the arguments are valid lambda arguments, false otherwise
	 * @throws Exception
	 */
	public static boolean validateLambdaList(List<SemanticNode> lambdaList) throws AppendableException {
		if (lambdaList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, lambdaList.size());
		}
		SemanticNode lambdaSymbol = lambdaList.get(0);
		if (lambdaSymbol.type != SemanticNode.NodeType.SYMBOL
				|| !lambdaSymbol.asSymbol().equals(SemanticParserStatic.LAMBDA)) {
			throw new UnexpectedExpressionException(lambdaSymbol.toString());
		}

		SemanticNode lambdaArgs = lambdaList.get(1);

		if (lambdaArgs.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(lambdaArgs.toString());
		}

		return true;
	}

	/**
	 * Checks the arguments of the deftype special form
	 * 
	 * @param defTypeList checked arguments
	 * @return true if the arguments are valid otherwise throws Exception
	 * @throws Exception
	 */
	public static boolean validateDefTypeList(List<SemanticNode> defTypeList) throws AppendableException {
		if (defTypeList.size() != 2) {
			throw new InvalidNumberOfArgsException(2, defTypeList.size());
		}
		SemanticNode deftypeSymbol = defTypeList.get(0);
		if (deftypeSymbol.type != SemanticNode.NodeType.SYMBOL
				|| !deftypeSymbol.asSymbol().equals(SemanticParserStatic.TYPE)) {
			throw new UnexpectedExpressionException(deftypeSymbol.toString());
		}

		SemanticNode name = defTypeList.get(1);

		if (name.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(name.toString());
		}

		return true;
	}

	/**
	 * Checks the arguments of the defrep special form
	 * 
	 * @param defRepList checked arguments
	 * @return true if the arguments are valid, otherwise throws exception
	 * @throws AppendableException
	 */
	public static boolean validateDefRepList(List<SemanticNode> defRepList) throws AppendableException {
		if (defRepList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, defRepList.size() - 1);
		}
		SemanticNode defRepSymbol = defRepList.get(0);
		if (defRepSymbol.type != SemanticNode.NodeType.SYMBOL
				|| !defRepSymbol.asSymbol().equals(SemanticParserStatic.REPRESENTATION)) {
			throw new UnexpectedExpressionException(defRepSymbol.toString());
		}

		SemanticNode rName = defRepList.get(1);
		if (rName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(rName.toString());
		}

		SemanticNode tName = defRepList.get(2);
		if (tName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(tName.toString());
		}

		return true;
	}

	/**
	 * Checks the arguments of the if special form
	 * 
	 * @param ifList validated list of id special form
	 * @return true if it is valid if special form list, throws exception otherwise
	 * @throws AppendableException
	 */
	public static boolean validateIfList(List<SemanticNode> ifList) throws AppendableException {
		if (ifList.size() != 4) {
			throw new InvalidNumberOfArgsException(3, ifList.size() - 1);
		}

		return true;
	}

	/**
	 * Validates extended lambda special form list
	 * 
	 * @param l       validated list
	 * @param typeLet
	 * @return true if list validates, throws exception otherwise
	 * @throws AppendableException
	 */
	public static boolean validateElambdaList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (l.size() != 2) {
			throw new InvalidNumberOfArgsException(1, l.size() - 1);
		}

		SemanticNode argsList = l.get(1);
		if (argsList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(argsList.toString());
		}

		return true;
	}

	/**
	 * Validates the list of implementations for extended lambda
	 * 
	 * @param l       list of implementations
	 * @param typeLet
	 * @return true if list validates, otherwise throws AppendableException
	 * @throws AppendableException
	 */
	public static boolean validateImplementations(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		for (SemanticNode n : l) {
			if (n.type != SemanticNode.NodeType.LIST) {
				throw new UnexpectedExpressionException(n.toString());
			}
			Validations.validateImplementation(n.asList(), typeLet);
		}
		return true;
	}

	/**
	 * Validates a single implementation for extended lambda
	 * 
	 * @param l       list containing the implementation
	 * @param typeLet
	 * @return true if implementation validates, otherwise throws excetpion
	 * @throws AppendableException
	 */
	public static boolean validateImplementation(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (l.size() != 2) {
			throw new AppendableException("Badly formed implementation " + l);
		}

		SemanticNode typeList = l.get(0);

		if (typeList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(typeList.toString());
		}
		return true;
	}

	/**
	 * Validates if semantic node is valid variable type pair
	 * 
	 * @param pair validated semantic node
	 * @return true if node is valid variable type pair, throws exception otherwise
	 * @throws AppendableException
	 */
	public static boolean validateVariableTypePair(SemanticNode pair) throws AppendableException {
		if (pair.type != SemanticNode.NodeType.LIST || pair.asList().size() != 2) {
			throw new UnexpectedExpressionException(pair.toString());
		}
		List<SemanticNode> l = pair.asList();

		SemanticNode type = l.get(0);
		if (!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type.toString());
		}

		SemanticNode variable = l.get(1);
		if (variable.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(variable.toString());
		}

		return true;
	}

	/**
	 * Returns true if given node can be type identifier, returns false otherwise
	 * 
	 * @param n inspected node
	 * @return true or false
	 * @throws AppendableException if n cannot be type identifier
	 */
	private static boolean isTypeIdentifierNode(SemanticNode n) throws AppendableException {
		if (n.type == SemanticNode.NodeType.LIST) {
			List<SemanticNode> l = n.asList();
			try {
				return l.stream().map(ThrowingFunction.wrapper(x -> Validations.isTypeIdentifierNode(x))).reduce(true,
						Boolean::logicalAnd);
			} catch (RuntimeException re) {
				AppendableException e = (AppendableException) re.getCause();
				throw e;
			}
		}
		if (n.type == SemanticNode.NodeType.ARROW) {
			Pair<SemanticNode, SemanticNode> p = n.asArrow();
			return Validations.isTypeIdentifierNode(p.first) && Validations.isTypeIdentifierNode(p.second);
		}

		return n.type == SemanticNode.NodeType.SYMBOL || n.type == SemanticNode.NodeType.PAIR;
	}

	/**
	 * Validates if l is a valid defconversion list, throws exception if validation
	 * fails
	 * 
	 * @param l validated list
	 * @return true if l is valid defconversion list, false otherwise
	 */
	public static boolean validateDefconversionList(List<SemanticNode> l) throws AppendableException {
		if (l.size() != 5) {
			throw new InvalidNumberOfArgsException(4, l.size() - 1);
		}

		SemanticNode defConversion = l.get(0);
		if (defConversion.type != SemanticNode.NodeType.SYMBOL
				|| !defConversion.asSymbol().equals(SemanticParserStatic.CONVERSION)) {
			throw new UnexpectedExpressionException(defConversion.toString());
		}

		if (!Validations.isTypeIdentifierNode(l.get(1))) {
			throw new UnexpectedExpressionException(l.get(1).toString());
		}

		if (!Validations.isTypeIdentifierNode(l.get(2))) {
			throw new UnexpectedExpressionException(l.get(2).toString());
		}

		SemanticNode c = l.get(3);
		if (c.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(c.toString());
		}

		return true;
	}

	/**
	 * Validates if l is a valid define list, throws exception if valiation fails
	 * 
	 * @param l validated list
	 * @return true if l is valid define list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateDefineList(List<SemanticNode> l) throws AppendableException {
		if (l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}

		SemanticNode define = l.get(0);
		if (define.type != SemanticNode.NodeType.SYMBOL || !define.asSymbol().equals(SemanticParserStatic.DEFINE)) {
			throw new UnexpectedExpressionException(define.toString());
		}

		if (l.get(1).type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(l.get(1).toString());
		}

		return true;
	}

	/**
	 * Validates if l is a valid cons list, throws exception if validation fails
	 * 
	 * @param l validated list
	 * @return true if l is valid cons list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateConsList(List<SemanticNode> l) throws AppendableException {
		if (l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}

		SemanticNode cons = l.get(0);
		if (cons.type != SemanticNode.NodeType.SYMBOL || !cons.asSymbol().equals(SemanticParserStatic.CONS)) {
			throw new UnexpectedExpressionException(cons.toString());
		}

		return true;
	}

	/**
	 * Validates if l is a valid error list, throws exception if validation fails
	 * 
	 * @param l validated list
	 * @return true if l is valid error list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateErrorList(List<SemanticNode> l) throws AppendableException {
		if (l.size() != 2) {
			throw new InvalidNumberOfArgsException(1, l.size() - 1);
		}

		SemanticNode error = l.get(0);
		if (error.type != SemanticNode.NodeType.SYMBOL || !error.asSymbol().equals(SemanticParserStatic.ERROR)) {
			throw new UnexpectedExpressionException(error.toString());
		}
		SemanticNode msg = l.get(1);
		if (msg.type != SemanticNode.NodeType.STRING) {
			throw new UnexpectedExpressionException(msg.toString());
		}

		return true;
	}

	/**
	 * Validates list of AndExpression
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if list does not validate
	 */
	public static void validateAndList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode and = specialFormList.get(0);
		if (and.type != SemanticNode.NodeType.SYMBOL || !and.asSymbol().equals(SemanticParserStatic.AND)) {
			throw new UnexpectedExpressionException(and.toString());
		}

	}

	/**
	 * Validates list of OrExpression
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if list does not validate
	 */
	public static void validateOrList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() != 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode or = specialFormList.get(0);
		if (or.type != SemanticNode.NodeType.SYMBOL || !or.asSymbol().equals(SemanticParserStatic.OR)) {
			throw new UnexpectedExpressionException(or.toString());
		}

	}

	/**
	 * Validates list of Define Constructor expression
	 * 
	 * @param l validated list
	 * @throws AppendableException if list does not validate
	 */
	public static void validateDefineConstructorList(List<SemanticNode> l) throws AppendableException {
		if (l.size() != 5) {
			throw new InvalidNumberOfArgsException(4, l.size() - 1);
		}

		SemanticNode constructor = l.get(0);
		if (constructor.type != SemanticNode.NodeType.SYMBOL
				|| !constructor.asSymbol().equals(SemanticParserStatic.CONSTRUCTOR)) {
			throw new UnexpectedExpressionException(constructor.toString());
		}

		SemanticNode typeName = l.get(1);
		if (typeName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(typeName.toString());
		}

		SemanticNode representationName = l.get(2);
		if (representationName.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(representationName.toString());
		}

		SemanticNode argumentList = l.get(3);
		if (argumentList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(argumentList.toString());
		}
	}

	/**
	 * Validates Convert special form list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateConvertList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() != 4) {
			throw new InvalidNumberOfArgsException(3, specialFormList.size() - 1);
		}

		SemanticNode convert = specialFormList.get(0);
		if (convert.type != SemanticNode.NodeType.SYMBOL || !convert.asSymbol().equals(SemanticParserStatic.CONVERT)) {
			throw new UnexpectedExpressionException(convert.toString());
		}

		SemanticNode from = specialFormList.get(1);
		if (!Validations.isTypeIdentifierNode(from)) {
			throw new UnexpectedExpressionException(from.toString());
		}

		SemanticNode to = specialFormList.get(2);
		if (!Validations.isTypeIdentifierNode(to)) {
			throw new UnexpectedExpressionException(to.toString());
		}
	}

	/**
	 * Validates Construt special form list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateConstructList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode construct = specialFormList.get(0);
		if (construct.type != SemanticNode.NodeType.SYMBOL
				|| !construct.asSymbol().equals(SemanticParserStatic.CONSTRUCT)) {
			throw new UnexpectedExpressionException(construct.toString());
		}

		SemanticNode type = specialFormList.get(1);
		if (type.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(type.toString());
		}

		SemanticNode representation = specialFormList.get(2);
		if (representation.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(representation.toString());
		}
	}

	/**
	 * Validates deconstruct-as special form
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateDeconstructList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode deconstructAs = specialFormList.get(0);
		if (deconstructAs.type != SemanticNode.NodeType.SYMBOL
				|| !deconstructAs.asSymbol().equals(SemanticParserStatic.DECONSTRUCT)) {
			throw new UnexpectedExpressionException(deconstructAs.toString());
		}

		SemanticNode type = specialFormList.get(2);
		if (!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type.toString());
		}
	}

	/**
	 * Validates can-deconstruct-as special form list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateCanDeconstructAsList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode deconstructAs = specialFormList.get(0);
		if (deconstructAs.type != SemanticNode.NodeType.SYMBOL
				|| !deconstructAs.asSymbol().equals(SemanticParserStatic.CAN_DECONSTRUCT_AS)) {
			throw new UnexpectedExpressionException(deconstructAs.toString());
		}

		SemanticNode type = specialFormList.get(2);
		if (!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type.toString());
		}
	}

	/**
	 * Validates let-type special form list
	 * 
	 * @param specialFormList validated list
	 * @param typeLet         current typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateLetTypeList(List<SemanticNode> specialFormList, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode letType = specialFormList.get(0);
		if (letType.type != SemanticNode.NodeType.SYMBOL || !letType.asSymbol().equals(SemanticParserStatic.LET_TYPE)) {
			throw new UnexpectedExpressionException(letType.toString());
		}

		SemanticNode typeVariableList = specialFormList.get(1);
		if (typeVariableList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(typeVariableList.toString());
		}

	}

	/**
	 * Validates typeVariableList
	 * 
	 * @param list validated list
	 * @throws UnexpectedExpressionException if validation fails
	 */
	public static void validateTypeVariableList(List<SemanticNode> list) throws UnexpectedExpressionException {
		for (SemanticNode n : list) {
			if (n.type != SemanticNode.NodeType.SYMBOL) {
				throw new UnexpectedExpressionException(n.toString());
			}
		}
	}

	/**
	 * Valdiates instance-of-representation list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateInstanceOfRepresentationList(List<SemanticNode> specialFormList)
			throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode iofr = specialFormList.get(0);
		if (iofr.type != SemanticNode.NodeType.SYMBOL
				|| !iofr.asSymbol().contentEquals(SemanticParserStatic.INSTANCE_OF_REPRESENTATION)) {
			throw new UnexpectedExpressionException(iofr.toString());
		}

		SemanticNode type = specialFormList.get(2);
		if (!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type.toString());
		}
	}

	/**
	 * validates instance-of list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateInstanceOfList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() < 3) {
			throw new InvalidNumberOfArgsException(2, specialFormList.size() - 1);
		}

		SemanticNode iof = specialFormList.get(0);
		if (iof.type != SemanticNode.NodeType.SYMBOL
				|| !iof.asSymbol().contentEquals(SemanticParserStatic.INSTANCE_OF)) {
			throw new UnexpectedExpressionException(iof.toString());
		}

		SemanticNode type = specialFormList.get(2);
		if (!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type.toString());
		}

	}

	/**
	 * Validates eapply special form list
	 * 
	 * @param specialFormList validated list
	 * @throws AppendableException if validation fails
	 */
	public static void validateEapplyList(List<SemanticNode> specialFormList) throws AppendableException {
		if (specialFormList.size() != 4 && specialFormList.size() != 3) {
			throw new AppendableException(
					"error, invalid number of arguments.\n eapply has syntax \n\t(eapply fun arg-tuple)\nOR\n\t(eapply fun arg-tuple ranking-fun)");
		}

		SemanticNode eapply = specialFormList.get(0);
		if (eapply.type != SemanticNode.NodeType.SYMBOL
				|| !eapply.asSymbol().contentEquals(SemanticParserStatic.EAPPLY)) {
			throw new UnexpectedExpressionException(eapply.toString());
		}
	}

	/**
	 * Validates get special form list
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateGetList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}
		
		SemanticNode get = l.get(0);
		if(get.type != SemanticNode.NodeType.SYMBOL
				|| !get.asSymbol().contentEquals(SemanticParserStatic.GET)) {
			throw new UnexpectedExpressionException(get.toString());
		}
	}
	
	/**
	 * Validates tuple special form list
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateTupleList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		if(l.size() < 1) {
			throw new AppendableException("Badly formed tuple: " + l);
		}
		
		SemanticNode tuple = l.get(0);
		if(tuple.type != SemanticNode.NodeType.SYMBOL
				|| !tuple.asSymbol().contentEquals(SemanticParserStatic.TUPLE)) {
			throw new UnexpectedExpressionException(tuple.toString());
		}
	}
	
	/**
	 * Validates let binding pair
	 * @param l let binding pair list
	 * @param typeLet used typelet 
	 * @throws AppendableException if validation fails
	 */
	public static void validateLetBinding(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet) 
		throws AppendableException {
		if(l.size() != 2) {
			throw new AppendableException("Invalid let binding got " + l);
		}
		
		SemanticNode symbol = l.get(0);
		if(symbol.type != SemanticNode.NodeType.SYMBOL) {
			throw new AppendableException("First part of let binding must be symbol, got " + symbol);
		}
	}
	
	/**
	 * Validates let special form list
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateLetList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		Validations.validateLetLike(SemanticParserStatic.LET, l, typeLet);
	}
	
	/**
	 * Validates let* special form list
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateLetAstList(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		Validations.validateLetLike(SemanticParserStatic.LET_AST, l, typeLet);
	}
	
	/**
	 * Validates special form list with let structure
	 * @param firstSymbol fist symbol that should be at list head
	 * @param l validated list
	 * @param typeLet used typeLet
	 * @throws AppendableException if validation fails
	 */
	private static void validateLetLike(String firstSymbol, List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		if(l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}
		
		SemanticNode let = l.get(0);
		if(let.type != SemanticNode.NodeType.SYMBOL
				|| !let.asSymbol().contentEquals(firstSymbol)) {
			throw new UnexpectedExpressionException(let.toString());
		}
		
		SemanticNode bindings = l.get(1);
		if(bindings.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(bindings.toString());
		}
		
		List<SemanticNode> bList = bindings.asList();
		
		if(!bList.stream().allMatch(node -> node.type == SemanticNode.NodeType.LIST)) {
			throw new AppendableException("Badly formed bindings expected ((symbol expr)*) got " + bList);
		}
		
		try {
		bList.stream()
			.forEach(ThrowingConsumer.wrapper(
					node -> Validations.validateLetBinding(node.asList(), typeLet)));
		}catch(RuntimeException re) {
			if(re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException)re.getCause();
				throw e;
			}
			throw re;
		}		
	}
	
	/**
	 * Validates loop special form
	 * 
	 * @param l special form list
	 * @param typeLet used type let
	 * @throws AppendableException if anything goes awry
	 */
	public static void validateLoop(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if (l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}

		SemanticNode loop = l.get(0);
		if (loop.type != SemanticNode.NodeType.SYMBOL || !loop.asSymbol().equals(SemanticParserStatic.LOOP)) {
			throw new UnexpectedExpressionException(loop.asSymbol());
		}

		SemanticNode bindings = l.get(1);
		if (bindings.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(bindings.toString());
		}

		List<SemanticNode> bList = bindings.asList();

		if (!bList.stream().allMatch(node -> node.type == SemanticNode.NodeType.LIST)) {
			throw new AppendableException("Badly formed bindings expected ((symbol expr)*) got " + bList);
		}

		try {
			bList.stream()
					.forEach(ThrowingConsumer.wrapper(node -> Validations.validateLetBinding(node.asList(), typeLet)));
		} catch (RuntimeException re) {
			if (re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException) re.getCause();
				throw e;
			}
			throw re;
		}
	}
	
	/**
	 * Validates Recur special form
	 * 
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if validation fails
	 */
	public static void validateRecur(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
			throws AppendableException {
		if(l.size() < 1) {
			throw new InvalidNumberOfArgsException(1, l.size() - 1);
		}
		
		SemanticNode recur = l.get(0);
		if(recur.type != SemanticNode.NodeType.SYMBOL
				|| !recur.asSymbol().equals(SemanticParserStatic.RECUR)) {
			throw new UnexpectedExpressionException(recur.asSymbol());
		}
	}
	
	/**
	 * Validates Extend special form.
	 * 
	 * @param l validated list
	 * @param typeLet used typelet
	 * @throws AppendableException if anything is wrong
	 */
	public static void validateExtend(List<SemanticNode> l, Map<TypeVariable, TypeVariable> typeLet)
		throws AppendableException {
		if(l.size() > 4 || l.size() < 3) {
			throw new InvalidNumberOfArgsException(2, l.size() - 1);
		}
		
		SemanticNode extend = l.get(0);
		if(extend.type != SemanticNode.NodeType.SYMBOL
				|| !extend.asSymbol().equals(SemanticParserStatic.EXTEND)) {
			throw new UnexpectedExpressionException(extend.asSymbol());
		}
	}
}
