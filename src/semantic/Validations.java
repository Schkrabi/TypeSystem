package semantic;

import java.util.List;

import parser.SemanticNode;
import util.AppendableException;

/**
 * This class contains static validation methods used by SemanticParser
 * @author Mgr. Radomir Skrabal
 *
 */
public class Validations {

	/**
	 * Checks the arguments of the Lambda special form
	 * 
	 * @param lambdaList
	 *            checked arguments
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
			throw new UnexpectedExpressionException(lambdaSymbol);
		}
	
		SemanticNode lambdaArgs = lambdaList.get(1);
	
		if (lambdaArgs.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(lambdaArgs);
		}
	
		return true;
	}

	/**
	 * Checks the arguments of the deftype special form
	 * 
	 * @param defTypeList
	 *            checked arguments
	 * @return true if the arguments are valid otherwise throws Exception
	 * @throws Exception
	 */
	public static boolean validateDefTypeList(List<SemanticNode> defTypeList) throws AppendableException {
		if (defTypeList.size() != 2) {
			throw new InvalidNumberOfArgsException(2, defTypeList.size());
		}
		SemanticNode deftypeSymbol = defTypeList.get(0);
		if (deftypeSymbol.type != SemanticNode.NodeType.SYMBOL
				|| !deftypeSymbol.asSymbol().equals(SemanticParserStatic.DEFTYPE)) {
			throw new UnexpectedExpressionException(deftypeSymbol);
		}
	
		SemanticNode name = defTypeList.get(1);
	
		if (name.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(name);
		}
	
		//SemanticNode constructor = defTypeList.get(2);
		//if (constructor.type != SemanticNode.NodeType.LIST) {
		//	throw new UnexpectedExpressionException(constructor);
		//}
	
		return true;
	}

	/**
	 * Checks the arguments of the defrep special form
	 * @param defRepList checked arguments
	 * @return true if the arguments are valid, otherwise throws exception
	 * @throws AppendableException
	 */
	public static boolean validateDefRepList(List<SemanticNode> defRepList) throws AppendableException{
		if(defRepList.size() != 3) {
			throw new InvalidNumberOfArgsException(3, defRepList.size() - 1);
		}
		SemanticNode defRepSymbol = defRepList.get(0);
		if(defRepSymbol.type != SemanticNode.NodeType.SYMBOL
				|| !defRepSymbol.asSymbol().equals(SemanticParserStatic.DEFREP)) {
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
		
		//SemanticNode constructor = defRepList.get(3);
		//if(constructor.type != SemanticNode.NodeType.LIST) {
		//	throw new UnexpectedExpressionException(constructor);
		//}
		
		return true;
	}

	/**
	 * Checks the arguments of the if special form
	 * @param ifList validated list of id special form 
	 * @return true if it is valid if special form list, throws exception otherwise
	 * @throws AppendableException
	 */
	public static boolean validateIfList(List<SemanticNode> ifList) throws AppendableException{
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
	public static boolean validateElambdaList(List<SemanticNode> l) throws AppendableException{
		if(l.size() < 3) {
			throw new AppendableException("Too few arguments (" + l.size() + ")");
		}
		
		SemanticNode argsList = l.get(1);
		if (argsList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(argsList);
		}
	
		try{
			Validations.validateImplementations(l.subList(3, l.size()));
		}catch(AppendableException e){
			e.appendMessage(" in " + l);
			throw e;
		}
		
		
		return true;
	}

	/**
	 * Validates the list of implementations for extended lambda
	 * @param l list of implementations
	 * @return true if list validates, otherwise throws AppendableException
	 * @throws AppendableException
	 */
	public static boolean validateImplementations(List<SemanticNode> l) throws AppendableException{
		for(SemanticNode n : l) {
			if(n.type != SemanticNode.NodeType.LIST) {
				throw new UnexpectedExpressionException(n);
			}
			Validations.validateImplementation(n.asList());
		}
		return true;
	}

	/**
	 * Validates a single implementation for extended lambda
	 * @param l list containing the implementation
	 * @return true if implementation validates, otherwise throws excetpion
	 * @throws AppendableException
	 */
	public static boolean validateImplementation(List<SemanticNode> l) throws AppendableException{
		if(l.size() != 2) {
			throw new AppendableException("Badly formed implementation " + l);
		}
		
		SemanticNode typeList = l.get(0);
		
		if(typeList.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(typeList);
		}
		return true;
	}

	/**
	 * Validates if semantic node is valid variable type pair
	 * @param pair validated semantic node
	 * @return true if node is valid variable type pair, throws exception otherwise
	 * @throws AppendableException
	 */
	public static boolean validateVariableTypePair(SemanticNode pair) throws AppendableException{
		if(SemanticParserStatic.isSimpleSymbol(pair)) {
			return true;
		}
		
		if(pair.type != SemanticNode.NodeType.LIST
				|| pair.asList().size() != 2) {
			throw new UnexpectedExpressionException(pair);
		}
		List<SemanticNode> l = pair.asList();
		
		SemanticNode type = l.get(0);
		if(!Validations.isTypeIdentifierNode(type)) {
			throw new UnexpectedExpressionException(type);
		}
		
		SemanticNode variable = l.get(1);
		if(variable.type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(variable);
		}
			
		return true;
	}
	
	/**
	 * Returns true if given node can be type identifier, returns false otherwise
	 * @param n inspected node
	 * @return true or false
	 * @throws AppendableException if n cannot be type identifier
	 */
	private static boolean isTypeIdentifierNode(SemanticNode n) {
		return n.type == SemanticNode.NodeType.SYMBOL
				|| n.type == SemanticNode.NodeType.PAIR;
	}

	/**
	 * Validates if l is a valid defconversion list, throws exception if validation fails
	 * @param l validated list
	 * @return true if l is valid defconversion list, false otherwise
	 */
	public static boolean validateDefconversionList(List<SemanticNode> l) throws AppendableException{
		if(l.size() != 4) {
			throw new InvalidNumberOfArgsException(3, l.size() - 1);
		}
		
		SemanticNode defConversion = l.get(0);
		if(defConversion.type != SemanticNode.NodeType.SYMBOL
				|| !defConversion.asSymbol().equals(SemanticParserStatic.DEFCONVERSION)) {
			throw new UnexpectedExpressionException(defConversion);
		}
		
		if(!Validations.isTypeIdentifierNode(l.get(1))) {
			throw new UnexpectedExpressionException(l.get(1));
		}
		
		if(!Validations.isTypeIdentifierNode(l.get(2))) {
			throw new UnexpectedExpressionException(l.get(2));
		}
		
		SemanticNode c = l.get(3);
		if(c.type != SemanticNode.NodeType.LIST) {
			throw new UnexpectedExpressionException(c);
		}
		
		return true;
	}
	
	/**
	 * Validates if l i a valid defconstructor list, throws exception if validation fails
	 * @param l validated list
	 * @return true if l is valid defconversion list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateDefConstructorList(List<SemanticNode> l) throws AppendableException {
		if(l.size() != 3){
			throw new InvalidNumberOfArgsException(2, l.size()-1);
		}
		
		SemanticNode defConstructor = l.get(0);
		if(defConstructor.type != SemanticNode.NodeType.SYMBOL
				|| !defConstructor.asSymbol().equals(SemanticParserStatic.DEFCONSTRUCTOR)){
			throw new UnexpectedExpressionException(defConstructor);			
		}
		
		if(!Validations.isTypeIdentifierNode(l.get(1))){
			throw new UnexpectedExpressionException(l.get(1));
		}
		
		SemanticNode c = l.get(2);
		if(c.type != SemanticNode.NodeType.LIST){
			throw new UnexpectedExpressionException(c);
		}
		
		return true;
	}
	
	/**
	 * Validates if l is a valid define list, throws exception if valiation fails
	 * @param l validated list
	 * @return true if l is valid define list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateDefineList(List<SemanticNode> l) throws AppendableException{
		if(l.size() != 3) {
			throw new InvalidNumberOfArgsException(2, l.size()-1);
		}
		
		SemanticNode define = l.get(0);
		if(define.type != SemanticNode.NodeType.SYMBOL
				|| !define.asSymbol().equals(SemanticParserStatic.DEFINE)) {
			throw new UnexpectedExpressionException(define);
		}
		
		if(l.get(1).type != SemanticNode.NodeType.SYMBOL) {
			throw new UnexpectedExpressionException(l.get(1));
		}
		
		return true;
	}
	
	/**
	 * Validates if l is a valid cons list, throws exception if validation fails
	 * @param l validated list
	 * @return true if l is valid define list, false otherwise
	 * @throws AppendableException
	 */
	public static boolean validateConsList(List<SemanticNode> l) throws AppendableException{
		if(l.size() != 3){
			throw new InvalidNumberOfArgsException(2, l.size()-1);
		}
		
		SemanticNode cons = l.get(0);
		if(cons.type != SemanticNode.NodeType.SYMBOL
				|| !cons.asSymbol().equals(SemanticParserStatic.CONS)) {
			throw new UnexpectedExpressionException(cons);
		}
		
		return true;
	}
}
