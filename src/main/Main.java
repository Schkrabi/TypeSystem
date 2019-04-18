package main;

import interpretation.Environment;

import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import operators.Operator;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import conversions.IntToIntRomanWrapper;
import conversions.IntToIntStringWrapper;
import conversions.IntRomanToIntWrapper;
import conversions.IntRomanToIntStringWrapper;
import conversions.IntStringToIntWrapper;
import conversions.IntStringToIntRomanWrapper;

import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import parser.SemanticNode;
import semantic.SemanticParser;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.ClojureCodeGenerator;
import expression.Expression;
import expression.Tuple;
import expression.TypeConstructionLambda;
import expression.Variable;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Main {

	/**
	 * Main entrypoint
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try{
		Main.init();
		//Very basic options, didn't wanted to add more external libraries etc...
		if(args.length == 0){
			//Interactive parser mode
			Main.interpretLoop();
		}
		else if(args.length == 1) {
			//Load code and interpret further
			Main.load(Paths.get(args[0]));
		}
		else if(args.length == 2){
			//Compiler mode
			Main.compile(Paths.get(args[0]), Paths.get(args[1]));
		}
		else{
			System.out.println("Wrong number of arguments specified.");
			return;
		}
		}catch(Exception e){
			e.printStackTrace();
			return;
		}
	}

	private static Environment initTopLevelEnvironment() throws Exception {
		Environment env = new Environment();
		env.put(new Variable(Operator.Addition.toString()), Operator.Addition);
		env.put(new Variable(Operator.Subtraction.toString()), Operator.Subtraction);
		env.put(new Variable(Operator.Multiplication.toString()), Operator.Multiplication);
		env.put(new Variable(Operator.Division.toString()), Operator.Division);
		env.put(new Variable(Operator.NumericEqual.toString()), Operator.NumericEqual);
		env.put(new Variable(Operator.LesserThan.toString()), Operator.LesserThan);
		env.put(new Variable(Operator.And.toString()), Operator.And);
		env.put(new Variable(Operator.Or.toString()), Operator.Or);
		env.put(new Variable(Operator.Not.toString()), Operator.Not);
		env.put(new Variable(Operator.BitAnd.toString()), Operator.BitAnd);
		env.put(new Variable(Operator.BitOr.toString()), Operator.BitOr);
		env.put(new Variable(Operator.Concantenation.toString()), Operator.Concantenation);
		env.put(new Variable(Operator.Car.toString()), Operator.Car);
		env.put(new Variable(Operator.Cdr.toString()), Operator.Cdr);
		env.put(new Variable("nil"), new Tuple(new Expression[0]));
		env.put(new Variable(Operator.Equals.toString()), Operator.Equals);
		//env.put(new Variable(Deconstruct.singleton.toString()), Deconstruct.singleton);
		
		env.put(new Variable("Int"), TypeConstructionLambda.IntPrimitiveConstructor);
		env.put(new Variable("Int:String"), TypeConstructionLambda.IntStringConstructor);
		env.put(new Variable("Int:Roman"), TypeConstructionLambda.IntRomanConstructor);
		env.put(new Variable("String"), TypeConstructionLambda.StringPrimitiveConstructor);
		env.put(new Variable("Double"), TypeConstructionLambda.DoublePrimitiveConstructor);
		env.put(new Variable("Bool"), TypeConstructionLambda.BoolPrimitiveConstructor);
		
		return env;
	}
	
	private static void initTypesConversions() throws Exception{
		TypeConcrete.TypeInt.addConversion(TypeRepresentation.TypeIntRoman, IntToIntRomanWrapper.IntToIntRoman);
		TypeConcrete.TypeInt.addConversion(TypeRepresentation.TypeIntString, IntToIntStringWrapper.IntToIntString);
		TypeRepresentation.TypeIntRoman.addConversion(TypeConcrete.TypeInt, IntRomanToIntWrapper.IntRomanToInt);
		TypeRepresentation.TypeIntRoman.addConversion(TypeRepresentation.TypeIntString, IntRomanToIntStringWrapper.IntRomanToIntString);
		TypeRepresentation.TypeIntString.addConversion(TypeConcrete.TypeInt, IntStringToIntWrapper.IntStringToInt);
		TypeRepresentation.TypeIntString.addConversion(TypeRepresentation.TypeIntRoman, IntStringToIntRomanWrapper.IntStringToIntRoman);
	}
	
	private static void init() throws Exception{
		Main.initTypesConversions();
	}
	
	private static void interpretLoop() throws Exception{
		Scanner input = new Scanner(System.in);
		Environment topLevel = Main.initTopLevelEnvironment();
		
		try {			
			SemanticParser semanticParser = new SemanticParser();
			while (true) {
				System.out.print(">");
				CharStream charStream = new ANTLRInputStream(input.nextLine());
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);

				ExprsContext exprsContext = parser.exprs();
				List<Expression> exprs = new ArrayList<Expression>();
				
				for(SemanticNode s : exprsContext.val){
					exprs.add(semanticParser.parseNode(s));
				}

				for (Expression e : exprs) {
					Expression expr = e.substituteTopLevelVariables(topLevel);
					expr.infer(topLevel);
					System.out.println(expr.interpret(topLevel));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}
	
	private static void compile(Path inputPath, Path outputPath) throws Exception{
		Reader input = null;
		Writer output = null;
		Environment topLevel = Main.initTopLevelEnvironment();
		
		try{
			input = Files.newBufferedReader(inputPath);
			
			CharStream charStream = new ANTLRInputStream(input);
			TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
			SchemeParser parser = new SchemeParser(tokens);
			ExprsContext exprsContext = parser.exprs();
			
			List<Expression> l = new LinkedList<Expression>();
			List<Expression> exprs = new ArrayList<Expression>();
			SemanticParser semanticParser = new SemanticParser();
			
			for(SemanticNode s : exprsContext.val){
				exprs.add(semanticParser.parseNode(s));
			}
			
			for(Expression e : exprs){
				Expression expr = e.substituteTopLevelVariables(topLevel);
				expr.infer(topLevel);
				l.add(expr);
			}
			
			output = Files.newBufferedWriter(outputPath, Charset.defaultCharset());
			ClojureCodeGenerator.toClojureCode(l, output);
		}catch(Exception e){
			e.printStackTrace();
		}finally{
			if(input != null){
				input.close();
			}
			if(output != null){
				output.close();
			}
		}
	}
	
	private static void load(Path inputPath) throws Exception {
		Reader input = null;
		Scanner inputI = null;
		Environment topLevel = Main.initTopLevelEnvironment();

		try{
			input = Files.newBufferedReader(inputPath);
			
			CharStream charStream = new ANTLRInputStream(input);
			TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
			SchemeParser parser = new SchemeParser(tokens);
			ExprsContext exprsContext = parser.exprs();
			
			List<Expression> exprs = new ArrayList<Expression>();
			SemanticParser semanticParser = new SemanticParser();
			
			for(SemanticNode s : exprsContext.val){
				exprs.add(semanticParser.parseNode(s));
			}
			
			for(Expression e : exprs){
				Expression expr = e.substituteTopLevelVariables(topLevel);
				expr.infer(topLevel);
				System.out.println(expr.interpret(topLevel));
			}
			
			inputI = new Scanner(System.in);
			while (true) {
				System.out.print(">");
				charStream = new ANTLRInputStream(inputI.nextLine());
				tokens = new CommonTokenStream(new SchemeLexer(charStream));
				parser = new SchemeParser(tokens);

				exprsContext = parser.exprs();
				exprs = new ArrayList<Expression>();
				
				for(SemanticNode s : exprsContext.val){
					exprs.add(semanticParser.parseNode(s));
				}

				for (Expression e : exprs) {
					Expression expr = e.substituteTopLevelVariables(topLevel);
					expr.infer(topLevel);
					Expression interpreted = expr.interpret(topLevel); 
					System.out.println(interpreted);
				}
			}
		}catch(Exception e){
			e.printStackTrace();
		}finally{
			if(input != null){
				input.close();
			}
			if(inputI != null){
				inputI.close();
			}
		}
	}
}
