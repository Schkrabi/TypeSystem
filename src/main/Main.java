package main;

import interpretation.Environment;

import java.io.IOException;
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

import operators.Addition;
import operators.Car;
import operators.Cdr;
import operators.Concantenation;
import operators.Subtraction;
import operators.Multiplication;
import operators.Division;
import operators.And;
import operators.Or;
import operators.Not;
import operators.BitAnd;
import operators.BitOr;

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

	private static Environment initTopLevelEnvironment() {
		Environment env = new Environment();
		env.put(new Variable("+"), Addition.singleton);
		env.put(new Variable("-"), Subtraction.singleton);
		env.put(new Variable("*"), Multiplication.singleton);
		env.put(new Variable("/"), Division.singleton);
		env.put(new Variable("and"), And.singleton);
		env.put(new Variable("or"), Or.singleton);
		env.put(new Variable("not"), Not.singleton);
		env.put(new Variable("bit-and"), BitAnd.singleton);
		env.put(new Variable("bit-or"), BitOr.singleton);
		env.put(new Variable("concat"), Concantenation.singleton);
		env.put(new Variable("car"), Car.singleton);
		env.put(new Variable("cdr"), Cdr.singleton);
		env.put(new Variable("nil"), new Tuple(new Expression[0]));
		
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
	
	private static void interpretLoop(){
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
					expr.infer();
					System.out.println(expr.interpret(topLevel));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}
	
	private static void compile(Path inputPath, Path outputPath) throws IOException{
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
				expr.infer();
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
}
