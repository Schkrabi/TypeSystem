package main;

import interpretation.Environment;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.ClojureCodeGenerator;
import expression.Addition;
import expression.Expression;
import expression.IntBinary;
import expression.IntRoman;
import expression.IntString;
import expression.Subtraction;
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
		
		return env;
	}
	
	private static void initTypesConversions() throws Exception{
		TypeConcrete.TypeInt.addConversion(TypeRepresentation.TypeIntRoman, IntBinary.ToIntRomanWrapper.class);
		TypeConcrete.TypeInt.addConversion(TypeRepresentation.TypeIntString, IntBinary.ToIntStringWrapper.class);
		TypeRepresentation.TypeIntRoman.addConversion(TypeConcrete.TypeInt, IntRoman.ToIntBinaryWrapper.class);
		TypeRepresentation.TypeIntRoman.addConversion(TypeRepresentation.TypeIntString, IntRoman.ToIntStringWrapper.class);
		TypeRepresentation.TypeIntString.addConversion(TypeConcrete.TypeInt, IntString.ToIntBinaryWrapper.class);
		TypeRepresentation.TypeIntString.addConversion(TypeRepresentation.TypeIntRoman, IntString.ToIntRomanWrapper.class);
	}
	
	private static void init() throws Exception{
		Main.initTypesConversions();
	}
	
	private static void interpretLoop(){
		Scanner input = new Scanner(System.in);
		Environment topLevel = Main.initTopLevelEnvironment();
		
		try {			
			while (true) {
				System.out.print(">");
				CharStream charStream = new ANTLRInputStream(input.nextLine());
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);

				ExprsContext exprsContext = parser.exprs();

				for (Expression e : exprsContext.val) {
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
		try{
			input = Files.newBufferedReader(inputPath);
			
			CharStream charStream = new ANTLRInputStream(input);
			TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
			SchemeParser parser = new SchemeParser(tokens);
			ExprsContext exprsContext = parser.exprs();
			
			List<Expression> l = new LinkedList<Expression>();
			
			for(Expression e : exprsContext.val){
				Expression expr = e.substituteTopLevelVariables(new Environment());
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
