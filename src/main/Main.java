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

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import parser.SchemeLexer;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;
import parser.SemanticNode;
import semantic.SemanticParser;
import util.AppendableException;
import util.ClojureCodeGenerator;
import expression.Expression;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
@SuppressWarnings("deprecation")
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

	public static Environment initTopLevelEnvironment() throws AppendableException {
		Environment.initTopLevelEnvitonment();
		return Environment.topLevelEnvironment;
	}
	
	private static void initTypesConversions() throws Exception{
		
	}
	
	public static void init() throws Exception{
		Main.initTypesConversions();
	}
	
	private static void interpretLoop() throws Exception{
		Scanner input = new Scanner(System.in);
		Environment topLevel = Main.initTopLevelEnvironment();
		
		try {			
			while (true) {
				System.out.print(">");
				CharStream charStream = new ANTLRInputStream(input.nextLine());
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);

				ExprsContext exprsContext = parser.exprs();
				List<Expression> exprs = new ArrayList<Expression>();
				
				for(SemanticNode s : exprsContext.val){
					exprs.add(SemanticParser.parseNode(s));
				}

				for (Expression e : exprs) {
					e.infer(topLevel);
					System.out.println(e.interpret(topLevel));
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
			
			for(SemanticNode s : exprsContext.val){
				exprs.add(SemanticParser.parseNode(s));
			}
			
			for(Expression e : exprs){
				e.infer(topLevel);
				l.add(e);
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
			
			for(SemanticNode s : exprsContext.val){
				exprs.add(SemanticParser.parseNode(s));
			}
			
			for(Expression e : exprs){
				e.infer(topLevel);
				System.out.println(e.interpret(topLevel));
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
					exprs.add(SemanticParser.parseNode(s));
				}

				for (Expression e : exprs) {
					e.infer(topLevel);
					Expression interpreted = e.interpret(topLevel); 
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
