package velka.lang.compiler;

import velka.lang.interpretation.Environment;

import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import velka.lang.interpretation.TypeEnvironment;
import velka.lang.interpretation.Compiler;

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
		try {
			Environment topLevel = Environment.initTopLevelEnvitonment();
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(topLevel);

			Compiler.init(topLevel, typeEnv);
			// Very basic options, didn't wanted to add more external libraries etc...
			if (args.length == 0) {
				// Interactive parser mode
				Compiler.repl(System.in, System.out, topLevel, typeEnv, true);
			} else if (args.length == 1) {
				// Load code and interpret
				InputStream inStream = null;
				try {
					inStream = Files.newInputStream(Paths.get(args[0]));
					Compiler.interpret(inStream, topLevel, typeEnv);
				}catch(Exception e) {
					e.printStackTrace();
				}finally {
					if(inStream != null) {
						inStream.close();
					}
				}
			} else if (args.length == 2) {
				// Compiler mode
				InputStream inStream = null;
				PrintStream outStream = null;
				
				try {
					inStream = Files.newInputStream(Paths.get(args[0]));
					outStream = new PrintStream(Files.newOutputStream(Paths.get(args[1])));
					Compiler.clojure(inStream, outStream, topLevel, typeEnv);
				} catch(Exception e) {
					e.printStackTrace();
				}finally {
					if(inStream != null) {
						inStream.close();
					}
					if(outStream != null) {
						outStream.close();
					}
				}
			} else {
				System.out.println("Wrong number of arguments specified.");
				return;
			}
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}
	}
}
