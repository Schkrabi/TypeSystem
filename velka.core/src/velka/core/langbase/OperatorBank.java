package velka.core.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;

import velka.core.abstraction.Operator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.util.DeclarableInTypeEnvironment;
import velka.core.util.OperatorBankUtil;
import velka.util.AppendableException;
import velka.util.ClojureHelper;

/**
 * Class to implement oprator bank for automatic environment variable and clojure code generation
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class OperatorBank {

	/**
	 * Gets namespace name of this operator bank
	 * @return
	 */
	public String getNamespace() {
		return velka.core.util.Constants.PACKAGE + "." + this.name();
	}
	
	/**
	 * Gets relative directory where clojure code file will be located 
	 * @return
	 */
	public Path path() {
		return velka.core.util.Constants.LOCATION;
	}
	
	/**
	 * Gets file name of the clojure code file
	 * @return
	 */
	protected abstract String name();
	
	public Path fileNameJava() {
		return Paths.get(this.name() + ".java");
	}
	
	public Path fileNameClojure() {
		return Paths.get(this.name() + ".clj");
	}
	
	/**
	 * Initializes this operator bank in type environment
	 * @param env environment accompanying type environment on initialization
	 * @param typeEnv where initialized
	 * @throws AppendableException
	 */
	public void initInEnvironment(Environment env) throws AppendableException {
		try {
			// Constructors
			for(Operator o : OperatorBankUtil.getConstructors(this.getClass())) {
				if(!(o instanceof DeclarableInTypeEnvironment)) {
					throw new AppendableException("Cannot auto initialize constructor " + o.toString() + " from operator bank " + this.getClass().getName() + " since it does not implement " + DeclarableInTypeEnvironment.class.getName() + " interface");
				}
				DeclarableInTypeEnvironment c = (DeclarableInTypeEnvironment)o;
				c.declareInTypeEnvironment(env);
			}
			
			// Conversions
			for(Operator o : OperatorBankUtil.getConversions(this.getClass())) {
				if(!(o instanceof DeclarableInTypeEnvironment)) {
					throw new AppendableException("Cannot auto initialize conversion " + o.toString() + " from operator bank " + this.getClass().getName() + " since it does not implement " + DeclarableInTypeEnvironment.class.getName() + " interface");
				}
				DeclarableInTypeEnvironment c = (DeclarableInTypeEnvironment)o;
				c.declareInTypeEnvironment(env);
			}
			
			// Operators
			for(Operator o : OperatorBankUtil.getOperators(this.getClass())) {
				env.put(o.getInternalSymbol(), o);
			}
			
		} catch (IllegalArgumentException e) {
			throw new AppendableException(e.toString());
		} catch (IllegalAccessException e) {
			throw new AppendableException(e.toString());
		} 	
	}
	
	/**
	 * Gets relative path of clojure code file
	 * @return
	 */
	public Path clojureFilePath() {
		return this.path().resolve(this.fileNameClojure());
	}
	
	public Path javaFilePath() {
		return this.path().resolve(this.fileNameJava());
	}
	
	/**
	 * Generates the clojure code file to given destination
	 * @param dest destination
	 * @return Path to the file
	 * @throws IOException if write is not successful
	 */
	public Path generateClojureFile(Path dest) throws IOException {
		Path finalPath = dest.resolve(this.clojureFilePath());
		return Files.writeString(finalPath, this.clojureDefinitions(this.getClass(), this.getNamespace()));
	}
	
	/**
	 * Writes definitions of operator bank file
	 * @return
	 */
	protected String clojureDefinitions(Class<?> clazz, String Namespace) {
		StringBuilder sb = new StringBuilder();
		
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		sb.append(ClojureHelper.declareNamespace(Namespace));
		
		try {
			Environment env = TopLevelEnvironment.instantiate();
			
			var constructors = OperatorBankUtil.getConstructors(clazz);
			for(var constructor : constructors) {
				sb.append(constructor.toClojureCode(env));
			}
			
			var conversions = OperatorBankUtil.getConversions(clazz);
			for(var conversion : conversions) {
				sb.append(OperatorBankUtil.conversionDefinition(conversion, env));
			}
			
			var operators = OperatorBankUtil.getOperators(clazz);
		
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDeclaration(operator));
			}
			
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDef(operator, env));
			}
			
		} catch (Exception e) {
			System.err.println("Error generating file for " + clazz.getName() + " :" + e.getMessage());
			return "";
		}
		
		return sb.toString();
	}

	public static Collection<OperatorBank> operatorBanks = Arrays.asList(
			ConstructorOperators.singleton(),
			ConversionOperators.singleton(),
			Operators.singleton(),
			ListNative.singleton(),
			JavaLinkedList.singleton(),
			JavaBitSet.singleton(),
			Scanner.singleton(),
			TreeMap.singleton(),
			JavaListIterator.singleton(),
			TreeSet.instance(),
			HashSet.instance()
			);
}
