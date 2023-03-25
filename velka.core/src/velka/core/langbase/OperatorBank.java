package velka.core.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import velka.core.abstraction.Operator;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
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
	public abstract String getNamespace();
	/**
	 * Gets relative directory where clojure code file will be located 
	 * @return
	 */
	public abstract Path getPath();
	/**
	 * Gets file name of the clojure code file
	 * @return
	 */
	public abstract Path getFileName();	
	
	/**
	 * Initializes types of this operator bank
	 */
	public abstract void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException ;
	
	/**
	 * Initializes this operator bank in type environment
	 * @param env environment accompanying type environment on initialization
	 * @param typeEnv where initialized
	 * @throws AppendableException
	 */
	public void initInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		this.initTypes(typeEnv);
		
		try {
			for(Operator o : OperatorBankUtil.getConstructors(this.getClass())) {
				if(!(o instanceof DeclarableInTypeEnvironment)) {
					throw new AppendableException("Cannot auto initialize constructor " + o.toString() + " from operator bank " + this.getClass().getName() + " since it does not implement " + DeclarableInTypeEnvironment.class.getName() + " interface");
				}
				DeclarableInTypeEnvironment c = (DeclarableInTypeEnvironment)o;
				c.declareInTypeEnvironment(env, typeEnv);
			}
			
			for(Operator o : OperatorBankUtil.getConversions(this.getClass())) {
				if(!(o instanceof DeclarableInTypeEnvironment)) {
					throw new AppendableException("Cannot auto initialize conversion " + o.toString() + " from operator bank " + this.getClass().getName() + " since it does not implement " + DeclarableInTypeEnvironment.class.getName() + " interface");
				}
				DeclarableInTypeEnvironment c = (DeclarableInTypeEnvironment)o;
				c.declareInTypeEnvironment(env, typeEnv);
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
	public Path getRelative() {
		return this.getPath().resolve(this.getFileName());
	}
	
	/**
	 * Generates the clojure code file to given destination
	 * @param dest destination
	 * @return Path to the file
	 * @throws IOException if write is not successful
	 */
	public Path generateFile(Path dest) throws IOException {
		Path finalPath = dest.resolve(this.getRelative());
		return Files.writeString(finalPath, this.writeDefinitions(this.getClass(), this.getNamespace()));
	}
	
	/**
	 * Writes definitions of operator bank file
	 * @return
	 */
	protected String writeDefinitions(Class<?> clazz, String Namespace) {
		StringBuilder sb = new StringBuilder();
		
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		sb.append(ClojureHelper.declareNamespace(Namespace));
		
		try {
			List<Operator> operators = OperatorBankUtil.getOperators(clazz);
		
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDeclaration(operator));
			}
			
			Environment env = Environment.initTopLevelEnvironment();
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			for(Operator operator : operators) {
				sb.append(Operator.makeOperatorDef(operator, env, typeEnv));
			}
			
			List<Operator> conversions = OperatorBankUtil.getConversions(clazz);
			for(Operator conversion : conversions) {
				sb.append(OperatorBankUtil.conversionDefinition(conversion, env, typeEnv));
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
			JavaArrayList.singleton(),
			JavaLinkedList.singleton(),
			JavaBitSet.singleton(),
			Scanner.singleton(),
			TreeMap.singleton()
			);
}
