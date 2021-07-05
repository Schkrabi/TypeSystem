package velka.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.types.Substitution;
import velka.util.AppendableException;

/**
 * Exception thrown when set of substitutions cannot be merged
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class SubstitutionCollectionCannotBeMerged extends AppendableException {

	/**
	 *  Serial version id
	 */
	private static final long serialVersionUID = 7804784677173214147L;
	
	/**
	 * Merged substitutions
	 */
	private final List<Substitution> merged;
	
	public SubstitutionCollectionCannotBeMerged(Collection<? extends Substitution> merged) {
		super("Cannot merge following substitution: " + merged.toString());
		this.merged = new ArrayList<Substitution>(merged);
	}
	
	/**
	 * Getter for substitution set
	 * @return copy of merged
	 */
	public List<Substitution> getMerged(){
		return new ArrayList<Substitution>(this.merged);
	}
}
