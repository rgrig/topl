package topl;

import java.util.List;
import java.util.ArrayList;

public class QuantifierElimination {
	
	
	/**
	 * Quantifier Elimination procedure, eliminates the variable with index eliminate and produces a 
	 * list of valid disjoint sets based on the input set list
	 * 
	 * @param eliminate - The Variable to eliminate
	 * @param variables - Current valid variables
	 * @param sets - A list of input sets
	 * @return A list of valid sets after elimination
	 */
	public static List<DisjointSet> QuantifierElimnation (int eliminate, int[] variables, List<DisjointSet> sets) {
		ArrayList<DisjointSet> newSets = new ArrayList<DisjointSet>();
		for(DisjointSet set : sets) {
			for(int i = 0; i < variables.length; i++) {
				/* So we check that the current value we are choosing as a replacement is not the 
				 * same and is the minimum index to avoid duplicate sets */
				if(i != eliminate && set.equalities.get(i) == i) { 
					DisjointSet newSet = set.cloneAndReplace(eliminate, i);
					if(newSet.isValid()) {				
						boolean unique = true;
						for(DisjointSet testSet : newSets) {
							if(testSet.isEqual(newSet)) {
								unique = false;
								break;
							}
						}
						if(unique) newSets.add(newSet);
					}
				}
			}
		}
		return newSets;
	}
}