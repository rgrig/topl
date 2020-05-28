package topl;

import java.util.List;
import java.util.Iterator;

public class QuantifierElimination {
	
	
	public static Node QuantifierElimnation (int eliminate, int[] variables, List<DisJointSet> sets) {
		for(DisJointSet set : sets) {
			
			
		}
		
	}
	
	
	public class DisJointSet implements Clonable {
		
		public int[] equalities;
		public ArrayList<Int> disequalities;
		
		public DisJointSet(int[] equalities, ArrayList<Int> disequalities) {
			this.equalities =  equalities;
			this.disequalities = disequalities;
		}
		
		public boolean isEqual(DisJointSet set) {
			if(this.equalities.length != set.equalities.length || this.disequalties.size() != set.disequalties.size()) return false;
			for(int i = 0; i < equalities.length; i++) {
				if(equalities[i] != set.equalities[i]) return false;
			}
			Iterator i1 = disequalities.get
		}
		
		public Object Clone() {
			return new DisJointSet(equalities.clone(), disequalities.clone());
		}
		
	}
}
