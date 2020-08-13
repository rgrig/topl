package topl;

import java.util.List;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import topl.Checker.Binding;

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
	public static List<DisJointSet> QuantifierElimnation (int eliminate, int[] variables, List<DisJointSet> sets) {
		ArrayList<DisJointSet> newSets = new ArrayList<DisJointSet>();
		for(DisJointSet set : sets) {
			for(int i = 0; i < variables.length; i++) {
				/* So we check that the current value we are choosing as a replacement is not the 
				 * same and is the minimum index to avoid duplicate sets */
				if(i != eliminate && set.equalities.get(i) == i) { 
					DisJointSet newSet = set.cloneAndReplace(eliminate, i);
					if(newSet.isValid()) {				
						boolean unique = true;
						for(DisJointSet testSet : newSets) {
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
	
	/**
	 * Class representing a Dis-Joint Set, this contains an array of variable indices 
	 * where the value stored is the smallest index of the equal set e.g.
	 * {0,0,0,3,3} would represent x0 == x1 == x2 AND x3 == x4
	 * It also contains a hashset of disequalities to complete the representation
	 * 
	 * @author James Brookhouse
	 *
	 */
	public static class DisJointSet implements Cloneable {
		
		public static DisJointSet emptySet() {
			return new DisJointSet(new ArrayList<Binding>(), new ArrayList<Integer>(), new HashSet<DisEquality>());
		}
		
		public ArrayList<Integer> equalities;
		public ArrayList<Binding> variables;
		public HashSet<DisEquality> disequalities;
		
		public DisJointSet(ArrayList<Binding> variables, ArrayList<Integer> equalities, HashSet<DisEquality> disequalities) {
			this.variables = variables;
			this.equalities =  equalities;
			this.disequalities = disequalities;
		}
		
		public void insert(Binding b) {
			for(int i = 0; i < variables.size(); i++) {
				// If true then we have an update to an existing binding so we should reset the equalities value
				if(b.compareTo(variables.get(i)) == 0) {
					variables.set(i, b);
					equalities.set(i, i);
					return;
				}
			}
			// We have a new binding
			variables.add(b);
			equalities.add(b.variable);
		}
		
		public int size() {
			return variables.size();
		}
		
		public Binding get(Binding binding) {
			// We don't keep binding values up to date if they are modified via elimination or the addition 
			// of constraints so we cannot just grab the value in variables but check the equalities.
			Binding shadowBinding = new Binding(equalities.get(binding.variable)); 
			for(Binding b : variables) {
				if(shadowBinding.compareTo(b) == 0) {
					return new Binding(binding.variable, shadowBinding.value);
				}
			}
			return null;
		}
		
		public boolean isEqual(DisJointSet set) {
			if(this.equalities.size() != set.equalities.size() || this.disequalities.size() != set.disequalities.size()) return false;
			for(int i = 0; i < equalities.size(); i++) {
				if(this.equalities.get(i) != set.equalities.get(i)) return false;
			}
			Iterator<DisEquality> i1 = this.disequalities.iterator();
			Iterator<DisEquality> i2 = set.disequalities.iterator();
			while(i1.hasNext()) {
				DisEquality dis1 = i1.next();
				DisEquality dis2 = i2.next();
				if(dis1.a != dis2.a || dis1.b != dis2.b) return false;
			}
			return true;
		}
		
		/**
		 * Checks if a DisjointSet is valid by checking the dis-equalities and equalities
		 * @return if the DisJointSet is valid
		 */
		public Boolean isValid() {
			for(DisEquality dis : disequalities) {
				if( equalities.get(dis.a) == equalities.get(dis.b)) return false;
			}
			return true;
		}
		
		/**
		 * Clones the DisJointSet and replaces the variable we wish to eliminate with a 
		 * replacement
		 * @param eliminate
		 * @param replace
		 * @return
		 */
		public DisJointSet cloneAndReplace(int eliminate, int replace) {
			DisJointSet set = (DisJointSet) this.clone();
			set.replace(eliminate, replace);
			return set;
		}
		
		/**
		 * Replace a variable we want to eliminate
		 * @param eliminate
		 * @param replace
		 */
		public void replace(int eliminate, int replace) {
			while(equalities.get(replace) != replace) {
				replace = equalities.get(replace);
			}
			equalities.set(eliminate, replace);
			for(DisEquality dis : disequalities) {
				if(dis.a == eliminate) dis.a = replace;
				if(dis.b == eliminate) dis.a = replace;
			}
		}
		
		@Override
		public Object clone() {
			HashSet<DisEquality> newList = new HashSet<DisEquality>();
			for(DisEquality dis : disequalities) {
				newList.add((DisEquality)dis.clone());
			}
			ArrayList<Integer> newEqualities = new ArrayList<Integer>();
			for(Integer i : equalities) {
				newEqualities.add(i);
			}
			ArrayList<Binding> newVariables = new ArrayList<Binding>();
			for(Binding b : variables) {
				newVariables.add(new Binding(b.variable,b.value));
			}
			return new DisJointSet(newVariables, newEqualities, newList);
		}
	}
	
	/**
	 * Class to encapsulate a dis-equality, the order of the variables a and b 
	 * should not matter for equality and hashing purposes.
	 * 
	 * @author James Brookhouse
	 *
	 */
	public class DisEquality implements Cloneable, Comparable<DisEquality>{
		public int a;
		public int b;
		
		public DisEquality(int a, int b) {
			this.a = a;
			this.b = b;
		}
		
		@Override
		public Object clone() {
			return new DisEquality(a,b);
		}
		
		@Override
		public int hashCode() {
			Integer A = a;
			Integer B = b;
			return A.hashCode()^B.hashCode();
		}
		
		@Override
		public int compareTo(DisEquality dis) {
			if(dis.a == a && dis.b == b) return 0;
			if(dis.a == b && dis.b == a) return 0;
			return -1;
		}
	}
}