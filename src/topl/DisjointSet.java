package topl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import topl.Checker.Binding;


/**
 * Class representing a Dis-Joint Set, this contains an array of variable indices 
 * where the value stored is the smallest index of the equal set e.g.
 * {0,0,0,3,3} would represent x0 == x1 == x2 AND x3 == x4
 * It also contains a hashset of disequalities to complete the representation
 * 
 * @author James Brookhouse
 *
 */
public class DisjointSet implements Cloneable {
	
	public static DisjointSet emptySet() {
		return new DisjointSet(new ArrayList<Binding>(), new ArrayList<Integer>(), new HashSet<DisEquality>());
	}
	
	public ArrayList<Integer> equalities;
	public ArrayList<Binding> variables;
	public HashSet<DisEquality> disequalities;
	
	public DisjointSet(ArrayList<Binding> variables, ArrayList<Integer> equalities, HashSet<DisEquality> disequalities) {
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
	
	public boolean isEqual(DisjointSet set) {
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
	public DisjointSet cloneAndReplace(int eliminate, int replace) {
		DisjointSet set = (DisjointSet) this.clone();
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
		return new DisjointSet(newVariables, newEqualities, newList);
	}
}
