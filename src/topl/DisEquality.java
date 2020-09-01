package topl;

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