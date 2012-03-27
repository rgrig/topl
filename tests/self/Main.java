public class Main {
    public static void main(String[] args) {
	Treap<String> t0 = Treap.empty();
	Treap<String> t1 = t0.insert("Radu");
	Treap<String> t2 = t1.insert("Dino");
	Treap<String> t3 = t2.insert("Grigore");
	Treap<String> t4 = t3.insert("Distefano");
	System.out.println(t4);
	Treap<String> t5 = t3.insert("Dijkstra");
	System.out.println(t5);
    }
}
