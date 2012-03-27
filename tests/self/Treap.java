import java.util.ArrayDeque;
import java.util.Random;
import java.util.Iterator;

public class Treap<T extends Comparable<T>> implements Iterable<T> {
    static class Itr<T extends Comparable<T>> implements Iterator<T> {
        ArrayDeque<Treap<T>> stack = new ArrayDeque<Treap<T>>();

        Itr(Treap<T> root) {
            goLeft(root);
        }

        private void goLeft(Treap<T> node) {
            assert node != null;
            while (node.data != null) {
                stack.addFirst(node);
                node = node.left;
            }
        }

        @Override
            public boolean hasNext() {
            return !stack.isEmpty();
        }

        @Override
            public T next() {
            Treap<T> node = stack.peekFirst();
            T result = node.data;
            if (node.right.data != null) {
                goLeft(node.right);
            } else {
                stack.removeFirst();
                while (!stack.isEmpty() && stack.peekFirst().right == node) {
                    node = stack.removeFirst();
                }
            }
            return result;
        }

        @Override
            public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    static final private Random random = new Random(123);

    final int priority;
    final T data;
    final Treap<T> left;
    final Treap<T> right;
    final int hash;

    private Treap() {
        hash = 0;
        priority = 0;
        data = null;
        left = right = null;
        check();
    }

    private Treap(int priority, T data, Treap<T> left, Treap<T> right) {
        this.priority = priority;
        this.data = data;
        this.left = left;
        this.right = right;
        this.hash = left.hash + right.hash + data.hashCode();
        // Invariant {check()} may be broken at this point!
    }

    static <T extends Comparable<T>> Treap<T> empty() {
        return new Treap<T>();
    }

    boolean check() {
        assert check(null, null, Integer.MAX_VALUE);
        return true;
    }

    boolean check(T minimum, T maximum, int high) {
        assert high > 0;
        if (data == null) {
            assert left == null;
            assert right == null;
            assert priority == 0;
        } else {
            assert left != null;
            assert right != null;
            assert priority > 0;
            assert priority <= high;
            if (minimum != null) {
                assert minimum.compareTo(data) <= 0;
            }
            if (maximum != null) {
                assert data.compareTo(maximum) <= 0;
            }
            assert left.check(minimum, data, priority);
            assert right.check(data, maximum, priority);
        }
        return true;
    }

    private Treap<T> rotateLeft() {
        assert data != null;
        return new Treap<T>(
                            right.priority, right.data,
                            new Treap<T>(priority, data, left, right.left),
                            right.right);
    }

    private Treap<T> rotateRight() {
        assert data != null;
        return new Treap<T>(
                            left.priority, left.data,
                            left.left,
                            new Treap<T>(priority, data, left.right, right));
    }

    private Treap<T> balance() {
        assert data != null;
        assert left.priority <= priority || right.priority <= priority;
        Treap<T> result = this;
        if (left.priority > priority) {
            result = result.rotateRight();
        } else if (right.priority > priority) {
            result = result.rotateLeft();
        }
        assert result.check();
        return result;
    }

    private Treap<T> insert(int newPriority, T newData) {
        assert newData != null;
        assert newPriority > 0;
        if (data == null) {
            return new Treap<T>(newPriority, newData, this, this);
        } else {
            int c = newData.compareTo(data);
            if (c < 0) {
                return new Treap<T>(priority, data,
                                    left.insert(newPriority, newData),
                                    right)
                    .balance();
            } else if (c > 0) {
                return new Treap<T>(priority, data,
                                    left,
                                    right.insert(newPriority, newData))
                    .balance();
            } else {
                return this;
            }
        }
    }

    Treap<T> insert(T data) {
        if (logTreap) {
            System.out.println("Inserting " + data);
        }
        return insert(random.nextInt(), data);
    }

    static boolean priorityLess(int p, int q) {
        return p < q || (p == q && random.nextBoolean());
    }

    Treap<T> remove(T oldData) {
        if (logTreap) {
            System.out.println("Removing " + oldData);
        }
        Treap<T> result = this;
        if (data != null) {
            int c = oldData.compareTo(data);
            if (c < 0) {
                result = new Treap<T>(priority, data,
                                      left.remove(oldData),
                                      right);
            } else if (c > 0) {
                result = new Treap<T>(priority, data,
                                      left,
                                      right.remove(oldData));
            } else {
                if (left.data == null && right.data == null) {
                    return left;
                } else if (left.data == null) {
                    return right.remove(oldData);
                } else if (right.data == null) {
                    return left.remove(oldData);
                } else if (priorityLess(left.priority, right.priority)) {
                    result = rotateLeft();
                    result = new Treap<T>(result.priority, result.data,
                                          left.remove(oldData),
                                          right);
                } else {
                    result = rotateRight();
                    result = new Treap<T>(result.priority, result.data,
                                          left,
                                          right.remove(oldData));
                }
            }
        }
        assert result.check();
        return result;
    }

    public T get(T x) {
        assert x != null;
        if (data == null) {
            return null;
        } else {
            int c = x.compareTo(data);
            if (c < 0) {
                return left.get(x);
            } else if (c > 0) {
                return right.get(x);
            } else {
                return data;
            }
        }
    }

    // Used mostly for debugging.
    public int size() {
        int s = data == null? 0 : 1;
        if (left != null) s += left.size();
        if (right != null) s += right.size();
        return s;
    }

    @Override
        public int hashCode() {
        return hash;
    }

    @Override
        public boolean equals(Object other) {
        Treap otherTreap = (Treap) other; // yes, cast exception wanted
        return this == other ||
            (hash == otherTreap.hash &&
             equalIterators(iterator(), otherTreap.iterator()));
    }

    @Override
        public Iterator<T> iterator() {
        return new Itr<T>(this);
    }

    @Override
        public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        for (T t : this) {
            sb.append(" " + t);
        }
        sb.append(" ]");
        return sb.toString();
    }

    static <T> boolean equalIterators(Iterator<T> i, Iterator j) {
        while (i.hasNext() && j.hasNext()) {
            if (!i.next().equals(j.next())) { // yes, NullExc wanted
                return false;
            }
        }
        return i.hasNext() == j.hasNext();
    }

    private static boolean logTreap = false;
}
