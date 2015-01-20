// header {{{
package topl;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
// }}}
public class Checker {
    /*  Implementation Notes {{{
        Some classes have a method {check()} that asserts if some object
        invariant is broken. These functions always return {true} so that
        you can say {assert x.check()} in case you want to skip them
        completely when assertions are not enabled.

     }}} */
    // Random (well, for some very weak eyes) {{{
    static class Random {
        int seed;

        Random(int seed) {
            assert seed != 0;
            this.seed = seed;
        }

        // post: ret > 0
        int nextInt() {
            seed *= 0x9e3779b9;
            return seed < 0? -seed : seed;
        }

        boolean nextBoolean() {
            seed *= 0x9e3779b9;
            return (seed & 1) != 0;
        }
    }
    // }}}
    // Queue<T> {{{
    static class Queue<T> implements Iterable<T> {
        final public T a, b; // inv: a != null || b == null
        private int hash;
        Queue(T a, T b) {
            assert a != null || b == null;
            this.a = a;
            this.b = b;
            this.hash = -1;
        }
        static private <T> Queue<T> mk(T a, T b) {
            return new Queue<T>(a, b);
        }
        static <T> Queue<T> empty() {
            return mk(null, null);
        }
        public Queue<T> push(T x) {
            assert x != null;
            assert b == null;
            if (a == null) {
                return mk(x, b);
            } else {
                return mk(a, x);
            }
        }
        public Queue<T> pop() {
            assert a != null;
            return mk(b, null);
        }
        public T top() {
            assert a != null;
            return a;
        }
        public int size() {
            return a == null? 0 : (b == null? 1 : 2);
        }
        @Override
        public Iterator<T> iterator() {
            return new Itr();
        }
        @Override
        public int hashCode() {
            if (hash == -1) {
                hash = 0;
                if (a != null) {
                    hash += a.hashCode();
                    if (b != null) {
                        hash += hashCode();
                    }
                }
                // If unlucky, hash might be -1 here.
            }
            return hash;
        }
        @SuppressWarnings("unchecked")
        @Override
        public boolean equals(Object other) {
            Queue<T> otherQueue = (Queue<T>) other; // yes, exception wanted
            return
                ((a == null) == (otherQueue.a == null))
                && (a == null || a.equals(otherQueue.a))
                && ((b == null) == (otherQueue.b == null))
                && (b == null || b.equals(otherQueue.b));
        }
        private class Itr implements Iterator<T> {
            int state;

            @Override
            public boolean hasNext() {
                return (state == 0 && a != null) || (state == 1 && b != null);
            }

            @Override
            public T next() {
                ++state;
                return state == 1? a : b;
            }
            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }

        public String toString () {
            StringBuilder sb = new StringBuilder();
            sb.append('<');
            for (T t : this) {
                sb.append(" " + t);
            }
            sb.append(" >");
            return sb.toString();
        }
    }
    // }}}
    // Treap<T extends Comparable<T>> {{{
    static class Treap<T extends Comparable<T>> implements Iterable<T> {
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

        @SuppressWarnings("unchecked")
        @Override
        public boolean equals(Object other) {
            Treap<T> otherTreap = (Treap<T>) other; // yes, cast exception wanted
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
    }
    // }}}
    // HSet<T> {{{
    static class HSet<T> implements Iterable<T> {
        Object[] data;
        int size;
        int bits;

        private HSet() {
            // nothing
        }

        public static <T> HSet<T> make(int newSize) {
            HSet<T> result = new HSet<T>();
            result.bits = hashBits(newSize);
            result.data = new Object[1 << result.bits];
            return result;
        }

        public void add(T x) {
            maybeGrow(size + 1);
            internalAdd(x);
        }

        public void add(T[] xs, int from, int to) {
            maybeGrow(size + to - from);
            for (int i = from; i < to; ++i) {
                internalAdd(xs[i]);
            }
        }

        public boolean contains(T x) {
            return data[search(x)] != null;
        }

        public int size() {
            return size;
        }

        public void clear() {
            size = 0;
            data = new Object[data.length];
        }

        @Override
        public Iterator<T> iterator() {
            return new Itr();
        }

        // Compute how many bits to use for hash values such that
        // the load factor is limited.
        static int hashBits(int size) {
            int result;
            for (result = 0; 4 * size > 3 * (1 << result); ++result);
            return result;
        }

        void maybeGrow(int newSize) {
            if (4 * newSize <= 3 * data.length) {
                return;
            }
            bits = hashBits(newSize);
            Object[] oldData = data;
            data = new Object[1 << bits];
            size = 0;
            for (Object x : oldData) {
                if (x != null) {
                    internalAdd(x);
                }
            }
        }

        int search(Object x) {
            assert x != null;
            int h = ((x.hashCode() * 1327217909) >> (32 - bits)) & (data.length - 1);
            while (data[h] != null && !data[h].equals(x)) {
                h = (h + 1) & (data.length - 1);
            }
            return h;
        }

        void internalAdd(Object x) {
            int h = search(x);
            if (data[h] == null) {
                ++size;
                data[h] = x;
            }
        }

        class Itr implements Iterator<T> {
            int index;
            int togo;

            Itr() {
                index = -1;
                togo = size;
            }

            @Override
            public boolean hasNext() {
                return togo > 0;
            }

            @SuppressWarnings("unchecked")
            @Override
            public T next() {
                while (data[++index] == null);
                --togo;
                return (T) data[index];
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }
    }
    // }}}
    // helper functions {{{
    private static boolean isConstant(Object o) {
        return
            o instanceof Integer
            || o instanceof String
            || o instanceof Boolean;
    }

    private static boolean valueEquals(Object o1, Object o2) {
        if (isConstant(o1)) {
            return o1.equals(o2);
        }
        return o1 == o2;
    }

    private static int valueHashCode(Object x) {
        if (isConstant(x)){
            return x.hashCode();
        } else {
            return System.identityHashCode(x); // handles null
        }
    }

    private static int valuesHashCode(Object[] xs) {
        if (xs == null) {
            return 0;
        }
        int h = 0;
        for (Object x : xs) {
            h = 31 * h + valueHashCode(x) + 1;
        }
        return h;
    }

    // TODO(rgrig): Might want to produce a set with a faster {contains}
    static HSet<Integer> setOf(int[] xs) {
        HSet<Integer> r = HSet.make(xs.length);
        for (int x : xs) {
            r.add(x);
        }
        return r;
    }

    static <T> boolean equalIterators(Iterator<T> i, Iterator<T> j) {
        while (i.hasNext() && j.hasNext()) {
            if (!i.next().equals(j.next())) { // yes, NullExc wanted
                return false;
            }
        }
        return i.hasNext() == j.hasNext();
    }
    // }}}
    // property AST {{{
    // Event must *not* use the JDK at *all*; otherwise JVM initialization
    // may crash.
    public static class Event {
        final int id;
        final Object[] values;

        StackTraceElement[] callStack;

        public Event(int id, Object[] values) {
            this.id = id;
            this.values = values;
            assert check();
        }

        boolean check() {
            assert 0 <= id;
            assert values != null;
            return true;
        }

        @Override
        public int hashCode() {
            return id + valuesHashCode(values);
        }

        @Override
        public boolean equals(Object other) {
            Event otherEvent = (Event) other;
            if (id != otherEvent.id || values.length != otherEvent.values.length) {
                return false;
            }
            for (int i = 0; i < values.length; ++i) {
                if (!valueEquals(values[i], otherEvent.values[i])) {
                    return false;
                }
            }
            return true;
        }
    }

    // Note: x.equals(y) ==> (x.compareTo(y) == 0), but not vice-versa.
    static class Binding implements Comparable<Binding> {
        final int variable;
        final Object value;

        public Binding(int variable, Object value) {
            assert variable >= 0 : "variables are used as indices";
            this.variable = variable;
            this.value = value;
        }

        public Binding(int variable) {
            this(variable, null);
        }

        @Override
        public int compareTo(Binding other) {
            // Overflow safe, because variable is nonnegative.
            return variable - other.variable;
        }

        @Override
        public int hashCode() {
            return variable + valueHashCode(value);
        }

        @Override
        public boolean equals(Object other) {
            Binding otherBinding = (Binding) other;
            return variable == otherBinding.variable
                && valueEquals(value, otherBinding.value);
        }

        @Override
        public String toString() {
            return variable + "->" + value;
        }
    }

    static class State {
        static class Parent {
            final State state;
            final Queue<Event> events;
            Parent(State state, Queue<Event> events) {
                assert state != null;
                assert events != null;
                this.state = state;
                this.events = events;
            }
        }

        static long idPool = -1;
        final long id;
        public long id() { return id; }

        // These contribute to the identity of a State.
        final int vertex;
        final Treap<Binding> store;
        final Queue<Event> events;

        // History for trace reporting. Does not affect automaton semantics,
        // so it's ignored by {@code equals}.
        Parent parent;

        // How many non-skip transitions were taken to build this state.
        int time;

        private State(int vertex, Treap<Binding> store, Queue<Event> events,
                Parent parent, int time) {
            this.id = ++idPool;
            this.vertex = vertex;
            this.store = store;
            this.events = events;
            this.parent = parent;
            this.time = time;
        }

        @Override
        public int hashCode() {
            return vertex + store.hashCode() + events.hashCode();
        }

        @Override
        public boolean equals(Object other) {
            State otherState = (State) other; // yes, I want exc otherwise
            return vertex == otherState.vertex &&
                store.equals(otherState.store) &&
                events.equals(otherState.events);
        }

        static State start(int vertex) {
            return new State(vertex, Treap.<Binding>empty(),
                    Queue.<Event>empty(), null, 0);
        }

        static State make(int vertex, Treap<Binding> store, Queue<Event> events,
                Queue<Event> consumed, State parent) {
            return new State(vertex, store, events,
                    new Parent(parent, consumed), parent.time + 1);
        }

        State pushEvent(Event event) {
            return new State(vertex, store, events.push(event), parent, time);
        }

        State popEvent() {
            return new State(vertex, store, events.pop(), parent, time);
        }

        public String toString () {
            StringBuilder sb = new StringBuilder();
            sb.append("Vertex: " + vertex);
            sb.append("\nStore:\n" + store);
            sb.append("\nEvents in queue:\n" + events);
            if (parent != null) {
                sb.append("\n---reached via events---\n" + parent.events);
                sb.append("\n---from state---\n" + parent.state);
            }
            return sb.toString();
        }
    }

    interface Guard {
        boolean evaluate(Event event, Treap<Binding> store);
    }

    static class AndGuard implements Guard {
        final Guard[] children;

        AndGuard(Guard[] children) {
            this.children = children;
        }

        @Override
        public boolean evaluate(Event event, Treap<Binding> store) {
            for (Guard g : children) {
                if (!g.evaluate(event, store)) {
                    return false;
                }
            }
            return true;
        }

        @Override
        public String toString() {
            if (children.length == 0) return "*";
            if (children.length == 1) return children[0].toString();
            StringBuffer s = new StringBuffer();
            s.append("and (");
            for (Guard g : children) {
                s.append(g);
                s.append(", ");
            }
            s.delete(s.length()-2, s.length());
            s.append(")");
            return s.toString();
        }
    }

    static class NotGuard implements Guard {
        final Guard child;

        NotGuard(Guard child) {
            this.child = child;
        }

        @Override
        public boolean evaluate(Event event, Treap<Binding> store) {
            return !child.evaluate(event, store);
        }

        @Override
        public String toString() {
            return "not (" + child + ")";
        }
    }

    static class StoreEqualityGuard implements Guard {
        final int eventIndex;
        final int storeIndex;

        StoreEqualityGuard(int eventIndex, int storeIndex) {
            this.eventIndex = eventIndex;
            this.storeIndex = storeIndex;
        }

        @Override
        public boolean evaluate(Event event, Treap<Binding> store) {
            Binding b = new Binding(storeIndex);
            boolean eq = valueEquals(event.values[eventIndex], store.get(b).value);
            if (logGuard) {
                System.out.println(eq ? "matches store" : "does NOT match store");
            }
            return eq;
        }

        @Override
        public String toString() {
            return "event[" + eventIndex + "] == store[" + storeIndex + "]";
        }
    }

    static class ConstantEqualityGuard implements Guard {
        final int eventIndex;
        final Object value;

        ConstantEqualityGuard(int eventIndex, Object value) {
            this.eventIndex = eventIndex;
            this.value = value;
        }

        @Override
        public boolean evaluate(Event event, Treap<Binding> store) {
            return (value == null)?
                event.values[eventIndex] == null :
                valueEquals(value, event.values[eventIndex]);
        }

        @Override
        public String toString() {
            return value + " == event[" + eventIndex + "]";
        }
    }

    static class TrueGuard implements Guard {
        @Override
        public boolean evaluate(Event event, Treap<Binding> store) {
            return true;
        }

        @Override
        public String toString() {
            return "*";
        }
    }

    static class Action {
        static class Assignment {
            final int storeIndex;
            final int eventIndex;

            Assignment(int storeIndex, int eventIndex) {
                this.storeIndex = storeIndex;
                this.eventIndex = eventIndex;
            }
        }

        HashMap<Integer, Integer> assignments;

        Action(Assignment[] init) {
            assignments = new HashMap<Integer, Integer>();
            for (Assignment a : init) {
                assert !assignments.containsKey(a.storeIndex);
                assignments.put(a.storeIndex, a.eventIndex);
            }
        }

        Treap<Binding> apply(Event event, Treap<Binding> store) {
            for (Map.Entry<Integer, Integer> e : assignments.entrySet()) {
                Object value = event.values[e.getValue()];
                store = store.insert(new Binding(e.getKey(), value));
            }
            return store;
        }
    }

    static class TransitionStep {
        final HSet<Integer> eventIds;
        final Guard guard;
        final Action action;

        TransitionStep(int[] eventIds, Guard guard, Action action) {
            this.eventIds = setOf(eventIds);
            this.guard = guard;
            this.action = action;
        }

        boolean evaluateGuard(Event event, Treap<Binding> store) {
            return eventIds.contains(event.id)
                && guard.evaluate(event, store);
        }
    }

    static class Transition {
        final TransitionStep[] steps;
        final int target;

        Transition(TransitionStep[] steps, int target) {
            this.steps = steps;
            this.target = target;
        }

        Transition(TransitionStep oneStep, int target) {
            this(new TransitionStep[]{oneStep}, target);
        }
    }

    static class Automaton {
        private static class VertexEvent {
            int vertex;
            int eventId;
            public VertexEvent(int vertex, int eventId) {
                this.vertex = vertex;
                this.eventId = eventId;
            }
            @Override
            public boolean equals(Object o) {
                if (o instanceof VertexEvent) {
                    VertexEvent ve = (VertexEvent)o;
                    return (vertex == ve.vertex && eventId == ve.eventId);
                }
                else return false;
            }
            @Override
            public int hashCode() {
                return 31*vertex + 101*eventId;
            }
        }
        private boolean[][] observable;
           // {observable[p][e]} is on iff property {p} observes event {e}

        final int[] startVertices;
        final String[] errorMessages;

        final int[] filterOfState;

        final Transition[][] transitions;
            // {transitions[vertex]} are the outgoing transitions of {vertex}

        public int[] maximumTransitionDepths;
            // {maximumTransitionDepths[vertex]} is the maximum depths of outgoing transitions of {vertex}

        final String[] eventNames;

        final String[] vertexNames;

        /**
         * @param startVertices startVertices[p] has start vertex for property p
         * @param erorMessages erorMessages[i] is null if vertex i is not accepting
         * @param transitions transitions[i][j] is transtion from vertex i to vertex j
         * @param filterOfState filterOfState[i] is the property that vertex i belongs to
         * @param filters filters[p][n] is the event id of the n'th event that property p observes
         */
        Automaton(int[] startVertices, String[] errorMessages,
                  Transition[][] transitions, int[] filterOfState,
                  int[][] filters, String[] eventNames, String[] vertexNames) {
            this.startVertices = startVertices;
            this.errorMessages = errorMessages;
            this.filterOfState = filterOfState;
            this.transitions = transitions;
            this.eventNames = eventNames;
            this.vertexNames = vertexNames;
            maximumTransitionDepths = new int[transitions.length];
            for (int s = 0; s < transitions.length; ++s) {
                maximumTransitionDepths[s] = 0;
                for (Transition t : transitions[s]) {
                    maximumTransitionDepths[s] = Math.max(
                                maximumTransitionDepths[s], t.steps.length);
                }
            }
            observable = new boolean[filters.length][eventNames.length];
            for (int f = 0; f < filters.length; ++f) {
                for (int v = 0; v < filters[f].length; ++v) {
                    observable[f][filters[f][v]] = true;
                }
            }
            assert check();
        }

        boolean check() {
            assert transitions != null;
            assert errorMessages.length == transitions.length;
            for (int v : startVertices) {
                assert 0 <= v && v < transitions.length;
                assert errorMessages[v] == null;
            }
            for (Transition[] ts : transitions) {
                assert ts != null;
                for (Transition t : ts) {
                    assert t != null;
                    assert 0 <= t.target && t.target < transitions.length;
                    assert t.steps != null;
                    for (TransitionStep s : t.steps) {
                        assert s != null;
                        assert s.eventIds != null;
                        assert s.guard != null;
                        assert s.action != null;
                        // TODO(rgrig): Bounds for integers in guards/actions.
                    }
                }
            }
            return true;
        }

        boolean isObservable(int eventId, int vertex) {
            return observable[filterOfState[vertex]][eventId];
        }
    }
    // }}}
    // checker {{{
    public enum SelectionStrategy {
        RANDOM,
        NEWEST,
        OLDEST
    };

    public boolean checkerEnabled = true;

    // These should be printed by [Toplc.pp_constants_table], to make
    // them easily accessible to users.
    public boolean captureCallStacks = false;
    public int historyLength = 10;
    public boolean onlyLogEvents = false;
    public PrintWriter automatonLog = null;
    public int statesLimit = 10;
    public SelectionStrategy selectionStrategy = SelectionStrategy.NEWEST;

    private Throwable throwable = new Throwable();

    private int totalStates = 0; // estimate, refreshed when doing GC
    private int operations = 0; // estimate of work done since the last GC

    final private Automaton automaton;
    private HSet<State> states;

    public Checker(Automaton automaton) {
        this.automaton = automaton;
        this.states = HSet.make(automaton.startVertices.length);
        for (int v : automaton.startVertices) {
            states.add(State.start(v));
        }
    }

    private IdentityHashMap<Object, Integer> printingIds;

    int getIdForPrinting(Object o) {
        Integer id = printingIds.get(o);
        if (id == null) {
            id = printingIds.size();
            printingIds.put(o, id);
        }
        return id;
    }

    void printValues(Object[] values) {
        boolean first = true;
        System.err.printf("(");
        for (Object v : values) {
            if (first) {
                first = false;
            } else {
                System.err.printf(",");
            }
            if (v instanceof String) {
                System.err.printf("\"%s\"", (String) v);
            } else if (v instanceof Integer) {
                System.err.printf("%d", (Integer) v);
            } else if (v instanceof Boolean) {
                System.err.printf("%b", (Boolean) v);
            } else {
                System.err.printf("obj[%d]", getIdForPrinting(v));
            }
        }
        System.err.printf(")");
    }

    void printEventQueue(Queue<Event> events) {
        boolean first = true;
        for (Event e : events) {
            if (first) {
                first = false;
            } else {
                System.err.printf("; ");
            }
            System.err.printf("%s", automaton.eventNames[e.id]);
            printValues(e.values);
        }
        first = true;
        for (Event e : events) {
            if (e.callStack == null) {
                continue;
            }
            if (first) {
                System.err.println();
                first = false;
            } else {
                System.err.println("       -- ; --");
            }
            for (int i = 1; i < e.callStack.length; ++i) {
                System.err.printf("       %s\n", e.callStack[i].toString());
            }
        }
    }

    void printErrorTraceRec(State errorState, int historyLength) {
        --historyLength;
        if (historyLength < 0) {
            return;
        }
        if (errorState.parent != null) {
            printErrorTraceRec(errorState.parent.state, historyLength);
        }
        System.err.printf("  -> %s: ", automaton.vertexNames[errorState.vertex]);
        if (errorState.parent != null) {
            printEventQueue(errorState.parent.events);
        }
        System.err.printf("\n");
    }

    void printErrorTrace(State errorState) {
        printingIds = new IdentityHashMap<Object, Integer>();
        printErrorTraceRec(errorState, historyLength);
        printingIds = null;
    }

    void reportError(String msg, State errorState) {
        System.err.printf("TOPL: %s\n", msg);
        printErrorTrace(errorState);
    }

    private static void unmarkAfterGc(State s) {
        assert s != null;
        assert s.time < 0;
        s.time = -s.time;
        if (s.parent != null && s.parent.state.time < 0) {
            unmarkAfterGc(s.parent.state);
        }
    }

    static Random random = new Random(123);

    static <T> void swap(T[] a, int i, int j) {
        T t = a[i];
        a[i] = a[j];
        a[j] = t;
    }

    // PRE: i < k ==> i <= j < k
    // POST: a[i..j) <= a[j] <= a[j..k)
    // VARIANT: k - i
    private static void selectOldest(State[] a, int i, int j, int k) {
        int ii, jj, kk;
        if (i >= k) return;
        jj = i + random.nextInt() % (k - i);
        swap(a, i, jj);
        for (ii = jj = kk = i + 1; kk < k; ++kk) {
            // INV: a[ii..jj) <= a[i] < a[jj..kk)
            if (a[kk].time <= a[i].time) {
                swap(a, jj++, kk);
            }
        }
        swap(a, i, --jj);
        if (j < jj) {
            selectOldest(a, i, j, jj);
        } else if (j > jj) {
            selectOldest(a, jj + 1, j, k);
        }
    }

    private void logEvent(Event event) {
        assert onlyLogEvents;
        if (printingIds == null) {
            printingIds = new IdentityHashMap<Object, Integer>();
        }
        System.err.printf("TOPL LOG %s", automaton.eventNames[event.id]);
        printValues(event.values);
        System.err.println();
    }

    private void logAddState(State state) {
        assert automatonLog != null;
        automatonLog.printf("add_child %d %d%n",
            state.parent.state.id(), state.id());
    }

    private void logDeactivateState(long stateId) {
        assert automatonLog != null;
        automatonLog.printf("deactivate %d%n", stateId);
    }

    public synchronized void check(Event event) {
        try {
            if (!checkerEnabled) {
                return;
            }
            checkerEnabled = false;
            if (onlyLogEvents) {
                logEvent(event);
            } else {
                if (captureCallStacks) {
                    throwable.fillInStackTrace();
                    event.callStack = throwable.getStackTrace();
                }
                internalCheck(event);
            }
            checkerEnabled = true;
        } catch (Throwable t) {
            System.err.println("TOPL: INTERNAL ERROR");
            t.printStackTrace();
        }
    }

    private static Queue<Event> noEvent = Queue.empty();

    private void internalCheck(Event event) {
        if (logState) {
            boolean first = true;
            System.out.printf("States");
            for (State s : states) {
                System.out.printf("\n  %s ( vertex = %s(%d); len(events) = %d; len(bindings) = %d; time = %d )",
                        first ? "{" : ",",
                        automaton.vertexNames[s.vertex], s.vertex,
                        s.events.size(), s.store.size(),
                        s.time);
                first = false;
            }
            System.out.printf(" }\n");
            System.out.printf("event %d: %s\n", event.id, automaton.eventNames[event.id]);
        }
        HSet<State> newActiveStates = HSet.make(states.size());
        for (State state : states) {
            if (automaton.transitions[state.vertex].length == 0) {
                continue;
            }
            if (!automaton.isObservable(event.id, state.vertex)) {
                newActiveStates.add(state);
                continue;
            }
            state = state.pushEvent(event);
            if (state.events.size() < automaton.maximumTransitionDepths[state.vertex]) {
                newActiveStates.add(state);
                continue;
            }
            boolean anyEnabled = false;
            for (Transition transition : automaton.transitions[state.vertex]) {
                // Evaluate transition.
                // NOTE: Performance here matters a lot.
                TransitionStep[] steps = transition.steps;
                Queue<Event> events = state.events;
                assert 0 < steps.length;
                assert steps.length <= 2;
                assert steps.length <= events.size();
                assert events.size() <= 2;
                Treap<Binding> store = state.store;
                if (!steps[0].evaluateGuard(events.a, store)) {
                    continue;
                }
                store = steps[0].action.apply(events.a, store);
                if (steps.length == 2) {
                    if (!steps[1].evaluateGuard(events.b, store)) {
                        continue;
                    }
                    store = steps[1].action.apply(events.b, store);
                }

                // Figure out the consumed events and the remaining ones.
                Queue<Event> consumed, remaining;
                if (steps.length == events.size()) {
                    consumed = events;
                    remaining = noEvent;
                } else {
                    assert steps.length == 1;
                    consumed = noEvent.push(events.top());
                    remaining = events.pop();
                }

                // Record the transition.
                anyEnabled = true;
                State newState = State.make(transition.target, store,
                                            remaining, consumed, state);
                newActiveStates.add(newState);
                if (automatonLog != null) logAddState(newState);

                String msg = automaton.errorMessages[transition.target];
                if (msg != null) {
                    reportError(msg, newState);
                }
            }
            if (!anyEnabled) {
//DBG System.out.println("stay"); //DBG
                newActiveStates.add(state.popEvent());
            }
        }

        // Approximate.
        if (newActiveStates.size() > statesLimit) {
            int from = 0;
            State[] all = new State[newActiveStates.size()];
            {   int i = 0;
                for (State s : newActiveStates) {
                    all[i++] = s;
                }
            }
            newActiveStates.clear();
            switch (selectionStrategy) {
                case RANDOM:
                    from = random.nextInt() % (all.length - statesLimit + 1);
                    break;
                case OLDEST:
                    from = 0;
                    selectOldest(all, 0, statesLimit - 1, all.length);
                    break;
                case NEWEST:
                    from = all.length - statesLimit;
                    selectOldest(all, 0, from, all.length);
                    break;
                default:
                    assert false;
            }
            newActiveStates.add(all, from, from + statesLimit);
            assert newActiveStates.size() == statesLimit;
        }

        // Commit to new active states.
        if (automatonLog != null) {
            long[] oldIds = new long[states.size()];
            long[] newIds = new long[newActiveStates.size()];
            {   int i = 0;
                for (State s : states) oldIds[i++] = s.id();
                i = 0;
                for (State s : newActiveStates) newIds[i++] = s.id();
            }
            Arrays.sort(oldIds);
            Arrays.sort(newIds);
            for (long i : oldIds) {
                if (Arrays.binarySearch(newIds, i) < 0) {
                    logDeactivateState(i);
                }
            }
        }
        states = newActiveStates;

        // Truncate traces (GC old states).
        operations += states.size();
        if (operations > 3 * totalStates) {
            operations = totalStates = 0;
            ArrayDeque<State> p;
            ArrayDeque<State> q = new ArrayDeque<State>();
            for (State s : states) {
                q.addLast(s);
                s.time = -s.time;
                ++totalStates;
            }
            for (int i = 0; i < historyLength; ++i) {
                p = q;
                q = new ArrayDeque<State>();
                for (State s : p) {
                    if (s.parent != null && s.parent.state.time > 0) {
                        q.addLast(s.parent.state);
                        s.parent.state.time = -s.parent.state.time;
                        ++totalStates;
                    }
                }
            }
            for (State s : q) {
                s.parent = null;
            }
            for (State s : states) {
                unmarkAfterGc(s);
            }
        }
    }
    // }}}
    // parsing {{{
    public static class Parser {
        final Scanner scan;
        final String[] strings;
        final Object[] constants;

        Parser(Scanner scan, String[] strings, Object[] constants) {
            this.scan = scan;
            this.strings = strings;
            this.constants = constants;
        }

        /** Returns {@code null} if something goes wrong. */
        public static Checker checker(
                String automatonFile, String stringsFile, Object[] constants) {
            try {
                ArrayDeque<String> strings = new ArrayDeque<String>();
                Scanner scan = new Scanner(
                        ClassLoader.getSystemResourceAsStream(stringsFile));
                while (scan.hasNextLine()) {
                    strings.addLast(scan.nextLine());
                }
                scan = new Scanner(
                        ClassLoader.getSystemResourceAsStream(automatonFile));
                String[] stringsArray = strings.toArray(new String[0]);
                return new Checker(new Parser(scan, stringsArray, constants)
                        .automaton());
            } catch (Exception e) { // method is used as a static initializer
                e.printStackTrace();
                return null;
            }
        }

        Automaton automaton() {
            int[] startVertices = ints();
            String[] errorMessages = strings();
            String[] vertexNames = new String[scan.nextInt()];
            Transition[][] transitions = new Transition[vertexNames.length][];
            for (int i = 0; i < transitions.length; ++i) {
                vertexNames[i] = oneString();
                transitions[i] = transitions();
            }
            int filterOfState[] = ints();
            int[][] filters = new int[scan.nextInt()][];
            for (int i = 0; i < filters.length; ++i) {
                filters[i] = ints();
            }
            String[] eventNames = strings();
            return new Automaton(startVertices, errorMessages, transitions,
                                 filterOfState, filters, eventNames, vertexNames);
        }

        Transition[] transitions() {
            Transition[] transitions = new Transition[scan.nextInt()];
            for (int i = 0; i < transitions.length; ++i) {
                transitions[i] = transition();
            }
            return transitions;
        }

        Transition transition() {
            TransitionStep[] steps = new TransitionStep[scan.nextInt()];
            for (int i = 0; i < steps.length; ++i) {
                steps[i] = step();
            }
            return new Transition(steps, scan.nextInt());
        }

        TransitionStep step() {
            int[] eventIds = ints();
            Guard guard = guard();
            Action action = action();
            return new TransitionStep(eventIds, guard, action);
        }

        Guard guard() {
            Guard[] atoms = new Guard[scan.nextInt()];
            for (int i = 0; i < atoms.length; ++i) {
                if (scan.nextInt() == 0) {
                    int eventIndex = scan.nextInt();
                    int storeIndex = scan.nextInt();
                    atoms[i] = new StoreEqualityGuard(eventIndex, storeIndex);
                } else {
                    int eventIndex = scan.nextInt();
                    Object value = constants[scan.nextInt()];
                    atoms[i] = new ConstantEqualityGuard(eventIndex, value);
                }
            }
            return new AndGuard(atoms);
        }

        Action action() {
            Action.Assignment[] assignments =
                new Action.Assignment[scan.nextInt()];
            for (int i = 0; i < assignments.length; ++i) {
                int storeIndex = scan.nextInt();
                int eventIndex = scan.nextInt();
                assignments[i] = new Action.Assignment(storeIndex, eventIndex);
            }
            return new Action(assignments);
        }

        int[] ints() {
            int[] result = new int[scan.nextInt()];
            for (int i = 0; i < result.length; ++i) {
                result[i] = scan.nextInt();
            }
            return result;
        }

        String oneString() {
            int n = scan.nextInt();
            if (n < 0) {
                return null;
            } else {
                return strings[n];
            }
        }

        String[] strings() {
            String[] result = new String[scan.nextInt()];
            for (int i = 0; i < result.length; ++i) {
                result[i] = oneString();
            }
            return result;
        }
    }
    // }}}
    // debug {{{
    private String eventIdsToString(HSet<Integer> eventIds) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        boolean later = false;
        for (Integer id : eventIds) {
            if (later) {
                sb.append(", ");
            }
            sb.append(id + ": " + automaton.eventNames[id]);
            later = true;
        }
        sb.append("]");
        return sb.toString();
    }

    public String toDOT(int cap) {
        StringBuilder s = new StringBuilder();
        s.append("digraph Property {\n");
        // add states as circles
        for (int i = 0; i < automaton.transitions.length; i++) {
            s.append("  S_");
            s.append(i);
            s.append(" [label=\"");
            s.append(automaton.vertexNames[i]);
            if (automaton.errorMessages[i] != null) {
                s.append(" : ");
                s.append(automaton.errorMessages[i]);
                s.append("\", shape=box];\n");
            }
            else s.append("\", shape=circle];\n");
        }
        // make start states double circles
        for (int i : automaton.startVertices) {
            s.append("  S_");
            s.append(i);
            s.append(" [shape=doublecircle];\n");
        }
        // add transitions
        for (int i = 0; i < automaton.transitions.length; i++)
            for (Transition transition : automaton.transitions[i]) {
                s.append("  S_");
                s.append(i);
                s.append(" -> S_");
                s.append(transition.target);
                s.append(" [label=\"");
                for (TransitionStep step : transition.steps) {
                    s.append(cap <= 0 || step.eventIds.size() <= cap ? eventIdsToString(step.eventIds) : "[" + step.eventIds.size() + " ids (>" + cap + ")]");
                    s.append(step.guard.toString());
                    s.append("<");
                    for(Map.Entry<Integer, Integer> a : step.action.assignments.entrySet()) {
                        s.append(a.getKey());
                        s.append(" <- ");
                        s.append(a.getValue());
                        s.append(", ");
                    }
                    if (step.action.assignments.size() > 0) s.delete(s.length()-2, s.length());
                    s.append(">; ");
                }
                if (transition.steps.length > 0) s.delete(s.length()-2, s.length());
                s.append("\"];\n");
            }
        s.append("}\n");
        return s.toString();
    }

    public String toDOT() {
        return toDOT(0);
    }

    private static boolean logGuard = false;
    private static boolean logState = false;
    private static boolean logTreap = false;
    // }}}
}
// vim:sts=4:sw=4:ts=8:et:
