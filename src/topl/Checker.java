// header {{{
package topl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
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
        private T a, b; // inv: a != null || b == null
        private int hash;
        Queue(T a, T b) {
            assert a != null || b == null;
            this.a = a;
            this.b = b;
            if (a != null) {
                hash = a.hashCode();
                if (b != null) {
                    hash += b.hashCode();
                }
            }
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
            return hash;
        }
        @Override
        public boolean equals(Object other) {
            Queue otherQueue = (Queue) other; // yes, exception wanted
            return a == otherQueue.a && b == otherQueue.b;
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
    /*
    static class Queue<T> implements Iterable<T> {
        static private class N<T> {
            T data;
            N<T> next;
            int size;
            private N(T data, N<T> next, int size) {
                this.data = data;
                this.next = next;
                this.size = size;
            }
            static <T> N<T> mk(T data, N<T> next) {
                return new N<T>(data, next, 1 + sizeN(next));
            }
        }
        static private <T> N<T> reverseN(N<T> n) {
            N<T> r;
            for (r = null; n != null; n = n.next) {
                r = N.mk(n.data, r);
            }
            return r;
        }
        static private <T> int sizeN(N<T> n) {
            return n == null? 0 : n.size;
        }
        private N<T> front;
        private N<T> back;
        private int hash;

        // not fail-fast
        private class Itr implements Iterator<T> {
            N<T> next;
            boolean frontDone = false;

            Itr() {
                next = front;
                maybeSwap();
            }

            private void maybeSwap() {
                if (next == null && !frontDone) {
                    next = reverseN(back);
                    frontDone = true;
                }
            }

            @Override
            public boolean hasNext() {
                return next != null;
            }

            @Override
            public T next() {
                T r = next.data;
                next = next.next;
                maybeSwap();
                return r;
            }
            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }

        private Queue(N<T> front, N<T> back, int hash) {
            this.front = front;
            this.back = back;
            this.hash = hash;
        }
        static private <T> Queue<T> mk(N<T> front, N<T> back, int hash) {
            return new Queue<T>(front, back, hash);
        }
        static <T> Queue<T> empty() {
            return new Queue<T>(null, null, 0);
        }
        private void maybeSwap() {
            if (front == null) {
                front = reverseN(back);
                back = null;
            }
        }
        public Queue<T> push(T x) {
            assert x != null;
            return Queue.mk(front, N.mk(x, back), hash + x.hashCode());
        }
        public Queue<T> pop() {
            maybeSwap();
            if (front == null) {
                throw new RuntimeException("queue empty");
            }
            return Queue.mk(front.next, back, hash - front.data.hashCode());
        }
        public T top() {
            maybeSwap();
            if (front == null) {
                throw new RuntimeException("queue empty");
            }
            return front.data;
        }
        public int size() {
            return sizeN(front) + sizeN(back);
        }
        @Override
        public Iterator<T> iterator() {
            return new Itr();
        }
        @Override
        public int hashCode() {
            return hash;
        }
        @Override
        public boolean equals(Object other) {
            Queue otherQueue = (Queue) other; // yes, exception wanted
            return this == otherQueue ||
                (hash == otherQueue.hash &&
                equalIterators(iterator(), otherQueue.iterator()));
        }
    }*/
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
    }
    // }}}
    // helper functions {{{
    private static boolean valueEquals(Object o1, Object o2) {
        if (o1 instanceof Integer)
            return o1.equals(o2);
        return o1 == o2;
    }

    // TODO(rgrig): Might want to produce a set with a faster {contains}
    static Set<Integer> setOf(int[] xs) {
        HashSet<Integer> r = new HashSet<Integer>();
        for (int x : xs) {
            r.add(x);
        }
        return r;
    }

    static <T> boolean equalIterators(Iterator<T> i, Iterator j) {
        while (i.hasNext() && j.hasNext()) {
            if (!i.next().equals(j.next())) { // yes, NullExc wanted
                return false;
            }
        }
        return i.hasNext() == j.hasNext();
    }
    // }}}
    // property AST {{{
    public static class Event {
        final int id;
        final Object[] values;

        public Event(int id, Object[] values) {
            this.id = id;
            this.values = values;
            assert check();
        }

        boolean check() {
            assert values != null;
            return true;
        }
    }

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

        // These contribute to the identity of a State.
        final int vertex;
        final Treap<Binding> store;
        final Queue<Event> events;

        // These keep a history for reporting traces. From the point of view
        // of semantics of the automaton, two states that differ in their
        // histories are essentially equivalent.
        int time;  // length of the parent list
        Parent parent;

        private State(int vertex, Treap<Binding> store, Queue<Event> events,
                Parent parent) {
            this.vertex = vertex;
            this.store = store;
            this.events = events;
            this.parent = parent;
            this.time = 1 + (parent == null? 0 : parent.state.time);
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
                    Queue.<Event>empty(), null);
        }

        static State make(int vertex, Treap<Binding> store, Queue<Event> events,
                Queue<Event> consumed, State parent) {
            return new State(vertex, store, events,
                    new Parent(parent, consumed));
        }

        State pushEvent(Event event) {
            return new State(vertex, store, events.push(event), parent);
        }

        State popEvent() {
            return new State(vertex, store, events.pop(), parent);
        }

        public String toString () {
            StringBuilder sb = new StringBuilder();
            sb.append("Verttex: " + vertex);
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
        final Set<Integer> eventIds;
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
            int maxEvent = 0;
            for (int[] f : filters) {
                for (int e : f) {
                    if (e > maxEvent) maxEvent = e;
                }
            }
            observable = new boolean[filters.length][maxEvent + 1];
            for (int f = 0; f < filters.length; f++) {
                for (int e = 0; e < maxEvent; e++) {
                    observable[f][e] = true;
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
    public boolean verbose = false;
    public boolean checkerEnabled = false;
    public int historyLength = 10;

    final private Automaton automaton;
    private HashSet<State> states;

    public Checker(Automaton automaton) {
        this.automaton = automaton;
        this.states = new HashSet<State>();
        for (int v : automaton.startVertices) {
            states.add(State.start(v));
        }
    }

    void printEventQueue(Queue<Event> events) {
        StringBuilder sb = new StringBuilder();
        sb.append('<');
        for (Event e : events) {
            sb.append(" " + automaton.eventNames[e.id] + "(");
            boolean later = false;
            for(Object o : e.values) {
                if (later) {
                    sb.append(", ");
                }
                sb.append(o.toString());
                later = true;
            }
            sb.append(")");
        }
        sb.append(" >");
        System.err.println(sb.toString());
    }

    void printErrorState(State errorState) {
        System.err.println(automaton.vertexNames[errorState.vertex]);
    }

    void printErrorTrace(State errorState) {
        if (errorState.parent != null) {
            printErrorTrace(errorState.parent.state);
            System.err.println("\n---- via events ----");
            printEventQueue(errorState.parent.events);
            System.err.println("\n--- got to state ---");
        }
        printErrorState(errorState);
    }

    void reportError(String msg, State errorState) {
        System.err.printf("TOPL: %s\n", msg);
        if (verbose) {
            System.err.println("TOPL: Error trace:");
            printErrorTrace(errorState);
        }
    }

    // Used for truncating traces.
    private static void fixTimeOfState(State s) {
        assert s != null;
        assert s.time == -1;
        s.time = 1;
        if (s.parent == null || s.parent.state.time != -1) {
            s.parent = null;
        } else {
            fixTimeOfState(s.parent.state);
            s.time += s.parent.state.time;
        }
    }

    public synchronized void check(Event event) {
        if (!checkerEnabled) {
            return;
        }
        checkerEnabled = false;
        if (logState) {
            boolean first = true;
            System.out.printf("States");
            for (State s : states) {
                System.out.printf("\n  %s ( vertex = %s; len(events) = %d; len(bindings) = %d )",
                        first ? "{" : ",",
                        s.vertex, s.events.size(), s.store.size());
            }
            System.out.printf(" }\n");
            System.out.printf("event %d\n", event.id);
        }
        HashSet<State> newActiveStates = new HashSet<State>(2 * states.size());
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
//DBG System.out.print("try " + state.vertex + " -> " + transition.target //DBG
//DBG         + " with events");                                          //DBG
//DBG for (Event e : state.events) System.out.print(" " + e.id);          //DBG
//DBG System.out.println();                                               //DBG
                // evaluate transition
                Treap<Binding> store = state.store;
                Queue<Event> events = state.events;
                Queue<Event> consumed = Queue.empty();
                int i;
                for (i = 0; i < transition.steps.length; ++i) {
                    TransitionStep step = transition.steps[i];
                    Event stepEvent = events.top();
                    events = events.pop();
                    consumed = consumed.push(stepEvent);
                    if (!step.evaluateGuard(stepEvent, store)) {
                        break;
                    }
//DBG System.out.println("step"); //DBG
                    store = step.action.apply(stepEvent, store);
                }

                // record transition
                if (i == transition.steps.length) {
//DBG System.out.println("tran"); //DBG
                    anyEnabled = true;
                    State newState = State.make(transition.target, store,
                                                events, consumed, state);
                    newActiveStates.add(newState);

                    // check for error state
                    String msg = automaton.errorMessages[transition.target];
                    if (msg != null) {
                        reportError(msg, newState);
                    }
                }
            }
            if (!anyEnabled) {
//DBG System.out.println("stay"); //DBG
                newActiveStates.add(state.popEvent());
            }
        }

        // Truncate traces.
        boolean doGc = true;
        for (State s : newActiveStates) {
            if (s.time < 2 * historyLength) {
                doGc = false;
                break;
            }
        }
        if (doGc) {
            // Mark those to keep.
            ArrayDeque<State> p;
            ArrayDeque<State> q = new ArrayDeque<State>();
            for (State s : newActiveStates) {
                q.addLast(s);
                s.time = -1;
            }
            for (int i = 1; i < historyLength; ++i) {
                p = q;
                q = new ArrayDeque<State>();
                for (State s : p) {
                    if (s.parent != null && s.parent.state.time != -1) {
                        q.addLast(s.parent.state);
                        s.parent.state.time = -1;
                    }
                }
            }

            // Update times and cut parent links.
            for (State s : newActiveStates) {
                fixTimeOfState(s);
            }
        }

        states = newActiveStates;
        checkerEnabled = true;
    }
    // }}}
    // parsing {{{
    public static class Parser {
        final Scanner scan;
        final Object[] constants;

        Parser(Scanner scan, Object[] constants) {
            this.scan = scan;
            this.constants = constants;
        }

        /** Returns {@code null} if something goes wrong. */
        public static Checker checker(String filename, Object[] constants) {
            try {
                Scanner scan = new Scanner(ClassLoader.getSystemResourceAsStream(filename));
                return new Checker(new Parser(scan, constants).automaton());
            } catch (Exception e) { // method is used as a static initializer
                e.printStackTrace();
                return null;
            }
        }

        Automaton automaton() {
            int[] startVertices = ints();
            String[] errorMessages = strings();
            Transition[][] transitions = new Transition[scan.nextInt()][];
            for (int i = 0; i < transitions.length; ++i) {
                transitions[i] = vertex();
            }
            int filterOfState[] = ints();
            int[][] filters = new int[scan.nextInt()][];
            for (int i = 0; i < filters.length; ++i) {
                filters[i] = ints();
            }
            String[] eventNames = new String[scan.nextInt()];
            for (int i = 0; i < eventNames.length; ++i) {
                int index = scan.nextInt();
                eventNames[index] = scan.next();
            }
            String[] vertexNames = new String[scan.nextInt()];
            for (int i = 0; i < eventNames.length; ++i) {
                vertexNames[i] = scan.next();
            }
            return new Automaton(startVertices, errorMessages, transitions,
                                 filterOfState, filters, eventNames, vertexNames);
        }

        Transition[] vertex() {
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

        String[] strings() {
            String[] result = new String[scan.nextInt()];
            for (int i = 0; i < result.length; ++i) {
                result[i] = (String) constants[scan.nextInt()];
            }
            return result;
        }
    }
    // }}}
    // debug {{{
    private String eventIdsToString(Set<Integer> eventIds) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        boolean later = false;
        for (Integer id : eventIds) {
            if (later) {
                sb.append(", ");
            }
            sb.append(automaton.eventNames[id]);
            later = true;
        }
        sb.append("]");
        return sb.toString();
    }

    public String toDOT(int cap) {
        StringBuffer s = new StringBuffer();
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
                    for(Map.Entry a : step.action.assignments.entrySet()) {
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
/* TODO {{{
    - do *NOT* use any Java libraries
    - write some tests
    - add 'final' where possible
}}} */
// vim:sts=4:sw=4:ts=8:et:
