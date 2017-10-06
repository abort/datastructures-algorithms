import java.util.*;
import java.io.*;
import java.util.zip.CheckedInputStream;

// Due to scala version being too slow :(
public class SuffixTree {
    class FastScanner {
        StringTokenizer tok = new StringTokenizer("");
        BufferedReader in;

        FastScanner() {
            in = new BufferedReader(new InputStreamReader(System.in));
        }

        String next() throws IOException {
            while (!tok.hasMoreElements())
                tok = new StringTokenizer(in.readLine());
            return tok.nextToken();
        }

        int nextInt() throws IOException {
            return Integer.parseInt(next());
        }
    }
    private class Node implements Comparable<Node> {
        int id;
        int start;
        int end;
        int level;
        Map<Character, Node> children = null;
        Node parent = null;
        char[] sequence = null;

        String value() {
            return new String(sequence, start, end - start);
        }

        boolean isRoot() {
            return start == 0 && end == sequence.length;
        }

        boolean isCompressionCandidate() {
            return parent != null && parent.children.size() == 1 && !parent.isRoot();
        }

        Node(int id, int start, int end, int level, Map<Character, Node> children, Node parent, char[] sequence) {
            this.start = start;
            this.end = end;
            this.children = children;
            this.sequence = sequence;
            this.level = level;
            this.parent = parent;
            this.id = id;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Node node = (Node) o;
            return id == node.id;
        }

        @Override
        public int hashCode() {
            return Objects.hash(id);
        }

        @Override
        public int compareTo(Node o) {
            /*
            if (start < o.start) return start - o.start;
            else if (start == o.start) return end - o.end;
            else return start - o.start;*/
            return o.level - level;
        }
    }


    public SuffixTree() {
    }

    // Build a suffix tree of the string text and return a list
    // with all of the labels of its edges (the corresponding 
    // substrings of the text) in any order.
    public List<String> computeSuffixTreeEdges(String text) {
        char[] sequence = text.toCharArray();
        final Set<Node> nodes = new HashSet<>();
        final IndexMinPQ<Node> processQueue = new IndexMinPQ<>(((sequence.length * (sequence.length + 1)) / 2 + 1));

        int id = 0;
        Node root = new Node(id++, 0, sequence.length, 0, new HashMap<>(), null, sequence);
        processQueue.insert(root.id, root);
        for (int offset = 0; offset < sequence.length; offset++) {
            Node p = root;
            for (int i = offset; i < sequence.length; i++) {
                Character c = sequence[i];
                if (p.children.containsKey(c)) p = p.children.get(c);
                else {
                    Node node = new Node(id++, i, i + 1, p.level + 1, new HashMap<>(), p, sequence);
                    p.children.put(c, node);
                    // If we have more than 1 child, remove from optimization queue
                    if (p.children.size() > 1 && processQueue.contains(p.id)) {
                        processQueue.delete(p.id);
                        //System.out.println("deleted " + p.id);
                    }
                    else if (p.children.size() == 1 && !processQueue.contains(p.id)) {
                        // add to queue if we have 1 child (guaranteed by p.children.put)
                        //System.out.println("insert " + p.id + " (max: " + processQueue.maxN + ")");
                        processQueue.insert(p.id, p);
                    }
                    //System.out.println("\tinsert " + node.id + " (max: " + processQueue.maxN + ")");
                    processQueue.insert(node.id, node); // add leaf
//                    leaves.remove(p);
//                    leaves.add(node);
                    nodes.add(node);
                    p = p.children.get(c);
                }
            }
        }

        compress(nodes, processQueue);

        // Maybe root.tosequence is more safe....
        List<Node> output = new ArrayList<>(nodes);
        //Collections.sort(output);

        List<String> result = new ArrayList<>(output.size());

        output.forEach(n -> result.add(n.value()));
        return result;
    }

    private void compress(Set<Node> nodes, IndexMinPQ<Node> toProcess) {
        //int initlen = nodes.size();

        while (!toProcess.isEmpty()) {
            Node n = toProcess.minKey();
            toProcess.delMin();
            if (nodes.contains(n)) {
                for (Node c : n.children.values()) {
                    if (nodes.contains(c)) {
                        Set<Node> deleted = compress(c, nodes);
                        for (Node d : deleted) if (toProcess.contains(d.id)) toProcess.delete(d.id);
                    }
                }
            }
        }
    }
    private Set<Node> compress(Node node, Set<Node> nodes) {
        Set<Node> deleted = new HashSet<>();
        Node n = node;
        while (node.isCompressionCandidate()) {
            //System.out.println("removing " + node.value() + " level: " + node.level);
            Node p = n.parent;
            p.children = n.children;
            p.end = n.end;

            //processed.add(node);
            nodes.remove(n);
            deleted.add(n);

            // prevent loitering
            Node c = n;
            n = p;
            c.parent = null;
            c.children.clear(); // why not null? who is accessing this :X
            c = null;
        }
        return deleted;
    }


    static public void main(String[] args) throws IOException {
        new SuffixTree().run();
    }

    public void print(List<String> x) {
        for (String a : x) {
            System.out.println(a);
        }
    }

    public void run() throws IOException {
        FastScanner scanner = new FastScanner();
        String text = scanner.next();
        List<String> edges = computeSuffixTreeEdges(text);
        print(edges);
    }

    public class IndexMinPQ<Key extends Comparable<Key>> implements Iterable<Integer> {
        private int maxN;        // maximum number of elements on PQ
        private int n;           // number of elements on PQ
        private int[] pq;        // binary heap using 1-based indexing
        private int[] qp;        // inverse of pq - qp[pq[i]] = pq[qp[i]] = i
        private Key[] keys;      // keys[i] = priority of i

        /**
         * Initializes an empty indexed priority queue with indices between {@code 0}
         * and {@code maxN - 1}.
         *
         * @param maxN the keys on this priority queue are index from {@code 0}
         *             {@code maxN - 1}
         * @throws IllegalArgumentException if {@code maxN < 0}
         */
        public IndexMinPQ(int maxN) {
            if (maxN < 0) throw new IllegalArgumentException();
            this.maxN = maxN;
            n = 0;
            keys = (Key[]) new Comparable[maxN + 1];    // make this of length maxN??
            pq = new int[maxN + 1];
            qp = new int[maxN + 1];                   // make this of length maxN??
            for (int i = 0; i <= maxN; i++)
                qp[i] = -1;
        }

        /**
         * Returns true if this priority queue is empty.
         *
         * @return {@code true} if this priority queue is empty;
         * {@code false} otherwise
         */
        public boolean isEmpty() {
            return n == 0;
        }

        /**
         * Is {@code i} an index on this priority queue?
         *
         * @param i an index
         * @return {@code true} if {@code i} is an index on this priority queue;
         * {@code false} otherwise
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         */
        public boolean contains(int i) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            return qp[i] != -1;
        }

        /**
         * Returns the number of keys on this priority queue.
         *
         * @return the number of keys on this priority queue
         */
        public int size() {
            return n;
        }

        /**
         * Associates key with index {@code i}.
         *
         * @param i   an index
         * @param key the key to associate with index {@code i}
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws IllegalArgumentException if there already is an item associated
         *                                  with index {@code i}
         */
        public void insert(int i, Key key) {
            if (i < 0) throw new IllegalArgumentException();
            if (i >= maxN) throw new IllegalArgumentException();
            if (contains(i)) throw new IllegalArgumentException("index is already in the priority queue");
            n++;
            qp[i] = n;
            pq[n] = i;
            keys[i] = key;
            swim(n);
        }

        /**
         * Returns an index associated with a minimum key.
         *
         * @return an index associated with a minimum key
         * @throws NoSuchElementException if this priority queue is empty
         */
        public int minIndex() {
            if (n == 0) throw new NoSuchElementException("Priority queue underflow");
            return pq[1];
        }

        /**
         * Returns a minimum key.
         *
         * @return a minimum key
         * @throws NoSuchElementException if this priority queue is empty
         */
        public Key minKey() {
            if (n == 0) throw new NoSuchElementException("Priority queue underflow");
            return keys[pq[1]];
        }

        /**
         * Removes a minimum key and returns its associated index.
         *
         * @return an index associated with a minimum key
         * @throws NoSuchElementException if this priority queue is empty
         */
        public int delMin() {
            if (n == 0) throw new NoSuchElementException("Priority queue underflow");
            int min = pq[1];
            exch(1, n--);
            sink(1);
            assert min == pq[n + 1];
            qp[min] = -1;        // delete
            keys[min] = null;    // to help with garbage collection
            pq[n + 1] = -1;        // not needed
            return min;
        }

        /**
         * Returns the key associated with index {@code i}.
         *
         * @param i the index of the key to return
         * @return the key associated with index {@code i}
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws NoSuchElementException   no key is associated with index {@code i}
         */
        public Key keyOf(int i) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
            else return keys[i];
        }

        /**
         * Change the key associated with index {@code i} to the specified value.
         *
         * @param i   the index of the key to change
         * @param key change the key associated with index {@code i} to this key
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws NoSuchElementException   no key is associated with index {@code i}
         */
        public void changeKey(int i, Key key) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
            keys[i] = key;
            swim(qp[i]);
            sink(qp[i]);
        }

        /**
         * Change the key associated with index {@code i} to the specified value.
         *
         * @param i   the index of the key to change
         * @param key change the key associated with index {@code i} to this key
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @deprecated Replaced by {@code changeKey(int, Key)}.
         */
        @Deprecated
        public void change(int i, Key key) {
            changeKey(i, key);
        }

        /**
         * Decrease the key associated with index {@code i} to the specified value.
         *
         * @param i   the index of the key to decrease
         * @param key decrease the key associated with index {@code i} to this key
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws IllegalArgumentException if {@code key >= keyOf(i)}
         * @throws NoSuchElementException   no key is associated with index {@code i}
         */
        public void decreaseKey(int i, Key key) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
            if (keys[i].compareTo(key) <= 0)
                throw new IllegalArgumentException("Calling decreaseKey() with given argument would not strictly decrease the key");
            keys[i] = key;
            swim(qp[i]);
        }

        /**
         * Increase the key associated with index {@code i} to the specified value.
         *
         * @param i   the index of the key to increase
         * @param key increase the key associated with index {@code i} to this key
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws IllegalArgumentException if {@code key <= keyOf(i)}
         * @throws NoSuchElementException   no key is associated with index {@code i}
         */
        public void increaseKey(int i, Key key) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
            if (keys[i].compareTo(key) >= 0)
                throw new IllegalArgumentException("Calling increaseKey() with given argument would not strictly increase the key");
            keys[i] = key;
            sink(qp[i]);
        }

        /**
         * Remove the key associated with index {@code i}.
         *
         * @param i the index of the key to remove
         * @throws IllegalArgumentException unless {@code 0 <= i < maxN}
         * @throws NoSuchElementException   no key is associated with index {@code i}
         */
        public void delete(int i) {
            if (i < 0 || i >= maxN) throw new IllegalArgumentException();
            if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
            int index = qp[i];
            exch(index, n--);
            swim(index);
            sink(index);
            keys[i] = null;
            qp[i] = -1;
        }


        /***************************************************************************
         * General helper functions.
         ***************************************************************************/
        private boolean greater(int i, int j) {
            return keys[pq[i]].compareTo(keys[pq[j]]) > 0;
        }

        private void exch(int i, int j) {
            int swap = pq[i];
            pq[i] = pq[j];
            pq[j] = swap;
            qp[pq[i]] = i;
            qp[pq[j]] = j;
        }


        /***************************************************************************
         * Heap helper functions.
         ***************************************************************************/
        private void swim(int k) {
            while (k > 1 && greater(k / 2, k)) {
                exch(k, k / 2);
                k = k / 2;
            }
        }

        private void sink(int k) {
            while (2 * k <= n) {
                int j = 2 * k;
                if (j < n && greater(j, j + 1)) j++;
                if (!greater(k, j)) break;
                exch(k, j);
                k = j;
            }
        }


        /***************************************************************************
         * Iterators.
         ***************************************************************************/

        /**
         * Returns an iterator that iterates over the keys on the
         * priority queue in ascending order.
         * The iterator doesn't implement {@code remove()} since it's optional.
         *
         * @return an iterator that iterates over the keys in ascending order
         */
        public Iterator<Integer> iterator() {
            return new HeapIterator();
        }

        private class HeapIterator implements Iterator<Integer> {
            // create a new pq
            private IndexMinPQ<Key> copy;

            // add all elements to copy of heap
            // takes linear time since already in heap order so no keys move
            public HeapIterator() {
                copy = new IndexMinPQ<Key>(pq.length - 1);
                for (int i = 1; i <= n; i++)
                    copy.insert(pq[i], keys[pq[i]]);
            }

            public boolean hasNext() {
                return !copy.isEmpty();
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }

            public Integer next() {
                if (!hasNext()) throw new NoSuchElementException();
                return copy.delMin();
            }
        }
    }
}
