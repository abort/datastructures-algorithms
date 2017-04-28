import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.StringTokenizer;

public class BuildHeapJava {
    private int[] data;
    private List<Swap> swaps;

    private FastScanner in;
    private PrintWriter out;

    public static void main(String[] args) throws IOException {
        new BuildHeapJava().solve();
    }

    private void readData() throws IOException {
        int n = in.nextInt();
        data = new int[n];
        for (int i = 0; i < n; ++i) {
            data[i] = in.nextInt();
        }
    }

    private void writeResponse() {
        out.println(swaps.size());
        for (Swap swap : swaps) {
            out.println(swap.index1 + " " + swap.index2);
        }
    }

    private void generateSwaps() {
        swaps = new ArrayList<Swap>();
        final Stack<Integer> work = new Stack<Integer>();
        for (int i = 0; i < data.length / 2; i++) work.push(i);

        while (!work.isEmpty()) {
            final int i = work.pop();
            int minIndex = i;

            final int childrenOffset = i * 2 + 1;
            if (childrenOffset < data.length && data[childrenOffset] < data[minIndex]) {
                minIndex = childrenOffset;
            }
            if (childrenOffset + 1 < data.length && data[childrenOffset + 1] < data[minIndex]) {
                minIndex = childrenOffset + 1;
            }

            if (minIndex != i) {
                final int rootValue = data[i];
                data[i] = data[minIndex];
                data[minIndex] = rootValue;
                swaps.add(new Swap(i, minIndex));
                work.push(minIndex);
            }
        }
    }

    public void solve() throws IOException {
        in = new FastScanner();
        out = new PrintWriter(new BufferedOutputStream(System.out));
        readData();
        generateSwaps();
        writeResponse();
        out.close();
    }

    static class Swap {
        int index1;
        int index2;

        public Swap(int index1, int index2) {
            this.index1 = index1;
            this.index2 = index2;
        }
    }

    static class FastScanner {
        private BufferedReader reader;
        private StringTokenizer tokenizer;

        public FastScanner() {
            reader = new BufferedReader(new InputStreamReader(System.in));
            tokenizer = null;
        }

        public String next() throws IOException {
            while (tokenizer == null || !tokenizer.hasMoreTokens()) {
                tokenizer = new StringTokenizer(reader.readLine());
            }
            return tokenizer.nextToken();
        }

        public int nextInt() throws IOException {
            return Integer.parseInt(next());
        }
    }
}
