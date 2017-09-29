import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;

public class SuffixTreeJava {
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

    int letterToIndex(char letter) {
        switch (letter) {
            case 'A':
                return 0;
            case 'C':
                return 1;
            case 'G':
                return 2;
            case 'T':
                return 3;
            case '$':
                return 4;
            default:
                assert (false);
                return JavaNode.NA;
        }
    }

    List<JavaNode> textToTree(String text) {
        List<JavaNode> tree = new ArrayList<>();
        int count = 0;
        tree.add(new JavaNode(0, -1, count++));
        int length = text.length();

        for (int j = 0; j < length; j++) {
            int initialStart = length - 1 - j;
            int initialOffset = j;
            JavaNode currentJavaNode = tree.get(0);
            while (currentJavaNode.next[letterToIndex(text.charAt(initialStart))] > 0) {
                currentJavaNode = tree.get(currentJavaNode.next[letterToIndex(text.charAt(initialStart))]);
                int currentStart = currentJavaNode.start;
                int currentOffset = currentJavaNode.offset;
                int removeIndex = 1;
                for (int i = 1; i < currentOffset + 1; i++) {
                    if (text.charAt(currentStart + i) != text.charAt(initialStart + i)) {
                        break;
                    }
                    removeIndex++;
                }

                if (currentOffset + 1 - removeIndex > 0) {
                    JavaNode newJavaNode = new JavaNode(currentStart + removeIndex, currentOffset - removeIndex, count++);
                    currentJavaNode.start = initialStart;
                    currentJavaNode.offset = removeIndex - 1;
                    tree.add(newJavaNode);
                    if (currentJavaNode.haveNeighbours) {
                        newJavaNode.next = Arrays.copyOf(currentJavaNode.next, currentJavaNode.next.length);
                        newJavaNode.haveNeighbours = true;
                        currentJavaNode.initNext();
                    }
                    currentJavaNode.next[letterToIndex(text.charAt(newJavaNode.start))] = newJavaNode.id;
                    currentJavaNode.haveNeighbours = true;
                }
                initialStart += removeIndex;
                initialOffset -= removeIndex;
            }
            JavaNode newJavaNode = new JavaNode(initialStart, initialOffset, count++);
            tree.add(newJavaNode);
            currentJavaNode.next[letterToIndex(text.charAt(initialStart))] = newJavaNode.id;
            currentJavaNode.haveNeighbours = true;
        }
        return tree;
    }

    // Build a suffix tree of the string text and return a list
    // with all of the labels of its edges (the corresponding
    // substrings of the text) in any order.
    public List<String> computeSuffixTreeEdges(String text) {
        List<String> result = new ArrayList<String>();
        List<JavaNode> tree = textToTree(text);
        Queue<Integer> queue = new LinkedList<>();
        queue.add(0);
        while (!queue.isEmpty()) {
            JavaNode currentJavaNode = tree.get(queue.poll());
            if (currentJavaNode.offset != -1) {
                result.add(text.substring(currentJavaNode.start, currentJavaNode.start + currentJavaNode.offset + 1));
            }
            for (int i = 0; i < JavaNode.Letters; i++) {
                if (currentJavaNode.next[i] > 0) queue.add(currentJavaNode.next[i]);
            }
        }
        return result.stream().map(String::trim).collect(Collectors.toList());
    }


    static public void main(String[] args) throws IOException {
        new SuffixTreeJava().run();
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
}

class JavaNode {
    public static final int Letters = 5;
    public static final int NA = -1;
    public int next[];
    int start;
    int offset;
    public int id;
    public boolean haveNeighbours;

    JavaNode() {
        next = new int[Letters];
        Arrays.fill(next, NA);
    }


    JavaNode(int start, int offset, int id) {
        this();
        this.start = start;
        this.offset = offset;
        this.id = id;
    }

    public void initNext() {
        Arrays.fill(next, NA);
        haveNeighbours = false;
    }
}