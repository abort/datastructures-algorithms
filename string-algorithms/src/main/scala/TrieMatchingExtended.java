import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TrieMatchingExtended implements Runnable {
    class Node
    {
        public static final int Letters =  4;
        public Node next [];
        public boolean pattern = false;

        Node ()
        {
            next = new Node[Letters];
        }
    }


    int letterToIndex (char letter)
    {
        switch (letter)
        {
            case 'A': return 0;
            case 'C': return 1;
            case 'G': return 2;
            case 'T': return 3;
            default: assert (false); return -1;
        }
    }

    Set <Integer> solve (String text, int n, int minPatternLength, List <String> patterns) {
        Set<Integer> result = new HashSet<>();

        final Node root = new Node();
        for (String p : patterns) {
            Node node = root;
            char[] arr = p.toCharArray();
            for (int i = 0; i < arr.length; i++) {
                final int index = letterToIndex(arr[i]);
                if (node.next[index] == null) {
                    node.next[index] = new Node();
                }
                node = node.next[index];
                if (i == arr.length - 1) {
                    node.pattern = true;
                }
            }
        }

        int upperbound = text.length() - minPatternLength;
        for (int i = 0; i <= upperbound; i++) {
            Node node = root;
            for (int j = i; j < text.length(); j++) {
                final char c = text.charAt(j);
                final Node child = node.next[letterToIndex(c)];
                if (child == null) break;

                node = child;
                if (node.pattern) {
                    result.add(i);
                }
            }
        }
        // write your code here

        return result;
    }

    public void run () {
        try {
            BufferedReader in = new BufferedReader (new InputStreamReader (System.in));
            String text = in.readLine ();
            int n = Integer.parseInt (in.readLine ());
            List <String> patterns = new ArrayList <String> ();
            int minPatternLength = Integer.MAX_VALUE;
            for (int i = 0; i < n; i++) {
                final String line = in.readLine ();
                patterns.add (line);
                if (line.length() < minPatternLength) minPatternLength = line.length();
            }

            Set <Integer> ans = solve (text, n, minPatternLength, patterns);
            ans.stream().sorted().forEach(a -> System.out.print(a + " "));
        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    public static void main (String [] args) {
        new Thread (new TrieMatchingExtended()).start ();
    }
}
