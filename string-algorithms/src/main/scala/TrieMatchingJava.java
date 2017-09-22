import com.sun.org.apache.xpath.internal.operations.Bool;

import java.io.*;
import java.util.*;
import java.util.function.Predicate;

class Node
{
    public static final int Letters =  4;
    public Node next [];

    public boolean isSentinel() {
        for (int i = 0; i < Letters; i++) {
            if (next[i] != null) return false;
        }
        return true;
    }
    Node ()
    {
        next = new Node [Letters];
    }
}

public class TrieMatchingJava implements Runnable {
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

    List <Integer> solve (String text, int n, int minPatternLength, List <String> patterns) {
        List <Integer> result = new ArrayList <Integer> ();

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
                if (node.isSentinel()) {
                    result.add(i);
                    break;
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

            List <Integer> ans = solve (text, n, minPatternLength, patterns);

            for (int j = 0; j < ans.size (); j++) {
                System.out.print ("" + ans.get (j));
                System.out.print (j + 1 < ans.size () ? " " : "\n");
            }
        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    public static void main (String [] args) {
        new Thread (new TrieMatchingJava()).start ();
    }
}
