package net.reduls.gomoku;

import java.util.List;
import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

public final class WordId {
    public static final int nodeCount;
    public static final long[] nodes;
    public static final char[] codeMap = new char[0x10000];
    
    static {
        int tmpNodeCount = 0;
        long[] tmpNodes = null;
        try {
            DataInputStream in =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/word-id.bin")));
            
            tmpNodeCount = in.readInt();
            tmpNodes = new long[tmpNodeCount];
            
            for(int i=0; i < tmpNodeCount; i++)
                tmpNodes[i] = in.readLong();

            in.close();
        } catch (IOException ex) {}
        nodeCount = tmpNodeCount;
        nodes = tmpNodes;

        try {
            DataInputStream in =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/code-map.bin")));
            
            for(int i=0; i < tmpNodeCount; i++)
                codeMap[i] = in.readChar();
            
            in.close();
        } catch (IOException ex) {}        
    }

    public static int getId (String str) {
        int node = 0;
        int id = -1;
        int i = 0;
        
        for(;; i++) {
            if(i==str.length())
                return is_terminal(node) ? incId(id,node) : -1;
            char arc = codeMap[str.charAt(i)];
            int next = base(node)+arc;
            if(chck(next) != arc)
                return -1;
            id = incId(id, node);
            node = next;
        }
    }

    public static interface Callback {
        void call(int start, int offset, List<Morp.Info> morps);
    }
 
    public static void eachCommonPrefix(String str, int start, Callback fn) {
        int node = 0;
        int id = -1;
        int i = start;

        for(;; i++) {

            if(is_terminal(node))
                fn.call(start, i, Morp.surface_morps_map.get(incId(id,node)));

            if(i==str.length())
                return;
            char arc = codeMap[str.charAt(i)];
            int next = base(node)+arc;
            if(chck(next) != arc)
                return;
            id = incId(id, node);
            node = next;
        }
    }

    // XXX: 読み込み時にデコードしておいた方が良い?
    private static char chck(int node) {
        return (char)((nodes[node]>>24) & 0xFFFF);
    }

    private static int base(int node) {
        return (int)(nodes[node] & 0xFFFFFF);
    }

    private static boolean is_terminal(int node) {
        return ((nodes[node]>>40) & 0x1) == 0x1;
    }

    private static int siblingTotal(int node) {
        return (int)(nodes[node]>>41);
    }

    private static int incId(int id, int node) {
        return id + (is_terminal(node) ? 1 : 0) + siblingTotal(node);
    }

    //
    public static final class SampleCallback implements Callback {
        public void call(int start, int offset, List<Morp.Info> morps) {
            System.out.println(start+"~"+offset+":");
            for(Morp.Info mi : morps) 
                System.out.println("\t"+mi.posId+", "+mi.cost);
            System.out.println("");
        }
    }

    public static void main(String[] args) {
        WordId.eachCommonPrefix(args[0], 0, new SampleCallback());
    }

    /*
      public static void main(String[] args) {
      System.out.println("=> "+WordId.getId(args[0]));
      }
    */
}
