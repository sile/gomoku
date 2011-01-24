package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class SurfaceId {
    private static final int idOffset;
    private static final int nodeCount;
    private static final long[] nodes;
    
    static {
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("surface-id.bin");
            
            nodeCount = Misc.readInt(in);
            nodes = new long[nodeCount];
            for(int i=0; i < nodeCount; i++)
                nodes[i] = Misc.readLong(in);
            Misc.close(in);
        }
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("category.bin");
            idOffset = Misc.readInt(in);
            Misc.close(in);
        }
    }
 
   public static interface Callback {
        void call(int start, int end, int surfaceId);
    }
 
    public static int get(String surface) {
        int node = 0;
        int id = idOffset-1;
        
        for(int i=0;; i++) {
            if(i==surface.length())
                return isTerminal(node) ? incId(id,node) : -1;

            final char arc = Char.code(surface.charAt(i));
            final int next = base(node)+arc;
            if(chck(next) != arc)
                return -1;
            id = incId(id, node);
            node = next;
        }
    }

    public static void eachCommonPrefix(String text, int start, Callback fn) {
        int node = 0;
        int id = idOffset-1;
        
        for(int i=start;; i++) {
            if(isTerminal(node))
                fn.call(start, i, incId(id,node));
            
            if(i==text.length())
                return;
            
            final char arc = Char.code(text.charAt(i));
            final int next = base(node)+arc;
            if(chck(next) != arc)
                return;
            id = incId(id, node);
            node = next;
        }
    }

    private static char chck(int node) {
        return (char)((nodes[node]>>24) & 0xFFFF);
    }
    
    private static int base(int node) {
        return (int)(nodes[node] & 0xFFFFFF);
    }

    private static boolean isTerminal(int node) {
        return ((nodes[node]>>40) & 0x1) == 0x1;
    }

    private static int siblingTotal(int node) {
        return (int)(nodes[node]>>41);
    }

    private static int incId(int id, int node) {
        return id + (isTerminal(node) ? 1 : 0) + siblingTotal(node);
    }
}