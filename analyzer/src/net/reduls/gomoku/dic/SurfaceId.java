package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class SurfaceId {
    private static final int idOffset;
    private static final long[] nodes;
    
    static {
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("surface-id.bin");
            
            final int nodeCount = Misc.readInt(in);
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
 
    public static void eachCommonPrefix(String text, int start, WordDic.Callback fn) {
        int node = 0;
        int id = idOffset;
        
        for(int i=start;; i++) {
            if(isTerminal(node))
                WordDic.eachViterbiNode(fn, id++, start, i-start, false);
            
            if(i==text.length())
                return;
            
            final char arc = Char.code(text.charAt(i));
            final int next = base(node)+arc;
            if(chck(next) != arc)
                return;
            node = next;
            id += siblingTotal(node);
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
}