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
            final int extCount = Misc.readInt(in);  // XXX: unused
            
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

        CodeStream in = new CodeStream(text,start);
        for(int i=start;; i++) {
            if(isTerminal(node))
                WordDic.eachViterbiNode(fn, id++, start, in.position(), false);
            
            if(in.isEos())
                return;
            
            final char arc = in.read();
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