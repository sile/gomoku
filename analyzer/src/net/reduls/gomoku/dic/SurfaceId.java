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
        long node = nodes[0];
        int id = idOffset;

        final CodeStream in = new CodeStream(text,start);
        for(;;) {
            if(isTerminal(node))
                WordDic.eachViterbiNode(fn, id++, start, in.position(), false);
            
            if(in.isEos())
                return;
            
            if(checkEncodedChildren(in, node)==false)
                return;
            
            final char arc = in.read();
            final long next = nodes[base(node)+arc];
            if(chck(next) != arc)
                return;
            node = next;
            id += siblingTotal(node);
        }
    }

    private static boolean checkEncodedChildren(CodeStream in, long node) {
        switch(type(node)) {
        case 0:
            return checkEC(in,node,0) && checkEC(in,node,1);
        case 1:
            return checkEC(in,node,0);
        default:
            return true;
        }
    }
    private static boolean checkEC(CodeStream in, long node, int n) {
        char chck = (char)((node>>(40+8*n)) & 0xFF);
        return chck==0 || (in.read() == chck &&
                           in.isEos() == false);
    }

    private static char chck(long node) {
        return (char)((node>>32) & 0xFF);
    }
    
    private static int base(long node) {
        return (int)(node & 0x1FFFFFFF);
    }

    private static boolean isTerminal(long node) {
        return ((node>>31) & 1)==1;
    }

    private static int type(long node) {
        return (int)((node>>29) & 3);
    }

    private static int siblingTotal(long node) {
        switch (type(node)) {
        case 0:
            return (int)((node>>56) & 0xFF);
        case 1:
            return (int)((node>>48) & 0xFFFF);
        default:
            return (int)((node>>40) & 0xFFFFFF);
        }
    }
}