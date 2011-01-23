package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Matrix {
    private final static short[][] matrix;
    
    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("matrix.bin");

        final int leftNum = Misc.readInt(in);
        final int rightNum = Misc.readInt(in);
        matrix = new short[leftNum][rightNum];
        
        for(int l=0; l < leftNum; l++) 
            for(int r=0; r < rightNum; r++)
                matrix[l][r] = Misc.readShort(in);

        Misc.close(in);
    }

    public static short linkCost(int leftId, int rightId) {
        return matrix[leftId][rightId];
    }
}
