package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Diff {
    private final static short[][] matrix;
    
    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("diff.bin");

        final int leftNum = Misc.readInt(in);
        final int rightNum = Misc.readInt(in);
        matrix = new short[leftNum][rightNum];
        
        for(int l=0; l < leftNum; l++) 
            for(int r=0; r < rightNum; r++)
                matrix[l][r] = Misc.readShort(in);

        Misc.close(in);
    }
    
    // posId1よりposId2が低いコストになるには、その差がどれだけ以内である必要があるかを返す
    public static short cost(short posId1, short posId2) {
        return matrix[posId1][posId2];
    }
}
