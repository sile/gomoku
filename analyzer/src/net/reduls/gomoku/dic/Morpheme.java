package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Morpheme {
    private static final int[][] surId_to_morps;
    
    static {
        DataInputStream in1 = Misc.openDictionaryDataAsDIS("id-morphemes-map.bin");
        DataInputStream in2 = Misc.openDictionaryDataAsDIS("morpheme.bin");

        final int surfaceIdLimit = Misc.readInt(in1);
        final int morphemeCount = Misc.readInt(in2);
        surId_to_morps = new int[surfaceIdLimit][];

        for(int i=0; i < surfaceIdLimit; i++) {
            final byte length = Misc.readByte(in1);
            final int[] morps = new int[length];
            for(int j=0; j < length; j++)
                morps[j] = encode_info(Misc.readShort(in2), Misc.readShort(in2));
            surId_to_morps[i] = morps;
        }
        
        Misc.close(in1);
        Misc.close(in2);
    }
    
    public static int[] getMorphemes(int surfaceId) {
        return surId_to_morps[surfaceId];
    }

    public static short posId(int info) {
        return (short)(info>>16);
    }

    public static short cost(int info) {
        return (short)(info&0xFFFF);
    }

    private static int encode_info(short posId, short cost) {
        return (posId << 16) + (cost & 0xFFFF);
    }
}