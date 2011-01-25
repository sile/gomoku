package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Morpheme {
    public static final class Info {
        public final short cost;
        public final short posId;

        public Info(short posId, short cost) {
            this.cost = cost;
            this.posId = posId;
        }
        
        public String feature() {
            return PartsOfSpeech.get(posId);
        }
    }

    private static final Info[][] surId_to_morps;
    
    static {
        DataInputStream in1 = Misc.openDictionaryDataAsDIS("id-morphemes-map.bin");
        DataInputStream in2 = Misc.openDictionaryDataAsDIS("morpheme.bin");

        final int surfaceIdLimit = Misc.readInt(in1);
        final int morphemeCount = Misc.readInt(in2);
        surId_to_morps = new Info[surfaceIdLimit][];

        for(int i=0; i < surfaceIdLimit; i++) {
            final byte length = Misc.readByte(in1);
            final Info[] morps = new Info[length];
            for(int j=0; j < length; j++)
                morps[j] = new Info(Misc.readShort(in2), Misc.readShort(in2));
            surId_to_morps[i] = morps;
        }
        
        Misc.close(in1);
        Misc.close(in2);
    }
    
    public static Info[] getMorphemes(int surfaceId) {
        return surId_to_morps[surfaceId];
    }
}