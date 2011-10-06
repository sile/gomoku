package net.reduls.gomoku.dic;

import java.io.BufferedReader;
import java.util.ArrayList;
import net.reduls.gomoku.util.Misc;

public final class PartsOfSpeech {
    private static final String[] posArray;

    static {
        BufferedReader in = Misc.openDictionaryDataAsBR("pos.bin");
        ArrayList<String> lines = new ArrayList<String>();

        for(String line=Misc.readLine(in); line!=null; line=Misc.readLine(in))
            lines.add(line);
        Misc.close(in);

        posArray = new String[lines.size()];
        for(int i=0; i < posArray.length; i++)
            posArray[i] = lines.get(i);
    }

    public static final String get(int posId) {
        return posArray[posId];
    }
}