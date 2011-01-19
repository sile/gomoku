package net.reduls.gomoku;

import java.io.DataInputStream;
import java.io.IOException;

public final class Util {
    static int readInt(DataInputStream in) {
        try {
            return in.readInt();
        } catch (IOException ex) {
            return 0;
        }
    }

    static short readShort(DataInputStream in) {
        try {
            return in.readShort();
        } catch (IOException ex) {
            return 0;
        }
    }
}

