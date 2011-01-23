package net.reduls.gomoku.util;

import java.io.InputStream;
import java.io.BufferedReader;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.IOException;

public final class Misc {
    public static InputStream openDictionaryData(String filename) {
        return Misc.class.getResourceAsStream("/net/reduls/gomoku/dicdata/"+filename);
    }

    public static DataInputStream openDictionaryDataAsDIS(String filename) {
        return new DataInputStream(new BufferedInputStream(openDictionaryData(filename)));
    }

    public static String readLine(BufferedReader in) {
        try {
            return in.readLine();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }
    
    public static void close(Closeable in) {
        try {
            in.close();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static int readInt(DataInput in) {
        try {
            return in.readInt();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static short readShort(DataInput in) {
        try {
            return in.readShort();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static byte readByte(DataInput in) {
        try {
            return in.readByte();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }

    public static char readChar(DataInput in) {
        try {
            return in.readChar();
        } catch (IOException ex) {
            throw new AssertionError(ex.getMessage());
        }
    }
}