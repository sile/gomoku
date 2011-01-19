package net.reduls.gomoku;

import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

public final class Matrix {
    public final static short[][] matrix;
    
    static {
        DataInputStream in = 
            new DataInputStream
            (new BufferedInputStream
             (Matrix.class.getResourceAsStream("data/matrix.bin")));
        try {
            final int leftNum = Util.readInt(in);
            final int rightNum = Util.readInt(in);
            matrix = new short[leftNum][rightNum];

            for(int l=0; l < leftNum; l++) 
                for(int r=0; r < rightNum; r++)
                    matrix[l][r] = Util.readShort(in);
        } finally {
            try {
                in.close();
            } catch (Exception ex) {
            }
        }
    }

    public static void main(String[] args) {
        for (short[] nn : matrix)
            for(short n : nn)
                System.out.println(n);
    }
}

