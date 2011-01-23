package net.reduls.gomoku;

import java.util.List;
import java.util.ArrayList;
import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

public final class CodeCategory {
    public static final class Category {
        public final int id; // XXX: 不要にしたい?
        public final boolean invoke;
        public final boolean group;
        public final byte length;
        
        public Category(int id, boolean invoke, boolean group, byte length) {
            this.id = id;
            this.invoke = invoke;
            this.group = group;
            this.length = length;
        }
    }

    public static final class Code {
        public final Category category;
        public final short mask;

        public Code(Category category, short mask) {
            this.category = category;
            this.mask = mask;
        }
    }

    public final static List<Code> codes;

    static {
        List<Category> categorys = new ArrayList<Category>();
        try {
            DataInputStream in =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/category.bin")));
            for(int id=0;; id++) 
                categorys.add(new Category(id,
                                           in.readByte()==1,
                                           in.readByte()==1,
                                           in.readByte()));
        } catch(IOException ex) {}
        for(Category c : categorys)
            System.out.println(c.invoke+"_"+c.group+"_"+c.length);
        
        codes = new ArrayList<Code>(0x10000);
        try {
            DataInputStream in =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/code.bin")));
            for(int i=0; i < 0x10000; i++) 
                codes.add(new Code(categorys.get(in.readByte()),
                                   in.readShort()));
            in.close();
        } catch(IOException ex) {}        
    }

    public static boolean isCompatible(char code1, char code2) {
        return (codes.get(code1).mask & codes.get(code2).mask) != 0;
    }

    public static void main(String[] args) {
        for(Code c : codes) 
            System.out.println(c.category.invoke+":"+
                               c.category.group+":"+
                               c.category.length+"#"+
                               c.mask);
    }
}