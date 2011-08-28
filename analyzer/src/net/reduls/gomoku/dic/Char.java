package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Char {
    public static final class Category {
        public final int id;
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
    
    private final static Category[] categorys;
    private final static short[] compatibleMasks;

    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("category.bin");
        
        final int charCategoryNum = Misc.readInt(in);
        Category[] charCategorys = new Category[charCategoryNum];
        for(int i=0; i < charCategoryNum; i++)
            charCategorys[i] = new Category(i,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in));
        Misc.close(in);
        
        
        DataInputStream in2 = Misc.openDictionaryDataAsDIS("code.bin");
        
        final int codeLimit = Misc.readInt(in2);
        categorys = new Category[codeLimit];
        compatibleMasks = new short[codeLimit];
        for(int i=0; i < codeLimit; i++) {
            categorys[i] = charCategorys[Misc.readByte(in2)];
            compatibleMasks[i] = Misc.readShort(in2);
        }
        Misc.close(in2);
    }

    public static final Category category(char ch) {
        return categorys[ch];
    }

    public static final boolean isCompatible(char c1, char c2) {
        return (compatibleMasks[c1] & compatibleMasks[c2]) != 0;
    }
}