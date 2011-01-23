package net.reduls.gomoku;

import java.util.List;

public final class Unknown {
    private static final CodeCategory.Category space = 
        CodeCategory.codes.get(' ').category;

    public static void search(String text, int start, List<ViterbiNode> result) {
        final char ch = text.charAt(start);
        final CodeCategory.Category ct = CodeCategory.codes.get(ch).category;
        
        if(result.isEmpty()==false && ct.invoke==false)
            return;

        final boolean isSpace = ct==space;
        final int limit = Math.min(text.length(), ct.length+start);
        int i=start;
        for(; i < limit; i++) {
            WordDic.searchFromTrieId(ct.id, start, (i-start)+1, isSpace, result);
            if(i+1!=limit && CodeCategory.isCompatible(ch, text.charAt(i+1))==false)
                return;
        }

        if(ct.group && i < text.length()) {
            for(; i < text.length(); i++)
                if(CodeCategory.isCompatible(ch, text.charAt(i)) == false) {
                    WordDic.searchFromTrieId(ct.id, start, i-start, isSpace, result);
                    return;
                }
            WordDic.searchFromTrieId(ct.id, start, text.length()-start, isSpace, result);
        }
    }
}