package net.reduls.gomoku.dic;

import java.util.List;

public final class Unknown {
    private static final Char.Category space = Char.category(' ');
    
    public static void search(String text, int start, List<ViterbiNode> result) {
        final char ch = text.charAt(start);
        final Char.Category ct = Char.category(ch);
        
        if(result.isEmpty()==false && ct.invoke==false)
            return;
        
        final boolean isSpace = ct==space;
        final int limit = Math.min(text.length(), ct.length+start);
        int i=start;
        for(; i < limit; i++) {
            WordDic.search(ct.id, start, (i-start)+1, isSpace, result);
            if(i+1!=limit && Char.isCompatible(ch, text.charAt(i+1))==false)
                return;
        }

        if(ct.group && i < text.length()) {
            for(; i < text.length(); i++)
                if(Char.isCompatible(ch, text.charAt(i)) == false) {
                    WordDic.search(ct.id, start, i-start, isSpace, result);
                    return;
                }
            WordDic.search(ct.id, start, text.length()-start, isSpace, result);
        }
    }
}