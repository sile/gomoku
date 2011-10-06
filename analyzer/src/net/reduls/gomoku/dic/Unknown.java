package net.reduls.gomoku.dic;

public final class Unknown {
    private static final Char.Category space = Char.category(' ');

    public static void search(String text, int start, WordDic.Callback fn) {
        final char ch = text.charAt(start);
        final Char.Category ct = Char.category(ch);

        if(fn.isEmpty()==false && ct.invoke==false)
            return;

        final boolean isSpace = ct==space;
        final int limit = Math.min(text.length(), ct.length+start);
        int i=start;
        for(; i < limit; i++) {
            WordDic.eachViterbiNode(fn, ct.id, start, (i-start)+1, isSpace);
            if(i+1!=limit && Char.isCompatible(ch, text.charAt(i+1))==false)
                return;
        }

        if(ct.group && i < text.length()) {
            for(; i < text.length(); i++)
                if(Char.isCompatible(ch, text.charAt(i)) == false) {
                    WordDic.eachViterbiNode(fn, ct.id, start, i-start, isSpace);
                    return;
                }
            WordDic.eachViterbiNode(fn, ct.id, start, text.length()-start, isSpace);
        }
    }
}