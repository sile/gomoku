package net.reduls.gomoku.dic;

public final class ViterbiNode {
    public int cost = 0;
    public ViterbiNode prev = null;
    
    public final Morpheme.Info word;
    public final int start;
    public final short length;
    public final boolean isSpace;
    
    public ViterbiNode(int start, short length, Morpheme.Info word, boolean isSpace) {
        this.start = start;
        this.length = length;
        this.word = word;
        this.isSpace = isSpace;
    }

    public static ViterbiNode makeBOSEOS() {
        return new ViterbiNode(0, (short)0, null, false);
    }
}