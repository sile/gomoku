package net.reduls.gomoku;

public final class ViterbiNode {
    public int cost = 0;
    public ViterbiNode prev = null;
    
    public final int wcost;
    public final short posId;
    public final int start;
    public final short length;

    public final boolean isSpace;
    
    public ViterbiNode(int wcost, int beg, short len, short id, boolean space) {
        this.wcost = wcost;
        posId = id;
        length = len;
        isSpace = space;
        start = beg;
    }

    public static ViterbiNode makeBOSEOS() {
        return new ViterbiNode(0,0,(short)0,(short)0,false);
    }
}