package net.reduls.gomoku;

import java.util.List;
import java.util.ArrayList;
import net.reduls.gomoku.dic.ViterbiNode;
import net.reduls.gomoku.dic.WordDic;
import net.reduls.gomoku.dic.Unknown;
import net.reduls.gomoku.dic.Matrix;
import net.reduls.gomoku.dic.PartsOfSpeech;

public final class Tagger {
    static class ViterbiNodeList extends ArrayList<ViterbiNode> {}
    
    private static final ViterbiNodeList BOS_NODES = new ViterbiNodeList();
    static {
        BOS_NODES.add(ViterbiNode.makeBOSEOS());
    }
    
    public static List<Morpheme> parse(String text) {
        return parse(text, new ArrayList<Morpheme>(text.length()/2));
    }

    public static List<Morpheme> parse(String text, List<Morpheme> result) {
        for(ViterbiNode vn=parseImpl(text); vn!=null; vn=vn.prev) {
            final String surface = text.substring(vn.start, vn.start+vn.length);
            final String feature = PartsOfSpeech.get(vn.posId);
            result.add(new Morpheme(surface, feature, vn.start));
        }
        return result;
    }

    public static List<String> wakati(String text) {
        return wakati(text, new ArrayList<String>(text.length()/1));
    }

    public static List<String> wakati(String text, List<String> result) {
        for(ViterbiNode vn=parseImpl(text); vn!=null; vn=vn.prev)
            result.add(text.substring(vn.start, vn.start+vn.length));
        return result;
    }
    
    public static ViterbiNode parseImpl(String text) {
        final int len = text.length();
        final ViterbiNodeList[] nodesAry = new ViterbiNodeList[len+1];
        nodesAry[0] = BOS_NODES;
        
        MakeLattice fn = new MakeLattice(nodesAry);
        for(int i=0; i < len; i++) {
            if(nodesAry[i] != null) {
                fn.set(i);
                WordDic.search(text, i, fn);
                Unknown.search(text, i, fn);
            }
        }

        ViterbiNode cur = setMincostNode(ViterbiNode.makeBOSEOS(), nodesAry[len]).prev;
        ViterbiNode head = null;
        while(cur.prev != null) {
            final ViterbiNode tmp = cur.prev;
            cur.prev = head;
            head = cur;
            cur = tmp;
        }
        return head;
    }

    private static ViterbiNode setMincostNode(ViterbiNode vn, ViterbiNodeList prevs) {
        final ViterbiNode f = vn.prev = prevs.get(0);
        int minCost = f.cost + Matrix.linkCost(f.posId, vn.posId);
        
        for(int i=1; i < prevs.size(); i++) {
            final ViterbiNode p = prevs.get(i);
            final int cost = p.cost + Matrix.linkCost(p.posId, vn.posId);

            if(cost < minCost) {
                minCost = cost;
                vn.prev = p;
            }
        }
        vn.cost += minCost;
        
        return vn;
    }

    private static final class MakeLattice implements WordDic.Callback {
        private final ViterbiNodeList[] nodesAry;
        private int i;
        private ViterbiNodeList prevs;
        private boolean empty=true;

        public MakeLattice(ViterbiNodeList[] nodesAry) {
            this.nodesAry = nodesAry;
        }
        
        public void set(int i) {
            this.i = i;
            prevs = nodesAry[i];
            nodesAry[i] = null;
            empty = true;
        }

        public void call(ViterbiNode vn) {
            empty=false;

            final int end = i+vn.length;
            if(nodesAry[end]==null)
                nodesAry[end] = new ViterbiNodeList();

            if(vn.isSpace)
                nodesAry[end].addAll(prevs);
            else
                nodesAry[end].add(setMincostNode(vn, prevs));
        }
        
        public boolean isEmpty() { return empty; }
    }
}
