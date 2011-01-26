package net.reduls.gomoku;

import java.util.List;
import java.util.ArrayList;
import net.reduls.gomoku.dic.ViterbiNode;
import net.reduls.gomoku.dic.WordDic;
import net.reduls.gomoku.dic.Unknown;
import net.reduls.gomoku.dic.Matrix;
import net.reduls.gomoku.dic.PartsOfSpeech;

public final class Tagger {
    private static final ArrayList<ViterbiNode> BOS_NODES = new ArrayList<ViterbiNode>(1);
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
    
    private static final class Fn implements WordDic.Callback {
        private ArrayList<ArrayList<ViterbiNode>> nodesAry;
        private final int i;
        private final ArrayList<ViterbiNode> prevs;
        private boolean first=true;

        public Fn(ArrayList<ArrayList<ViterbiNode>> nodesAry, int i) {
            this.nodesAry = nodesAry;
            this.i = i;
            prevs = nodesAry.get(i);
        }
        public void call(ViterbiNode vn) {
            first=false;
            if(vn.isSpace)
                nodesAry.get(i+vn.length).addAll(prevs);
            else
                nodesAry.get(i+vn.length).add(setMincostNode(vn, prevs));
        }
        public boolean isEmpty() { return first; }
    }

    public static ViterbiNode parseImpl(String text) {
        final int len = text.length();
        final ArrayList<ArrayList<ViterbiNode>> nodesAry = 
            new ArrayList<ArrayList<ViterbiNode>>(len+1);
        
        nodesAry.add(BOS_NODES);
        for(int i=1; i <= len; i++)
            nodesAry.add(new ArrayList<ViterbiNode>());
        
        for(int i=0; i < len; i++) {
            if(nodesAry.get(i).isEmpty()==false) {
                Fn fn = new Fn(nodesAry,i);
                WordDic.search(text, i, fn);
                Unknown.search(text, i, fn);
            }
        }

        ViterbiNode cur = setMincostNode(ViterbiNode.makeBOSEOS(), nodesAry.get(len)).prev;

        ViterbiNode head = null;
        while(cur.prev != null) {
            final ViterbiNode tmp = cur.prev;
            cur.prev = head;
            head = cur;
            cur = tmp;
        }
        return head;
    }

    private static ViterbiNode setMincostNode(ViterbiNode vn, ArrayList<ViterbiNode> prevs) {
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
}