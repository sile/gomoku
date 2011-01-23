package net.reduls.gomoku;

import java.util.List;
import java.util.ArrayList;

public final class Tagger {
    private static final ArrayList<ViterbiNode> BOS_NODES = new ArrayList<ViterbiNode>(1);
    static {
        BOS_NODES.add(ViterbiNode.makeBOSEOS());
    }
    
    public List<Morpheme> parse(String text) {
        return parse(text, new ArrayList<Morpheme>(text.length()/2));
    }

    public List<Morpheme> parse(String text, List<Morpheme> result) {
        for(ViterbiNode vn=parseImpl(text); vn!=null; vn=vn.prev) {
            final String surface = text.substring(vn.start, vn.start+vn.length);
            final String feature = WordDic.feature(vn.posId);
            result.add(new Morpheme(surface, feature, vn.start));
        }
        return result;
    }

    public List<String> wakati(String text) {
        return wakati(text, new ArrayList<String>(text.length()/1));
    }

    public List<String> wakati(String text, List<String> result) {
        for(ViterbiNode vn=parseImpl(text); vn!=null; vn=vn.prev)
            result.add(text.substring(vn.start, vn.start+vn.length));
        return result;
    }
    
    public ViterbiNode parseImpl(String text) {
        final int len = text.length();
        final ArrayList<ArrayList<ViterbiNode>> nodesAry = 
            new ArrayList<ArrayList<ViterbiNode>>(len+1);
        final ArrayList<ViterbiNode> perResult = new ArrayList<ViterbiNode>();

        nodesAry.add(BOS_NODES);
        for(int i=1; i <= len; i++)
            nodesAry.add(new ArrayList<ViterbiNode>());
        
        for(int i=0; i < len; i++, perResult.clear()) {
            if(nodesAry.get(i).isEmpty()==false) {
                WordDic.search(text, i, perResult);
                Unknown.search(text, i, perResult);
                
                final ArrayList<ViterbiNode> prevs = nodesAry.get(i);
                for(int j=0; j < perResult.size(); j++) {
                    final ViterbiNode vn = perResult.get(j);
                    if(vn.isSpace)
                        nodesAry.get(i+vn.length).addAll(prevs);
                    else
                        nodesAry.get(i+vn.length).add(setMincostNode(vn, prevs));
                }
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

    private ViterbiNode setMincostNode(ViterbiNode vn, ArrayList<ViterbiNode> prevs) {
        final ViterbiNode f = vn.prev = prevs.get(0);
        vn.cost = f.cost + Matrix.linkCost(f.posId, vn.posId);
        
        for(int i=1; i < prevs.size(); i++) {
            final ViterbiNode p = prevs.get(i);
            final int cost = p.cost + Matrix.linkCost(p.posId, vn.posId);
            if(cost < vn.cost) {
                vn.cost = cost;
                vn.prev = p;
            }
        }
        vn.cost += vn.wcost;
        return vn;
    }
}