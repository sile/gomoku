package net.reduls.gomoku;

import java.util.List;

public final class WordDic {
    public static String feature(int wordId) {
        return Feature.feature[wordId];
    }

    public static void search(String text, int start, List<ViterbiNode> result) {
        WordId.eachCommonPrefix(text, start, new Collect(result));
    }

    public static void searchFromTrieId(int trieId, int start, int wordLength, boolean isSpace,
                                        List<ViterbiNode> result) {
        for(Morp.Info info : Morp.surface_morps_map.get(trieId)) 
            result.add(new ViterbiNode(info.cost,
                                       start,
                                       (short)wordLength,
                                       info.posId,
                                       false));
    }

    private static class Collect implements WordId.Callback {
	public final List<ViterbiNode> ms;
	public Collect(List<ViterbiNode> result) { ms = result; }
	
	public void call(int start, int offset, List<Morp.Info> infos) {
            // XXX: infoはコストをもっているので、ここで渡してしまって良い
            for(Morp.Info info : infos)
		ms.add(new ViterbiNode(info.cost,
                                       start, 
                                       (short)(offset-start), 
                                       info.posId,
                                       false));
	}
    }
}