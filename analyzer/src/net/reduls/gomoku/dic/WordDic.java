package net.reduls.gomoku.dic;

import java.util.List;

public final class WordDic {
    private static class Collect implements SurfaceId.Callback {
	private final List<ViterbiNode> result;

	public Collect(List<ViterbiNode> result) { 
            this.result = result;
        }
	
	public void call(int start, int end, int surfaceId) {
            addMorphemes(surfaceId, start, end-start, false, result);
	}
    }
    
    public static void search(String text, int start, List<ViterbiNode> result) {
        SurfaceId.eachCommonPrefix(text, start, new Collect(result));
    }

    public static void search(int surfaceId, int start, int length, boolean isSpace, 
                              List<ViterbiNode> result) {
        addMorphemes(surfaceId, start, length, isSpace, result);
    }

    private static void addMorphemes(int surfaceId, int start, int length, boolean isSpace, 
                                     List<ViterbiNode> result) {
        Morpheme.Info[] mis = Morpheme.getMorphemes(surfaceId);
        for(int i=0; i < mis.length; i++)
            result.add(new ViterbiNode(start,
                                       (short)length,
                                       mis[i],
                                       isSpace));        
    }
}