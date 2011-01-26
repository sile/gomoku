package net.reduls.gomoku.dic;

import java.util.List;

public final class WordDic {
    public static interface Callback {
        public void call(ViterbiNode vn);
        public boolean isEmpty();
    }

    private static class Collect implements SurfaceId.Callback {
	private final Callback fn;

	public Collect(Callback fn) {
            this.fn = fn;
        }
	
	public void call(int start, int end, int surfaceId) {
            addMorphemes(surfaceId, start, end-start, false, fn);
	}
    }
    
    public static void search(String text, int start, Callback fn) {
        SurfaceId.eachCommonPrefix(text, start, new Collect(fn));
    }

    public static void search(int surfaceId, int start, int length, boolean isSpace, 
                              Callback fn) {
        addMorphemes(surfaceId, start, length, isSpace, fn);
    }

    private static void addMorphemes(int surfaceId, int start, int length, boolean isSpace, 
                                     Callback fn) {
        Morpheme.Info[] mis = Morpheme.getMorphemes(surfaceId);
        for(int i=0; i < mis.length; i++)
            fn.call(new ViterbiNode(start,
                                    (short)length,
                                    mis[i],
                                    isSpace));        
    }
}