package net.reduls.gomoku.bin;

import java.io.IOException;
import net.reduls.gomoku.ParallelTagger;
import net.reduls.gomoku.Morpheme;
import net.reduls.gomoku.util.ReadLine;
import java.util.List;
import java.util.Iterator;
import java.util.NoSuchElementException;

public final class ParallelGomoku {
    public static void main(String[] args) throws IOException {
        if(!(args.length==0 || (args.length==1 && args[0].equals("-wakati")))) {
	    System.err.println("Usage: java net.reduls.igo.bin.ParallelGomoku [-wakati]");
	    System.exit(1);
	}

	final boolean doWakati = args.length==1 ? true : false;
	
        final ParallelTagger tagger = new ParallelTagger();
        final ParallelTagger.Result result = 
            tagger.parse(new LineIterator(new ReadLine(System.in)));

        /*
	if(doWakati)
            for(List<Morpheme> ms : result) {
		for(String w : Tagger.wakati(s))
		    System.out.print(w+" ");
		System.out.println("");
	    }
            else*/
        for(List<Morpheme> ms : result) {
            for(Morpheme m : ms)
                System.out.println(m.surface+"\t"+m.feature);
            System.out.println("EOS");
        }
    }   

    private static final class LineIterator implements Iterator<String> {
        private final ReadLine rl;
        private String line;
        
        public LineIterator(ReadLine rl) {
            this.rl = rl;
            try {
                line = rl.read();
            } catch (Exception e) {
                line = null;
            }
        }
        public boolean hasNext() {
            return line != null;
        }
        public String next() {
            String tmp = line;
            if(tmp==null)
                throw new NoSuchElementException();
            try {
                line = rl.read();
            } catch (Exception e) {
                line = null;
            }
            return tmp;
        }
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}