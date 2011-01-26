package net.reduls.gomoku.bin;

import java.io.IOException;
import net.reduls.gomoku.Tagger;
import net.reduls.gomoku.Morpheme;
import net.reduls.gomoku.util.ReadLine;

public final class Gomoku {
    public static void main(String[] args) throws IOException {
        if(!(args.length==0 || (args.length==1 && args[0].equals("-wakati")))) {
	    System.err.println("Usage: java net.reduls.igo.bin.Gomoku [-wakati]");
	    System.exit(1);
	}

	final boolean doWakati = args.length==1 ? true : false;
	
	final ReadLine rl = new ReadLine(System.in);
	if(doWakati)
	    for(String s=rl.read(); s != null; s=rl.read()) {
		for(String w : Tagger.wakati(s))
		    System.out.print(w+" ");
		System.out.println("");
	    }
	else
	    for(String s=rl.read(); s != null; s=rl.read()) {
		for(Morpheme m : Tagger.parse(s))
		    System.out.println(m.surface+"\t"+m.feature);
		System.out.println("EOS");
	    }
    }   
}