package net.reduls.gomoku.bin;

import java.io.IOException;
import net.reduls.gomoku.Tagger;
import net.reduls.gomoku.Morpheme;
import net.reduls.gomoku.util.ReadLine;

/**
 * 形態素解析コマンド
 */
public final class Gomoku {
    public static void main(String[] args) throws IOException {
        /*
	if(!(args.length==1 || args.length==2) ||
	   (args.length==2 && args[0].equals("-wakati")==false)) {
	    System.err.println("Usage: java net.reduls.igo.bin.gomoku [-wakati] <dictionary directory>");
	    System.exit(1);
	}
        */
	//final String  dicDir   = args.length==2 ? args[1] : args[0];
	final boolean doWakati = args.length==1 ? true : false;
	final Tagger  tagger   = new Tagger();
	
	final ReadLine rl = new ReadLine(System.in);
	if(doWakati)
	    for(String s=rl.read(); s != null; s=rl.read()) {
		for(String w : tagger.wakati(s))
		    System.out.print(w+" ");
		System.out.println("");
	    }
	else
	    for(String s=rl.read(); s != null; s=rl.read()) {
		for(Morpheme m : tagger.parse(s))
		    System.out.println(m.surface+"\t"+m.feature);
		System.out.println("EOS");
	    }
    }   
}