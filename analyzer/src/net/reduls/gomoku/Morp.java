package net.reduls.gomoku;

import java.util.List;
import java.util.ArrayList;

import java.io.DataInputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

public final class Morp {
    public static final class Info {
        public final short posId;
        public final short cost;
        public Info(short posId, short cost) { this.posId=posId; this.cost=cost; }
    }

    public static final List<ArrayList<Info>> surface_morps_map; 
    public static final List<Info> g_infos; // XXX:
  
    static {
        surface_morps_map = new ArrayList<ArrayList<Info>>();
        g_infos = new ArrayList<Info>();
        
        try {
            DataInputStream in =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/id-morps.bin")));
            DataInputStream in2 =
                new DataInputStream
                (new BufferedInputStream
                 (Matrix.class.getResourceAsStream("data/morp.bin")));
            int cur=in.readInt();
            for(;;) {
                int end=in.readInt();
                ArrayList<Info> infos = new ArrayList<Info>(end-cur);
                for(; cur < end; cur++) {
                    infos.add(new Info(in2.readShort(), in2.readShort()));
                    g_infos.add(new Info(in2.readShort(), in2.readShort())); // XXX:
                }
                surface_morps_map.add(infos);
            } 

            //in.close();
            //in2.close();
        } catch (Exception ex) {
            System.err.println(ex.getMessage());
        }
    }

    public static void main (String[] args) {
        for(ArrayList<Info> infos : surface_morps_map){ 
            for(Info info : infos)
                System.out.print(info.posId+"#"+info.cost+" ");
            System.out.println("");
        }
    }
}
