package net.reduls.gomoku;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.ArrayList;

public final class Feature {
  public static final String[] feature;
  
  static {
    BufferedReader in = 
      new BufferedReader 
       (new InputStreamReader(Feature.class.getResourceAsStream("data/pos.bin")));
    ArrayList<String> lines = new ArrayList<String>();
    try {
      try {
        for(String line=in.readLine(); line!=null; line=in.readLine())
          lines.add(line);
      } catch (IOException ex) {}
    } finally {
      try {
        in.close();
      } catch (IOException ex) {}
    }
    feature = new String[lines.size()];
    for(int i=0; i < feature.length; i++)
      feature[i] = lines.get(i);
  }

  public static void main(String[] args) {
    for(String f : Feature.feature)
      System.out.println(f);
  }
}
