package net.reduls.gomoku.util;

import java.io.InputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class ReadLine {
    private final BufferedReader br;

    public ReadLine(InputStream in) throws IOException {
	br = new BufferedReader(new InputStreamReader(in));
    }
    
    public void close() {
	try {
	    br.close();
	} catch (IOException e) {}
    }

    public String read() throws IOException {
	return br.readLine();
    }
}