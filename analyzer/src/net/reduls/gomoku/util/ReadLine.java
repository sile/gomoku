package net.reduls.gomoku.util;

import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.EOFException;
import java.io.InputStreamReader;

/**
 * 行読み込み用のクラス
 */
public class ReadLine {
    private final LineNumberReader br;

    /**
     * 入力ストリームを元に、このクラスのインスタンスを作成する
     *
     * @throws IOException 入出力エラーが発生した場合に送出される
     */
    public ReadLine(InputStream in) throws IOException {
	br = new LineNumberReader(new InputStreamReader(in));
    }
    
    /**
     * ファイルを元に、このクラスのインスタンスを作成する
     *
     * @param filepath 読み込むファイルのパス
     * @param encoding 読み込むファイルのエンコーディング
     * @throws IOException 入出力エラーが発生した場合に送出される
     */
    public ReadLine(String filepath, String encoding) throws IOException {
	br = new LineNumberReader(new InputStreamReader(new FileInputStream(filepath), encoding));
    }
    
    /**
     * 読み込みを終了する
     */
    public void close() {
	try {
	    br.close();
	} catch (IOException e) {}
    }

    /**
     * 一行読み込む
     *
     * @return 読み込んだ文字列. ストリームの終端に達してしる場合は、nullを返す
     * @throws IOException 入出力エラーが発生した場合に送出される
     */
    public String read() throws IOException {
	return br.readLine();
    }

    /**
     * 一行読み込む
     *
     * @return 読み込んだ文字列
     * @throws EOFException ストリームの終端に達している場合に送出される
     * @throws IOException 入出力エラーが発生した場合に送出される
     */
    public String readEof() throws IOException, EOFException {
	final String s=br.readLine();
	if(s==null)
	    throw new EOFException();
	return s;
    }
    
    /**
     * これまでに読み込んだ行数を返す
     *
     * @return これまでに読み込んだ行数
     */
    public int lineNumber() {
	return br.getLineNumber();
    }
}