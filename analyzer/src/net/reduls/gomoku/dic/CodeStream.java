package net.reduls.gomoku.dic;

public final class CodeStream {
    final private CharSequence src;
    final private int end;
    private int pos;
    private int code;
    private int octetPos;
    private int octetLen;
    
    // TODO: unicode対応
    public CodeStream(CharSequence str, int start) {
        src = str;
        end = src.length();
        pos = start;
        code = start==end ? 0 : str.charAt(start);

        octetLen = octetLength(code);
        octetPos = octetLen;
    }

    public boolean isEos() {
        return pos==end;
    }

    public char read() {
        char c = peek();
        eat();
        return c;
    }

    public int position() {
        return pos;
    }

    private int octetLength(int code) {
        if(code < 0x80)    return 1;
        if(code < 0x800)   return 2;
        if(code < 0x10000) return 3;
        return 4;
    }

    private char peek() {
        if(octetPos==octetLen) {
            switch(octetLen) {
            case 1:  return (char)code;
            case 2:  return (char)(0xC0 + (byte)(((code>>6)&0x1F)));
            case 3:  return (char)(0xE0 + (byte)(((code>>12)&0xF)));
            default: return (char)(0xF0 + (byte)(((code>>18)&0x7)));
            }
        }
        final int byteOffset = (octetPos-1)*6;
        return (char)(0x80 + (byte)((code>>byteOffset)&0x3F));
    }
    
    private void eat() {
        octetPos -= 1;
        if(octetPos==0) {
            pos += 1;
            if(isEos()==false) {
                code = src.charAt(pos);
                octetLen = octetLength(code);
                octetPos = octetLen;
            }
        }
    }
}