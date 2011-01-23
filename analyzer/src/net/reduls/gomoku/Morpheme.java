package net.reduls.gomoku;

public final class Morpheme {
    public final String surface;
    public final String feature;
    public final int start;
    public Morpheme(String surface, String feature, int start) {
        this.surface = surface;
        this.feature = feature;
        this.start = start;
    }
}