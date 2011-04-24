package net.reduls.gomoku;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.List;
import java.util.ArrayList;
import java.util.NoSuchElementException;

public final class ParallelTagger {
    private final List<Thread> consumerThreads;
    private final BlockingQueue<Sentence> sentenceQueue;
    private final BlockingQueue<MorphemesWithId> morphemeQueue;
    private final static int CORE_NUM = Runtime.getRuntime().availableProcessors();
    
    public ParallelTagger() {
        sentenceQueue = new ArrayBlockingQueue<Sentence>(CORE_NUM*5);
        morphemeQueue = new PriorityBlockingQueue<MorphemesWithId>();

        consumerThreads = new ArrayList<Thread>();
        for(int i=0; i < CORE_NUM; i++) {
            //System.err.println("#IN: "+i);
            Thread t = new ConsumerThread();
            t.setDaemon(true);
            consumerThreads.add(t);
            t.start();
            //System.err.println("#OUT: "+i);
        }
    }
    
    public Result parse(Iterator<String> sentenceIterator) {
        return new Result(sentenceIterator);
    }

    public final class Result implements Iterator<List<Morpheme>>, Iterable<List<Morpheme>> {
        private int curId = 0;
        
        public Result(Iterator<String> sentenceIterator) {
            morphemeQueue.clear();
            
            Thread producer = new ProducerThread(sentenceIterator);
            producer.setDaemon(true);
            producer.start();
        }
        
        public Iterator<List<Morpheme>> iterator() {
            return this;
        }

        public boolean hasNext() {
            return morphemeQueue.peek() != NullMorphemes;
        }
        public List<Morpheme> next() {
            MorphemesWithId m = morphemeQueue.peek();
            while(m==null || m.id != curId) {
                try {
                    Thread.sleep(1);
                } catch(Exception e) {}
                m = morphemeQueue.peek();
            }
            if(m == NullMorphemes)
                throw new NoSuchElementException();
            curId++;
            morphemeQueue.poll();
            return m.morphemes;
        }
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private final static class Sentence implements Comparable<Sentence> {
        final public int id;
        final public String sentence;

        public Sentence(int id, String sentence) {
            this.id = id;
            this.sentence = sentence;
        }
        public int compareTo(Sentence s) {
            return id - s.id;
        }
    }
    private static final MorphemesWithId NullMorphemes = new MorphemesWithId(Integer.MAX_VALUE,null);

    private final static class MorphemesWithId implements Comparable<MorphemesWithId> { 
        final public int id;
        final public List<Morpheme> morphemes;
        
        public MorphemesWithId(int id, List<Morpheme> morphemes) {
            this.id = id;
            this.morphemes = morphemes;
        }

        public int compareTo(MorphemesWithId s) {
            return id - s.id;
        }
    }
    private static final Sentence NullSentence = new Sentence(-1,null);

    private final class ProducerThread extends Thread {
        private final Iterator<String> sentenceIterator;        
        public ProducerThread(Iterator<String> sentenceIterator) {
            this.sentenceIterator = sentenceIterator;
        }
        
        public void run() {
            int id = 0;
            while(sentenceIterator.hasNext())
                try {
                    String s = sentenceIterator.next();
                    sentenceQueue.put(new Sentence(id++, s/*sentenceIterator.next()*/));
                    //System.err.println("#PUT "+s);
                } catch (Exception e) {}
            sentenceQueue.add(NullSentence);
        }
    }

    private final class ConsumerThread extends Thread {
        public void run() {
            for(;;) {
                try {
                    //System.err.println("# RUN: "+sentenceQueue);
                    Sentence s = sentenceQueue.take();
                    //System.err.println("#GET: "+s.sentence);
                    if(s==NullSentence) {
                        morphemeQueue.put(NullMorphemes);
                    } else {
                        morphemeQueue.put(new MorphemesWithId(s.id, Tagger.parse(s.sentence)));
                    }
                } catch(Exception e) {
                    System.err.println(e);
                }
            }
        }
    }
}