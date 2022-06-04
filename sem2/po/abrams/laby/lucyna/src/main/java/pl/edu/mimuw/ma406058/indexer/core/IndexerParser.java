package pl.edu.mimuw.ma406058.indexer.core;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

public class IndexerParser {
    private Indexer indexer;

    private IndexerParser(Indexer indexer) {
        this.indexer = indexer;
    }

    private void purge() {
        indexer.deleteIndex();
    }


    private void add(String dirPath) {
        Path tempDirPath = Paths.get(dirPath);

        try {
            System.out.println("Adding: " + dirPath + "\n");

            indexer.addToObservedDir(tempDirPath);
            indexer.indexDirectory(WatchDir.getRecursiveSubDirectories(tempDirPath));
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }


    private void rm(String dirPath) {
        Path tempDirPath = Paths.get(dirPath);

        try {
            System.out.println("Removing: " + dirPath + "\n");

            indexer.removeFromObservedDir(tempDirPath);
            indexer.indexDirectoryDelete(WatchDir.getRecursiveSubDirectories(tempDirPath));
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }


    private void reindex() {
        ArrayList<Path> pathList = indexer.getObservedDir();

        for (Path actualPath : pathList) {
            try {
                System.out.println("Reindexing: " + actualPath.toString() + "\n");

                ArrayList<Path> subDirectories = WatchDir.getRecursiveSubDirectories(actualPath);
                indexer.indexDirectoryDelete(subDirectories);
                indexer.indexDirectory(subDirectories);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        }
    }


    private void list() {
        ArrayList<Path> pathList = indexer.getObservedDir();

        for (Path actualPath : pathList) {
            try {
                System.out.println(new File(actualPath.toString()).getCanonicalPath());
            }
            catch (IOException e) {
                System.err.println(e.getMessage());
            }
        }
    }

    private void watchDir() {
        try {
            new WatchDir(indexer).processEvents();
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }

    private void wrongNumberOfArguments(int expected, int got) {
        System.err.println("Expected" + expected +  "argument(got:" + got +")");
        System.exit(-1);
    }


    private void parseArgs(String[] args) {
        if (args.length == 0) {
            watchDir();
        } else {
            switch (args[0]) {
                case "--purge": {
                    if (args.length > 1) {
                        wrongNumberOfArguments(1, args.length);
                    }

                    purge();
                    break;
                }

                case "--add": {
                    if (args.length > 2) {
                        wrongNumberOfArguments(2, args.length);
                    }

                    add(args[1]);
                    break;
                }

                case "--rm": {
                    if (args.length > 2) {
                        wrongNumberOfArguments(2, args.length);
                    }

                    rm(args[1]);
                    break;
                }

                case "--reindex": {
                    if (args.length > 1) {
                        wrongNumberOfArguments(1, args.length);
                    }

                    reindex();
                    break;
                }

                case "--list": {
                    if (args.length > 1) {
                        wrongNumberOfArguments(1, args.length);
                    }

                    list();
                    break;
                }

                default: {
                    System.err.println("Wrong program argument(s)");
                }
            }
        }
    }

    public static void main(String[] args) {
        Path indexPath = Paths.get(System.getProperty("user.home") + "/.index/");
        Analyzer analyzer = Indexer.createAnalyzer(new StandardAnalyzer());

        try (IndexWriter indexWriter = new IndexWriter(FSDirectory.open(indexPath),
                new IndexWriterConfig(analyzer).setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND))) {

            Indexer indexer = new Indexer(indexWriter);

            new IndexerParser(indexer).parseArgs(args);
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
