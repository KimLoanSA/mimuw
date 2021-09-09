package pl.edu.mimuw.ma406058.indexer.core;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper;
import org.apache.lucene.analysis.pl.PolishAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.tika.exception.TikaException;
import pl.edu.mimuw.ma406058.indexer.extractor.Extractor;
import pl.edu.mimuw.ma406058.indexer.extractor.ExtractorResult;
import pl.edu.mimuw.ma406058.indexer.extractor.LanguageException;
import pl.edu.mimuw.ma406058.indexer.extractor.TypeException;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Indexer {
    private Path indexPath = Paths.get(System.getProperty("user.home") + "/.index/");
    private IndexWriter indexWriter;

    public Indexer(IndexWriter indexWriter) {
        this.indexWriter = indexWriter;
    }

    public static Analyzer createAnalyzer (Analyzer defaultAnalyzer) {
        Map<String, Analyzer> analyzerPerField = new HashMap<>();
        analyzerPerField.put("en", new EnglishAnalyzer());
        analyzerPerField.put("pl", new PolishAnalyzer());

        return new PerFieldAnalyzerWrapper(defaultAnalyzer, analyzerPerField);
    }

    public void indexDirectory(ArrayList<Path> pathList) {
        try {
            System.out.println("Indexing:");

            for (Path actualPath : pathList) {
                try {
                    ExtractorResult actualFile = Extractor.parseFile(actualPath);
                    Document actualDoc = new Document();

                    System.out.println(actualPath.toString());

                    actualDoc.add(new StringField("filePath", actualPath.toString(), Field.Store.YES));
                    actualDoc.add(new TextField("fileName", actualFile.getFileName(), Field.Store.YES));
                    actualDoc.add(new TextField(actualFile.getFileLanguage(), actualFile.getFileContent(), Field.Store.YES));

                    indexWriter.addDocument(actualDoc);
                    indexWriter.commit();
                } catch (TypeException | LanguageException e) {
                    System.err.println("Wrong type or language of file:" + actualPath.toString());
                }
            }

            System.out.println("\n");
        } catch (IOException | TikaException e) {
            System.err.println(e.getMessage());
        }
    }

    public void indexDirectoryDelete(ArrayList<Path> pathList) {
        try {
            System.out.println("Deleting index:");
            for (Path actualPath : pathList) {
                System.out.println(actualPath.toString());

                indexWriter.deleteDocuments(new Term("filePath", actualPath.toString()));
                indexWriter.commit();
            }

            System.out.println("\n");
        } catch (IOException e){
            System.err.println(e.getMessage());
        }
    }

    private ArrayList<Document> getObservedDirInDoc() {
        ArrayList<Document> resList = new ArrayList<>();

        try (IndexReader indexReader = DirectoryReader.open(indexWriter)) {
            IndexSearcher indexSearcher = new IndexSearcher(indexReader);

            Query query = new NormsFieldExistsQuery("indexPath");
            TopDocs resDoc = indexSearcher.search(query, Integer.MAX_VALUE);

            for (ScoreDoc actualDoc : resDoc.scoreDocs) {
                resList.add(indexSearcher.doc(actualDoc.doc));
            }

        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        return resList;
    }

    public ArrayList<Path> getObservedDir() {
        ArrayList<Path> resList = new ArrayList<>();

        for (Document actDoc : getObservedDirInDoc()) {
            resList.add(Paths.get(actDoc.getField("indexPath").stringValue()));
        }

        return resList;
    }

    public void addToObservedDir(Path dirPath) {
        try {
            Document actDoc = new Document();
            actDoc.add(new TextField("indexPath", dirPath.toString(), Field.Store.YES));
            actDoc.add(new StringField("indexPathString", dirPath.toString(), Field.Store.YES));

            indexWriter.addDocument(actDoc);
            indexWriter.commit();
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    public void removeFromObservedDir(Path dirPath) {
        try {
            indexWriter.deleteDocuments(new Term("indexPathString", dirPath.toString()));
            indexWriter.commit();
        } catch (IOException e){
            System.err.println(e.getMessage());
        }
    }

    public void deleteIndex() {
        try {
            indexWriter.deleteAll();
        }
        catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
