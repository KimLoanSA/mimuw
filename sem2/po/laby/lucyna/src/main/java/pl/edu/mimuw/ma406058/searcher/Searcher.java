package pl.edu.mimuw.ma406058.searcher;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.search.uhighlight.DefaultPassageFormatter;
import org.apache.lucene.search.uhighlight.UnifiedHighlighter;
import pl.edu.mimuw.ma406058.indexer.core.Indexer;

import java.io.IOException;

public class Searcher {
    public enum QueryType {
        TERM, PHRASE, FUZYY;
    }

    private String language;
    private boolean details;
    private int limit;
    private boolean color;
    private String ansiBold = "\u001b[1m";
    private String ansiRed = "\u001B[31m";
    private String ansiReset = "\u001B[0m";
    private String ansiColor = ansiBold;
    private QueryType queryType;
    private IndexSearcher indexSearcher;
    private Analyzer analyzer = Indexer.createAnalyzer(new StandardAnalyzer());


    public Searcher(String language, boolean details, int limit, boolean color,
                    QueryType queryType, IndexSearcher indexSearcher, IndexReader indexReader) {
        this.language = language;
        this.details = details;
        this.limit = limit;
        this.color = color;
        this.queryType = queryType;
        this.indexSearcher = indexSearcher;
    }

    public void setLanguage(String language) {
        if (language.equals("en") || language.equals("pl")) {
            this.language = language;
        }
    }

    public void setDetails(boolean details) {
        this.details = details;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }

    public void setColor(boolean color) {
        this.color = color;

        if (color) {
            ansiColor = ansiRed;
        } else {
            ansiColor = ansiBold;
        }
    }

    public void setQueryType(QueryType queryType) {
        this.queryType = queryType;
    }


    private Query createQuery(String input) {
        if (queryType == QueryType.TERM) {
            try {
                return new BooleanQuery.Builder()
                        .add(new QueryParser("fileName", analyzer).parse(input), BooleanClause.Occur.SHOULD)
                        .add(new QueryParser(language, analyzer).parse(input), BooleanClause.Occur.SHOULD)
                        .build();
            } catch (ParseException e) {
                System.err.println(e.getMessage());
            }

        }

        if (queryType == QueryType.PHRASE) {
            PhraseQuery.Builder builderContent = new PhraseQuery.Builder();
            PhraseQuery.Builder builderName = new PhraseQuery.Builder();

            for (String actString : input.split(" ")) {
                builderContent.add(new Term(language, actString));
                builderName.add(new Term("fileName", actString));
            }

            Query contentQuery =  builderContent.build();
            Query nameQuery = builderName.build();

            return new BooleanQuery.Builder()
                    .add(contentQuery, BooleanClause.Occur.SHOULD)
                    .add(nameQuery, BooleanClause.Occur.SHOULD)
                    .build();
        }

        if (queryType == QueryType.FUZYY) {
            return new FuzzyQuery(new Term(language, input));
        }

        return null;
    }

    private void printQuery(TopDocs topDocs, String[] contects) {
        System.out.println("File count: " + ansiBold + topDocs.scoreDocs.length + ansiReset);
        int i = 0;

        for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
            try {
                Document actDoc = indexSearcher.doc(scoreDoc.doc);

                System.out.println(ansiBold + actDoc.get("filePath") + ":" + ansiReset);

                if (details) {
                    System.out.println(contects[i++]);
                }

            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        }
    }

    public void query(String input) {
        try {
            Query query = createQuery(input);
            TopDocs topDocs = indexSearcher.search(query, limit);

            if (!details) {
                printQuery(topDocs, null);
                return;
            }

            UnifiedHighlighter highlighter = new UnifiedHighlighter(indexSearcher, analyzer);
            highlighter.setFormatter(new DefaultPassageFormatter(ansiColor, ansiReset, ". . . ", false));

            String[] strings = highlighter.highlight(language, query, topDocs, 1000000);

            printQuery(topDocs, strings);

        } catch (IOException e){
            System.err.println(e.getMessage());
        }
    }
}
