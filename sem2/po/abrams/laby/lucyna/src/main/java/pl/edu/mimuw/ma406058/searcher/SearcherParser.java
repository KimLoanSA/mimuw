package pl.edu.mimuw.ma406058.searcher;

import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.store.FSDirectory;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.NoSuchElementException;
import java.util.Scanner;

public class SearcherParser {
    private Searcher searcher;

    public SearcherParser(Searcher searcher) {
        this.searcher = searcher;
    }

    private void ready() {
        System.out.print("> ");
    }

    private void error() {
        System.err.println("Wrong command");
    }

    private boolean parse(String input) {
        if (input.length() == 0) {
            error();

            return true;
        }

        if (!input.startsWith("%")) {
            return false;
        }


        if (input.equals("%term")) {
            searcher.setQueryType(Searcher.QueryType.TERM);
            return true;
        }

        if (input.equals("%phrase")) {
            searcher.setQueryType(Searcher.QueryType.PHRASE);
            return true;
        }

        if (input.equals("%fuzzy")) {
            searcher.setQueryType(Searcher.QueryType.FUZYY);
            return true;
        }


        if (input.length() == "%lang pl".length() && input.startsWith("%lang ") &&
                (input.substring(6).equals("en") || input.substring(6).equals("pl"))) {

            searcher.setLanguage(input.substring(6));
            return true;
        }

        if (input.length() >= "%color on".length() && input.startsWith("%color ")) {
                if (input.substring(7).equals("on") || input.substring(7).equals("off")) {

                searcher.setColor(input.substring(7).equals("on"));
                return true;
            }
        }

        if (input.startsWith("%limit ")) {
            try {
                int number = Integer.parseInt(input.substring(7));

                if (number < 0) {
                    error();
                    return true;
                }

                searcher.setLimit(number == 0 ? Integer.MAX_VALUE : number);
                return true;
            }
            catch (NumberFormatException e) {
                error();
                return true;
            }
        }

        if (input.length() >= "%details on".length() && input.startsWith("%details ")) {
               if (input.substring(9).equals("on") || input.substring(9).equals("off")) {
                   searcher.setDetails(input.substring(9).equals("on"));
                   return true;
               }
        }

        error();
        return true;
    }

    private void processEvents() {
        try (Scanner scanner = new Scanner(System.in)) {
            for (;;) {
                try {
                    ready();
                    String input = scanner.nextLine();
                    if (!parse(input)) {
                        searcher.query(input);
                    }
                } catch (NoSuchElementException e) {
                    return;
                }
            }
        }

    }

    public static void main(String[] args) {
        Path indexPath = Paths.get(System.getProperty("user.home") + "/.index/");

        try (IndexReader indexReader = DirectoryReader.open(FSDirectory.open(indexPath))) {

            IndexSearcher indexSearcher = new IndexSearcher(indexReader);
            Searcher searcher = new Searcher("en", false, Integer.MAX_VALUE,
                    false, Searcher.QueryType.TERM, indexSearcher, indexReader);

            new SearcherParser(searcher).processEvents();

        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }
}
