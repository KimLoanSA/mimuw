package pl.edu.mimuw.ma406058.indexer.extractor;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.langdetect.OptimaizeLangDetector;
import org.apache.tika.language.detect.LanguageDetector;
import org.apache.tika.language.detect.LanguageResult;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

public class Extractor {

    private static boolean checkType(String fileType) {
        //troche przesada, ale nie znalazlem sposobu zeby to ladnie zrobic, przynajmniej latwo dodac typy
        //skopiowane z https://tika.apache.org/1.13/formats.html#Full_list_of_Supported_Formats

        String[] types = {"application/x-tika-ooxml", "application/vnd.",
                            "application/x-vnd.oasis.", "application/vnd.oasis",
                            "text/plain", "application/rtf", "application/pdf" };

        for (String actType : types) {
            if (fileType.startsWith(actType)) {
                return true;
            }
        }

        return false;
    }


    public static ExtractorResult parseFile(Path filePath) throws TypeException, LanguageException, IOException, TikaException {
        Tika tika = new Tika();

        if (!checkType(tika.detect(filePath))) { //zly format pliku
            throw new TypeException();
        }

        String fileText = tika.parseToString(filePath);
        String fileName = filePath.getFileName().toString();

        LanguageDetector languageDetector = new OptimaizeLangDetector().loadModels();
        LanguageResult languageResultFile = languageDetector.detect(fileText);

        if (!languageResultFile.isLanguage("en") && !languageResultFile.isLanguage("pl")) { //zly jezyk
            throw new LanguageException();
        }

        return new ExtractorResult(fileName, fileText, languageResultFile.getLanguage());
    }
}
