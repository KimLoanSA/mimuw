package pl.edu.mimuw.ma406058.indexer.extractor;

public class ExtractorResult {

    private String fileName;
    private String fileContent;
    private String fileLanguage;

    public ExtractorResult(String fileName, String fileContent, String fileLanguage) {
        this.fileName = fileName;
        this.fileContent = fileContent;
        this.fileLanguage = fileLanguage;
    }

    public String getFileName() {
        return fileName;
    }

    public String getFileContent() {
        return fileContent;
    }

    public String getFileLanguage() {
        return fileLanguage;
    }
}
