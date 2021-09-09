import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class ZipDirectory {
    private List<String> fileList = new ArrayList<>();
    private String dir;
    private String zipFile;

    public ZipDirectory(String dir, String zipFile) {
        this.dir = dir;
        this.zipFile = zipFile;

        this.compressDirectory();
    }

    public void compressDirectory() {
        File directory = new File(dir);
        getFileList(directory);

        try (FileOutputStream fos = new FileOutputStream(zipFile);
             ZipOutputStream zos = new ZipOutputStream(fos)) {

            for (String filePath : fileList) {
                    System.out.println("Compressing: " + filePath);

                        // Creates a zip entry.
                        String name = filePath.substring(
                                directory.getAbsolutePath().length() + 1,
                                filePath.length());

                        ZipEntry zipEntry = new ZipEntry(name);
                        zos.putNextEntry(zipEntry);

                        // Read file content and write to zip output stream.
                        try (FileInputStream fis = new FileInputStream(filePath)) {
                            byte[] buffer = new byte[1024];
                            int length;
                            while ((length = fis.read(buffer)) > 0) {
                                zos.write(buffer, 0, length);
                            }

                            // Close the zip entry.
                            zos.closeEntry();
                        } catch (Exception e) {
                            e.printStackTrace();
                            return;
                        }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get files list from the directory recursive to the sub directory.
     */
    private void getFileList(File directory) {
        fileList.clear();
        File[] files = directory.listFiles();
        if (files != null && files.length > 0) {
            for (File file : files) {
                if (!file.isFile()) {
                    getFileList(file);
                }
            }

            for (File file : files) {
                if (file.isFile()) {
                    if (!file.getName().equals(".DS_Store")) {
                        fileList.add(file.getAbsolutePath());
                    }
                }
            }
        }

    }
}