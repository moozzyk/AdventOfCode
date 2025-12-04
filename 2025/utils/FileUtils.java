package utils;

import java.io.*;
import java.util.*;

public class FileUtils {
    public static List<String> readLines(String fileName) throws IOException {
        List<String> lines = new ArrayList<>();
        try (var br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }
}