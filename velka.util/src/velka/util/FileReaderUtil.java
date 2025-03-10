package velka.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileReaderUtil {
	
	public static final int BUFFER_SIZE = 8192;

	public static InputStream readResourceFile(String fileName) {
        ClassLoader classLoader = FileReaderUtil.class.getClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream(fileName);
        
        if (inputStream == null) {
            throw new IllegalArgumentException("File not found: " + fileName);
        }

        return inputStream;
    }
	
	public static void transferData(InputStream inputStream, OutputStream outputStream) throws IOException {
        byte[] buffer = new byte[BUFFER_SIZE]; 
        int bytesRead;
        
        while ((bytesRead = inputStream.read(buffer)) != -1) {
            outputStream.write(buffer, 0, bytesRead);
        }

        outputStream.flush(); 
    }
	
	public static void copyResourceTo(String filename, Path destination) {
		var istream = FileReaderUtil.readResourceFile(filename);
		OutputStream ostream;
		try {
			ostream = Files.newOutputStream(destination);
			FileReaderUtil.transferData(istream, ostream);
			istream.close();
			ostream.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
