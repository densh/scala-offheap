package offheap.jni;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

public final class JNILoader {
    private static final Path NativeLibraryDirectory = Paths.get(System.getProperty("java.io.tmpdir"), "scala-offheap");
    private static final String NativeLibraryName = System.mapLibraryName("scalaOffheap");

    static void ensureLoaded() {
        try {
            ensureTempFolder();
            writeNativeLibraryToTempFolder();
            addTempFolderToLibraryPath();
            System.loadLibrary("scalaOffheap");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void ensureTempFolder() {
        if (!NativeLibraryDirectory.toFile().isDirectory() || !NativeLibraryDirectory.toFile().canRead()) {
            NativeLibraryDirectory.toFile().mkdirs();
        }

        System.out.println("Native Library Directory: " + NativeLibraryDirectory);
    }

    private static void writeNativeLibraryToTempFolder() throws IOException {
        try (InputStream inputStream = JNILoader.class.getClassLoader().getResourceAsStream(NativeLibraryName)) {
            final Path nativeLibraryPath = NativeLibraryDirectory.resolve(NativeLibraryName);
            System.out.println("Writing native library to: " + nativeLibraryPath);
            Files.copy(inputStream, nativeLibraryPath, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static void addTempFolderToLibraryPath() throws Exception {
        System.setProperty("java.library.path", NativeLibraryDirectory + File.pathSeparator + System.getProperty("java.library.path"));

        // this forces JVM to reload "java.library.path" property
        Field fieldSysPath = ClassLoader.class.getDeclaredField("sys_paths");
        fieldSysPath.setAccessible(true);
        fieldSysPath.set(null, null);

        System.out.println("java.library.path is now: " + System.getProperty("java.library.path"));
    }
}