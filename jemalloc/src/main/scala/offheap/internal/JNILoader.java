package scala.offheap.internal;

import java.io.*;
import java.lang.reflect.Field;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;

public final class JNILoader {
    private static final Path NativeLibraryDirectory = Paths.get(System.getProperty("java.io.tmpdir"), "scala-offheap");
    private static final String NativeLibraryName = System.mapLibraryName("scalaOffheap");
    private static final String JemallocLibraryName = String.format("%s.2", System.mapLibraryName("jemalloc"));
    private static final Set<PosixFilePermission> ExecutePermissions = new HashSet<PosixFilePermission>() {
        {
            add(PosixFilePermission.GROUP_EXECUTE);
            add(PosixFilePermission.OWNER_EXECUTE);
            add(PosixFilePermission.OTHERS_EXECUTE);
            add(PosixFilePermission.GROUP_READ);
            add(PosixFilePermission.OWNER_READ);
            add(PosixFilePermission.OTHERS_READ);
        }
    };

    static void ensureLoaded() {
        try {
            ensureTempFolder();
            writeNativeLibraryToTempFolder(NativeLibraryName);
            writeNativeLibraryToTempFolder(JemallocLibraryName);
            addTempFolderToLibraryPath();
            System.out.println("ENV ... " + System.getenv("LD_LIBRARY_PATH"));
            final Process exec = Runtime.getRuntime().exec("ldd /tmp/scala-offheap/libscalaOffheap.so");
            final BufferedReader is = new BufferedReader(new InputStreamReader(exec.getInputStream()));
            final BufferedReader es = new BufferedReader(new InputStreamReader(exec.getErrorStream()));
            String line;
            while ((line = is.readLine()) != null) {
                System.out.println(line);
            }

            while ((line = es.readLine()) != null) {
                System.out.println(line);
            }
            System.loadLibrary("scalaOffheap");
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void ensureTempFolder() throws IOException {
        if (Files.exists(NativeLibraryDirectory))
            removeTempFolder();

        Files.createDirectories(NativeLibraryDirectory);

        System.out.println("Native Library Directory: " + NativeLibraryDirectory);
    }

    private static void removeTempFolder() throws IOException {
        Files.walkFileTree(NativeLibraryDirectory, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException e) throws IOException {
                if (e == null) {
                    Files.delete(dir);
                    return FileVisitResult.CONTINUE;
                } else {
                    throw e;
                }
            }
        });
    }

    private static void writeNativeLibraryToTempFolder(String libraryName) throws IOException {
        final Path nativeLibraryPath = NativeLibraryDirectory.resolve(libraryName);
        System.out.println("Writing native library to: " + nativeLibraryPath);

        try (InputStream inputStream = JNILoader.class.getClassLoader().getResourceAsStream(libraryName)) {
            Files.copy(inputStream, nativeLibraryPath, StandardCopyOption.REPLACE_EXISTING);
            Files.setPosixFilePermissions(nativeLibraryPath, ExecutePermissions);
        }
    }

    private static void addTempFolderToLibraryPath() throws Exception {
        System.setProperty("java.library.path", NativeLibraryDirectory + File.pathSeparator + System.getProperty("java.library.path"));

        forceReloadOfJavaLibraryPath();

        System.out.println("java.library.path is now: " + System.getProperty("java.library.path"));
    }

    private static void forceReloadOfJavaLibraryPath() throws NoSuchFieldException, IllegalAccessException {
        Field fieldSysPath = ClassLoader.class.getDeclaredField("sys_paths");
        fieldSysPath.setAccessible(true);
        fieldSysPath.set(null, null);
    }
}
