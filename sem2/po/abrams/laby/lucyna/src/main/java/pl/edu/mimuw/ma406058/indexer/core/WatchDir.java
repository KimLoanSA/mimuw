package pl.edu.mimuw.ma406058.indexer.core;

/*
 * Copyright (c) 2008, 2010, Oracle and/or its affiliates. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Oracle nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import java.nio.file.*;
import static java.nio.file.StandardWatchEventKinds.*;
import static java.nio.file.LinkOption.*;
import java.nio.file.attribute.*;
import java.io.*;
import java.util.*;

public class WatchDir {

    private final WatchService watcher;
    private final Map<WatchKey,Path> keys;
    private Indexer indexer;

    @SuppressWarnings("unchecked")
    static <T> WatchEvent<T> cast(WatchEvent<?> event) {
        return (WatchEvent<T>)event;
    }


    public static ArrayList<Path> getRecursiveSubDirectories(Path dirPath) throws IOException {
        ArrayList<Path> resultList = new ArrayList<>();

        Files.walkFileTree(dirPath, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attr) {
                try {
                    if (Files.isRegularFile(file)) {
                        resultList.add(file);
                    }
                } catch (SecurityException e) {
                    System.err.println(e.getMessage());
                }

                return FileVisitResult.CONTINUE;
            }
        });

        return resultList;
    }

    /**
     * Register the given directory with the WatchService
     */
    private void register(Path dir) throws IOException {
        WatchKey key = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
        keys.put(key, dir);
    }

    /**
     * Register the given directory, and all its sub-directories, with the
     * WatchService.
     */
    private void registerAll(final Path start) throws IOException {
        // register directory and sub-directories
        Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
                    throws IOException
            {
                register(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    /**
     * Creates a WatchService and registers the given directory
     */
    public WatchDir(Indexer indexer) throws IOException {
        this.watcher = FileSystems.getDefault().newWatchService();
        this.keys = new HashMap<>();
        this.indexer = indexer;
        ArrayList<Path> dirList = indexer.getObservedDir();

        System.out.println("Watching: \n");

        for (Path dir : dirList) {
            System.out.println(dir.toString());

            registerAll(dir);
        }
    }

    /**
     * Process all events for keys queued to the watcher
     */
    public void processEvents() {
        for (;;) {

            ArrayList<Path> filesToRemove = new ArrayList<>();
            ArrayList<Path> filesToAdd = new ArrayList<>();

            // wait for key to be signalled
            WatchKey key;
            try {
                key = watcher.take();
            } catch (InterruptedException x) {
                return;
            }

            Path dir = keys.get(key);
            if (dir == null) {

                System.err.println("WatchKey not recognized!!");
                continue;
            }
            for (WatchEvent<?> event: key.pollEvents()) {
                WatchEvent.Kind kind = event.kind();

                // TBD - provide example of how OVERFLOW event is handled
                if (kind == OVERFLOW) {
                    continue;
                }

                // Context for directory entry event is the file name of entry
                WatchEvent<Path> ev = cast(event);
                Path name = ev.context();
                Path child = dir.resolve(name);

                if (kind == ENTRY_CREATE) {
                    System.out.println("Created: " + child.toString());
                }

                if (kind == ENTRY_MODIFY) {
                    System.out.println("Modified: " + child.toString());
                }

                if (kind == ENTRY_DELETE) {
                    System.out.println("Deleted: " + child.toString());
                }

                try {
                    if (kind == ENTRY_DELETE || kind == ENTRY_MODIFY) {
                        if (Files.isDirectory(child, NOFOLLOW_LINKS)) {
                            filesToRemove.addAll(getRecursiveSubDirectories(child));
                        } else {
                            filesToRemove.add(child);
                        }
                    }

                    // if directory is created, and watching recursively, then
                    // register it and its sub-directories
                    if (kind == ENTRY_CREATE || kind == ENTRY_MODIFY) {
                        if (Files.isDirectory(child, NOFOLLOW_LINKS)) {
                            if (kind == ENTRY_CREATE) {
                                registerAll(child);
                            }
                            filesToAdd.addAll(getRecursiveSubDirectories(child));
                        } else {
                            filesToAdd.add(child);
                        }
                    }
                } catch (IOException e) {
                    System.err.println(e.getMessage());
                }
            }

            indexer.indexDirectory(filesToAdd);
            indexer.indexDirectoryDelete(filesToRemove);

            // reset key and remove from set if directory no longer accessible
            boolean valid = key.reset();
            if (!valid) {
                keys.remove(key);

                // all directories are inaccessible
                if (keys.isEmpty()) {
                    break;
                }
            }
        }
    }
}