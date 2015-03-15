#!/bin/bash

sbt "project jmh" clean 'run -jvmArgs "-Xms4g -Xmx4g -XX:+UseConcMarkSweepGC" -bm sample -wi 20 -i 30 -t 4 -f 1 GCBinaryTree' > gc_cms.result
sbt "project jmh" clean 'run -jvmArgs "-Xms4g -Xmx4g -XX:+UseParallelGC"      -bm sample -wi 20 -i 30 -t 4 -f 1 GCBinaryTree' > gc_par.result
sbt "project jmh" clean 'run -jvmArgs "-Xms4g -Xmx4g -XX:+UseG1GC"            -bm sample -wi 20 -i 30 -t 4 -f 1 GCBinaryTree' > gc_g1.result

sbt                     "project jmh" clean 'run -jvmArgs "-Xms512m -Xmx4g -XX:+UseParallelGC" -bm sample -wi 20 -i 30 -t 4 -f 1 OffheapBinaryTree' > oh_checked.result
sbt -Doffheap.unchecked "project jmh" clean 'run -jvmArgs "-Xms512m -Xmx4g -XX:+UseParallelGC" -bm sample -wi 20 -i 30 -t 4 -f 1 OffheapBinaryTree' > oh_unchecked.result
