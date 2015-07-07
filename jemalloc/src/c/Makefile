HEADERS  = $(shell find . -name *.h)
SOURCES  = $(shell find . -name *.cpp)
OBJECTS  = $(SOURCES:%.cpp=%.o) 
LIBRARY  = libscalaOffheap.so

WORKDIR   = $(shell pwd)
JEMALLOC  = $(WORKDIR)/../../target/usr
JAVA      = $(shell readlink -f `which java`)
JAVADIR   = $(shell dirname $(JAVA))

# TODO AR figure out how to make virtual box image pick up host java. Uncomment first line to build on normal host
#JNIROOT   = $(JAVADIR)/../../include
JNIROOT   = /usr/lib/jvm/java-8-oracle/include/

CC        = g++
CFLAGS    = -D__STDC_LIMIT_MACROS -DJEMALLOC_MANGLE -c -g -fPIC -O3 -Wall -std=c++0x -I$(JEMALLOC)/include/ -I$(JNIROOT) -I$(JNIROOT)/linux 
LDFLAGS   = -shared -fPIC -L$(JEMALLOC)/lib -Wl,-rpath,$(JEMALLOC)/lib -Wl,-Bstatic -ljemalloc -Wl,-Bdynamic -pthread

all: $(LIBRARY)

.PHONY: clean
clean:
	rm -f *.o *.so

$(LIBRARY): $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

$(OBJECTS): %.o: %.cpp
	$(CC) $(CFLAGS) $< -o $@
