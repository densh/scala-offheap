HEADERS  = $(shell find . -name *.h)
SOURCES  = $(shell find . -name *.cpp)
OBJECTS  = $(SOURCES:%.cpp=%.o) 
LIBRARY  = libscalaOffheap.so

WORKDIR   = $(shell pwd)
JEMALLOC  = $(WORKDIR)/../../target/usr
JAVA      = $(shell readlink -f `which java`)
JAVADIR   = $(shell dirname $(JAVA))
JNIROOT   = $(JAVADIR)/../../include
CC        = g++
CFLAGS    = -DJEMALLOC_MANGLE -c -g -fPIC -O3 -Wall -I$(JEMALLOC)/include/ -I$(JNIROOT) -I$(JNIROOT)/linux 
LDFLAGS   = -shared -L$(JEMALLOC)/lib -Wl,-rpath,$(JEMALLOC)/lib -Wl,-Bstatic -ljemalloc -Wl,-Bdynamic -pthread

all: $(LIBRARY)

.PHONY: clean
clean:
	rm -f *.o *.so

$(LIBRARY): $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

$(OBJECTS): %.o: %.cpp
	$(CC) $(CFLAGS) $< -o $@
