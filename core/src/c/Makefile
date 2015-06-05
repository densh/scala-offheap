HEADERS  = $(shell find . -name *.h)
SOURCES  = $(shell find . -name *.cpp)
OBJECTS  = $(SOURCES:%.cpp=%.o) 
LIBRARY  = jemallocWrapper.so

# Assumes jemalloc configured with: './configure --prefix=/tmp/usr --with-jemalloc-prefix=je'
JEMALLOC  = /tmp/usr
JAVA      = $(shell readlink -f `which java`)
JAVADIR   = $(shell dirname $(JAVA))
JNIROOT   = $(JAVADIR)/../../include
CC        = g++
CFLAGS    = -DJEMALLOC_MANGLE -c -g -fPIC -O3 -std=c++11 -Wall -I$(JEMALLOC)/include/ -I$(JNIROOT) -I$(JNIROOT)/linux 
LDFLAGS   = -fPIC -L$(JEMALLOC)/lib -Wl,-rpath,$(JEMALLOC)/lib -static -ljemalloc -dynamic -pthread

all: $(LIBRARY)

.PHONY: clean
clean:
	rm -f $(OBJECTS) $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

$(OBJECTS): %.o: %.cpp
	$(CC) $(CFLAGS) $< -o $@
