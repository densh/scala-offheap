The makefile assumes a few things

1) Download and extract the jemalloc artifact into /tmp
2) Run the configure script with: './configure --prefix=/tmp/usr --with-jemalloc-prefix=je'
3) Manually append '-fPIC' to 'CFLAGS' in the resulting Makefile
