DEFS = -I$(srcdir) -I$(top_srcdir) $(CFLAGS) @DEFS@
LIBOBJS = @LIBOBJS@
EXTRA_DIST = basename.c getopt.c getopt.h getopt1.c
CLEANFILES = rdcc rtcp

bin_PROGRAMS = rdcc rtcp

rtcp_SOURCES = tcp.c
rtcp_LDADD = $(LIBOBJS)
rdcc_SOURCES = dcc.c
rdcc_LDADD = $(LIBOBJS)

package: all
install-package: package install
