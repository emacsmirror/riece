DEFS = -I$(srcdir) -I$(top_srcdir) $(CFLAGS) @DEFS@
LIBOBJS = @LIBOBJS@
EXTRA_DIST = basename.c getopt.c getopt.h getopt1.c
CLEANFILES = ldcc ltcp

bin_PROGRAMS = ldcc ltcp

ltcp_SOURCES = tcp.c
ltcp_LDADD = $(LIBOBJS)
ldcc_SOURCES = dcc.c
ldcc_LDADD = $(LIBOBJS)

package: all
install-package: package install
