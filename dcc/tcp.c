/* tcp.c - TCP/IP stream emulation for GNU Emacs.
 * Copyright (C) 1988, 1989, 1992, 1993 Free Software Foundation, Inc.
 * Copyright (C) 1998-2002  Daiki Ueno
 *
 * This file is part of Liece.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
/* This program is based on `tcp' comming from old GNUS distribution
   written by Masanobu Umeda <umerin@mse.kyutech.ac.jp>. */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <netinet/in.h>
#define _GNU_SOURCE
#include <getopt.h>

#ifdef HAVE_BASENAME
# ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#  ifdef basename
#   undef basename
#  endif
# endif
# include <string.h>
#else
# define basename(path) (rindex((path), '/') + 1)
#endif

#ifndef NI_MAXHOST
# define NI_MAXHOST 1025
#endif

static char *progname;

void version () {
	printf("%s (Liece) 1.4.0\n"
	       "Copyright (C) 1998, 1999 Daiki Ueno\n"
	       "This is free software; see the source for copying conditions.  There is NO\n"
	       "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n", 
	       progname);
}

void usage() {
  printf("Usage: %s [options] host [service]\n", progname);
}


main (argc, argv)
  int argc;
  char *argv[];
{
  struct protoent *proto;
  int family, socktype;
  struct sockaddr *addr;
  size_t addrlen;
#ifdef HAVE_GETADDRINFO
  struct addrinfo *in, hints;
#else
  struct hostent *host;
  struct servent *serv;
  struct sockaddr_in sin;
#endif
  char *hostname = NULL, *service = "ircd";
  int port;
  fd_set *readfds, *writefds;
  int server, emacsIn = fileno (stdin), emacsOut = fileno (stdout); 
  char buffer[1024], *retry;
  int nbuffer, wret, false = 0;
  int c;
  
  progname = (char *) basename (argv[0]);

  while (1)
    {
      int this_option_optind = optind ? optind : 1;
      int option_index = 0;
      static struct option long_options[] =
	{
	  {"version", 0, 0, 'v'},
	  {"help", 0, 0, 'h'},
	  {0, 0, 0, 0}
	};
    
      c = getopt_long (argc, argv, "vh", long_options, &option_index);
      if (c == -1)
	break;
    
      switch (c)
	{
	case 'v':
	  version ();
	  exit (1);
	  break;
	case 'h':
	  usage ();
	  exit (1);
	  break;
	default:
	  break;
	}
    }
  
  if (argc < 2)
    {
      usage();
      exit (1);
    }
  if (argc >= 2)
    hostname = argv[1];
  if (argc >= 3)
    service = argv[2];
  
  proto = getprotobyname ("tcp");
  if (!proto)
    {
      perror ("getprotobyname");
      exit (1);
    }

#ifdef HAVE_GETADDRINFO
  memset (&hints, 0, sizeof (hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = proto->p_proto;
  if (getaddrinfo (hostname, service, &hints, &in) < 0)
    {
      perror ("getaddrinfo");
      exit (1);
    }
  family = in->ai_family;
  socktype = in->ai_socktype;
  addr = in->ai_addr;
  addrlen = in->ai_addrlen;
  freeaddrinfo (in);
#else
  memset (&sin, 0, sizeof (sin));
  host = gethostbyname (hostname);
  if (!host)
    return -1;
  memcpy (&sin.sin_addr, host->h_addr, host->h_length);
  serv = getservbyname (service, proto->p_name);
  if (serv)
    sin.sin_port = htons (serv->s_port);
  else if (isdigit (service[0]))
    sin.sin_port = htons (atoi (service));
  family = sin.sin_family = AF_INET;
  socktype = SOCK_STREAM;
  addr = (struct sockaddr *)&sin;
  addrlen = sizeof (sin);
#endif

  server = socket (family, socktype, 0);
  if (server == -1)
    {
      perror ("socket");
      exit (1);
    }

  setsockopt (server, SOL_SOCKET, SO_REUSEADDR, 
	      (const char *) &false, sizeof (false));

  if (connect (server, addr, addrlen) < 0)
    {
      perror ("connect");
      close (server);
      exit (1);
    }

#ifdef O_NDELAY
  fcntl (server, F_SETFL, O_NDELAY);
#endif /* O_NDELAY */

  /* Connection established. */

  readfds = (fd_set *) calloc(server + 1, sizeof (fd_mask));
  writefds = (fd_set *) calloc(server + 1, sizeof (fd_mask));

  while (1)
    {
      FD_SET (server, readfds);
      FD_SET (emacsIn, readfds);
      if (select (server+1, readfds, NULL, NULL, NULL) == -1)
	{
	  perror ("select");
	  exit (1);
	}
      if (FD_ISSET (emacsIn, readfds))
	{
	  /* From Emacs */
	  nbuffer = read (emacsIn, buffer, sizeof buffer -1);

          if (nbuffer == 0)
	    goto finish;
	  for (retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret)
	    {
	      FD_SET (server, writefds);
	      if (select (server+1, NULL, writefds, NULL, NULL) == -1)
		{
		  perror ("select");
		  exit (1);
		}
	      wret = write (server, retry, nbuffer);
	      if (wret < 0) goto finish;
	    }
	}
      if (FD_ISSET (server, readfds))
	{
	  /* From NNTP server */
	  nbuffer = read (server, buffer, sizeof buffer -1);
	  if (nbuffer == 0)
	    goto finish;
	  for (retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret)
	    {
	      FD_SET (emacsOut, writefds);
	      if (select (emacsOut+1, NULL, writefds, NULL, NULL) == -1)
		{
		  perror ("select");
		  exit (1);
		}
	      wret = write (emacsOut, retry, nbuffer);
	      if (wret < 0) goto finish;
	    }
	}
    }

  /* End of communication. */
 finish:
  close (server);
  close (emacsIn);
  close (emacsOut);
  exit (0);
}
