/* dcc.c
 * Copyright (C) 1998-2003  Daiki Ueno
 *
 * This file is part of Riece.
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

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <netinet/in.h>
#define _GNU_SOURCE
#include <getopt.h>

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN 31
#endif

#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif

#ifdef HAVE_MEMMOVE
# ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#  ifdef basename
#   undef basename
#  endif
# endif
# include <string.h>
#else
# define memmove(x,y,z) bcopy((y), (x), (z))
#endif

#ifndef HAVE_BASENAME
# define basename(path) (rindex((path), '/') + 1)
#endif

static void usage();
static int prepare_listen_port();
static int prepare_connect_port();

static int receive_file();
static int send_file();
static int select_loop();
static int chat_listen();
static int chat_connect();

static u_long primary_address_of();
static u_long extract_addr_of_string();
static u_long get_address_externally();

static char *progname;

void version () {
	fprintf(stderr,
					"%s (%s) %s\n"
					"Copyright (C) 1998-2003 Daiki Ueno\n"
					"This is free software; see the source for copying conditions.  There is NO\n"
					"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n", 
					progname, PACKAGE, VERSION);
}

void usage () {
  fprintf(stderr,
					"Usage: %s [global-options] command [arguments...]\n"
					"where global-options are -v, -h, etc.\n"
					"where command is one of send, receive, chat, resolve.\n"
					"where arguments depend on the specific command.\n\n"
					"send <port> <filename>\n"
					"receive <host> <port> <size> <filename>\n"
					"chat listen <port>\n"
					"chat connect <host> <port>\n"
					"resolve [hosts ...]\n",
					progname);
}

int prepare_listen_port (int ip_port) {
  int sock, tries;
  int opt = 1;
  static struct sockaddr_in server;

  
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("opening stream socket");
    exit(1);
  }

#ifdef SO_REUSEADDR
  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, 
		 (char *)&opt, sizeof (opt)) < 0) {
    perror ("setsockopt SO_REUSEADDR");
  }
#endif

  /* Bind a port to listen for new connections */

  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons (ip_port);
  for (tries = 0; tries < 10; tries++) {
    if (bind (sock, (struct sockaddr *) &server, sizeof (server))) {
      if (tries >= 9) {
	perror ("binding stream socket");
	exit (1);
      }
      perror ("binding stream socket. retry in 20 seconds");
      sleep (20);		/* wait 20 seconds and try again */
    } else
      break;
  }
  listen (sock, 64);
  return (sock);
}

u_long get_address_externally(char *ircserver) {
  int i, len, dummy;
  u_long addr;
  struct hostent *hp;
  struct sockaddr_in server, client;

  addr = 0xc6290004;                          /* dummy addr --- rootA */
  if (ircserver && (hp = gethostbyname(ircserver)) != NULL) {
    addr = ntohl(((struct in_addr *)hp->h_addr_list[0])->s_addr);
  }
  if ((dummy = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("opening stream socket");
    return -1;
  }
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(addr);
  server.sin_port = htons(7);                 /* dummy port --- echo */
  for (i = 0; i < 8; i++) {
    server.sin_zero[i] = 0;
  }
  if (connect(dummy, (struct sockaddr *)&server, sizeof(server)) < 0) {
    perror ("connecting remote socket");
    return -1;
  }
  len = sizeof(client);
  if (getsockname(dummy, (struct sockaddr *)&client, &len) < 0) 
    return -1;
  close(dummy);
  return ntohl(client.sin_addr.s_addr);
}


/*
 * send_file(int port, char *ifile)
 * listens to connections to port, and when connection established
 * sends ifile to that socket
 */
int send_file (int port, char *ifile) {
  int sock, ifd, ofd, len;
  u_long addr, bytessent = 0;
  char buf[ BUFSIZ * 8 ];
  fd_set readfds, writefds, fdset;
  struct stat statbuf;
  char namebuf[ MAXHOSTNAMELEN ];
  struct hostent *hp;
  struct sockaddr_in sin;

  if ((ifd = open (ifile, O_RDONLY)) < 0) { 
    /* error in opening file to send */
    close(ofd);
    return 1;
  }

  gethostname(namebuf, sizeof (namebuf));
  fstat (ifd, &statbuf);

  sock = prepare_listen_port(port);
  len = sizeof (struct sockaddr_in);
  if (getsockname(sock, (struct sockaddr *)&sin, &len) == 0) 
    port = ntohs(sin.sin_port);
 
	if ((addr = get_address_externally (NULL)) < 0) {
		gethostname(namebuf, sizeof (namebuf));
		if (hp = gethostbyname(namebuf)) 
			addr = ((struct in_addr *) (hp->h_addr_list)[0])->s_addr;
		else
			return 2;
	}

	printf ("DCC send %s %d %u %d\n", ifile, port, addr, statbuf.st_size);
  
  ofd = accept(sock, (struct sockaddr *) 0, (int *) 0);
  
  while ((len = read (ifd, buf, sizeof (buf))) > 0) {
    write (ofd, buf, len);	
    bytessent += len;
    while ((len = read (ofd, buf, sizeof (u_long))) &&
	   ntohl (*(u_long *) buf) != bytessent);
  }
  close (ofd);
  close (ifd);
  printf ("*** DCC file %s sent\n", ifile);

  return 0;
}

/*
 * receive_file(u_long host, int port, char *ifile)
 * connects to (host,port) and reads everything send from there
 * for every packet received gives back how much actually got
 * puts everything in ifile
 */
int receive_file (u_long host, int port, int size, char *ifile) {
  int sock, ifd, ofd, len, bytesreceived = 0, toread, prev = 0;
  char buf[ BUFSIZ * 8 ];
  fd_set readfds, writefds, fdset;
  u_long netsize;
    
  if ((ofd = open(ifile, O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0) {
    fprintf(stderr, "open: opening file: %s\n", ifile);
    return 1;
  }
  ifd = prepare_connect_port (host, port);
  if ((toread = sizeof (buf)) > size)
    toread = size;
  while (bytesreceived < size && (len = read (ifd, buf, toread)) > 0) {
    write (ofd, buf, len);
    bytesreceived += len;
    netsize = htonl (bytesreceived);
    lseek (ifd, 0, 2);
    write (ifd, &netsize, 4);
    lseek (ifd, 0, 2);
    if (toread > size - bytesreceived)
      toread = size - bytesreceived;
    if (bytesreceived - prev > size / 5) {
      printf ("DCC %s %d%% (%d/%d bytes) received\n", ifile,
	      100 * bytesreceived / size, bytesreceived, size);
      prev = bytesreceived;
    }
  }
  printf ("*** DCC file %s received\n", ifile);
  close (ifd);
  close (ofd);

  return 0;
}

/*
 * select_loop(int sfd)
 * listens fd given, reads stdin and sends it to socket 
 * anything read from socket is send to stdout
 */
int select_loop (int sfd) {
  int ofd, len, bytesreceived = 0;
  char buf[ BUFSIZ * 8 ];
  fd_set readfds, writefds, fdset;

  for (;;) {
    FD_ZERO (&readfds);
    FD_SET (sfd, &readfds);
    FD_SET (0, &readfds);
    if (select (32, &readfds, 0, 0, 0) < 0) {
      perror ("select");
      close (sfd);
      return 1;
    }
	
    if (FD_ISSET (sfd, &readfds)) {
      if ((len = read(sfd, buf, sizeof (buf))) == 0) {
	close (sfd);
	return 0;
      }
      write (1, buf, len);
      FD_CLR (sfd, &readfds);
    }
    if (FD_ISSET (0, &readfds))	{
      if ((len = read (0, buf, sizeof (buf))) == 0) {
	close (sfd);
	return 0;
      }
      write(sfd, buf, len);
      FD_CLR (ofd, &readfds);
    }
  }
}

int prepare_connect_port (u_long host, int port) {
  int sock;
  static struct hostent *hp;
  static struct sockaddr_in server;
    
  sock = socket (AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror ("opening stream socket");
    exit (1);
  }
  server.sin_family = AF_INET;
  
  server.sin_addr.s_addr = ntohl (host);
  server.sin_port = htons (port);
    
  if (connect(sock, (struct sockaddr *) &server, sizeof (server)) < 0) {
    perror ("connecting remote socket");
    return 0;
  }
  
  return sock;
}

u_long extract_addr_of_string (char *str) {
  u_long result = 0;

#ifndef HAVE_STRTOUL
  while (*str++) 
    result = result * 10 + *str - '0';
#else /* !HAVE_STRTOUL */
  result = strtoul(str, NULL, 10);
#endif /* HAVE_STRTOUL */
  return result;
}

u_long primary_address_of (char *host) {   
  struct hostent *hp;
  u_long addr;
  
  if ((hp = gethostbyname(host)) == NULL)
    addr = inet_addr(host);
  else
    memmove(&addr, hp->h_addr_list[ 0 ], 4);
  
  return ntohl(addr);
}

int chat_listen(int port) {
  struct sockaddr_in sin;
  struct hostent *hp;
	u_long addr;
  int sock, len;
  char namebuf[ MAXHOSTNAMELEN ];
    
  sock = prepare_listen_port (port);
  
  len = sizeof (struct sockaddr_in);
  if (getsockname(sock, (struct sockaddr *)&sin, &len) == 0) 
    port = ntohs(sin.sin_port);

	if ((addr = get_address_externally (NULL)) < 0) {
		gethostname(namebuf, sizeof (namebuf));
		if (hp = gethostbyname(namebuf)) 
			addr = ((struct in_addr *) (hp->h_addr_list)[0])->s_addr;
		else
			return 2;
	}

	printf("DCC chat %u %d\n", addr, port);
  
  if ((sock = accept(sock, (struct sockaddr *) 0, (int *) 0)) > -1) {
		printf("DCC chat established\n");
    return select_loop(sock);
	}
  
  return 1;
}

int chat_connect(u_long host, int port) {
  int sock;
  
  if ((sock = prepare_connect_port(host, port)) > -1) {
    printf("DCC chat established\n");
    return select_loop(sock);
  }
  
  return 1;
}


int main (int argc, char **argv) {
  char *host = "localhost";
  char *action;
  int c, status = 0;

  progname = (char *)basename(argv[ 0 ]);

	while (1)	{
		int this_option_optind = optind ? optind : 1;
		int option_index = 0;
		static struct option long_options[] =	{
			{"version", 0, 0, 'v'},
			{"help", 0, 0, 'h'},
			{0, 0, 0, 0}
		};
			
		c = getopt_long (argc, argv, "vh", long_options, &option_index);
		if (c == -1)
			break;
		
		switch (c) {
		case 'v':
			version();
			exit(0);
			break;
		case 'h':
			usage();
			exit(0);
			break;
		default:
			break;
		}
	}

  if (argc > 1) {
    action = argv[ 1 ];
  } else {
    usage();
    exit(1);
  }

  if (!strcmp(action, "resolve")) {
    if (argc < 3) {
      usage();
      exit(1);
    } else {
      u_long i, addr;
      for (i = 2; i < argc; i++) {
	addr = primary_address_of(argv[i]);
	if (addr != -1)
	  printf("%u\n", addr);
	else
	  printf("0\n");
      }
      status = 0;
    }
  }

  if (!strcmp(action, "send")) {
    if (argc != 4) {
      usage();
      exit(1);
    }
    status = send_file (atoi(argv[ 2 ]), argv[ 3 ]);
  } else if (!strcmp(action, "receive")) {
    if (argc != 6) {
      usage();
      exit(1);
    }
    status = 
      receive_file (extract_addr_of_string(argv[ 2 ]),
		    atoi(argv[ 3 ]), atoi(argv[ 4 ]), argv[ 5 ]);
  } else if (!strcmp(action, "chat")) {
    if (argc > 3) {
      if (!strcmp(argv[ 2 ], "listen")) {
	if (argc != 4) {
	  usage();
	  exit(1);
	}
	status = chat_listen(atoi(argv[ 3 ]));
      } else if (!strcmp(argv[ 2 ], "connect")) {
	if (argc != 5) {
	  usage();
	  exit(1);
	}
	status = chat_connect(extract_addr_of_string(argv[ 3 ]), 
			      atoi(argv[ 4 ]));
      } else {
	usage();
	exit(1);
      }	
    }
  } else {
    usage();
    exit(1);
  }

  return status;
}

/*
 * Local variables:
 *  compile-command: "gcc -DHAVE_STRTOUL -Wall -O6 -o dcc dcc.c"
 *  c-indent-level: 2
 *  c-basic-offset: 2
 *  tab-width: 2
 * End:
 */
