
#define DEBUG(X,Y) fprintf(stderr,X,Y)
#define ERROR(X,Y) fprintf(stderr,X,Y)

typedef struct 
{
  int     ival;
  speed_t bval;
} baud_t;

#define NULLFDS   ((fd_set *) 0)
#define NULLTV    ((struct timeval *) 0)

#define PACKET         2
#define IO_BUFSIZE  4096
