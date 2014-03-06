/*
   Copyright (c) 2012 - 2013, Dmitry Kolesnikov
   All Rights Reserved.
  
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
  
      http://www.apache.org/licenses/LICENSE-2.0
  
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

Usage:
   erl_tty <device>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/param.h>
#include <fcntl.h>
#include <termios.h>

#include <signal.h>
#include <ctype.h>   
#include <unistd.h>

#include "erl_tty.h"

#define  MAX_BAUD    23
baud_t bauds[MAX_BAUD] = {
   {0       , B0      },
   {50      , B50     },
   {75      , B75     },
   {110     , B110    },
   {134     , B134    },
   {150     , B150    },
   {200     , B200    },
   {300     , B300    },
   {600     , B600    },
   {1200    , B1200   },
   {1800    , B1800   },
   {2400    , B2400   },
   {4800    , B4800   },
   {9600    , B9600   },
   {19200   , B19200  },   
   {38400   , B38400  },   
   {57600   , B57600  },   
   {115200  , B115200 },  
   {230400  , B230400 }
};


//
//
speed_t get_baud(const char *baud)
{
   int i   = 0;
   int val = atoi(baud);

   while (i < MAX_BAUD)
   {
      if (val == bauds[i].ival)
         return bauds[i].bval;
      i++;
   }

   return B0;
}

//
//
int open_tty(const char *dev, speed_t baud)
{
   int fd;
   struct termios tty;

   if ((fd = open(dev, O_RDWR | O_NOCTTY |  O_NDELAY )) < 0)
      return -1;

   /* config port */   
   tcgetattr(fd, &tty);

   cfsetispeed(&tty, baud);
   cfsetospeed(&tty, baud);
   
   // 8N1
   tty.c_cflag &= ~PARENB;
   tty.c_cflag &= ~CSTOPB;
   tty.c_cflag &= ~CSIZE;
   tty.c_cflag |= CS8;

   // no flow control
   tty.c_cflag &= ~CRTSCTS;
   tty.c_cflag |= CREAD | CLOCAL;  
   tty.c_iflag &= ~(IXON | IXOFF | IXANY); 

   // raw
   tty.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG); 
   tty.c_oflag &= ~OPOST;
   tcsetattr(fd,TCSANOW,&tty);

   DEBUG("[debug] tty: open device %s\n", dev);
   return fd;
}

//
// 
int io_recv(int fd, unsigned char buf[], int len)
{
   int i = 0;
   int r = 0;
   while(i < len)
   {
      if ((r = read(fd, &buf[i], len - i)) == 0)
         return i;
      i += r;
   }
   return i;
}

//
//
int erl_recv(int fd, unsigned char buf[], int size)
{
   if (io_recv(fd, buf, PACKET) != PACKET)
      return 0;

   int len = (((int) buf[0]) << 8) + ((int) buf[1]);
   if (len != io_recv(fd, &buf[PACKET], len))
      return 0;

   DEBUG("[error] tty: erlang msg %i\n", len);
   return len;
}


int main(int argc, char **argv) 
{
   DEBUG("[debug] tty: open device %s\n", argv[1]);
   DEBUG("[debug] tty: open device %s\n", argv[2]);


   int len     = 0;
   int err     = 0;
   int fd_in   = fileno(stdin);
   int fd_out  = fileno(stdout);
   int fd_tty  = open_tty(argv[1], get_baud(argv[2]));
   int fd_max  = -1;
   fd_set io_fds;
   unsigned char io[IO_BUFSIZE];

   if (fd_tty == -1)
   {
      ERROR("[error] tty: device error %s\n", argv[1]);
      return 1;
   }

   fd_max = (fd_in > fd_tty) ? fd_in : fd_tty;
   FD_ZERO(&io_fds);

   while(err >= 0)
   {
      FD_SET(fd_in,  &io_fds);
      FD_SET(fd_tty, &io_fds);

      err = select(fd_max + 1, &io_fds, NULL, NULL, NULL);
      if (err < 0)
      {
         ERROR("[error] tty: i/o error %i\n", err);
         return err;
      }

      if (FD_ISSET(fd_tty, &io_fds))
      {
         DEBUG("[debug] tty: serial fd %i\n", err);
         FD_CLR(fd_tty, &io_fds);

         if ( (len = read(fd_tty, &io[PACKET], IO_BUFSIZE) ) == 0)
         {
            return -1;
         } else {
            io[1] = (unsigned char) (len & 0xff);
            io[0] = (unsigned char) ((len >> 8) & 0xff);
            write(fd_out, io, len + PACKET);
         }
      }

      if (FD_ISSET(fd_in, &io_fds))
      {
         DEBUG("[debug] tty: erlang fd %i\n", fd_in);
         FD_CLR(fd_in, &io_fds);

         if ( (len = erl_recv(fd_in, io, IO_BUFSIZE)) == 0)
         {
            return -1;
         } else {
            write(fd_tty, &io[PACKET], len);
         }
      }
   }

   return 0;
}





