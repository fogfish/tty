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
   erl_tty node cookie
*/

#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_interface.h"
#include "ei.h"

#define  NODE_HOST   "localhost"
#define  NODE_ADDR   "127.0.0.1"

#define  IO_BUFSIZE  4096


typedef struct
{
   int           fd;
   unsigned char io[IO_BUFSIZE];
   ErlMessage    msg;
} TTYNode;

//
// 
int erl_tty_msg(TTYNode *node)
{
   ETERM *req,
         *pid,
         *msg,
         *rsp;

   req = erl_element(1, node->msg.msg);
   pid = erl_element(2, node->msg.msg);
   msg = erl_element(3, node->msg.msg);

   rsp = erl_format("{cnode, ~i}", 10);

   erl_send(node->fd, pid, rsp);

   erl_free_term(node->msg.from); 
   erl_free_term(node->msg.msg);
   erl_free_term(req);
   erl_free_term(pid);
   erl_free_term(msg);
   erl_free_term(rsp);

   return 1;
};


void erl_tty_loop(TTYNode *node)
{
   int res = 1;
   while(res > 0)
   {
      switch(erl_receive_msg(node->fd, node->io, IO_BUFSIZE, &node->msg))
      {
      case ERL_TICK:
         break;

      case ERL_ERROR:
         res = -1;
         break;

      default:
         if (node->msg.type == ERL_REG_SEND)
         {
            res = erl_tty_msg(node);
         }
      }
   }
}

int main(int argc, char **argv) 
{
   TTYNode node;
   struct in_addr addr;

   erl_init(NULL, 0);

   if (argc < 3)
      erl_err_quit("badargs");


   // if (erl_connect_init(1, argv[2], 0) == -1)
   //    erl_err_quit("erl_connect_init");
   addr.s_addr = inet_addr(NODE_ADDR);
   if (erl_connect_xinit(NODE_HOST, "c", "c@127.0.0.1", &addr, argv[2], 0) == -1)
      erl_err_quit("erl_connect_xinit");

   if ((node.fd = erl_connect(argv[1])) < 0)
      erl_err_quit("erl_connect");
   fprintf(stderr, "Connected to %s\n", argv[1]);

   erl_tty_loop(&node);

   return 0;
}





