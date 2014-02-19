-module(tty).

-export([start/0]).
-export([
   start_link/1
  ,start_link/2
  ,close/1
  ,send/2
  ,recv/2
]).

-define(PACKET,  4096).

%%
%%
start() ->
   application:start(?MODULE).

%%
%%
start_link(Name, Opts) ->
   supervisor:start_child(tty_sup, [Name, [{owner, self()} | Opts]]).

start_link(Opts) ->
   tty_port:start_link([{owner, self()} | Opts]).

%%
%%
close(Port) ->
   gen_server:cast(Port, close).

%%
%%
send(Port, Msg)
 when is_binary(Msg), byte_size(Msg) < ?PACKET ->
   gen_server:cast(Port, Msg);

send(_, _) ->
   exit(badrag).

%%
%%
recv(Port, Timeout) ->
   receive
      {tty, Port, Data} ->
         {ok, Data}
   after Timeout ->
      {error, timeout}
   end.