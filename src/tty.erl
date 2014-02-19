-module(tty).

-export([start/0]).
-export([
   start_link/1
  ,start_link/2
  ,close/1
  ,bind/2
  ,send/2
  ,recv/2
]).

-define(PACKET,  4096).

%%
%% start tty application
-spec(start/0 :: () -> ok | {error, any()}).

start() ->
   application:start(?MODULE).

%%
%% start serial port driver
%%  Options
%%    {tty,     list()} - path to serial device
%%    {baud, integer()} - baud rate
-spec(start_link/1 :: (list()) -> {ok, pid()} | {error, any()}).
-spec(start_link/2 :: (atom(), list()) -> {ok, pid()} | {error, any()}).

start_link(Opts) ->
   start_link(undefined, Opts).

start_link(Name, Opts) ->
   tty_port:start_link(Name, [{owner, self()} | Opts]).

%%
%% close serial port driver
-spec(close/1 :: (pid()) -> ok).

close(Port) ->
   gen_server:cast(Port, close).

%%
%% Bind a new process Pid to Port.
%% The controlling process is the process which receives messages from the port. 
%% If called by any other process than the current process, {error, not_owner} is returned.
-spec(bind/2 :: (pid(), pid()) -> ok | {error, any()}).

bind(Port, Pid) ->
   gen_server:call(Port, {bind, self(), Pid}).

%%
%% send message to port
-spec(send/2 :: (pid(), binary()) -> ok).

send(Port, Msg)
 when is_binary(Msg), byte_size(Msg) < ?PACKET ->
   gen_server:cast(Port, Msg);

send(_, _) ->
   exit(badrag).

%%
%% synchronously receive message from port
-spec(recv/2 :: (pid(), timeout()) -> {ok, binary()} | {error, any()}).

recv(Port, Timeout) ->
   Ref = erlang:monitor(process, Port),
   receive
      {tty, Port, Data} ->
         {ok, Data};
      {'DOWN', Ref, _, Port, Reason} ->
         {error, Reason}
   after Timeout ->
      {error, timeout}
   end.