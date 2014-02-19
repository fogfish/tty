-module(tty_port).
-behaviour(gen_server).

-export([
   start_link/1
  ,start_link/2,
   init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-define(DRIVER, erl_tty).

-record(srv, {
   port  = undefined :: any()
  ,name  = undefined :: atom()  
  ,owner = undefined :: pid()
}).


%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Name, Opts) ->
   gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

start_link(Opts) ->
   gen_server:start_link(?MODULE, [undefined, Opts], []).

init([Name, Opts]) ->
   erlang:process_flag(trap_exit, true),
   {tty,   TTY} = lists:keyfind(tty,   1, Opts),
   {baud, Baud} = lists:keyfind(baud,  1, Opts),  
   {owner, Pid} = lists:keyfind(owner, 1, Opts),  
   _  = erlang:monitor(process, Pid),
   Driver  = filename:join([priv_dir(), ?DRIVER]),
   Command = lists:flatten(
      io_lib:format("~s ~s ~b", [Driver, TTY, Baud])
   ),
   Port = erlang:open_port(
      {spawn, lists:flatten(Command)}, 
      [use_stdio, exit_status, binary, {packet, 2}]
   ),
   {ok, 
      #srv{
         port  = Port
        ,name  = Name
        ,owner = Pid
      } 
   }.

terminate(_, S) ->
   erlang:port_close(S#srv.port).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(Msg, S)
 when is_binary(Msg) ->
   S#srv.port ! {self(), {command, Msg}},
   {noreply, S};

handle_cast(close, S) ->
   {stop, normal, S};

handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info({_Port, {data, Data}}, #srv{name=undefined}=S) ->
   S#srv.owner ! {tty, self(), Data},
   {noreply, S};

handle_info({_Port, {data, Data}}, S) ->
   S#srv.owner ! {tty, S#srv.name, Data},
   {noreply, S};

handle_info({_Port, {exit_status, 0}}, S) ->
   {stop, normal, S};

handle_info({_Port, {exit_status, Reason}}, S) ->
   {stop, {?MODULE, Reason}, S};

handle_info({'DOWN', _, _, Pid, _}, #srv{owner = Pid}=S) ->
   {stop, normal, S};

handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _) ->
   {ok, S}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
priv_dir() ->
   case code:lib_dir(tty, priv) of
      {error, bad_name} ->
         priv;
      Priv ->
         Priv
   end.






