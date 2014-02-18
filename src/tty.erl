-module(tty).

-export([
   start/2
  ,send/2
]).


start(Dev, Baud) ->
   Port = io_lib:format("./priv/erl_tty ~s ~i", [Dev, Baud]),
   erlang:open_port(
      {spawn, lists:flatten(Port)}, 
      [use_stdio, binary, {packet, 2}]
   ).

send(Port, Msg) ->
   Port ! {self(),{command, Msg}}.

