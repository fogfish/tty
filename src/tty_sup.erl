%% @description
%%    elastic search i/o proxy
-module(tty_sup).

-export([
   start_link/0,
   init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, transient, 5000, Type, dynamic}).

%%
%%
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) -> 
   {ok,
      {
         {simple_one_for_one, 4, 10},
         [?CHILD(worker, tty_port)]
      }
   }.
