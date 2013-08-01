%% -*- mode: erlang; indent-tabs-mode: nil -*-
-module(beanstalk_app).
-behaviour(application).
-export([start/2,stop/1]). % required
-export([start/0]). % application api

%%% required callbacks

%% invoked by
%% application:start
%% (application_master:start_it_old,application_master:start_supervisor)
%%
%% @doc Standard application callback. Start the application's supervisor.
%%
-spec start(StartType :: normal |
                         {takeover, Node :: node()} |
                         {failover, Node :: node()},
            StartArgs :: term()) ->
                   {ok, pid()} |
                   {ok, pid(), State :: term()} |
                   {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    beanstalk_sup:start_link().

%% invoked by
%% application:stop
%% (application_master:loop_it)
%%
%% @doc Standard application callback. Stop the application.
%%
-spec stop(State :: term()) ->
                  term().
stop(_State) ->
    ok.


%%% application api

%%
%% @doc API to start application.
%%
-spec start() ->
                  ok |
                  {error, Reason :: term()}.
start() ->
    application:start(beanstalk).

%%% functions internal to your implementation
