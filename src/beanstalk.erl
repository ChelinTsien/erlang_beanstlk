%% @author chelin
%% @doc @todo Add description to beanstalk.


-module(beanstalk).
%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-include("beanstalk.hrl").
-include_lib("eunit/include/eunit.hrl").

close() ->
  gen_server:cast(beanstalk_server, {'stop'}).

addserver(Host, Port) ->
  Server=#server{host=Host,port=Port},
  gen_server:call(beanstalk_server, {addserver, Server}).

put(Tube, Data) ->
  put(Tube, Data, []).


put(Tube, Data, Params) ->
  gen_server:call(beanstalk_server, {Tube, {'put', Data, Params}}).

use(Tube) ->
  gen_server:call(beanstalk_server, {'use', Tube}).

reserve(Tube) ->
  gen_server:call(beanstalk_server, {Tube,{'reserve'}}, infinity).

reserve(Tube, Timeout) ->
  gen_server:call(beanstalk_server, {Tube,{'reserve-with-timeout', Timeout}}, infinity).

delete(Tube, ID) ->
  gen_server:call(beanstalk_server, {Tube,{'delete', ID}}).

bury(Tube, ID) ->
  bury(Tube, ID, 0).

bury(Tube, ID, Priority) ->
  gen_server:call(beanstalk_server, {Tube,{'bury', ID, Priority}}).

watch(Tube) ->
  gen_server:call(beanstalk_server, {'watch', Tube}).


ignore(Tube) ->
  gen_server:call(beanstalk_server, {'ignore', Tube}).

kick(Tube, Bound) ->
  gen_server:call(beanstalk_server, {Tube,{'kick', Bound}}).

start() ->
    beanstalk_app:start().