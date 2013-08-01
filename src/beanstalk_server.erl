%% @author chelin
%% @doc @todo Add description to beanstalkd_server.
-module(beanstalk_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]). % required
-export([terminate/2,code_change/3]). % required
%%-export([format_status/2]). % optional
-export([start_link/0]). % gen_server api

-include("beanstalk.hrl").

%%% required callbacks

%% invoked by
%% gen_server:start_link, gen_server:start
%% (gen_server:init_it)
%%
%% @doc Standard gen_server callback. Initial state for the gen_server
%% server process.
%%
-spec init(Args :: term()) ->
                  {ok, State :: term()} |
                  {ok, State :: term(), timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
init([]) ->
    {ok, #server_ring{points = [], buckets=[]}}.

%% invoked by
%% gen_server:call, gen_server:multi_call
%% (gen_server:handle_msg)
%%
%% @doc Standard gen_server callback. Handle synchronous requests.
%% Implement the api routines in flexihash.erl.
%%
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply,
                          Reply :: term(),
                          NewState :: term(),
                          timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop,
                          Reason :: term(),
                          Reply :: term(),
                          NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({addserver, Server}, _From, State) ->
    case connecct(Server, State) of 
        {error, Error} ->
            {stop, Error, State};
        NewState -> 
            {reply, NewState, NewState}
    end;

handle_call(Command = {watch, Tube}, _From, State) -> 
    {_Index, Server} = findserver(Tube, State),
    beanstalk_command:send(Server#server.socket, Command),
    server_response(Server, State);

handle_call(Command = {use, Tube}, _From, State) -> 
    {_Index, Server} = findserver(Tube, State),
    beanstalk_command:send(Server#server.socket, Command),
    server_response(Server, State);

handle_call(Command = {ignore, Tube}, _From, State) -> 
    {_Index, Server} = findserver(Tube, State),
    beanstalk_command:send(Server#server.socket, Command),
    server_response(Server, State);

handle_call({Tube, {put, Data, Params}}, _From, State) ->
    {_Index, Server} = findserver(Tube, State),
    Pri = proplists:get_value(pri, Params, 0),
    Delay = proplists:get_value(delay, Params, 0),
    TTR = proplists:get_value(ttr, Params, 60),
    beanstalk_command:send(Server#server.socket, {put, Pri, Delay, TTR, size(Data)}, Data),
    server_response(Server, State);

handle_call({Tube, Command}, _From, State) -> 
    {_Index, Server} = findserver(Tube, State),
    beanstalk_command:send(Server#server.socket, Command),
    server_response(Server, State).
   
%% invoked by
%% gen_server:cast, gen_server:abcast
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle a request not requiring a
%% reply. Unused in flexihash.
%%
-spec handle_cast(Request :: term(),
                  State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast({stop}, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:dispatch)
%%
%% @doc Standard gen_server callback. Handle non-request information.
%% Unused in flexihash.
%%
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_info({recycle, State1, Server}, _State) ->
    Pid = self(),
    erlang:start_timer(300000, Pid, Server),
    {noreply, State1};
handle_info(Server = #server{port=_Port, host=_Host}, State)  ->
    case connecct(Server, State) of
        {error, _Error} -> 
            Pid = self(),
            erlang:start_timer(300000, Pid, Server),
            {noreply, State};
       State1 ->     
            {noreply, State1}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% invoked by
%% (gen_server:terminate)
%%
%% @doc Standard gen_server callback. Clean up State before stopping.
%%
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) ->
                       Ignored :: term().
terminate(_Reason, _State) ->
  ok.


%% invoked by
%% (gen_server:system_code_change)
%%
%% @doc Standard gen_server callback. Change State as a result of a code
%% change during release upgrade or downgrade.
%%
-spec code_change(OldVsn :: (term() | {down, term()}),
                  State :: term(),
                  Extra :: term()) ->
                         {ok, NewState :: term()} |
                         {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% gen_server api

%%
%% @doc API suggested in OTP Design Principles User's Guide. Start
%% this gen_server process.
%%
-spec start_link() ->
                        {ok, Pid :: pid()} |
                        ignore |
                        {error,
                         Error :: {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

server_response(Server, State) ->
    case beanstalk_response:recv(Server#server.socket) of
       {ok, Reply, <<>>} ->  
            {reply, Reply, State};
       {error, closed}   -> 
           Pid = self(),
           spawn(fun() -> recycle_server(Pid,  State, Server) end),
       {reply, closed, State}
    end.


%%% functions internal to your implementation
connecct(Server, State) -> 
    case lists:member(Server, State#server_ring.server_list) of
         true -> State;
         false -> addserver(Server, State)
    end.

%% Generate the positions in the table for the specified target.
%%
location(Server) ->
    Key = lists:concat([Server#server.host, ":", Server#server.port, "-"]),
    SEED = erlang:crc32(Key),
    Points = Server#server.weight * ?CONSISTENT_POINTS,
    location(Server, SEED, Points, 0, []).


location(_Server, _SEED, _Points, _Points , Acc) ->
   Acc;
location(Server, SEED, Points, N, Acc) when N < Points -> 
   Position =  erlang:crc32(SEED, integer_to_list(N)),
   Acc1 = [Position|Acc],
   location(Server, SEED, Points, N+1, Acc1). 

addserver(Server, Serverlist) ->
  case gen_tcp:connect(Server#server.host, Server#server.port, [binary, {packet, 0}, {active, false}]) of
       {ok, Socket} ->
            Server1 = Server#server{socket=Socket},
            Position = location(Server1),
            New = lists:map(fun(P) -> {P, Server1} end, Position),
            ServerNum = Serverlist#server_ring.server_num +1,
            _Serverlist1 = Serverlist#server_ring{
                 sorted = false,
                 precomputed=false,
                 server_num=ServerNum, 
                 points = lists:append(New, Serverlist#server_ring.points),
                 server_list = lists:append([Server],Serverlist#server_ring.server_list )
            };
        Error ->
            {error, Error}
    end.

findserver(_Key, Serverlist) when Serverlist#server_ring.server_num =< 1 ->
     lists:nth( 1,Serverlist#server_ring.points);

findserver(Key, Serverlist) ->
    Servers = lists:sort(Serverlist#server_ring.points),
    Serverlist = if 
       Serverlist#server_ring.precomputed == false  -> 
            Buckets = populate_buckets(Servers),
            Serverlist#server_ring{buckets = Buckets, precomputed=true}
    end,
    Hash = erlang:crc32(Key),
    Pos = Hash rem ?CONSISTENT_BUCKETS,
    lists:nth(Pos,Serverlist#server_ring.buckets).
   
populate_buckets(Servers)->
    populate_buckets(Servers, ?CONSISTENT_BUCKETS, 0, []).
populate_buckets(_Servers, Buckets_num, Buckets_num,  Acc)-> 
    Acc;

populate_buckets(Servers, Buckets_num, N,  Acc) when N < Buckets_num -> 
    Pos = ?CONSISTENT_STEP * N,
    Bucket = buckets_find(Pos, 
                 lists:dropwhile(fun({Index, _}) -> Pos > Index end,
                                 Servers), 
                 Servers),
    Acc1 = [Bucket|Acc],
    populate_buckets(Servers, Buckets_num, N+1,  Acc1).


buckets_find(_Pos, [], []) ->
    {error, empty};
buckets_find(_Pos, [], [{_Index, Server} | _T]) ->
    Server;
buckets_find(Pos, [{Index, Server} | _T], _) when Pos =< Index ->
    Server.

recycle_server(Pid, Server_ring, Server) ->
    Points = lists:dropwhile(fun({_, Point}) -> Server#server.host == Point#server.host andalso Server#server.port == Point#server.port  end,
                       Server_ring#server_ring.points),
    Server_list = lists:dropwhile(fun({_, S}) -> Server#server.host == S#server.host andalso Server#server.port == S#server.port  end,
                       Server_ring#server_ring.server_list),
    Buckets = lists:dropwhile(fun({_, Bucket}) -> Server#server.host == Bucket#server.host andalso Server#server.port == Bucket#server.port  end,
                       Server_ring#server_ring.buckets),
    Server_ring = Server_ring#server_ring{server_list=Server_list, points=Points, server_num=Server_ring#server_ring.server_num -1, buckets =Buckets},
    
    Pid ! {recycle, Server_ring, Server}.
