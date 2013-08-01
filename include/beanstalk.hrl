%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% author Christopher Vance <cjsv@abacorix.com>
%% Copyright (c) 2012 Christopher Vance

%%% implementation records

-record(server, {host, port=11300, weight=1, socket}).
-record(server_ring, {sorted,precomputed,points,buckets,server_num=0, server_list=[]}).

-define(INIT_SEED, 16#ffffffff).
-define(CONSISTENT_POINTS, 160).
-define(CONSISTENT_BUCKETS, 1024).
-define(CONSISTENT_STEP,  16#ffffffff div ?CONSISTENT_BUCKETS).