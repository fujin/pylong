-module(pylon_cinfo).
-export([cluster_info_init/0, cluster_info_generator_funs/0]).

cluster_info_init() ->
    ok.

cluster_info_generator_funs() ->
    [
     {"Pylon status", fun status/1},
     {"Pylon ringready", fun ringready/1},
     {"Pylon transfers", fun transfers/1}
    ].

%% CPid is the data collector's pid.
status(CPid) ->
    cluster_info:format(CPid, "~p\n", [pylon_status:statistics()]).

ringready(CPid) ->
    cluster_info:format(CPid, "~p\n", [pylon_status:ringready()]).

transfers(CPid) ->
    cluster_info:format(CPid, "~p\n", [pylon_status:transfers()]).
