-module(pylon_app).

-behaviour(application).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-export([start/2, stop/1]).

%% App behavior hooks
-spec start(_StartType, _StartArgs) -> {_, _}.
start(_StartType, _StartArgs) ->
    %% Look at the epoch and generating an error message if it doesn't match up
    %% to our expectations
    check_epoch(),
    %% TODO: don't think we need this yet
    catch cluster_info:register_app(pylon_cinfo),
    case pylon_sup:start_link() of
        {ok, Pid} ->
            riak_core_capability:register({pylon, vnode_vclocks},
                                          [true, false],
                                          false,
                                          {pylon,
                                           vnode_vclocks,
                                           [{true, true}, {false, false}]}),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(_State) -> ok.
%% @spec stop(_State) -> ok
%% @doc
stop(_State) ->
    ok.

%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).

-spec check_epoch() -> ok.
%% @spec check_epoch() -> ok
%% @doc
check_epoch() ->
    %% doc for erlang:now/0 says return value is platform-dependent
    %% -> let's emit an error if this platform doesn't think the epoch
    %%    is Jan 1, 1970
    {MSec, Sec, _} = erlang:now(),
    GSec = calendar:datetime_to_gregorian_seconds(
             calendar:universal_time()),
    case GSec - ((MSec*1000000)+Sec) of
        N when (N < ?SEC_TO_EPOCH+5 andalso N > ?SEC_TO_EPOCH-5);
        (N < -?SEC_TO_EPOCH+5 andalso N > -?SEC_TO_EPOCH-5) ->            %% if epoch is within 10 sec of expected, accept it
            ok;
        N ->
            Epoch = calendar:gregorian_seconds_to_datetime(N),
            lager:error("Pylon expects your system's epoch to be Jan 1, 1970,"
                        "but your system says the epoch is ~p", [Epoch]),
            ok
    end.
