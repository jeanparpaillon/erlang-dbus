-module(dbus_auth).
-moduledoc """
Authentication utils
""".
-include_lib("kernel/include/file.hrl").

-export([detect_uid/0]).

%% `/proc/self' is owned by the uid the process runs as, which is the uid
%% that owns the keyring being read. `id -u' is the portable answer.
-spec detect_uid() -> {ok, integer()} | error.
detect_uid() ->
    case file:read_file_info("/proc/self") of
        {ok, #file_info{uid = Uid}} when is_integer(Uid) ->
            {ok, Uid};
        _ ->
            uid_from_id_command()
    end.

uid_from_id_command() ->
    case string:trim(os:cmd("id -u 2>/dev/null")) of
        [] ->
            error;
        Out ->
            case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Out) of
                true -> {ok, list_to_integer(Out)};
                false -> error
            end
    end.
