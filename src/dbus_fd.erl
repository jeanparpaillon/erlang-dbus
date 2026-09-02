-module(dbus_fd).
-moduledoc """
Lifecycle primitives for the raw file descriptors a message carries.

A `unix_fd` arriving through `SCM_RIGHTS` is a descriptor this OS process owns:
whoever ends up holding it has to close it, and OTP offers no way to.
""".

-export([
    close/1,
    dup/1
]).

-export_type([fd/0]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-doc "An OS file descriptor, as it appears in the `fds` field of a message.".
-type fd() :: non_neg_integer().

-doc """
Close a descriptor.

`close(2)` is not retried on `eintr`: the descriptor is already released when the
signal is delivered, so a retry would close whichever descriptor has taken the
number since. An `{error, _}` other than `ebadf` therefore still means the
descriptor is gone.
""".
-spec close(fd()) -> ok | {error, file:posix()}.
close(_Fd) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Duplicate a descriptor, close-on-exec.

For the caller that wants to keep a descriptor past the lifetime of the message it
came with: the copy is owned separately and has to be `close/1`d separately.
""".
-spec dup(fd()) -> {ok, fd()} | {error, file:posix()}.
dup(_Fd) ->
    erlang:nif_error(nif_library_not_loaded).

%%%
%%% Internals
%%%
init() ->
    case code:priv_dir(dbus) of
        {error, bad_name} ->
            {error, {load_failed, {no_priv_dir, dbus}}};
        Dir ->
            erlang:load_nif(filename:join(Dir, "dbus_fd"), 0)
    end.

%%%
%%% Tests
%%%
-ifdef(TEST).

%% Beyond any RLIMIT_NOFILE a system will grant, so it is a number no descriptor
%% can have -- unlike a descriptor just closed, whose number the VM may hand out
%% again between the two calls.
-define(NEVER_A_FD, 16#7FFFFFFE).

%% The one way to obtain a descriptor from Erlang. It stays owned by the socket,
%% so only copies of it are ever closed here.
with_fd(Fun) ->
    {ok, Sock} = socket:open(inet, stream, tcp),
    try
        {ok, Fd} = socket:getopt(Sock, {otp, fd}),
        Fun(Fd)
    after
        socket:close(Sock)
    end.

dup_then_close_test() ->
    with_fd(fun(Fd) ->
        {ok, Copy} = dup(Fd),
        ?assertNotEqual(Fd, Copy),
        ?assertEqual(ok, close(Copy))
    end).

dup_of_a_closed_fd_is_ebadf_test() ->
    ?assertEqual({error, ebadf}, dup(?NEVER_A_FD)).

close_of_a_closed_fd_is_ebadf_test() ->
    ?assertEqual({error, ebadf}, close(?NEVER_A_FD)).

bad_fd_test_() ->
    [
        ?_assertError(badarg, close(-1)),
        ?_assertError(badarg, close(not_a_fd)),
        ?_assertError(badarg, dup(-1)),
        ?_assertError(badarg, dup(<<0>>))
    ].

-endif.
