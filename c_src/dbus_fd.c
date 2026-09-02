/*
 * dbus_fd -- lifecycle primitives for raw OS file descriptors.
 *
 * A descriptor received through SCM_RIGHTS is a new descriptor owned by this
 * process, and OTP has no way to close an arbitrary one: file:close/1 wants an
 * Erlang file handle and socket:close/1 wants a socket, while a D-Bus UNIX_FD
 * may just as well be a pipe, an eventfd, a memfd or a device. These two calls
 * fill that gap and nothing more -- sendmsg/recvmsg stay in OTP's socket
 * module.
 *
 * close(2) is not retried on EINTR: on Linux, and per POSIX.1-2008, the
 * descriptor is released before the signal is delivered, so a retry would
 * close whatever number has been handed out since.
 */
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <erl_nif.h>
/* erl_errno_id/1 -- turns an errno value into the atom file:posix() uses. */
#include <erl_driver.h>

static ERL_NIF_TERM make_errno(ErlNifEnv *env, int error)
{
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_atom(env, erl_errno_id(error)));
}

/* Accepts a non-negative int; anything else is a badarg, never a close(-1). */
static int get_fd(ErlNifEnv *env, ERL_NIF_TERM term, int *fd)
{
    int value;

    if (!enif_get_int(env, term, &value) || value < 0)
        return 0;

    *fd = value;
    return 1;
}

static ERL_NIF_TERM close_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int fd;

    (void)argc;

    if (!get_fd(env, argv[0], &fd))
        return enif_make_badarg(env);

    if (close(fd) == 0)
        return enif_make_atom(env, "ok");

    return make_errno(env, errno);
}

static ERL_NIF_TERM dup_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int fd, copy;

    (void)argc;

    if (!get_fd(env, argv[0], &fd))
        return enif_make_badarg(env);

    /* F_DUPFD_CLOEXEC rather than dup(2): a descriptor kept across an
     * exec(2) by accident is a leak into a child process. */
    copy = fcntl(fd, F_DUPFD_CLOEXEC, 0);
    if (copy < 0)
        return make_errno(env, errno);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, copy));
}

static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)old_priv_data;
    (void)load_info;
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"close", 1, close_nif, 0},
    {"dup", 1, dup_nif, 0}
};

ERL_NIF_INIT(dbus_fd, nif_funcs, NULL, NULL, upgrade, NULL)
