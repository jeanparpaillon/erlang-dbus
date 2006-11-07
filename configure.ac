#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(dberl, 0.1, [Mikael Magnusson <mikma@users.sourceforge.net>])
AC_CONFIG_SRCDIR([configure.ac])

dnl AC_CANONICAL_HOST
dnl AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE

dnl AM_CONFIG_HEADER([config.h])

AC_ARG_VAR([ERL_FLAGS], [Erlang/OTP emulator flags [none]])dnl

# Checks for programs.
AC_PROG_CC
AC_PROG_AWK
dnl AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_NEED_ERLC
AC_ERLANG_PATH_ERL
AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_CHECK_LIB(kernel)
AC_ERLANG_CHECK_LIB(stdlib)
AC_ERLANG_CHECK_LIB(sasl)
AC_ERLANG_CHECK_LIB(crypto)

AC_DEFUN([AC_ERLANG_LIB_VER],
[AC_CACHE_CHECK([Erlang/OTP '$1' library version], [erlang_cv_lib_ver_$1],
   [if test "$ERLANG_LIB_VER_$1" = ""; then
       erlang_cv_lib_ver_$1=`echo "$ERLANG_LIB_DIR_$1"|sed -e "s/.*-\(.*\)/\1/"`
    fi])
AC_SUBST([ERLANG_LIB_VER_$1], [$erlang_cv_lib_ver_$1])
]) # AC_ERLANG_LIB_VER

AC_ERLANG_LIB_VER(kernel)
AC_ERLANG_LIB_VER(stdlib)
AC_ERLANG_LIB_VER(sasl)
AC_ERLANG_LIB_VER(crypto)

dnl AC_PATH_PROGS([ERLC],[erlc],,[$path])
dnl AC_PATH_PROGS([ERL],[erl],,[$path])
dnl AC_LANG_PUSH(C++)

AC_SUBST(ERL_FLAGS)

AC_CONFIG_FILES([
		 Makefile
		 src/Makefile
		 src/dberl/Makefile
		 example/Makefile
		])

AC_OUTPUT
