dnl erlang.m4
dnl Copyright (C) 2007  Mikael Magnusson
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU Library General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
dnl

AC_DEFUN([AC_ERLANG_LIB_VER],
[AC_CACHE_CHECK([Erlang/OTP '$1' library version], [erlang_cv_lib_ver_$1],
   [if test "$ERLANG_LIB_VER_$1" = ""; then
       erlang_cv_lib_ver_$1=`echo "$ERLANG_LIB_DIR_$1"|sed -e "s/.*-\(.*\)/\1/"`
    fi])
AC_SUBST([ERLANG_LIB_VER_$1], [$erlang_cv_lib_ver_$1])
ERLANG_LIB_VER_SUBST="$ERLANG_LIB_VER_SUBST -e 's,[@]ERLANG_LIB_VER_$1[@],\$(ERLANG_LIB_VER_$1),g'"
AC_SUBST([ERLANG_LIB_VER_SUBST])
]) # AC_ERLANG_LIB_VER
