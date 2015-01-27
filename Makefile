# Copyright 2015 Jean Parpaillon, all rights reserved
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
PROJECT = dbus

DEPS = lager procket inert
dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_procket = git https://github.com/msantos/procket.git master
dep_inert = git https://github.com/msantos/inert.git 0.2.1

COMPILE_FIRST = auth/dbus_auth

include erlang.mk

src/auth/dbus_auth_external.erl: ebin/dbus_auth.beam

src/auth/dbus_auth_cookie_sha1.erl: ebin/dbus_auth.beam

ebin/dbus_auth.beam: src/auth/dbus_auth.erl
