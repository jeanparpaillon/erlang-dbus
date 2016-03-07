A native erlang implementation of D-Bus
==============================================

D-BUs is now largely used in a lot of applications for
language-independant, object-oriented RPC system.

The erlang platform needs an erlang native implementation.

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=jeanparpaillon&url=https://github.com/lizenn/erlang-dbus.git&title=erlang-dbus&language=erlang&tags=github&category=software)
[![Build Status](https://travis-ci.org/lizenn/erlang-dbus.svg?branch=master)](https://travis-ci.org/lizenn/erlang-dbus)
[![Hex.pm](https://img.shields.io/hexpm/v/dbus.svg)](hex.pm version)
[![Hex.pm](https://img.shields.io/hexpm/dt/dbus.svg)](hex.pm dowloads)

## Documentation

* [API documentation](doc/README.md)
* [Manuel](https://github.com/lizenn/wiki)

## Current status

### The status:

* consume D-Bus services: ok
* connect through TCP and UNIX socket: ok
* connect through kernel: not ok (but can use unix socket emulation)
* tested: ok. 
  * erocci_backend_dbus includes a Python service with which eorcci_backend_dbus can interact
  * Another implementation of the same D-Bus interface is used in another project (Java): https://github.com/gibello/erocci-dbus-java

### TODO:

* expose D-Bus services
* some authentication mechanisms are not implemented, but architectures allows for easy extension (see https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_cookie_sha1.erl, https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_external.erl and https://github.com/lizenn/erlang-dbus/blob/master/src/dbus_auth_anonymous.erl)
* more doc, more examples
* more tests: unit tests regarding (un)marshaling would be really great, even if the above mentioned xample has rather complex interfaces which works both with Python and Java implementations
* provide facilities for standard interfaces: Properties, ObjectManager, etc.

### [AUTHORS](AUTHORS.md)
