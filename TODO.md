# TODO

* Figure out why `make ct` tests don't run on github actions
* Fix signal emission from services
* Make dializer happy
* Some authentication mechanisms are not implemented, but architectures allows for easy extension (see https://github.com/jeanparpaillon/erlang-dbus/blob/master/src/dbus_auth_cookie_sha1.erl, https://github.com/jeanparpaillon/erlang-dbus/blob/master/src/dbus_auth_external.erl and https://github.com/jeanparpaillon/erlang-dbus/blob/master/src/dbus_auth_anonymous.erl)
* Create new gen_dbus.erl that uses `handle_dbus_call(Name, Args)` form instead of current.
* Cleanup Supervisor & gen_server hierarchy
  * Remove superfluos gen_servers (proxy / peer / dbus)
  * Make service registration more explicit when using multiple busses
* Fix indentation
* More docs & examples
* More tests: unit tests regarding (un)marshaling would be really great, even if the above mentioned xample has rather complex interfaces which works both with Python and Java implementations
* Provide facilities for standard interfaces: Properties, ObjectManager, etc.
