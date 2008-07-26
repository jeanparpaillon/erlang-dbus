{application, hello,
 [{description, "D-BUS Hello example"},
  {vsn,"0.1"},
  {modules, [
	     hello,
	     hello_app,
	     hello_sup
	    ]},
  {registered, [dbus]},
  {mod, {hello_app, []}},
  {env, []},
  {applications, [
		  kernel,
		  stdlib,
		  sasl,
		  crypto,
		  dbus
		  ]}]}.
