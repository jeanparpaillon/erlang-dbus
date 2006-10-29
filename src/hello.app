{application, hello,
 [{description, "D-BUS Hello example"},
  {vsn,"0.0"},
  {modules, [
	     dbus,
	     hello
	    ]},
  {registered, [dbus]},
  {mod, {hello_app, []}},
  {env, []},
  {applications, [
		  kernel,
		  stdlib,
		  sasl,
		  crypto,
		  dberl
		  ]}]}.
