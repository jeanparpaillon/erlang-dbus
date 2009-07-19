{application, dbus_demo,
 [{description, "D-BUS Demo"},
  {vsn,"0.1"},
  {modules, [
	     dbus_example_client,
	     dbus_demo,
	     dbus_demo_app,
	     dbus_demo_hello,
	     dbus_demo_sup
	    ]},
  {registered, [dbus]},
  {mod, {dbus_demo_app, []}},
  {env, []},
  {applications, [
		  kernel,
		  stdlib,
		  sasl,
		  crypto,
		  dbus
		  ]}]}.
