{application, dberl,
 [{description, "D-BUS application"},
  {vsn,"0.0"},
  {modules, [
	     dberl,
	     dberl.auth,
	     dberl.bus,
	     dberl.call,
	     dberl.connection,
	     dberl.gen_dbus,
	     dberl.introspect,
	     dberl.marshaller,
	     dberl.message,
	     dberl.proxy,
	     dberl.service,
	     dberl.tcp_conn,
	     dberl.transport
	    ]},
  {registered, [dberl.bus_reg]},
  {mod, {dberl, []}},
  {env, []},
  {applications, [
		  kernel,
		  stdlib,
		  sasl,
		  crypto
		  ]}]}.
