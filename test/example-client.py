#!/usr/bin/env python

import dbus

bus = dbus.SessionBus()
remote_object = bus.get_object("org.za.hem.DBus", "/Root")

remote_object = bus.get_object("org.designfu.SampleService", "/SomeObjectX")
iface = dbus.Interface(remote_object, "org.designfu.SampleInterface")

hello_reply_list = remote_object.HelloWorld("Hello from example-client.py!", dbus_interface = "org.designfu.SampleInterface")

hello_reply_tuple = iface.GetTuple()

hello_reply_dict = iface.GetDict()

print (hello_reply_list)

print str(hello_reply_tuple)

print str(hello_reply_dict)

print remote_object.Ping(dbus_interface="org.freedesktop.DBus.Peer")

print remote_object.Introspect(dbus_interface="org.freedesktop.DBus.Introspectable")
