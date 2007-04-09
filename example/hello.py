#!/usr/bin/env python

import dbus

bus = dbus.SessionBus()
remote_object = bus.get_object("org.za.hem.DBus", "/Root")

dbus_interface = "org.designfu.SampleInterface"
iface = dbus.Interface(remote_object, dbus_interface)

hello_reply_list = remote_object.HelloWorld(17, "Hello from example-client.py!", dbus_interface=dbus_interface)
print (hello_reply_list)

remote_object.HelloWorld(1, {"php":"Rasmus Lerdorf",\
                             "perl":"Larry Wall",\
                             "python":"Guido van Rossum"})

hello_reply_tuple = iface.GetTuple()
print str(hello_reply_tuple)

hello_reply_dict = iface.GetDict()
print str(hello_reply_dict)
