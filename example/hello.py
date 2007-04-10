#!/usr/bin/env python

import dbus
import gobject
from dbus.mainloop.glib import DBusGMainLoop

def hello_signal_handler(x, y):
    print ("Received signal '%s.%s'"
           % (x, y))

def catchall_signal_handler(*args, **kwargs):
    print ("Caught signal (in catchall handler) "
           + kwargs['dbus_interface'] + "." + kwargs['member'])
    for arg in args:
        print "        " + str(arg)


DBusGMainLoop(set_as_default=True)

bus = dbus.SessionBus()
remote_object = bus.get_object("org.za.hem.DBus", "/Root")

remote_object.connect_to_signal("OnClick", hello_signal_handler)
#bus.add_signal_receiver(catchall_signal_handler, interface_keyword='dbus_interface', member_keyword='member')

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

loop = gobject.MainLoop()
loop.run()
