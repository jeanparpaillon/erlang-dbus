#!/usr/bin/env python

import dbus
import dbus.service
import dbus.glib
import gobject

class SampleObject(dbus.service.Object):
    def __init__(self, bus_name, object_path="/"):
        dbus.service.Object.__init__(self, bus_name, object_path)

    @dbus.service.method("org.lizenn.dbus.SampleInterface",
                         in_signature='s', out_signature='as')
    def HelloWorld(self, hello_message):
        print (str(hello_message))
        self.SampleSignal(42, 24)
        return ["Hello World", " from example-service.py"]

    @dbus.service.method("org.lizenn.dbus.SampleInterface",
                         out_signature='as')
    def GetTuple(self):
        return ("Hello Tuple", " from example-service.py")

    @dbus.service.method("org.lizenn.dbus.SampleInterface")
    def GetDict(self):
        return {"first": "Hello Dict", "second": " from example-service.py"}

    @dbus.service.signal("org.lizenn.dbus.SampleInterface")
    def SampleSignal(self, x, y):
        pass

session_bus = dbus.SessionBus()
service = dbus.service.BusName("org.lizenn.dbus.SampleService", bus=session_bus)
SampleObject(service, object_path="/root")
SampleObject(service, object_path="/root/child2")
SampleObject(service, object_path="/root/child2/little1")
SampleObject(service, object_path="/root/child2/little2")

mainloop = gobject.MainLoop()
mainloop.run()
