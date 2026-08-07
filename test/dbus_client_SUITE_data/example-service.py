#!/usr/bin/python3
# Deliberately not /usr/bin/env python3: this script needs the distro-provided
# dbus-python and PyGObject, which only exist for the system interpreter. A
# version manager (asdf, pyenv, ...) shimming python3 would otherwise win and
# the service would die on ImportError, leaving the suite with no bus name.

from gi.repository import GObject
import dbus
import dbus.service
import dbus.glib

class SampleObject(dbus.service.Object):
    def __init__(self, bus_name, object_path="/"):
        dbus.service.Object.__init__(self, bus_name, object_path)

    @dbus.service.method("net.lizenn.dbus.SampleInterface",
                         in_signature='s', out_signature='as')
    def HelloWorld(self, hello_message):
        print (str(hello_message))
        self.SampleSignal(42, 24)
        self.SampleSignal2()
        return ["Hello World", " from example-service"]

    @dbus.service.method("net.lizenn.dbus.SampleInterface",
                         out_signature='as')
    def GetTuple(self):
        return ("Hello Tuple", " from example-service")

    @dbus.service.method("net.lizenn.dbus.SampleInterface")
    def GetDict(self):
        return {"first": "Hello Dict", "second": " from example-service"}

    @dbus.service.method("net.lizenn.dbus.SampleInterface", in_signature='u')
    def GetString(self, size):
        s = ""
        i = size
        while (i > 0):
            s += "x"
            i -= 1
        return s

    @dbus.service.signal("net.lizenn.dbus.SampleInterface")
    def SampleSignal(self, x, y):
        print("SampleSignal")

    @dbus.service.signal("net.lizenn.dbus.SampleInterface")
    def SampleSignal2(self):
        print("SampleSignal2")


session_bus = dbus.SessionBus()
service = dbus.service.BusName("net.lizenn.dbus.SampleService", bus=session_bus)
SampleObject(service, object_path="/root")
SampleObject(service, object_path="/root/child1")
SampleObject(service, object_path="/root/child2/little1")
SampleObject(service, object_path="/root/child2/little2")

mainloop = GObject.MainLoop()
mainloop.run()
