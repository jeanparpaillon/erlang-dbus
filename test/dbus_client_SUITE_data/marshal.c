#include <stdlib.h>
#include <stdio.h>

#include <dbus/dbus.h>

void main(int argc, char* argv[]) {
  DBusMessage* m;
  dbus_uint32_t arg_a = 0x11223344;
  char arg_b = 0x42;
  const dbus_uint64_t array[] = {};
  const dbus_uint64_t *arg_c = array;
  char arg_d = 0x23;

  char* wire;
  int len;

  m = dbus_message_new_method_call(NULL,
				   "/",
				   NULL,
				   "Test");
  dbus_message_append_args(m,
			   DBUS_TYPE_UINT32, &arg_a,
			   DBUS_TYPE_BYTE, &arg_b,
			   DBUS_TYPE_ARRAY, DBUS_TYPE_UINT64, &arg_c, 0,
			   DBUS_TYPE_BYTE, &arg_d,
			   DBUS_TYPE_INVALID);
  dbus_message_marshal(m, &wire, &len);

  for (int i = 0; i < len; i++) {
    printf("%02x", wire[i]);
    if ( (i+1) % 8) {
      printf(" ");
    } else {
      printf("\n");
    }
  }
  
  exit(0);
}
