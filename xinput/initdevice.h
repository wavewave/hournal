#include <gtk/gtk.h>
#include <stdio.h>

void initdevice( void );  
{
  GList* dev_list;
  GdkDevice* device;
  dev_list = gdk_devices_list();
  while (dev_list != NULL) {
    printf ("one device\n"); 
    device = (GdkDevice *)dev_list->data;
    if (device != gdk_device_get_core_pointer()) {
#ifdef ENABLE_XINPUT_BUGFIX
      gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE);
      gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE);
#endif
      gdk_device_set_mode(device, GDK_MODE_SCREEN);
      printf("   yeah this is xinput device %s \n", device -> name); 
      } 
    dev_list = dev_list->next; 
  }

}
