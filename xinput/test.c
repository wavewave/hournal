#include <gtk/gtk.h>
#include <stdio.h>

gboolean
on_button_press_event           (GtkWidget       *widget,
				 GdkEventButton  *event,
				 gpointer         user_data)
{
  printf(" button pressed \n" ); 

  if( event->device == gdk_device_get_core_pointer() ) {
    printf(" it's core! \n " ) ; 
  }
  else {
    printf(" it's not core! \n" ); 
  }

  return FALSE; 
}

static void callback (GtkWidget *widget, 
		      gpointer data ) 
{
  g_print ("%s was pressed \n" , (char*) data) ; 
}

int 
main (int argc, char *argv [] ) 
{
  GtkWidget *window ; 
  GList* dev_list ; 
  GdkDevice *device; 
  GtkWidget *button; 

  gtk_init (&argc, &argv); 

  printf("hello\n");

  
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL); 

  button = gtk_button_new_with_label ( "hello" ) ; 

  

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
      printf("   yeah this is xinput device \n"); 
      } 
    dev_list = dev_list->next; 
  }
  



  gtk_container_add (GTK_CONTAINER (window), button); 
  gtk_widget_show (button); 
  gtk_widget_show (window); 


  printf("%d \n", GTK_WIDGET(button)->window) ;
  printf("%d \n", GTK_WIDGET(window)->window) ;

  gdk_input_set_extension_events(GTK_WIDGET(window)->window, 
  GDK_POINTER_MOTION_MASK | GDK_BUTTON_MOTION_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK,
  GDK_EXTENSION_EVENTS_ALL);

  g_signal_connect (button, "clicked", 
		    G_CALLBACK (callback), 
		    (gpointer) "cool button"); 

  g_signal_connect (G_OBJECT(button), "button_press_event", 
		    G_CALLBACK(on_button_press_event), 
		    NULL ); 
  g_signal_connect_swapped(G_OBJECT(window), "destroy", 
			   G_CALLBACK(gtk_main_quit), NULL ); 
  gtk_main () ; 

  return 0 ; 

}