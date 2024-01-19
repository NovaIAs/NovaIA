```tcl
# Create a custom Tkinter widget for displaying a zoomable image
class ZoomableImage(Tkinter.Canvas):
    def __init__(self, master, image_file):
        super().__init__(master)
        self.image = Tkinter.PhotoImage(file=image_file)
        self.scale = 1.0

        # Bind mouse wheel events to zoom in and out
        self.bind("<MouseWheel>", self.mouse_wheel)

        # Bind arrow keys to pan the image
        self.bind("<Left>", lambda event: self.pan(-1, 0))
        self.bind("<Right>", lambda event: self.pan(1, 0))
        self.bind("<Up>", lambda event: self.pan(0, -1))
        self.bind("<Down>", lambda event: self.pan(0, 1))

        # Draw the image
        self.draw_image()

    # Draw the image at the current scale and position
    def draw_image(self):
        width = self.image.width() * self.scale
        height = self.image.height() * self.scale
        self.create_image(width / 2, height / 2, image=self.image, scale=self.scale)

    # Zoom in or out based on the mouse wheel event
    def mouse_wheel(self, event):
        if event.delta > 0:
            self.scale *= 1.1
        else:
            self.scale *= 0.9
        self.draw_image()

    # Pan the image based on the arrow key event
    def pan(self, dx, dy):
        self.xview_scroll(dx, "units")
        self.yview_scroll(dy, "units")

# Create a Tkinter window
root = Tkinter.Tk()

# Create a ZoomableImage widget and display it
image = ZoomableImage(root, "image.png")
image.pack()

# Start the Tkinter event loop
root.mainloop()
```

Explanation:

1. We define a custom Tkinter widget called `ZoomableImage` that extends the `Tkinter.Canvas` class. This widget will be used to display a zoomable image.

2. In the constructor of the `ZoomableImage` class, we initialize the image to be displayed, the initial scale factor, and bind the mouse wheel and arrow key events to the appropriate methods.

3. The `draw_image` method draws the image at the current scale and position.

4. The `mouse_wheel` method handles the mouse wheel event and zooms in or out based on the direction of the wheel.

5. The `pan` method handles the arrow key events and pans the image in the specified direction.

6. We create a Tkinter window and instantiate a `ZoomableImage` widget with the image file "image.png". We then pack the widget into the window and start the Tkinter event loop.

When you run this code, you'll see a Tkinter window with a zoomable image. You can use the mouse wheel to zoom in and out, and the arrow keys to pan the image.