```tcl
# Create a custom Tkinter widget that displays a 3D rotating globe

import tkinter as tk
from PIL import Image, ImageTk
from math import sin, cos, pi

# Define the custom widget class
class Globe(tk.Canvas):
    def __init__(self, master, image_file, radius=100, speed=10):
        super().__init__(master, width=2*radius, height=2*radius)

        # Load the globe image and create a PhotoImage object
        self.image = Image.open(image_file)
        self.image_tk = ImageTk.PhotoImage(self.image)

        # Set the widget's size and position
        self.radius = radius
        self.x = radius
        self.y = radius

        # Set the globe's rotation speed
        self.speed = speed

        # Bind the mouse wheel event to the zoom() method
        self.bind("<MouseWheel>", self.zoom)

        # Start the animation loop
        self.animate()

    def zoom(self, event):
        # Increase or decrease the radius of the globe based on the mouse wheel delta
        if event.delta > 0:
            self.radius += 10
        elif event.delta < 0 and self.radius > 10:
            self.radius -= 10

        # Update the widget's size and position
        self.configure(width=2*self.radius, height=2*self.radius)
        self.x = self.radius
        self.y = self.radius

    def animate(self):
        # Calculate the new rotation angle
        angle = (self.speed / 1000) * self.t

        # Rotate the globe image
        self.image = self.image.rotate(angle)
        self.image_tk = ImageTk.PhotoImage(self.image)

        # Redraw the globe
        self.delete("all")
        self.create_image(self.x, self.y, image=self.image_tk)

        # Schedule the next animation frame
        self.t += 1
        self.after(10, self.animate)

# Create the main Tkinter window
root = tk.Tk()
root.title("3D Rotating Globe")

# Create the globe widget
globe = Globe(root, "earth.png")
globe.pack()

# Start the main loop
root.mainloop()
```

Explanation:

* The code creates a custom Tkinter widget called `Globe` that displays a 3D rotating globe.
* The `Globe` class inherits from the `tk.Canvas` class, which provides the basic functionality for drawing on a canvas.
* The `__init__()` method of the `Globe` class initializes the widget with the following parameters:
    * `master`: The parent widget of the globe widget.
    * `image_file`: The path to the image file that will be used to create the globe.
    * `radius`: The radius of the globe in pixels.
    * `speed`: The speed of the globe's rotation in degrees per second.
* The `zoom()` method of the `Globe` class handles the mouse wheel event and zooms in or out the globe based on the mouse wheel delta.
* The `animate()` method of the `Globe` class is responsible for animating the globe. It calculates the new rotation angle, rotates the globe image, redraws the globe, and schedules the next animation frame.
* The `create_image()` method of the `Globe` class is used to draw the globe image on the canvas.
* The `pack()` method of the `Globe` class is used to add the globe widget to the main window.
* The `mainloop()` method of the `root` window is used to start the main event loop.