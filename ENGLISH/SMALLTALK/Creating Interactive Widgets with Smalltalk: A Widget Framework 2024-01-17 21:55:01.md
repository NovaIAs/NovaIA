```smalltalk
Object subclass: #Widget [
    initialize: aRectangle {
        super initialize.
        bounds := aRectangle.
        color := Color white.
        minimumSize := Size new: 10 @ 10.
        maximumSize := Size new: 10000 @ 10000.
    }

    bounds: aRectangle { bounds := aRectangle }
    bounds { ^ bounds }

    color: aColor { color := aColor }
    color { ^ color }

    minimumSize: aSize { minimumSize := aSize }
    minimumSize { ^ minimumSize }

    maximumSize: aSize { maximumSize := aSize }
    maximumSize { ^ maximumSize }

    preferredSize { ^ bounds size }

    drawIn: aCanvas {
        aCanvas fillRectangle: bounds withColor: color.
    }

    accept: anEvent { ^ false }

    mouseDown: anEvent {
        lastPosition := anEvent position.
        ^ true
    }

    mouseDragged: anEvent {
        bounds := bounds translatedBy: anEvent position - lastPosition.
        lastPosition := anEvent position.
    }

    mouseUp: anEvent { ^ true }
]

Button subclass: #PushButton [
    initialize: aRectangle target: aTarget selector: aSelector {
        super initialize: aRectangle.
        target := aTarget.
        selector := aSelector.
    }

    accept: anEvent {
        ^ anEvent isMouseUp and: [self containsPoint: anEvent position]
    }

    mouseUp: anEvent {
        if (super mouseUp: anEvent) {
            target perform: selector.
        }
    }
]

Slider subclass: #HorizontalSlider [
    initialize: aRectangle target: aTarget selector: aSelector {
        super initialize: aRectangle.
        target := aTarget.
        selector := aSelector.
        thumbPosition := 0.
    }

    thumbPosition: aFloat { thumbPosition := aFloat }
    thumbPosition { ^ thumbPosition }

    drawIn: aCanvas {
        super drawIn: aCanvas.

        aCanvas fillRectangle: (bounds width - 20) @ (bounds height - 8) @ 20 @ 16
            withColor: Color black.
        aCanvas fillRectangle: (thumbPosition * (bounds width - 20)) @ (bounds height - 12) @ 20 @ 8
            withColor: Color blue.
    }

    accept: anEvent {
        ^ anEvent isMouseDown and: [self containsPoint: anEvent position]
    }

    mouseDown: anEvent {
        if (super mouseDown: anEvent) {
            startPosition := anEvent position.
        }
    }

    mouseDragged: anEvent {
        if (super mouseDragged: anEvent) {
            thumbPosition := (anEvent position x - startPosition x) / (bounds width - 20).
        }
    }

    mouseUp: anEvent {
        if (super mouseUp: anEvent) {
            target perform: selector withArguments: #(thumbPosition).
        }
    }
]
```

This code defines three classes in Smalltalk: `Widget`, `Button`, and `HorizontalSlider`.

The `Widget` class is a superclass for the other two classes. It defines the common properties and behaviors of all widgets, such as their bounds, color, minimum and maximum size, and how to draw themselves.

The `Button` class is a subclass of `Widget`. It represents a button that can be clicked to perform an action. The `Button` class defines the `target` and `selector` properties, which specify the object and method that should be invoked when the button is clicked.

The `HorizontalSlider` class is also a subclass of `Widget`. It represents a horizontal slider that can be used to select a value. The `HorizontalSlider` class defines the `thumbPosition` property, which specifies the position of the slider's thumb.

The code also includes a number of methods that define the behavior of the widgets. For example, the `accept:` method determines whether a widget accepts a particular event, the `mouseDown:` method is called when the mouse button is pressed on a widget, the `mouseDragged:` method is called when the mouse is dragged over a widget, and the `mouseUp:` method is called when the mouse button is released on a widget.

This code is a good example of how Smalltalk can be used to create complex and interactive graphical user interfaces.