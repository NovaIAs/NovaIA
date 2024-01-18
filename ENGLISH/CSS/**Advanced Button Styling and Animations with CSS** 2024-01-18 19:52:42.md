```css
/* CSS code to create a visually appealing and interactive button with advanced styling and animations: */


/* Basic button styles */
.button {
  display: inline-block;
  padding: 12px 24px;
  border: 1px solid #222;
  border-radius: 4px;
  font-size: 1.4rem;
  font-weight: bold;
  text-align: center;
  text-decoration: none;
  color: #000;
  background-color: #fff;
  transition: all 0.2s ease-in-out;
}

/* Button hover state */
.button:hover {
  background-color: #efefef;
  border-color: #000;
}

/* Button active state */
.button:active {
  background-color: #ddd;
  border-color: #333;
}

/* Button disabled state */
.button[disabled] {
  opacity: 0.5;
  cursor: not-allowed;
}

/* Button with primary style */
.button--primary {
  background-color: #428bca;
  color: #fff;
}

/* Button with secondary style */
.button--secondary {
  background-color: #f0ad4e;
  color: #fff;
}

/* Button with tertiary style */
.button--tertiary {
  border: 1px solid #222;
  background-color: transparent;
  color: #222;
}

/* Button with small size */
.button--small {
  padding: 8px 16px;
  font-size: 1.2rem;
}

/* Button with large size */
.button--large {
  padding: 16px 32px;
  font-size: 1.6rem;
}

/* Button with block width */
.button--block {
  display: block;
  width: 100%;
}

/* Button with rounded corners */
.button--round {
  border-radius: 50%;
}

/* Button with shadow effect */
.button--shadow {
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
}

/* Button with animation */
.button--animated {
  animation: button-animation 1s infinite alternate;
}

@keyframes button-animation {
  from {
    transform: scale(1);
  }
  to {
    transform: scale(1.1);
  }
}

/* Usage examples */
<button class="button">Default</button>
<button class="button button--primary">Primary</button>
<button class="button button--secondary">Secondary</button>
<button class="button button--tertiary">Tertiary</button>

<button class="button button--small">Small</button>
<button class="button button--large">Large</button>

<button class="button button--block">Block</button>
<button class="button button--round">Round</button>

<button class="button button--shadow">Shadow</button>
<button class="button button--animated">Animated</button>
```

**Explanation of the code:**

- We define various CSS classes for different button styles, sizes, and functionalities.
- The `.button` class contains basic button styles, including padding, border, font properties, and transitions.
- Hover, active, and disabled states are styled with separate classes to apply different styles on interaction.
- Different button styles are created with `.button--primary`, `.button--secondary`, and `.button--tertiary` classes.
- Size variations are handled with `.button--small` and `.button--large` classes.
- `.button--block` makes the button span the full width of its container.
- `.button--round` gives the button rounded corners.
- `.button--shadow` adds a subtle drop shadow to the button.
- `.button--animated` applies an infinite scaling animation to the button.
- The `@keyframes button-animation` defines the animation properties for the `.button--animated` class.
- In the usage examples, we show different ways to use these button classes to create various button variations.

This CSS code provides you with a comprehensive range of button styles, sizes, and animations that you can mix and match to create unique and engaging buttons on your website.