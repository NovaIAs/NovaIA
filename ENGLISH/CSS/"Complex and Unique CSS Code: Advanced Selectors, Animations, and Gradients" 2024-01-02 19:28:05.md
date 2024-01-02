Certainly! Here's a complex and unique CSS code that utilizes advanced selectors, animations, and gradients:

```css
/* Select all paragraphs inside a div with class "container" */
div.container p {
  font-family: 'Arial', sans-serif;
  font-size: 18px;
  line-height: 1.5;
  color: #333;
}

/* Apply an animated gradient background to headings inside the container */
div.container h1,
div.container h2,
div.container h3 {
  background: linear-gradient(to right, #ffcc00, #ff6699);
  background-size: 200% 200%;
  -webkit-animation: gradientAnimation 4s infinite;
  animation: gradientAnimation 4s infinite;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}

/* Keyframes for the gradient animation */
@-webkit-keyframes gradientAnimation {
  0% {
    background-position: 100% 0%;
  }
  50% {
    background-position: 0% 100%;
  }
  100% {
    background-position: 100% 0%;
  }
}

@keyframes gradientAnimation {
  0% {
    background-position: 100% 0%;
  }
  50% {
    background-position: 0% 100%;
  }
  100% {
    background-position: 100% 0%;
  }
}

/* Add a box shadow to images inside a div with class "image-container" */
div.image-container img {
  box-shadow: 0px 5px 10px rgba(0, 0, 0, 0.3);
}

/* Apply a diagonal stripe pattern to unordered lists inside a div with class "list-container" */
div.list-container ul {
  background-image: linear-gradient(45deg, rgba(0,0,0,0.2) 25%, transparent 25%, transparent 50%, rgba(0,0,0,0.2) 50%, rgba(0,0,0,0.2) 75%, transparent 75%, transparent);
  background-size: 10px 10px;
}

/* Style links with a hover effect */
a {
  color: #333;
  transition: color 0.3s ease;
}

a:hover {
  color: #ff6699;
}
```

Explanation:
- The code starts by selecting all paragraphs inside a div with the class "container" and applies some basic styling such as font family, size, color, and line height.
- Next, the code targets headings (h1, h2, and h3) inside the same container and applies a gradient background animation. The gradient animation is achieved by using keyframes that gradually change the background position, creating a smooth transition effect. The headings also have a special effect where their text is filled with the gradient using the `background-clip` and `text-fill-color` properties.
- Then, the code selects images inside a div with the class "image-container" and adds a box shadow to give them a subtle 3D effect.
- After that, unordered lists inside a div with the class "list-container" receive a diagonal stripe pattern as their background. This is achieved by using a linear gradient with transparent and semi-transparent colors at specific intervals.
- Finally, the code styles links with a hover effect, changing the color smoothly on hover.

This code combines various CSS techniques to create a visually appealing and unique design for a web page. Feel free to modify and adapt it to your specific needs.