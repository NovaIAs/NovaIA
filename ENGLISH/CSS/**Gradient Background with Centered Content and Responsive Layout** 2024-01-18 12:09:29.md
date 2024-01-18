```css
/* Outer Container */
.outer-container {
  width: 100vw;
  height: 100vh;
  background: linear-gradient(
    45deg,
    #FF416C 0%,
    #FFC30F 25%,
    #06B782 50%,
    #073B4C 75%,
    #7A52BE 100%
  );
}

/* Inner Container */
.inner-container {
  position: relative;
  width: 90%;
  height: 90%;
  margin: 5% auto;
  background: #fff;
}

/* Content Area */
.content-area {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 80%;
  height: 80%;
  padding: 30px;
}

/* Heading */
.heading {
  font-size: 36px;
  font-weight: 700;
  color: #333;
  margin-bottom: 20px;
}

/* Paragraph */
p {
  font-size: 18px;
  line-height: 1.6;
  color: #666;
}

/* Button */
.btn {
  display: inline-block;
  padding: 10px 20px;
  background: #FF416C;
  color: #fff;
  font-size: 16px;
  font-weight: 600;
  border: none;
  border-radius: 5px;
  cursor: pointer;
  transition: all 0.3s ease-in-out;
}

/* Hover Effect on Button */
.btn:hover {
  background: #FFC30F;
  color: #000;
}

/* Media Query for Smaller Screens */
@media screen and (max-width: 1024px) {
  .inner-container {
    width: 95%;
    height: 95%;
  }

  .content-area {
    width: 90%;
    height: 90%;
  }
}

/* Media Query for Even Smaller Screens */
@media screen and (max-width: 768px) {
  .heading {
    font-size: 28px;
  }

  p {
    font-size: 16px;
  }

  .btn {
    font-size: 14px;
    padding: 8px 16px;
  }
}
```

Explanation:

1. **Outer Container**: This defines the outer container that takes up the entire viewport and sets the background gradient.

2. **Inner Container**: The inner container is positioned in the center of the outer container, with some margin and a white background.

3. **Content Area**: This is an absolutely positioned element, centered within the inner container. It holds the actual content of the page.

4. **Heading and Paragraph**: The content area contains a heading and a paragraph, styled with appropriate font sizes, colors, and spacing.

5. **Button**: A simple button element with a hover effect to change its background color and text color.

6. **Media Queries**: The code uses media queries to adjust the layout for smaller screens, ensuring that the content is readable and usable on various devices.

This code creates a visually appealing page with a gradient background, centered content, and a button. It also demonstrates the use of media queries to make the layout responsive.