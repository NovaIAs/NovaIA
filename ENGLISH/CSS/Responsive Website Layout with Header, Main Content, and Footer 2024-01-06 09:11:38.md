```css
/* Global Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: Arial, sans-serif;
  line-height: 1.6;
  background-color: #f5f5f5;
  color: #333;
}

/* Header Styles */

header {
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
}

header h1 {
  float: left;
  font-size: 24px;
  margin: 0 20px;
  line-height: 60px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

header nav li {
  float: left;
  margin-right: 20px;
}

header nav li a {
  display: block;
  padding: 15px 20px;
  text-decoration: none;
  color: #fff;
}

header nav li a:hover {
  background-color: #444;
}

/* Main Content Styles */

main {
  width: 100%;
  padding: 20px;
}

main h2 {
  font-size: 30px;
  margin-bottom: 20px;
}

main p {
  font-size: 16px;
  line-height: 1.6;
}

/* Footer Styles */

footer {
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
}

footer p {
  float: left;
  margin: 0 20px;
  line-height: 60px;
}

footer social-icons {
  float: right;
}

footer social-icons ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

footer social-icons li {
  float: left;
  margin-right: 20px;
}

footer social-icons li a {
  display: block;
  width: 30px;
  height: 30px;
  line-height: 30px;
  text-align: center;
  background-color: #444;
  color: #fff;
  border-radius: 50%;
}

footer social-icons li a:hover {
  background-color: #555;
}

/* Media Queries */

@media (max-width: 768px) {
  header nav ul {
    display: block;
    margin: 0 auto;
    padding: 20px 0;
  }

  header nav li {
    float: none;
    margin-right: 0;
    margin-bottom: 20px;
  }

  main {
    padding: 20px 10px;
  }
}
```

**Explanation:**

This CSS code defines the styles for a basic HTML website with a header, main content, and footer. It includes global styles, header styles, main content styles, footer styles, and media queries for responsive design.

* **Global Styles:** The global styles define the default styles for all elements on the website, such as the font family, line height, background color, and color.

* **Header Styles:** The header styles define the styles for the website header, including the logo, navigation menu, and social media icons.

* **Main Content Styles:** The main content styles define the styles for the main content of the website, such as the page title, paragraphs, and images.

* **Footer Styles:** The footer styles define the styles for the website footer, including the copyright notice and social media icons.

* **Media Queries:** The media queries define the styles for different screen sizes, ensuring that the website is responsive and looks good on all devices.