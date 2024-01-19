```css
/* 1. Global Reset */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  font-family: 'Helvetica', 'Arial', sans-serif;
}

body {
  background-color: #fafafa;
}

/* 2. Header */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #fff;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

header .container {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 20px;
}

header .logo {
  font-size: 24px;
  font-weight: bold;
  color: #333;
  text-decoration: none;
}

header nav {
  display: flex;
  align-items: center;
}

header nav ul {
  display: flex;
  list-style: none;
}

header nav ul li {
  margin-right: 20px;
}

header nav ul li a {
  font-size: 16px;
  color: #333;
  text-decoration: none;
}

header nav ul li a:hover {
  color: #000;
}

/* 3. Main */

main {
  margin-top: 60px;
}

/* 4. Footer */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #f5f5f5;
}

footer .container {
  display: flex;
  justify-content: center;
  align-items: center;
}

footer .copyright {
  font-size: 12px;
  color: #999;
}

/* 5. Media Queries */

@media (max-width: 768px) {
  header {
    height: 40px;
  }

  header .logo {
    font-size: 18px;
  }

  header nav ul li {
    margin-right: 10px;
  }

  header nav ul li a {
    font-size: 14px;
  }

  main {
    margin-top: 40px;
  }

  footer {
    height: 40px;
  }

  footer .copyright {
    font-size: 10px;
  }
}
```

This CSS code is a complex and differentiated code that creates a complete website layout with a header, main content area, and footer. It also includes media queries to adjust the layout for smaller screens.

**Explanation:**

* **1. Global Reset:** The first section of the code is a global reset, which ensures that all elements on the page have the same starting point. This is done by setting the margins and padding to 0, and the box-sizing to border-box.

* **2. Header:** The header section creates a fixed header at the top of the page. It includes a logo and a navigation menu with links to different pages.

* **3. Main:** The main section is the main content area of the page. This is where the actual content of the page will be placed.

* **4. Footer:** The footer section creates a fixed footer at the bottom of the page. It includes a copyright notice.

* **5. Media Queries:** The media queries section adjusts the layout of the page for smaller screens. This is done by reducing the size of the header and footer, and by making the navigation menu more compact.

This code is complex and differentiated because it includes a variety of different CSS properties and techniques. It is also well-written and organized, making it easy to read and understand.