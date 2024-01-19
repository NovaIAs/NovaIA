```css
/* Global Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

html,
body {
  height: 100%;
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #333;
}

/* Header */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #fff;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.logo {
  float: left;
  margin-top: 15px;
  margin-left: 20px;
}

.logo img {
  height: 30px;
}

nav {
  float: right;
  margin-top: 10px;
  margin-right: 20px;
}

nav ul {
  list-style-type: none;
  display: flex;
}

nav li {
  margin-right: 20px;
}

nav a {
  text-decoration: none;
  color: #333;
}

nav a:hover {
  color: #000;
}

/* Main Content */

main {
  margin-top: 60px;
  padding: 20px;
}

.section {
  margin-bottom: 20px;
}

.section-title {
  font-size: 24px;
  margin-bottom: 10px;
}

.section-content {
  font-size: 16px;
}

/* Footer */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #f9f9f9;
  text-align: center;
}

footer p {
  margin: 0;
  padding: 10px;
  font-size: 12px;
  color: #999;
}
```

This CSS code creates a basic website layout with a header, main content area, and footer. The header contains a logo and navigation menu, the main content area contains multiple sections with titles and content, and the footer contains a copyright notice. The code is well-commented and easy to understand.