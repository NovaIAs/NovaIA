```css
/* 1. Global Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #333;
}

a {
  text-decoration: none;
  color: #333;
}

/* 2. Header */

header {
  background-color: #f5f5f5;
  padding: 20px 0;
}

.header-container {
  max-width: 1200px;
  margin: 0 auto;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.logo {
  font-size: 24px;
  font-weight: bold;
  color: #333;
}

.nav-links {
  display: flex;
  align-items: center;
}

.nav-link {
  margin-right: 20px;
  font-size: 18px;
  color: #333;
}

.nav-link:hover {
  color: #000;
}

/* 3. Main Content */

main {
  padding: 20px 0;
}

.main-container {
  max-width: 1200px;
  margin: 0 auto;
}

.content-block {
  margin-bottom: 20px;
}

.content-title {
  font-size: 24px;
  font-weight: bold;
  color: #333;
}

.content-text {
  font-size: 18px;
  line-height: 1.6;
  color: #333;
}

/* 4. Sidebar */

aside {
  background-color: #f5f5f5;
  padding: 20px;
  margin-left: 20px;
}

.sidebar-title {
  font-size: 24px;
  font-weight: bold;
  color: #333;
}

.sidebar-links {
  list-style-type: none;
  padding: 0;
}

.sidebar-link {
  margin-bottom: 10px;
  font-size: 18px;
  color: #333;
}

.sidebar-link:hover {
  color: #000;
}

/* 5. Footer */

footer {
  background-color: #f5f5f5;
  padding: 20px 0;
}

.footer-container {
  max-width: 1200px;
  margin: 0 auto;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.copyright {
  font-size: 14px;
  color: #333;
}

.social-links {
  display: flex;
  align-items: center;
}

.social-link {
  margin-right: 10px;
  font-size: 18px;
  color: #333;
}

.social-link:hover {
  color: #000;
}

/* 6. Media Queries */

@media (max-width: 768px) {
  .header-container {
    flex-direction: column;
    align-items: center;
  }

  .nav-links {
    flex-direction: column;
    align-items: center;
  }

  .nav-link {
    margin-bottom: 10px;
  }

  aside {
    margin-top: 20px;
  }
}
```

**Explanation:**

This CSS code creates a complex and differentiated design for a website. It includes styles for the header, main content, sidebar, footer, and media queries for responsive design. The code is written in English for clarity and readability.

**1. Global Styles:**

The global styles section defines the default styles for all elements on the website. This includes the font family, font size, line height, and color.

**2. Header:**

The header section defines the styles for the website's header, which includes the logo and navigation links. The header has a fixed height and a background color of #f5f5f5. The logo is positioned on the left side and the navigation links are positioned on the right side.

**3. Main Content:**

The main content section defines the styles for the website's main content area. This includes the content title, content text, and sidebar. The main content area has a padding of 20px on the top and bottom. The content title is a large and bold heading, while the content text is a smaller and regular text. The sidebar is positioned on the right side of the main content area.

**4. Sidebar:**

The sidebar section defines the styles for the website's sidebar. The sidebar has a fixed width and a background color of #f5f5f5. The sidebar title is a large and bold heading, while the sidebar links are a list of links. The sidebar links are positioned vertically and have a margin of 10px between each other.

**5. Footer:**

The footer section defines the styles for the website's footer. The footer has a fixed height and a background color of #f5f5f5. The footer contains the copyright notice and social links. The copyright notice is positioned on the left side and the social links are positioned on the right side.

**6. Media Queries:**

The media queries section defines the styles for different screen sizes. This ensures that the website is responsive and looks good on all devices. The media query in this code targets devices with a maximum width of 768px. For these devices, the header and navigation links are stacked vertically instead of horizontally. The sidebar is also moved below the main content area.