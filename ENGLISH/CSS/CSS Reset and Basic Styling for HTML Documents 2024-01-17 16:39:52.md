```css
/* CSS Reset */

*,
*::before,
*::after {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

html {
  font-size: 62.5%; /* 1rem = 10px */
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 1.6rem; /* 16px */
  line-height: 1.5;
  color: #333;
}

/* Base Styles */

a {
  text-decoration: none;
  color: #007bff;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
  line-height: 1.2;
}

p {
  margin-bottom: 1.5rem;
}

ul,
ol {
  padding-inline-start: 1rem;
}

/* Layout */

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -1rem;
  margin-left: -1rem;
}

.col {
  flex: 1 0 20%;
  max-width: 20%;
  padding-right: 1rem;
  padding-left: 1rem;
}

/* Components */

.btn {
  display: inline-block;
  padding: 0.5rem 1rem;
  border: 1px solid #007bff;
  border-radius: 0.25rem;
  background-color: #007bff;
  color: #fff;
  text-align: center;
  text-decoration: none;
  font-size: 1.2rem;
  line-height: 1.5;
  cursor: pointer;
  transition: all 0.3s ease-in-out;
}

.btn:hover {
  background-color: #0069d9;
}

.card {
  border: 1px solid #e5e5e5;
  border-radius: 0.25rem;
  background-color: #fff;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
}

.card-header {
  border-bottom: 1px solid #e5e5e5;
  padding-bottom: 0.5rem;
}

.card-body {
  padding-top: 0.5rem;
}

.card-footer {
  border-top: 1px solid #e5e5e5;
  padding-top: 0.5rem;
}

/* Utilities */

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-uppercase {
  text-transform: uppercase;
}

.text-lowercase {
  text-transform: lowercase;
}

.font-weight-bold {
  font-weight: bold;
}

.font-weight-light {
  font-weight: light;
}

.bg-primary {
  background-color: #007bff !important;
}

.bg-secondary {
  background-color: #6c757d !important;
}

.bg-success {
  background-color: #28a745 !important;
}

.bg-danger {
  background-color: #dc3545 !important;
}

.bg-warning {
  background-color: #ffc107 !important;
}

.bg-info {
  background-color: #17a2b8 !important;
}

.bg-light {
  background-color: #f8f9fa !important;
}

.bg-dark {
  background-color: #343a40 !important;
}

/* Media Queries */

@media (max-width: 768px) {
  .col {
    flex: 1 0 100%;
    max-width: 100%;
  }
}

/* Custom Styles */

.hero {
  background-image: url(https://source.unsplash.com/1200x800/?nature);
  background-size: cover;
  background-position: center;
  color: #fff;
  text-align: center;
  padding: 5rem 0;
}

.hero h1 {
  font-size: 3.6rem;
  line-height: 1.2;
}

.hero p {
  font-size: 1.8rem;
  