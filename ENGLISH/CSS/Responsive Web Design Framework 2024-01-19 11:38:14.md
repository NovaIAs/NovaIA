```css
/* Reset Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

body {
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  font-size: 16px;
  line-height: 1.5em;
  color: #333;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
  margin-bottom: 10px;
}

h1 {
  font-size: 36px;
}

h2 {
  font-size: 28px;
}

h3 {
  font-size: 22px;
}

h4 {
  font-size: 18px;
}

h5 {
  font-size: 16px;
}

h6 {
  font-size: 14px;
}

p {
  margin-bottom: 10px;
}

/* Layout */

.container {
  max-width: 1200px;
  padding: 0 15px;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.col {
  flex: 1 0 100%;
  padding: 0 15px;
}

.col-1 {
  flex: 1 0 8.333333%;
}

.col-2 {
  flex: 1 0 16.666667%;
}

.col-3 {
  flex: 1 0 25%;
}

.col-4 {
  flex: 1 0 33.333333%;
}

.col-5 {
  flex: 1 0 41.666667%;
}

.col-6 {
  flex: 1 0 50%;
}

.col-7 {
  flex: 1 0 58.333333%;
}

.col-8 {
  flex: 1 0 66.666667%;
}

.col-9 {
  flex: 1 0 75%;
}

.col-10 {
  flex: 1 0 83.333333%;
}

.col-11 {
  flex: 1 0 91.666667%;
}

.col-12 {
  flex: 1 0 100%;
}

.offset-1 {
  margin-left: 8.333333%;
}

.offset-2 {
  margin-left: 16.666667%;
}

.offset-3 {
  margin-left: 25%;
}

.offset-4 {
  margin-left: 33.333333%;
}

.offset-5 {
  margin-left: 41.666667%;
}

.offset-6 {
  margin-left: 50%;
}

.offset-7 {
  margin-left: 58.333333%;
}

.offset-8 {
  margin-left: 66.666667%;
}

.offset-9 {
  margin-left: 75%;
}

.offset-10 {
  margin-left: 83.333333%;
}

.offset-11 {
  margin-left: 91.666667%;
}

/* Colors */

$primary-color: #007bff;
$secondary-color: #6c757d;
$success-color: #28a745;
$info-color: #17a2b8;
$warning-color: #ffc107;
$danger-color: #dc3545;

/* Background Colors */

$bg-primary: #007bff;
$bg-secondary: #6c757d;
$bg-success: #28a745;
$bg-info: #17a2b8;
$bg-warning: #ffc107;
$bg-danger: #dc3545;

/* Text Colors */

$text-primary: #ffffff;
$text-secondary: #ffffff;
$text-success: #ffffff;
$text-info: #ffffff;
$text-warning: #ffffff;
$text-danger: #ffffff;

/* Borders */

$border-primary: #007bff;
$border-secondary: #6c757d;
$border-success: #28a745;
$border-info: #17a2b8;
$border-warning: #ffc107;
$border-danger: #dc3545;

/* Buttons */

.btn {
  display: inline-block;
  padding: 6px 12px;
  margin-