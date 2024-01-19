```javascript
// MODERN JAVASCRIPT MULTIPLE FILES BUILD SYSTEM WITH WEBPACK
// Non-Bundled Libraries
import React from 'react';
import ReactDOM from 'react-dom';

// Webpack Generated Bundles
import webpackBundleEsm from './es-modules-bundle-webpack';
import webpackBundleCjs from './cjs-bundle-webpack';

// Import from Non-Bundled Libraries
const name1 = "John";
const message1 = `webpackBundleEsm.name1: ${webpackBundleEsm.name1}`;
const name2 = webpackBundleEsm.name2;
const message2 = `webpackBundleEsm.name2: ${webpackBundleEsm.name2}`;

// Import from Webpack Bundles
const name3 = webpackBundleCjs.name3;
const message3 = `webpackBundleCjs.name3: ${webpackBundleCjs.name3}`;
const name4 = webpackBundleCjs.default;
const message4 = `webpackBundleCjs.default: ${webpackBundleCjs.default}`;

// Log the concatenated strings to the console
console.log(`${message1} - ${message2} - ${message3} - ${message4}`);

// Render the React component
ReactDOM.render(<h1>{name1} - {name2} - {name3} - {name4}</h1>, document.getElementById('root'));
```

This code demonstrates a complex JavaScript project setup using Webpack, which is a tool for bundling JavaScript modules. It imports multiple JavaScript files, some from non-bundled libraries and some from Webpack bundles. The code then concatenates strings from these files and logs them to the console. Finally, it uses the ReactDOM library to render a React component that displays the concatenated strings.

Here's an explanation of the code:

1. We start by importing React and ReactDOM from the non-bundled libraries.

2. Then, we import the Webpack-generated bundles, which contain the bundled JavaScript modules.

3. We import a variable, `name1`, from a non-bundled library, and define a string message that includes the value of `name1` imported from the Webpack bundle `webpackBundleEsm`.

4. We import another variable, `name2`, directly from the Webpack bundle `webpackBundleEsm`.

5. Similarly, we import variables `name3` and `name4` from the Webpack bundle `webpackBundleCjs`.

6. We log the concatenated strings, which include the values of the imported variables, to the console.

7. Finally, we use ReactDOM to render a React component that displays the concatenated strings in a browser.

This complex setup demonstrates the use of Webpack to bundle JavaScript modules from different sources, including non-bundled libraries and Webpack bundles. It also shows how to import and utilize modules from both types of sources in a JavaScript project.