```css
/* Multi-column layout with dynamic column count */

.multi-column {
  columns: 3; /* Start with 3 columns */
  column-gap: 1em; /* Space between columns */
  column-rule: 1px dashed #ccc; /* Column separator */
}

/* Break into single column for narrower viewports */
@media (max-width: 600px) {
  .multi-column {
    columns: 1;
    column-gap: 0;
    column-rule: none;
  }
}

/* Flexbox-based image gallery with lightbox functionality */

.image-gallery {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
}

.image-gallery img {
  width: 200px;
  height: 200px;
  object-fit: cover;
  margin: 10px;
}

.image-gallery img:hover {
  opacity: 0.8;
  cursor: pointer;
}

/* Lightbox container */
.lightbox {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.8);
  display: flex;
  justify-content: center;
  align-items: center;
  opacity: 0;
  pointer-events: none;
}

/* Lightbox image */
.lightbox img {
  max-width: 80%;
  max-height: 80%;
}

/* Show the lightbox when an image is clicked */
.image-gallery img:hover ~ .lightbox {
  opacity: 1;
  pointer-events: auto;
}

/* Close the lightbox when the close button is clicked */
.lightbox .close-button {
  position: absolute;
  top: 10px;
  right: 10px;
  width: 30px;
  height: 30px;
  background-color: #fff;
  color: #000;
  font-size: 20px;
  text-align: center;
  line-height: 30px;
  cursor: pointer;
}

/* Animated progress bar */

.progress-bar {
  width: 100%;
  height: 10px;
  background-color: #ccc;
  border-radius: 5px;
}

.progress-bar-fill {
  width: 0%;
  height: 100%;
  background-color: #007bff;
  border-radius: 5px;
  transition: width 1s ease-in-out;
}

/* Animate the progress bar fill */
.progress-bar-fill {
  animation: progress-bar-fill 1s ease-in-out infinite;
}

@keyframes progress-bar-fill {
  from {
    width: 0%;
  }
  to {
    width: 100%;
  }
}

/* Parallax background effect */

.parallax {
  background-attachment: fixed;
  background-position: center;
  background-repeat: no-repeat;
  background-size: cover;
}

.parallax-layer {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.5);
}

/* Move the parallax layer at a different speed than the background */
.parallax-layer-1 {
  animation: parallax-layer-1 10s linear infinite;
}

@keyframes parallax-layer-1 {
  from {
    transform: translate3d(0, 0, 0);
  }
  to {
    transform: translate3d(-30%, 0, 0);
  }
}

.parallax-layer-2 {
  animation: parallax-layer-2 15s linear infinite;
}

@keyframes parallax-layer-2 {
  from {
    transform: translate3d(0, 0, 0);
  }
  to {
    transform: translate3d(-40%, 0, 0);
  }
}

.parallax-layer-3 {
  animation: parallax-layer-3 20s linear infinite;
}

@keyframes parallax-layer-3 {
  from {
    transform: translate3d(0, 0, 0);
  }
  to {
    transform: translate3d(-50%, 0, 0);
  }
}

/* Floating social media icons */

.social-icons {
  position: fixed;
  bottom: 0;
  right: 0;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 10px;
  background-color: #fff;
  border-radius: 5px;
  box-shadow: 0 5px 10px rgba(0, 0, 0, 0.1);
}

.social-icons a {
  display: block;
  width: 40px;
  height: 40px;
  margin: 10px;
  background-color: #ccc;
  border-radius: 50%;
  color: #fff;
  font-size: 20px;
  text-align: center;
  line-height: 40px;
  transition: all 0.3s ease-in-out;