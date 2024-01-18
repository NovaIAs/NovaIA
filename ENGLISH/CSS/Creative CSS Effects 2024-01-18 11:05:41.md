```css
/* Custom Scrollbar */

::-webkit-scrollbar {
  width: 10px; /* width of the entire scrollbar */
}

::-webkit-scrollbar-track {
  background: #f1f1f1; /* color of the scrollbar track */
}

::-webkit-scrollbar-thumb {
  background: #888; /* color of the scrollbar thumb */
  border-radius: 10px; /* rounded corners of the scrollbar thumb */
}

::-webkit-scrollbar-thumb:hover {
  background: #555; /* hover color of the scrollbar thumb */
}

/* Glow Effect on Button Hover */

.button {
  position: relative;
  display: inline-block;
  padding: 12px 36px;
  border: 2px solid #000;
  text-decoration: none;
  color: #000;
  font-size: 16px;
  font-weight: bold;
  text-transform: uppercase;
  letter-spacing: 1px;
  transition: all 0.3s ease-in-out;
}

.button:hover {
  color: #fff;
  border-color: #fff;
  box-shadow: 0 0 10px #fff, 0 0 40px #fff, 0 0 80px #fff;
}

/* Liquid Text Effect */

.liquid-text {
  position: relative;
  display: inline-block;
  font-size: 36px;
  font-weight: bold;
  text-transform: uppercase;
  letter-spacing: 3px;
  animation: liquid 2s infinite alternate;
}

.liquid-text span {
  position: absolute;
  display: block;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  overflow: hidden;
}

.liquid-text span:nth-child(1) {
  animation: liquid-drop 2s infinite alternate;
  animation-delay: 0.2s;
}

.liquid-text span:nth-child(2) {
  animation: liquid-drop 2s infinite alternate;
  animation-delay: 0.4s;
}

.liquid-text span:nth-child(3) {
  animation: liquid-drop 2s infinite alternate;
  animation-delay: 0.6s;
}

.liquid-text span:nth-child(4) {
  animation: liquid-drop 2s infinite alternate;
  animation-delay: 0.8s;
}

@keyframes liquid {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(10px, -10px, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

@keyframes liquid-drop {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(0, 10px, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

/* Wave Loading Animation */

.wave-loading {
  position: relative;
  width: 100%;
  height: 100%;
  overflow: hidden;
}

.wave-loading .wave {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: linear-gradient(to bottom, #000 0%, #fff 100%);
  animation: wave 2s infinite alternate;
}

.wave-loading .wave:nth-child(2) {
  animation-delay: 1s;
}

@keyframes wave {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(0, -100%, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

/* Glitch Effect */

.glitch {
  position: relative;
  display: inline-block;
  font-size: 36px;
  font-weight: bold;
  text-transform: uppercase;
  letter-spacing: 3px;
  animation: glitch 1s infinite alternate;
}

.glitch span {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  overflow: hidden;
}

.glitch span:nth-child(1) {
  animation: glitch-shift 1s infinite alternate;
  animation-delay: 0.2s;
}

.glitch span:nth-child(2) {
  animation: glitch-shift 1s infinite alternate;
  animation-delay: 0.4s;
}

.glitch span:nth-child(3) {
  animation: glitch-shift 1s infinite alternate;
  animation-delay: 0.6s;
}

.glitch span:nth-child(4) {
  animation: glitch-shift 1s infinite alternate;
  animation-delay: 0.8s;
}

@keyframes glitch {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(0, -10px, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

@keyframes glitch-shift {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(10px, 0, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

/* Diagonal Stripes Pattern */

.diagonal-stripes {
  position: relative;
  width: 100%;
  height: 100%;
  background-image: linear-gradient(45deg, #000 0%, #fff 100%);
  background-size: 20px 20px;
  animation: stripes 2s infinite alternate;
}

@keyframes stripes {
  0% {
    transform: translate3d(0, 0, 0);
  }
  50% {
    transform: translate3d(10px, -10px, 0);
  }
  100% {
    transform: translate3d(0, 0, 0);
  }
}

/* Neon Text Effect */

.neon-text {
  position: relative;
  display: inline-block;
  font-size: 36px;
  font-weight: bold;
  text-transform: uppercase;
  letter-spacing: 3px;
  color: #fff;
  text-shadow: 0 0 10px #ff00ff, 0 0 20px #ff00ff, 0 0 30px #ff00ff, 0 0 40px #ff00ff, 0 0 50px #ff00ff, 0 0 60px #ff00ff, 0 0 70px #ff00ff;
}

/* 3D Cube Rotation Animation */

.cube {
  position: relative;
  width: 200px;
  height: 200px;
  transform-style: preserve-3d;
  animation: cube-rotate 10s infinite linear;
}

.cube .face {
  position: absolute;
  width: 100%;
  height: 100%;
  border: 1px solid #000;
  background-color: #fff;
}

.cube .face:nth-child(1) {
  transform: translateZ(100px);
}

.cube .face:nth-child(2) {
  transform: rotateY(90deg) translateZ(100px);
}

.cube .face:nth-child(3) {
  transform: rotateY(180deg) translateZ(100px);
}

.cube .face:nth-child(4) {
  transform: rotateY(270deg) translateZ(100px);
}

.cube .face:nth-child(5) {
  transform: rotateX(90deg) translateZ(100px);
}

.cube .face:nth-child(6) {
  transform: rotateX(180deg) translateZ(100px);
}

@keyframes cube-rotate {
  0% {
    transform: rotateX(0deg) rotateY(0deg);
  }
  100% {
    transform: rotateX(360deg) rotateY