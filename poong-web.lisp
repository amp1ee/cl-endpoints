(load "quicklisp/setup.lisp")
;;;; app.lisp
(ql:quickload '(:ningle :clack :clack-handler-hunchentoot :spinneret))

(defpackage #:pong-web
  (:use #:cl #:ningle)
  (:import-from #:spinneret #:with-html-string))
(in-package #:pong-web)

(defparameter *app* (make-instance 'ningle:app))
(defparameter *handler* nil)

(defun home-page ()
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "Lisp Pong")
      (:style "
body {
  margin: 0;
  background: #111;
  color: #eee;
  font-family: Arial, sans-serif;
}
.container {
  max-width: 900px;
  margin: 40px auto;
  padding: 24px;
}
.card {
  background: #1b1b1b;
  border-radius: 14px;
  padding: 24px;
  box-shadow: 0 8px 30px rgba(0,0,0,0.30);
}
a {
  color: #7dd3fc;
  text-decoration: none;
}
a:hover {
  text-decoration: underline;
}
"))
     (:body
      (:div :class "container"
            (:div :class "card"
                  (:h1 "Hunchentoot / Clack / Ningle Pong")
                  (:p "Your Pong game is now integrated into the web app.")
                  (:p (:a :href "/pong" "Open Pong"))
                  (:p (:a :href "/api/ping" "Test API endpoint"))))))))

(defun pong-page ()
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "Pong")
      (:style "
html, body {
  margin: 0;
  padding: 0;
  background: #000;
  color: #fff;
  font-family: Arial, sans-serif;
  height: 100%;
}
.wrapper {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 14px;
  padding: 20px;
  box-sizing: border-box;
}
canvas {
  border: 2px solid #fff;
  background: #000;
  display: block;
  max-width: 100%;
  height: auto;
}
.hint {
  opacity: 0.9;
  text-align: center;
}
a {
  color: #7dd3fc;
  text-decoration: none;
}
a:hover {
  text-decoration: underline;
}
"))
     (:body
      (:div :class "wrapper"
            (:h1 "Pong")
            (:canvas :id "pong" :width "800" :height "500")
            (:div :class "hint"
                  "W/S = left paddle, ↑/↓ = right paddle, Space = restart")
            (:div (:a :href "/" "Back to home"))
            (:script (:raw "
const canvas = document.getElementById('pong');
const ctx = canvas.getContext('2d');

const WIDTH = canvas.width;
const HEIGHT = canvas.height;

const PADDLE_WIDTH = 12;
const PADDLE_HEIGHT = 90;
const BALL_SIZE = 12;
const PADDLE_SPEED = 7;

let leftScore = 0;
let rightScore = 0;

const keys = new Set();

const leftPaddle = {
  x: 20,
  y: HEIGHT / 2 - PADDLE_HEIGHT / 2,
  dy: 0
};

const rightPaddle = {
  x: WIDTH - 20 - PADDLE_WIDTH,
  y: HEIGHT / 2 - PADDLE_HEIGHT / 2,
  dy: 0
};

const ball = {
  x: WIDTH / 2 - BALL_SIZE / 2,
  y: HEIGHT / 2 - BALL_SIZE / 2,
  vx: 5 * (Math.random() > 0.5 ? 1 : -1),
  vy: 3 * (Math.random() > 0.5 ? 1 : -1)
};

function resetBall(direction = 1) {
  ball.x = WIDTH / 2 - BALL_SIZE / 2;
  ball.y = HEIGHT / 2 - BALL_SIZE / 2;
  ball.vx = 5 * direction;
  ball.vy = (Math.random() * 4 - 2);
  if (Math.abs(ball.vy) < 1) ball.vy = ball.vy < 0 ? -2 : 2;
}

function clampPaddle(p) {
  if (p.y < 0) p.y = 0;
  if (p.y + PADDLE_HEIGHT > HEIGHT) p.y = HEIGHT - PADDLE_HEIGHT;
}

function rectsOverlap(ax, ay, aw, ah, bx, by, bw, bh) {
  return ax < bx + bw &&
         ax + aw > bx &&
         ay < by + bh &&
         ay + ah > by;
}

function update() {
  leftPaddle.dy = 0;
  rightPaddle.dy = 0;

  if (keys.has('w') || keys.has('W')) leftPaddle.dy = -PADDLE_SPEED;
  if (keys.has('s') || keys.has('S')) leftPaddle.dy = PADDLE_SPEED;

  if (keys.has('ArrowUp')) rightPaddle.dy = -PADDLE_SPEED;
  if (keys.has('ArrowDown')) rightPaddle.dy = PADDLE_SPEED;

  leftPaddle.y += leftPaddle.dy;
  rightPaddle.y += rightPaddle.dy;

  clampPaddle(leftPaddle);
  clampPaddle(rightPaddle);

  ball.x += ball.vx;
  ball.y += ball.vy;

  if (ball.y <= 0) {
    ball.y = 0;
    ball.vy *= -1;
  }

  if (ball.y + BALL_SIZE >= HEIGHT) {
    ball.y = HEIGHT - BALL_SIZE;
    ball.vy *= -1;
  }

  if (rectsOverlap(
      ball.x, ball.y, BALL_SIZE, BALL_SIZE,
      leftPaddle.x, leftPaddle.y, PADDLE_WIDTH, PADDLE_HEIGHT)) {
    ball.x = leftPaddle.x + PADDLE_WIDTH;
    ball.vx = Math.abs(ball.vx);
    ball.vy += (ball.y + BALL_SIZE / 2 - (leftPaddle.y + PADDLE_HEIGHT / 2)) * 0.08;
  }

  if (rectsOverlap(
      ball.x, ball.y, BALL_SIZE, BALL_SIZE,
      rightPaddle.x, rightPaddle.y, PADDLE_WIDTH, PADDLE_HEIGHT)) {
    ball.x = rightPaddle.x - BALL_SIZE;
    ball.vx = -Math.abs(ball.vx);
    ball.vy += (ball.y + BALL_SIZE / 2 - (rightPaddle.y + PADDLE_HEIGHT / 2)) * 0.08;
  }

  if (ball.x + BALL_SIZE < 0) {
    rightScore++;
    resetBall(-1);
  }

  if (ball.x > WIDTH) {
    leftScore++;
    resetBall(1);
  }
}

function drawCenterLine() {
  ctx.strokeStyle = '#666';
  ctx.setLineDash([10, 10]);
  ctx.beginPath();
  ctx.moveTo(WIDTH / 2, 0);
  ctx.lineTo(WIDTH / 2, HEIGHT);
  ctx.stroke();
  ctx.setLineDash([]);
}

function drawScore() {
  ctx.fillStyle = '#fff';
  ctx.font = '32px Arial';
  ctx.textAlign = 'center';
  ctx.fillText(`${leftScore}   :   ${rightScore}`, WIDTH / 2, 40);
}

function drawRect(x, y, w, h) {
  ctx.fillRect(x, y, w, h);
}

function render() {
  ctx.clearRect(0, 0, WIDTH, HEIGHT);

  drawCenterLine();
  drawScore();

  ctx.fillStyle = '#fff';
  drawRect(leftPaddle.x, leftPaddle.y, PADDLE_WIDTH, PADDLE_HEIGHT);
  drawRect(rightPaddle.x, rightPaddle.y, PADDLE_WIDTH, PADDLE_HEIGHT);
  drawRect(ball.x, ball.y, BALL_SIZE, BALL_SIZE);
}

function loop() {
  update();
  render();
  requestAnimationFrame(loop);
}

window.addEventListener('keydown', (e) => {
  keys.add(e.key);

  if (e.key === ' ') {
    leftScore = 0;
    rightScore = 0;
    resetBall(Math.random() > 0.5 ? 1 : -1);
  }
});

window.addEventListener('keyup', (e) => {
  keys.delete(e.key);
});

render();
loop();
")))))))

(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (home-page)))

(setf (ningle:route *app* "/pong")
      (lambda (params)
        (declare (ignore params))
        (pong-page)))

(setf (ningle:route *app* "/api/ping")
      (lambda (params)
        (declare (ignore params))
        '(200 (:content-type "application/json")
          ("{\"ok\":true,\"message\":\"pong backend alive\"}"))))

(defun start (&key (port 5000) (server :hunchentoot))
  (setf *handler*
        (clack:clackup *app* :port port :server server))
  (format t "Server started on http://127.0.0.1:~A~%" port)
  *handler*)

(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)
    (format t "Server stopped.~%")))
