(load #P"quicklisp/setup.lisp")

(ql:quickload '(:clack
                :ningle
                :clack-handler-hunchentoot
                :dexador
                :quri
                :lack-request
                :lack-response
                :jonathan
                :uiop
                :spinneret))
(format t "Libraries loaded...~%")

(defpackage #:sweb
  (:use #:cl)
  (:import-from #:spinneret #:with-html-string))

(in-package #:sweb)

(defun get-ow-api-key ()
  (or (uiop:getenv "OW_API_KEY")
      (error "OW API key not found in environment!")))

(defun get-er-api-key ()
  (or (uiop:getenv "ER_API_KEY")
      (error "ER API key not found in environment!")))

(defvar *app* (make-instance 'ningle:app))
(defvar *handler* nil)

;;; ------------------------------------------------------------
;;; HTML pages
;;; ------------------------------------------------------------

(defun home-page ()
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:title "sweb")
      (:style "
body {
  margin: 0;
  background: #111;
  color: #eee;
  font-family: Arial, sans-serif;
}
.container {
  max-width: 960px;
  margin: 40px auto;
  padding: 24px;
}
.card {
  background: #1b1b1b;
  border-radius: 14px;
  padding: 24px;
  box-shadow: 0 8px 30px rgba(0,0,0,0.30);
}
h1, h2 { margin-top: 0; }
ul { line-height: 1.8; padding-left: 20px; }
a { color: #7dd3fc; text-decoration: none; }
a:hover { text-decoration: underline; }
code {
  background: #222;
  padding: 2px 6px;
  border-radius: 6px;
}
"))
     (:body
      (:div :class "container"
       (:div :class "card"
        (:h1 "sweb utility endpoints")
        (:p "Available routes:")
        (:ul
         (:li (:a :href "/clack" "/clack"))
         (:li (:a :href "/roll-the-dice" "/roll-the-dice"))
         (:li (:a :href "/weather?city=Kyiv" "/weather?city=Kyiv"))
         (:li (:a :href "/exchange?currency=USD&to=UAH"
                  "/exchange?currency=USD&to=UAH"))
         (:li (:a :href "/pong" "/pong"))
         (:li (:a :href "/api/ping" "/api/ping")))
        (:p "Pong controls: "
            (:code "W/S")
            " for left paddle, "
            (:code "↑/↓")
            " for right paddle, "
            (:code "Space")
            " to restart.")))))))

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
       (:script
        (:raw "
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
  ball.vy = Math.random() * 4 - 2;
  if (Math.abs(ball.vy) < 1) {
    ball.vy = ball.vy < 0 ? -2 : 2;
  }
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
    ball.vy += (ball.y + BALL_SIZE / 2
               - (leftPaddle.y + PADDLE_HEIGHT / 2)) * 0.08;
  }

  if (rectsOverlap(
        ball.x, ball.y, BALL_SIZE, BALL_SIZE,
        rightPaddle.x, rightPaddle.y, PADDLE_WIDTH, PADDLE_HEIGHT)) {
    ball.x = rightPaddle.x - BALL_SIZE;
    ball.vx = -Math.abs(ball.vx);
    ball.vy += (ball.y + BALL_SIZE / 2
               - (rightPaddle.y + PADDLE_HEIGHT / 2)) * 0.08;
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
  ctx.fillText(`${leftScore} : ${rightScore}`, WIDTH / 2, 40);
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

;;; ------------------------------------------------------------
;;; Existing endpoints
;;; ------------------------------------------------------------

(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (home-page)))

(setf (ningle:route *app* "/clack")
      "
# Hey there!
")

(defparameter *dice-links*
  #("https://wikipedia.org/"
    "https://reddit.com/"
    "https://x.com/"
    "https://instagram.com/"
    "https://threads.com"
    "https://gigamonkeys.com/"))

(setf (ningle:route *app* "/roll-the-dice")
      (lambda (params)
        (declare (ignorable params))
        (declare (optimize (speed 3)))
        (let* ((roll (+ 1 (random 6)))
               (url (aref *dice-links* (1- roll))))
          `(302 (:location ,url) ("Rolling the dice...")))))

(defparameter *OW_API_KEY* (get-ow-api-key))

(setf (ningle:route *app* "/weather")
      (lambda (params)
        (declare (ignore params))
        (let* ((query (lack.request:request-query-parameters ningle:*request*))
               (city (or (cdr (assoc "city" query :test #'string=))
                         "Kyiv"))
               (url (format nil
                            "https://api.openweathermap.org/data/2.5/weather?q=~A&appid=~A&units=metric"
                            (quri:url-encode city)
                            *OW_API_KEY*)))
          (setf (getf (lack.response:response-headers ningle:*response*)
                      :content-type)
                "application/json; charset=utf-8")
          (dex:get url))))

(defparameter *ER_API_KEY* (get-er-api-key))
(defparameter *supported-symbols-cache* nil)

(defun json-response (status payload)
  (list status
        '("Content-Type" "application/json; charset=utf-8")
        (list (jonathan:to-json payload))))

(defun fetch-supported-symbols ()
  (let* ((url (format nil
                      "https://api.exchangeratesapi.io/v1/symbols?access_key=~A"
                      *ER_API_KEY*))
         (response (dex:get url))
         (json (jonathan:parse response :as :hash-table))
         (success (gethash "success" json))
         (symbols (gethash "symbols" json))
         (err (gethash "error" json)))
    (unless success
      (error "Could not fetch supported currency symbols: ~A"
             (if err
                 (or (gethash "message" err) err)
                 "unknown API error")))
    (unless symbols
      (error "API returned no symbols"))
    symbols))

(defun ensure-supported-symbols ()
  (or *supported-symbols-cache*
      (setf *supported-symbols-cache* (fetch-supported-symbols))))

(defun refresh-supported-symbols ()
  (setf *supported-symbols-cache* (fetch-supported-symbols)))

(defun supported-currency-p (code)
  (let ((symbols (ensure-supported-symbols))
        (up (and code (string-upcase code))))
    (and (stringp code)
         (= (length code) 3)
         (every #'alpha-char-p code)
         (gethash up symbols))))

(defun fetch-latest-rates-for (symbols)
  (let* ((url (format nil
                      "https://api.exchangeratesapi.io/v1/latest?access_key=~A&symbols=~A"
                      *ER_API_KEY*
                      symbols))
         (response (dex:get url))
         (json (jonathan:parse response :as :hash-table))
         (success (gethash "success" json))
         (rates (gethash "rates" json))
         (err (gethash "error" json)))
    (unless success
      (error "Could not fetch latest rates: ~A"
             (if err
                 (or (gethash "message" err) err)
                 "unknown API error")))
    (unless rates
      (error "API returned no rates"))
    rates))

(defun fetch-exchange-rate (from to)
  (let ((from (string-upcase from))
        (to (string-upcase to)))
    (cond
      ((string= from to) 1.0d0)
      (t
       ;; Free plans often restrict changing base, so compute via EUR.
       (let* ((symbols (cond ((string= from "EUR") to)
                             ((string= to "EUR") from)
                             (t (format nil "~A,~A" from to))))
              (rates (fetch-latest-rates-for symbols))
              (rate-from (if (string= from "EUR")
                             1.0d0
                             (gethash from rates)))
              (rate-to (if (string= to "EUR")
                           1.0d0
                           (gethash to rates))))
         (unless rate-from
           (error "Rate for source currency ~A not found" from))
         (unless rate-to
           (error "Rate for target currency ~A not found" to))
         (/ (coerce rate-to 'double-float)
            (coerce rate-from 'double-float)))))))

(setf (ningle:route *app* "/exchange")
      (lambda (params)
        (declare (ignore params))
        (let* ((query (lack.request:request-query-parameters ningle:*request*))
               (from (string-upcase
                      (or (cdr (assoc "currency" query :test #'string=))
                          "USD")))
               (to (string-upcase
                    (or (cdr (assoc "to" query :test #'string=))
                        "UAH"))))
          (handler-case
              (cond
                ((not (supported-currency-p from))
                 (json-response 400
                                `(:error "invalid source currency"
                                  :currency ,from)))
                ((not (supported-currency-p to))
                 (json-response 400
                                `(:error "invalid target currency"
                                  :to ,to)))
                (t
                 (let ((rate (fetch-exchange-rate from to)))
                   (json-response 200
                                  `(:from ,from :to ,to :rate ,rate)))))
            (dexador.error:http-request-bad-request (e)
              (json-response 400
                             `(:error "bad request to exchange API"
                               :details ,(dexador.error:response-body e))))
            (error (e)
              (json-response 500
                             `(:error ,(princ-to-string e))))))))

;;; ------------------------------------------------------------
;;; Pong endpoints
;;; ------------------------------------------------------------

(setf (ningle:route *app* "/pong")
      (lambda (params)
        (declare (ignore params))
        (pong-page)))

(setf (ningle:route *app* "/api/ping")
      (lambda (params)
        (declare (ignore params))
        '(200
          (:content-type "application/json")
          ("{\"ok\":true,\"message\":\"pong backend alive\"}"))))

;;; ------------------------------------------------------------
;;; Start / stop
;;; ------------------------------------------------------------

(defun start (&key (port 5321) (server :hunchentoot))
  (setf *handler* (clack:clackup *app* :port port :server server))
  (format t "Server started on http://127.0.0.1:~A~%" port)
  *handler*)

(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)
    (format t "Server stopped.~%")))

(start :port 5321 :server :hunchentoot)
(loop)
