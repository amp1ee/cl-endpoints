(defpackage #:sweb
  (:use :cl))

(load #P"/home/amplee/LISP/quicklisp/setup.lisp")

;
;;
;;;
;;
;
;;
;;;
(in-package #:sweb)
;;;
;;
;

(ql:quickload :clack)
(ql:quickload :ningle)
(ql:quickload :clack-handler-hunchentoot)
(format t "Clack load attempt complete...~%")
(ql:quickload '(:dexador :quri :lack-request :jonathan :uiop))

(defun get-ow-api-key ()
  (or (uiop:getenv "OW_API_KEY")
      (error "OW API key not found in environment!")))
(defun get-er-api-key ()
  (or (uiop:getenv "ER_API_KEY")
      (error "ER API key not found in environment!")))

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/clack")
  "<h1>Hey there!</h1>")

(defparameter *dice-links*
  #("https://bit.ly/4uJOD1D"
    "https://bit.ly/4lS9mws"
    "https://bit.ly/3PlN224"
    "https://bit.ly/4uPdxgE"
    "https://bit.ly/41hlopw"
    "https://shorturl.fm/YntLN"
   )
)

(setf (ningle:route *app* "/roll-the-dice")
      (lambda (params)
        (declare (ignorable params))
        (declare (optimize (speed 3)))
        (let* ((roll (+ 1 (random 6)))
               (url (aref *dice-links* (1- roll))))
          `(302 (:location ,url)
                ("Rolling the dice...")))))

(defparameter *OW_API_KEY* (get-ow-api-key))
(setf (ningle:route *app* "/weather")
      (lambda (params)
        (declare (ignore params))
        (let* ((query (lack.request:request-query-parameters ningle:*request*))
               (city (or (cdr (assoc "city" query :test #'string=)) "Kyiv"))
               (url (format nil
                            "https://api.openweathermap.org/data/2.5/weather?q=~A&appid=~A&units=metric"
                            (quri:url-encode city)
                            *OW_API_KEY*)))
          (setf (getf (lack.response:response-headers ningle:*response*) :content-type)
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
      (setf *supported-symbols-cache*
            (fetch-supported-symbols))))

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
  (let* ((base "EUR")
         (url (format nil
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
  (let* ((from (string-upcase from))
         (to (string-upcase to)))
    (cond
      ((string= from to) 1.0d0)
      (t
       ;; Free plans often restrict changing base.
       ;; So we fetch EUR-based rates and compute:
       ;; from->to = (EUR->to) / (EUR->from)
       (let* ((symbols
                (cond
                  ((string= from "EUR") to)
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
                                  `(:from ,from
                                    :to ,to
                                    :rate ,rate)))))

            (dexador.error:http-request-bad-request (e)
              (json-response 400
                             `(:error "bad request to exchange API"
                               :details ,(dexador.error:response-body e))))

            (error (e)
              (json-response 500
                             `(:error ,(princ-to-string e))))))))


;`\,.=~"o~Y~o"~=`\`,-\-_-=-~'`-,_~->-o-.`-'\__-\,
;;;
(clack:clackup *app* :port 5321 :debug t)
;;;
;,/`._~-=-0-=-~_,/,`=/~`->-~=~-0-~-<->-,'-~->>-/`

(format t "Started...~%")
(loop)

