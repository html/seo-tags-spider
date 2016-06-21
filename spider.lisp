(load ".quicklisp-install/require-quicklisp.lisp")

(ql:quickload :swank)
(swank:create-server :dont-close t :port 4005)

(ql:quickload :weblocks-utils)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml)
(ql:quickload :quri)
(ql:quickload :weblocks-prevalence)
(ql:quickload :cl-web-crawler)



(load "css-selectors/css-selectors.asd")
(ql:quickload :css-selectors)

(defpackage :seo-spider 
  (:use :cl))

(in-package :seo-spider)

(export '(parsed-web-page))

(load "oz-parsing-util/oz-parsing-util.lisp")
(load "php-funcs-for-cl/php-functions-for-cl.lisp")
(load "web-crawler-patch.lisp")

(defclass parsed-web-page ()
  ((id)
   (url :initarg :url)
   (title)
   (description)
   (keywords)
   (weight :initform 0 :accessor parsed-web-page-weight)
   (processed-p :initform nil)))

(weblocks-stores:defstore *pages-store* :prevalence (make-pathname :directory '(:relative "data")))
(weblocks-stores:open-stores)

(setf *cache-enabled-p* t)
(setf *drakma-request-max-tries* 1)
(setf drakma:*drakma-default-external-format* :utf-8)

(load "time-estimator.lisp")

; see game-republic weblocks-mongo-step-by-step
(defun display-progress (percent-completed)

  (when (> percent-completed 1)
    (error "Wrong percent, should be from 0 to 1"))

  (let* ((tips-count 80)
         (top-bottom-symbol "_")
         (space-symbol " ")
         (percent-actual (* percent-completed (* tips-count tips-count)))
         (percent-div (ceiling percent-actual tips-count)))
    (format t " ~{~A~}~%" (loop for i from 1 to tips-count collect (if (> i percent-div) space-symbol top-bottom-symbol)))
    (format t "[~{~A~}~{~A~}]~%" 
            (loop for i from 1 to percent-div collect (if (> i percent-div) space-symbol top-bottom-symbol))
            (if (= percent-div tips-count)
              (list "")
              (list* "|" (loop for i from percent-div to (- tips-count 2) collect space-symbol))))))

; Pages count session
(defun calculate-site-pages-count (site-url)
  (let ((time-estimator (make-instance 'time-estimator))
        (pages-count 0))

    (web-crawler::start-crawl-with-queue-modification
      site-url
      (lambda (url url-parent content)
        (declare (special web-crawler::*current-queue*))

        (ignore-errors 
          (when (zerop (mod (get-universal-time) 10))
            (let* ((units-to-process-count (length (uniqq::qlist web-crawler::*current-queue*)))
                   (processed-units-count (hash-table-count (uniqq::qhash web-crawler::*current-queue*)))
                   (time-passed (- (get-universal-time) (slot-value time-estimator 'start-time)))
                   (time-per-unit (/ time-passed processed-units-count)))

              ; Clearing screen

              (display-progress (/ (- processed-units-count units-to-process-count) processed-units-count))

              (format t "До конца осталось ~A минут~%" 
                      (float 
                        (/ 
                          (estimated-seconds-till-the-end 
                            time-estimator
                            (- processed-units-count units-to-process-count)
                            processed-units-count)
                          60)))

              (format t "Прошло ~A минут, на одну страницу идет ~A секунд~%" (float (/ time-passed 60)) (float time-per-unit))
              (format t "Всего страниц насчитали ~A~%" pages-count)

              (incf pages-count)))))
      :crawl-delay 0
      :uri-filter (let ((same-host-filter (web-crawler:make-same-host-filter site-url))
                        (skip-images-filter (web-crawler::make-skip-images-filter)))
                    (lambda (uri)
                      (and (funcall same-host-filter uri)
                           (funcall skip-images-filter uri))))
      :verbose t)))

(format t "You can use (calculate-site-pages-count <site-url>)")
