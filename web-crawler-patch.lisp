(in-package :web-crawler)

; XXX: can corrupt web-crawler dependent code 
(defmethod web-crawler::get-page ((uri puri:uri))
  ; TODO: support for redirects with caching
  (values (or 
            (seo-spider::get-content-from-url (puri:render-uri uri nil))
            "") uri))

(defvar *current-queue*)
; Modification to pass queue object to 
(defun start-crawl-with-queue-modification (uri processor &key uri-filter (crawl-delay 10) verbose)
  "Crawl the web, starting at <uri>.

<processor> must be a function that takes three arguments, the current url,
the parent of the current url and the content returned from doing a GET request on that url.  
The return value is ignored.

<:uri-filter> can be a predicate that should return false if the passed
  url should not be processed.  The url will be an instance of the
  puri:url class, which should be easier to deal with than plain text.
  If you want plain text, call (puri:render-uri url nil), which will
  return it.
  To limit crawling to one site, call (make-same-host-filter uri) and pass
  the return value in for :uri-filter.

<:crawl-delay> is a number of seconds to sleep between requests.  The
  default is 10.  It can be a fraction.

<:verbose> when true, prints out each uri being processed."
  (let ((queue (make-unique-queue :test 'equalp
                                  :key #'(lambda (u) (puri:render-uri (first u) nil)))))
    (declare (special *current-queue*))

    (setf *current-queue* queue)

    (labels ((crawl-page (uri)
               (multiple-value-bind (text real-uri) (get-page uri)
                 (let ((links (find-all-links text real-uri)))
                   (loop for link in links
                         do (setf (uri-fragment link) nil) ; get rid of links to anchors
                         do (when
                                (and (not (q-existed queue (list link real-uri)))
                                     (or (not uri-filter)
                                         (funcall uri-filter link)))
                               (q-add queue (list link real-uri)))))
                 (values text real-uri))))
      ;; START: with just the passed url
      (q-add queue (list (uri uri) nil))
      (loop until (q-empty queue)
            do
            (let* ((link-data (q-pop queue))
                   (parent-url (second link-data))
                   (next (first link-data)))
               (when (uri-is-allowed next)
                 (restart-case
                     (multiple-value-bind (page-text real-uri) (crawl-page next)
                       (when verbose
                         (format t "processing ~S~%" (render-uri real-uri nil)))
                       (funcall processor real-uri parent-url page-text)
                       (sleep crawl-delay))
                   (skip-page () nil)))))
      queue)))

