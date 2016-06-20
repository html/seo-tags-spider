; Copied from lisp-code

(defclass time-estimator ()
  ((start-time :initform (get-universal-time))))

(defmethod estimated-seconds-till-the-end ((obj time-estimator) units-passed all-units-count)
  (let* ((seconds-passed-already (- (get-universal-time) (slot-value obj 'start-time)))
         (percent-completed (/ units-passed all-units-count))
         (all-seconds-estimate (* seconds-passed-already (/ 1 percent-completed)))
         (seconds-till-the-end (- all-seconds-estimate seconds-passed-already)))

    seconds-till-the-end))

(defmethod display-estimated-time ((obj time-estimator) units-passed all-units-count)
  (format t "Estimated time till the end is ~A seconds~%" (float (estimated-seconds-till-the-end obj units-passed all-units-count))))
