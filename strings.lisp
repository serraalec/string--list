(defun prune-string (string)
  (string-trim '(#\' #\( #\) #\` #\,) string))
(defun string->list (string)
  (string->list-helper (prune-string string)))
(defun string->list-helper (string)
  (let ((index 0))
    (cond ((= index (length string)) '())
	  ((< index (length string))
	   (loop while (> (length string) index)
		 collect
		 (multiple-value-bind (element next-index) (read-from-string string t nil :start index)
		   (setf index next-index)
		   element))))))

(defun check (form result)
  (format t "~:[FAIL~;pass~] ...~a~%" (equal (string->list form) result) result))
(defun test ()
  (check "" '())
  (check  "sadf asdf asdzxf sadf weqr sadf" '(sadf asdf asdzxf sadf weqr sadf))
  (check "Vol. Mean Radius (km)    = 6371.01+-0.02   Mass x10^24 (kg)= 5.97219+-0.0006" '(  Vol. Mean Radius (km)    = 6371.01+-0.02   Mass x10^24 (kg)= 5.97219+-0.0006))
  (check "Vol () " '(vol))
  (check "test test ``" '(test test)))














