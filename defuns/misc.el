;;; package --- Summary.

;;; Commentary:

;;; Code:

(defun rac-current-path ()
  "Return the current path."
  (when buffer-file-name (file-truename buffer-file-name)))

(defun rac-image-size ()
  "Return the size of an image."
  (interactive)
  (message
   (let ((size (image-size (create-image (rac-current-path)) t)))
     (concat
      "Dimensions: " (number-to-string (car size))
      " x " (number-to-string (cdr size))))))

;;; misc.el ends here
