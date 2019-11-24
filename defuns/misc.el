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

(require 'json)

(defun org-export-json-buffer ()
  "Export org content to a JSON buffer."
  (interactive)
  (let* ((tree (org-element-parse-buffer 'object nil)))
    (org-element-map tree (append org-element-all-elements
                                  org-element-all-objects '(plain-text))
      (lambda (x)
        (if (org-element-property :parent x)
            (org-element-put-property x :parent "none"))
        (if (org-element-property :structure x)
            (org-element-put-property x :structure "none"))))
    (with-current-buffer (get-buffer-create "*ox-json*")
      (erase-buffer)
      (insert (json-encode tree))
      (json-pretty-print-buffer))
    (switch-to-buffer (get-buffer-create "*ox-json*"))))

(defun org-to-json (query)
  "Export headings that match QUERY to json."
  (org-map-entries
   (lambda ()
     (org-copy-subtree)
     (with-current-buffer (get-buffer-create "-t-")
       (yank)))
   query)
  (with-current-buffer "-t-" (org-export-json-buffer))
  (kill-buffer "-t-"))

;;; misc.el ends here
