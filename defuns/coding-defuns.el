(defun json->edn ()
  (interactive)
  (shell-command-on-region
    (region-beginning)
    (region-end)
    "jet --pretty --keywordize keyword --from json --to edn"
    (current-buffer)
    t))
