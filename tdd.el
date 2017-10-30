
(defun tdd-file ()
  "Loads the current buffer and runs the tests."
  (interactive)
  (save-some-buffers)
  (load-file (buffer-file-name))
  (ert :new))
