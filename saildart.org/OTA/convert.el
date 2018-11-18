(require 'cl)

(defvar *files*
  '("SINE.STF[1,OTA]1"
    "SINE1.STF[1,OTA]1"
    "SINE2.STF[1,OTA]"
    "SINE3.STF[1,OTA]"
    "SINE4.STF[1,OTA]"
    "SINE5.STF[1,OTA]"
    "SINE6.STF[1,OTA]1"))

(defvar *replacements*
  '(("π" "")
    ("↔" "")
    ("↑" "^")
    ("←" "_")
    ("⎇" "}")
    ("}" "~")
    ("␈" "")
    ("&lt;" "<")))

(defun delete-prologue-epilogue ()
  (goto-char (point-min))
  (search-forward "<pre id=\"u8lump\">")
  (delete-region (point-min) (1+ (point)))
  (goto-char (point-max))
  (search-backward "</pre>")
  (delete-region (point) (point-max)))

(defun replace-sail-characters ()
  (loop for x in *replacements* do
       (goto-char (point-min))
       (while (search-forward (first x) nil t)
         (replace-match (second x)))))

(defun unconcatenate-files ()
  (goto-char (point-min))
  (let ((unix-filename nil)
        (file-start nil))
    (while (search-forward "     Listing of >" nil t)
      (when unix-filename
        (write-region file-start (- (point) 19) unix-filename))
      (let ((start (point)))
        (search-forward " ")
        (let* ((magic-filename (buffer-substring start (1- (point)))))
          (setq unix-filename (replace-regexp-in-string
                               ">" "/" magic-filename))
          (setq unix-filename (concat "../../" unix-filename))
          (end-of-line)
          (setq file-start (1+ (point))))))
    (write-region file-start (point-max) unix-filename)))

(defun convert-from-saildart ()
  (interactive)
  (delete-prologue-epilogue)
  (replace-sail-characters)
  (unconcatenate-files))

(defun convert-all-files ()
  (interactive)
  (loop for i in *files* do
       (find-file i)
       (convert-from-saildart)
       (set-buffer-modified-p nil)
       (kill-buffer)))
