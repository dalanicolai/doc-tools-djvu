(defvar djvu-info-commands '(djvu-length
                             djvu-structured-text
                             djvu-page-sizes
                             djvu-bookmarks
                             djvu-annots))

(defun djvu-info (function &optional arg)
  (interactive (if (string= (file-name-extension (buffer-file-name)) "djvu")
                   (list (completing-read "Select info type: "
                                          djvu-info-commands)
                         current-prefix-arg)
                 (user-error "Buffer file not of `djvu' type")))
  (pp (pcase (intern-soft function)
        ('djvu-structured-text (call-interactively #'djvu-structured-text))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*djvu-info*")))
  (when arg (pop-to-buffer "*djvu-info*")))

(defun djvu-assert-djvu-file (file &optional no-error)
  (if (string= (file-name-extension file) "djvu")
      file
    (unless no-error
      (user-error "File must be a .djvu file"))))

(defun djvu-select-file ()
  "Return current djvu filename or otherwise select file."
  (if (djvu-assert-djvu-file buffer-file-name t)
      buffer-file-name
    (read-file-name "Select djvu file: " nil nil t nil
                    (lambda (f) (or (file-directory-p f)
                                    (djvu-assert-djvu-file f t))))))

(defun djvu-length (&optional file)
  (djvu-assert-djvu-file (or file buffer-file-name))
  (let ((length (string-to-number
                 (shell-command-to-string
                  (format "djvused -e n '%s'" (or file (djvu-select-file)))))))
    length))

;; NOTE returns nil when page is empty
(defun djvu-structured-text (&optional detail page file)
  "Interactively, this command should be called using the command
`djvu-info'."
  (interactive
   (let ((last-page (djvu-length)))
     (list (completing-read "Select detail: "
                            '(page column region para line word char))
           (read-number (format "Select page(s) (max %s): " last-page)
                        (or (scrap-current-page) 1))
           buffer-file-name)))
  (setq file (or file buffer-file-name))
  (if file
      (djvu-assert-djvu-file file)
    (setq file (djvu-select-file)))
  (let* ((output (shell-command-to-string
                  (concat  "djvutxt "
                           (when page (format "--page=%s " page))
                           (when detail (format "--detail=%s " detail))
                           (format "\"%s\"" file)))))
    (read (concat (if detail "(" "\"")
                  output
                  (if detail ")" "\"")))))

;;TODO replace 
;; (defun papyrus-djvu-text-contents (&optional detail page return)
;;   (unless (or return (or detail page))
;;     (user-error "When RETURN is nil, DETAIL and PAGE can not be both nil."))
;;   (let ((output (shell-command-to-string
;;                  (concat  "djvutxt "
;;                           (when page (format "--page=%s " page))
;;                           (when detail (format "--detail=%s " detail))
;;                           (format "\"%s\"" (buffer-file-name))))))
;;     (if return
;;         output
;;       (read (concat (unless page "(")
;;                     output
;;                     (unless page ")"))))))

(defun djvu-structural-filter (fn hidden-text-list &optional format-fn)
  (letrec ((elements nil)
           (recur (lambda (text)
                    (when (stringp (nth 5 text)) ;also 'non-word' elements can
                                                 ;contain strings
                      (setq w (1+ w)))
                    (if (funcall fn text)
                        (push (if format-fn (funcall format-fn text n w) text)
                              elements)
                      (unless (stringp (nth 5 text))
                        (mapcar (lambda (e)
                                  (funcall recur e))
                                (nthcdr 5 text))))))
           (n 0)
           (w 0))
    (if (symbolp (car hidden-text-list))
        (funcall recur hidden-text-list)
      (dolist (p hidden-text-list)
        (setq n (1+ n))
        (funcall recur p)))
    (nreverse elements)))

(defun djvu-text-elements (&optional detail page file)
  (djvu-structural-filter
   (lambda (e) (stringp (nth 5 e)))
   (djvu-structured-text (or detail 'char) page file)))

(defun djvu-search-word (word &optional contents)
  (djvu-structural-filter (lambda (e)
                            (when (stringp (nth 5 e))
                              (string-match word (nth 5 e))))
                          (or contents scrap-structured-contents)
                          (lambda (e p w) (cons (if contents w p) (cdr e)))))

(defun djvu-keyboard-annot (patt1 patt2)
  (interactive "sEnter start pattern: \nsEnter end pattern: ")
  (let* ((text (djvu-structured-text 'word 3))
         (m1 (djvu-search-word patt1 text))
         (m2 (djvu-search-word patt2 text)))
    (djvu-structural-filter (lambda (e) (and (stringp (nth 5 e))
                                             (<= (caar m1) (print w) (print (caar m2)))))
                            text
                            (lambda (e p w) (print e)))))

(defun djvu-page-sizes (&optional file)
  "The page sizes as stored in the document."
  (interactive)
  (if file
      (djvu-assert-djvu-file file)
    (setq file (djvu-select-file)))
  (mapcar (lambda (l)
            (let ((columns (split-string l "[ =]" t)))
              (cons (string-to-number (nth 1 columns))
                    (string-to-number (nth 3 columns)))))
          (process-lines "djvused" "-e" "'size'" file)))

;; (defun djvu-decode-thumbs (file outfile-base page &optional format max-width max-height)
;;   (let ((outdir (concat "/tmp/" (file-name-base file))))
;;     (unless (file-exists-p outdir)
;;       (make-directory (concat "/tmp/" (file-name-base file))))
;;     (call-process-shell-command
;;      (concat "ddjvu -format=tiff "
;;              (pcase page
;;                ((pred numberp) (format "-page=%s " page))
;;                ('eachpage "-eachpage "))
;;              (format "-size=%sx%s " (or max-width 200) (or max-height 200))
;;              "-quality=80 "
;;              "'" file "' "

;;              "'"
;;              outdir "/" outfile-base
;;              (if (numberp page) (number-to-string page) "%d")
;;              "."
;;              (if format (symbol-name format) "tif")
;;              "'"))))

(defun djvu-decode-pages (&optional file force)
  "Asynchronously create thumb files for all pages."
  (setq file (or file (buffer-file-name)))
  (let ((outdir (concat "/tmp/"
                        (file-name-as-directory (file-name-base file))
                        "pages/")))
    (unless (file-exists-p outdir)
      (make-directory outdir t))
    (let ((proc (start-process "ddjvu" "djvu decode thumbs" "ddjvu"
                               "-format=tiff"
                               "-eachpage"
                               (format "-size=%sx%s" 944 5000)
                               "-quality=50"
                               file
                               (concat outdir "page-%d.tiff"))))
      (set-process-sentinel proc (lambda (process event)
                                   (message "Create thumbs process %s" event))))))

(defun djvu-decode-thumbs (&optional file force)
  "Asynchronously create thumb files for all pages."
  (setq file (or file (buffer-file-name)))
  (let ((outdir (concat "/tmp/" (file-name-as-directory (file-name-base file)) "thumbs/")))
    (unless (file-exists-p outdir)
      (make-directory outdir))
    (let ((proc (start-process "ddjvu" "djvu decode thumbs" "ddjvu"
                               "-format=tiff"
                               "-eachpage"
                               (format "-size=%sx%s" 175 2000)
                               "-quality=50"
                               file
                               (concat outdir "thumb%d.tif"))))
      (set-process-sentinel proc (lambda (process event)
                                   (message "Create thumbs process %s" event))))))



;; TODO if images are not pbm or pgm, then we could create a tiff via a temp-file
;; to reduce memory usage

;; NOTE tiff version (uses much less memory than pnm, but requires temp file)
(defun djvu-decode-page (page width &optional file)
  (setq file (or file (buffer-file-name)))
  (let ((status (call-process "ddjvu" nil t nil
                              (format "-size=%dx%d" width 10000)
                              "-format=tiff"
                              (format "-page=%d" page)
                              "-quality=50" ;; for some files this argument is
                                            ;; essential
                              file
                              "/tmp/djvu-temp-img")))
    (unless (and status (zerop status))
      (error "Ddjvu error %s" status))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq coding-system-for-read 'binary)
      (insert-file-contents-literally "/tmp/djvu-temp-img")
      (buffer-substring-no-properties (point-min) (point-max)))))

;; (defun djvu-decode-page (page width &optional file)
;;   (setq file (or file (buffer-file-name)))
;;   (with-temp-buffer
;;     (set-buffer-multibyte nil)
;;     (let* ((coding-system-for-read 'raw-text)
;;            ;; For a rectangular image, ISIZE does not give us
;;            ;; the actual size of the image, but (max width height)
;;            ;; will be equal to ISIZE.
;;            (status (call-process "ddjvu" nil t nil
;;                                  (format "-size=%dx%d" width 5000)
;;                                  "-format=pnm" ;pnm automatically selects most
;;                                         ;efficient decoding of p(b/g/p)m
;;                                  (format "-page=%d" page)
;;                                  file)))
;;       (unless (zerop status)
;;         (error "Ddjvu error %s" status))
;;       (buffer-substring-no-properties
;;        (point-min) (point-max)))))

(defun djvu-djvused (command &optional page file)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
  ;; (with-current-buffer (get-buffer-create "test")
    (let ((format-command (concat (when page (format "select %d;" page))
                                  command)))
      (call-process-shell-command
       (format "djvused '%s' -e '%s'" file format-command)
       nil t)
      (when (> (buffer-size) 0)
        (goto-char (point-min))
        (when (string= command "print-ant")
          (while (re-search-forward " \\(#[[:alnum:]]+\\)" nil t)
            (replace-match " \"\\1\"")))
        (goto-char (point-min))
        (if (looking-at-p "(")
            (read (concat "(" (buffer-string) ")"))
          (buffer-string))))))

(defun djvu-bookmarks (&optional file)
  (djvu-djvused "print-outline" nil file))

(defun djvu-annots (&optional page file)
  (djvu-djvused "print-ant" (or page (scrap-current-page) file)))
