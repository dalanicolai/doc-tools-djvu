(defvar djvu-info-commands '(djvu-length djvu-text-contents djvu-page-sizes djvu-bookmarks))

(defun djvu-info (function &optional arg)
  (interactive (if (string= (file-name-extension (buffer-file-name)) "djvu")
                   (list (completing-read "Select info type: "
                                          djvu-info-commands)
                         current-prefix-arg)
                 (user-error "Buffer file not of `djvu' type")))
  (pp (funcall (intern-soft function))
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
  (interactive)
  (djvu-assert-djvu-file (or file buffer-file-name))
  (let ((length (string-to-number
                 (shell-command-to-string
                  (format "djvused -e n '%s'" (or file (djvu-select-file)))))))
    length))

                                        ;NOTE returns nil when page is empty
(defun djvu-text-contents (&optional detail page file)
  (setq file (or file (buffer-file-name)))
  (interactive
   (let ((last-page (djvu-length)))
     (list buffer-file-name
           (completing-read "Select detail: "
                            '(page column region para line word char))
           (read-number (format "Select page(s) (max %s): " last-page) 0))))
  (if file
      (djvu-assert-djvu-file file)
    (setq file (djvu-select-file)))
  (let* ((output (shell-command-to-string
                  (concat  "djvutxt "
                           (when page (format "--page=%s " page))
                           (when detail (format "--detail=%s " detail))
                           (format "\"%s\"" file))))
         (data (read (concat (if detail "(" "\"")
                             output
                             (if detail ")" "\"")))))
    data))

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

(defun djvu-decode-thumbs (&optional file force)
  "Asynchronously create thumb files for all pages."
  (setq file (or file (buffer-file-name)))
  (let ((outdir (concat "/tmp/" (file-name-as-directory (file-name-base file)))))
    (unless (file-exists-p outdir)
      (make-directory (concat "/tmp/" (file-name-base file))))
    (let ((proc (start-process "ddjvu" "djvu decode thumbs" "ddjvu"
                               "-format=tiff"
                               "-eachpage"
                               (format "-size=%sx%s" 175 2000)
                               "-quality=50"
                               file
                               (concat "/tmp/"
                                       (file-name-as-directory (file-name-base file))
                                       "thumb%d.tif"))))
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
                              ;; "-quality=50"
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

(defun djvu-bookmarks (&optional file)
  (interactive)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
    (call-process-shell-command
     (format "djvused '%s' -e 'print-outline'" (print file))
     nil t)
    (buffer-string)
    (when (> (buffer-size) 0)
      (while (search-backward "#" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (cdr (read (current-buffer))))))
