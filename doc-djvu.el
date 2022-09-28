(defun doc-djvu-invert-tiff (file &optional arg)
  (interactive "f\nP")
  (if (executable-find "tiffset")
      (call-process "tiffset" nil nil nil "-s" "262" (if arg "1" "0") file)
    (message "Unable to find `tifsett' shell command")))

(defvar doc-djvu-info-commands '(doc-djvu-length
                                 doc-djvu-structured-text
                                 doc-djvu-page-sizes
                                 doc-djvu-bookmarks
                                 doc-djvu-annots
                                 doc-djvu-parse-raw-contents))

(defun doc-djvu-info (function &optional arg)
  (interactive (if (string= (file-name-extension (buffer-file-name)) "djvu")
                   (list  (intern-soft (completing-read "Select info type: "
                                                        doc-djvu-info-commands))
                         current-prefix-arg)
                 (user-error "Buffer file not of `djvu' type")))
  (pp (pcase function
        ('doc-djvu-structured-text (call-interactively #'doc-djvu-structured-text))
        (var (funcall var)))
      (when arg
        (pop-to-buffer (get-buffer-create "*doc-djvu-info*")))))

(defun doc-djvu-assert-doc-djvu-file (file &optional no-error)
  (if (string= (file-name-extension file) "djvu")
      file
    (unless no-error
      (user-error "File must be a .djvu file"))))

(defun doc-djvu-select-file ()
  "Return current djvu filename or otherwise select file."
  (if (doc-djvu-assert-doc-djvu-file buffer-file-name t)
      buffer-file-name
    (read-file-name "Select djvu file: " nil nil t nil
                    (lambda (f) (or (file-directory-p f)
                                    (doc-djvu-assert-doc-djvu-file f t))))))

(defun doc-djvu-length (&optional file)
  (setq file (or file buffer-file-name))
  (doc-djvu-assert-doc-djvu-file file)
  (let ((length (string-to-number
                 (shell-command-to-string
                  (format "djvused -e n '%s'" (or file (doc-djvu-select-file)))))))
    length))

;; NOTE returns nil when page is empty
(defun doc-djvu-structured-text (&optional detail page file)
  "Interactively, this command should be called using the command
`doc-djvu-info'."
  (interactive
   (let ((last-page (doc-djvu-length)))
     (list (completing-read "Select detail: "
                            '(plain page column region para line word char))
           (read-number (format "Select page(s) (max %s): " last-page)
                        (or (doc-scroll-current-page) 1))
           buffer-file-name)))
  (setq file (or file buffer-file-name))
  (if file
      (doc-djvu-assert-doc-djvu-file file)
    (setq file (doc-djvu-select-file)))
  (when (string= detail "plain")
    (setq detail nil))
  (let* ((output (shell-command-to-string
                  (concat  "djvutxt "
                           (when page (format "--page=%s " page))
                           (when detail (format "--detail=%s " detail))
                           (format "\"%s\"" file)))))
    (read (concat (if detail "(" "\"")
                  output
                  (if detail ")" "\"")))))

;;TODO replace 
;; (defun papyrus-doc-djvu-text-contents (&optional detail page return)
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

(defun doc-djvu-structural-filter (fn hidden-text-list &optional format-fn)
  (letrec ((elements nil)
           (recur (lambda (text)
                    (when (stringp (nth 5 text)) ;also 'non-word' elements can
                                                 ;contain strings
                      (setq text-object (1+ text-object)))
                    (if (funcall fn text)
                        (push (if format-fn (funcall format-fn text page text-object) text)
                              elements)
                      (unless (stringp (nth 5 text))
                        (mapcar (lambda (e)
                                  (funcall recur e))
                                (nthcdr 5 text))))))
           (page 1)
           (text-object 0))
    (if (symbolp (car hidden-text-list))
        (funcall recur hidden-text-list)
      (dolist (p hidden-text-list)
        (funcall recur p)
        (setq page (1+ page))))
    (nreverse elements)))

(defun doc-djvu-text-elements (&optional detail page file)
  (doc-djvu-structural-filter
   (lambda (e) (stringp (nth 5 e)))
   (doc-djvu-structured-text (or detail 'char) page file)))

(defun doc-djvu-search-word (word &optional contents)
  (doc-djvu-structural-filter (lambda (e)
                            (when (stringp (nth 5 e))
                              (string-match word (nth 5 e))))
                          (or contents doc-scroll-structured-contents)
                          (lambda (e p w) (cons (if contents w p) (cdr e)))))

(defun doc-djvu-keyboard-annot (patt1 patt2)
  (interactive "sEnter start pattern: \nsEnter end pattern: ")
  (let* ((text (doc-djvu-structured-text 'word 3))
         (m1 (doc-djvu-search-word patt1 text))
         (m2 (doc-djvu-search-word patt2 text)))
    (doc-djvu-structural-filter (lambda (e) (and (stringp (nth 5 e))
                                             (<= (caar m1) (print w) (print (caar m2)))))
                            text
                            (lambda (e p w) (print e)))))

(defun doc-djvu-page-sizes (&optional file)
  "The page sizes as stored in the document."
  (interactive)
  (if file
      (doc-djvu-assert-doc-djvu-file file)
    (setq file (doc-djvu-select-file)))
  (mapcar (lambda (l)
            (let ((columns (split-string l "[ =]" t)))
              (cons (string-to-number (nth 1 columns))
                    (string-to-number (nth 3 columns)))))
          (process-lines "djvused" "-e" "'size'" file)))

;; (defun doc-djvu-decode-thumbs (file outfile-base page &optional format max-width max-height)
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

(defun doc-djvu-decode-directory (&optional file thumbs)
  (interactive "f\nP")
  (concat "/tmp/"
          (file-name-as-directory (file-name-base file))
          "pages/"))

(defun doc-backend-djvu-invert (&optional arg)
  "Invert color of pages.
Uses the tiffset command to invert the color of the pages. When
prefixed with the universal argument, undoes the inversion."
  (interactive "P")
  (dolist (f (directory-files (doc-djvu-decode-directory buffer-file-name)
                              t "tiff$"))
    (doc-djvu-invert-tiff f arg)))

(defun doc-djvu-decode-pages (width &optional file force)
  "Asynchronously create thumb files for all pages."
  (setq file (or file (buffer-file-name)))
  (let ((outdir (doc-djvu-decode-directory file)))
    (unless (file-exists-p outdir)
      (make-directory outdir t)
      (let ((proc (start-process "ddjvu" "djvu decode thumbs" "ddjvu"
                                 "-format=tiff"
                                 "-eachpage"
                                 (format "-size=%sx%s" width 5000)
                                 "-quality=50"
                                 file
                                 (concat outdir "page-%d.tiff"))))
        (set-process-sentinel proc (lambda (process event)
                                     (message "Create pages process %s" event)))))))

(defun doc-djvu-decode-thumbs (&optional file force)
  "Asynchronously create thumb files for all pages."
  (setq file (or file (buffer-file-name)))
  (let ((outdir (concat "/tmp/" (file-name-as-directory (file-name-base file)) "thumbs/")))
    (unless (print (file-exists-p outdir))
      (make-directory outdir)
      (let ((proc (start-process "ddjvu" "djvu decode thumbs" "ddjvu"
                                 "-format=tiff"
                                 "-eachpage"
                                 (format "-size=%sx%s" 175 2000)
                                 "-quality=50"
                                 file
                                 (concat outdir "thumb%d.tif"))))
        (set-process-sentinel proc (lambda (process event)
                                     (message "Create thumbs process %s" event)))))))



;; TODO if images are not pbm or pgm, then we could create a tiff via a temp-file
;; to reduce memory usage

;; NOTE tiff version (uses much less memory than pnm, but requires temp file)
(defun doc-djvu-decode-page (page width &optional file)
  (setq file (or file (buffer-file-name)))
  (let ((status (call-process "ddjvu" nil t nil
                              (format "-size=%dx%d" width 10000)
                              "-format=tiff"
                              (format "-page=%d" page)
                              "-quality=50" ;; for some files this argument is
                                            ;; essential
                              file
                              "/tmp/doc-djvu-temp-img")))
    (unless (and status (zerop status))
      (error "Ddjvu error %s" status))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq coding-system-for-read 'binary)
      (insert-file-contents-literally "/tmp/doc-djvu-temp-img")
      (buffer-substring-no-properties (point-min) (point-max)))))

;; (defun doc-djvu-decode-page (page width &optional file)
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

(defun doc-djvu-djvused (command &optional page file)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
  ;; (with-current-buffer (get-buffer-create "e")
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

(defun doc-djvu-bookmarks (&optional file)
  (doc-djvu-djvused "print-outline" nil file))

(defun doc-djvu-annots (&optional page file)
  (doc-djvu-djvused "print-ant" (or page (doc-scroll-current-page)) file))

(defun doc-djvu-annots-all (&optional file)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
    (call-process "djvused" nil t nil
                  file "-e"
                  "'output-ant'")
    (goto-char (point-min))
    (while (re-search-forward " \\(#[[:alnum:]]+\\)" nil t)
      (replace-match " \"\\1\""))
    (let ((split (split-string (buffer-string)
                               "\\(# -* \n\\|\\.\n\\|set-ant\n\\)"
                               t))
          lines
          annots)
      (while (cdr split)
        (let ((str (car split)))
          (when (and (string-match "^select" str)
                     (string-match "[[:digit:]]+$" str))
            (push (cons (string-to-number (match-string 0 str))
                        (cons nil
                              (read (concat "(" (cadr split) ")"))))
                  lines))
          (setq split (cdr split))))
      lines)))

(defun doc-djvu-raw-contents (&optional arg file)
  (interactive "P")
  (setq file (or file (buffer-file-name)))
  (let ((contents (with-temp-buffer
                    (call-process "djvused" nil t nil
                                  file "-e"
                                  "'output-all'")
                    (buffer-string))))
    (cond (arg (pop-to-buffer (get-buffer-create "*doc-djvu-raw-contents*"))
               (erase-buffer)
               (insert contents)
               (goto-char (point-min)))
          (t (buffer-string)))))

(defun doc-djvu-parse-raw-contents (&optional file)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
    (call-process "djvused" nil t nil
                  file "-e"
                  "'output-all'")

    ;; first convert all hex colors into strings
    (goto-char (point-min))
    (while (re-search-forward " \\(#[[:alnum:]]+\\)" nil t)
      (replace-match " \"\\1\""))

    (mapcar (lambda (e)
              (setq e (string-replace ".\n" "" e))
              (let* ((split-point (string-search "\n" e))
                     (i (split-string (seq-subseq e 0 split-point) " page "))
                     (c (read (concat "(" (seq-subseq e (1+ split-point)) ")"))))
                (list (if-let (page (cadr i))
                          (string-to-number page)
                        'metadata)
                      (car i)
                      (when (eq (car c) 'set-ant)
                        (cons 'annots (seq-take-while (lambda (x) (not (eq x 'set-txt))) (cdr c))))
                      (when-let (p (cl-position 'set-txt c))
                        (cons 'text (nthcdr (1+ p) c))))))
            (cdr (split-string (buffer-string) "# ------------------------- 
")))))

(defun doc-djvu-format-contents (contents)
  (substring
   (replace-regexp-in-string
    "\"\\(#[[:alnum:]]\\{6,6\\}\\)\""
    "\\1"
    (mapconcat (lambda (e)
                 (pcase-let ((`(,page ,image) e))
                   (concat image
                           (unless (eq page 'metadata)
                             (format " page %d" page))
                           "\n"
                           (mapconcat (lambda (se)
                                        (if (stringp se)
                                            (format "%s" se)
                                          (if (eq (car se) 'annots)
                                              (concat "set-ant\n"
                                                      (mapconcat (lambda (a)
                                                                   (format "%S" a))
                                                                 (cdr se)
                                                                 "\n")
                                                      "\n.")
                                            (when-let (text (cadr se))
                                              (format "set-txt\n%S\n\n." text)))))
                                      (cddr e)
                                      "\n"))))
               contents "\n# ------------------------- 
"))
   0 -1))

(defun doc-djvu-add-annot (url comment area &optional page &rest args)
  (setq page (or page (doc-scroll-current-page)))
  (let ((annot `(maparea ,url ,comment ,area ,@args)))
    (setf (doc-scroll-annots page) (append (doc-scroll-annots page) (list annot)))))

(defun doc-djvu-create-annot (&optional style color)
  (interactive (list (completing-read "Select type: "
                                      '(rect) nil t)
                     (completing-read "select-color: "
                                      '(yellow red green blue))))
  (let ((page (car doc-scroll-active-region)))
    (mapcar (lambda (a)
              (pcase-let ((`(,x1 ,y1 ,x2 ,y2) a))
                (setq a (list x1 y1 (- x2 x1) (- y2 y1))))
              (doc-djvu-add-annot "" ""
                              `(,(make-symbol (or style 'rect))
                                ,@a)
                              page
                              (list 'hilite
                                    (upcase (apply #'color-rgb-to-hex
                                                   (append (color-name-to-rgb (or color "yellow")) '(2)))))))
            (doc-scroll-active-regions (cdr doc-scroll-active-region))))
  (setq doc-scroll-active-region nil)
  (doc-scroll-update t))

(defun color-name-to-hex (name)
  (apply #'color-rgb-to-hex (append (color-name-to-rgb "red") '(2))))


;; TODO compare `doc-djvu-parse-raw-contents' with the 'current contents'
