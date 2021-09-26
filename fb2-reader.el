;;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-lib)
(require 'imenu)
(require 'dash)
(require 'f)
(require 's)

;; TODO: restore position
;; TODO: imenu
;; TODO: [] bindings for next-prev title

(defcustom fb2-reader-cache-dir (expand-file-name "fb2-reader" user-emacs-directory)
  ""
  :type 'string
  :group 'fb2-reader)

(defvar fb2-reader-index-filename "index.el")


(defvar-local fb2-reader-ids '()
  "List of pairs of node's ids and its positions in rendered FB2 document
They will be used to jump by links in document")

(defvar-local fb2-reader-toc '()
  "Table of contents of FB2 book")

(defvar-local fb2-reader-cot '()
  "Reversed table of content (to show title in header line)")

(defconst fb2-reader-header-line-format
  '(:eval (list (propertize " " 'display '((space :align-to 0)))
		"><> "
		(fb2-reader-current-chapter)
		" <><")))

(defun fb2-reader-parse (book item &optional tags face alignment indent)
  (or face (setq face 'default))
  (or tags (setq tags '()))
  (or alignment (setq alignment 'left))
  (or indent (setq indent 0))
  (if (stringp item)
		 (insert (propertize (string-trim item)
				     'face face
				     'fb2-reader-tags tags))
    (let ((current-tag (cl-first item))
	  (attributes (cl-second item))
	  (body (cddr item)))

      ;; Create list of all ids in document
      (when-let ((id (alist-get 'id attributes)))
	(push (cons (intern id) (point)) fb2-reader-ids))

      (cond ((equal current-tag 'text-author)
	     (fb2-reader--format-string book body tags face current-tag 'right indent))
	    ((equal current-tag 'poem)
	     (fb2-reader--parse-poem book body tags face current-tag))
	    ((equal current-tag 'title)
	     (fb2-reader--parse-title book body tags face current-tag)
	     )
	    ((equal current-tag 'cite)
	     (fb2-reader--parse-cite book body tags face current-tag)
	     )
	    ((equal current-tag 'empty-line)
	     (insert (propertize "\n" 'fb2-reader-tags (cons 'empty-line tags))))
	    ((equal current-tag 'image)
	    (fb2-reader--parse-image book attributes tags))
	    ((equal current-tag 'a)
	     (fb2-reader--parse-a-link book attributes body tags face current-tag)
	     )
	    ((equal current-tag 'p)
	     (fb2-reader--format-string book body tags
					 face current-tag alignment indent))
	    ((equal current-tag 'v)
	     (fb2-reader--format-string book body tags
					 face current-tag 'center indent))
	    ((equal current-tag 'strong)
	     (fb2-reader-parse book (cl-first body) tags (cons 'bold face)))
	    ((equal current-tag 'emphasis)
	     (fb2-reader-parse book (cl-first body) tags (cons 'italic face)))
	    ('t
	     (dolist (subitem body)
	       (fb2-reader-parse book subitem (cons current-tag tags) face)))))))

(defun fb2-reader--format-string (book body tags face curr-tag  alignment indent  &optional indent-first append-newline)
  (or indent-first (setq indent-first 2))
  (or append-newline (setq append-newline 't))
  (let* ((point-start (point))
	 (indent (+ indent 1))
	 ;; 1 appended to indent because of unwanted behavior
	 ;; of fill-paragraph  function. If fill-prefix equals
	 ;; nil or "" fill-paragraph uses spaces at
	 ;; beginning of the string as fill-prefix. If fill
	 ;; prefix is not empty it works as expected.
	 (prefix (string-join (make-list indent " ")))
	 (prefix-first (string-join (make-list (+ indent indent-first) " "))))

    (insert (propertize prefix-first 'fb2-reader-tags tags))
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) face alignment indent))
    (if append-newline
	(insert (propertize "\n" 'fb2-reader-tags tags)))
    (setq fill-prefix prefix)
    (fill-region point-start (point) alignment)))

(defun fb2-reader--insert-newline-maybe ()
  "Insert newline if there is no newline (except \"empty-line\" tag) inserted before."
  (let (prev-empty-line-p)
    (save-excursion
      (backward-char)
      (setq already-added
	    (and (equal (char-before) 10)
		 (equal (char-after) 10))))
    (unless already-added
      (insert (propertize "\n" 'fb2-reader-tags '(empty-line-special))))))

(defun fb2-reader--parse-title (book body tags face curr-tag)
  (let* ((title-level (--count (equal it 'section) tags))
	 (font-height (max 1.2 (- 1.8 (* title-level 0.2))))
	 (title-width (round (/ fill-column font-height)))
	 (title-face (cons (cons :height (list font-height)) face))
	 (fill-column-backup fill-column)
	 (start (point))
	 end
	 title-point)

    (when (> (line-number-at-pos) 1)	;don't insert separator if this is first title
      (insert "\n\n"))
    (setq title-point (point))
    (setq-local fill-column title-width)
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) title-face  'center 2))
    (setq-local fill-column fill-column-backup)
    (setq end (point))
    (insert (propertize "\n" 'fb2-reader-title 't))
    ;; Add title and position to table of contents
    (let* ((title (s-replace "\n" " " (s-trim (s-collapse-whitespace (buffer-substring-no-properties start (point))))))
	   (toc-elt (list title title-level title-point))
	   (cot-elt (list end title)))
      (push  toc-elt fb2-reader-toc)
      (push cot-elt fb2-reader-cot))))

(defun fb2-reader--parse-cite (book body tags face current-tag)
  (let* ((indent 4)
	 (fill-column-backup fill-column)
	 (new-fill-column (- fill-column indent)))
    (fb2-reader--insert-newline-maybe)
    (setq-local fill-column new-fill-column)
    (dolist (subitem body)
    (fb2-reader-parse book subitem (cons current-tag tags) face 'left indent))
    (setq-local fill-column fill-column-backup)
    (fb2-reader--insert-newline-maybe)))

(defun fb2-reader--parse-poem (book body tags face current-tag)
  (dolist (subitem body)
    (let ((subtags (cons current-tag tags))
	  (subtag (cl-first subitem))
	  (subbody (cddr subitem)))
      (if (equal subtag 'stanza)
	  (fb2-reader--insert-newline-maybe))
      (fb2-reader-parse book subitem (cons subtag subtags) face)))
  (insert (propertize "\n" 'fb2-reader-tags '('empty-line-special)))
  )


(defun fb2-reader--parse-image (book attributes tags)
  (when-let* ((id (replace-regexp-in-string "#" "" (cdr (car attributes))))
	      (binary (fb2-reader--find-binary book id))
	      (imgdata (fb2-reader--extract-image-data binary))
	      (data (car imgdata))
	      (type (cdr imgdata))
	      (img-raw (fb2-reader--create-image data type))
	      (size-raw (image-size img-raw 't))
	      (img-adj (fb2-reader--create-image data type
						 :max-width 400
						 :max-height 400))
	      (width-ch (car (image-size img-adj)))
	      (prefix-num (round (/ (- fill-column width-ch) 2)))
	      (prefix-str (string-join (make-list prefix-num " ")))
	      (fill-str (propertize " " 'fb2-reader-tags (cons 'image tags)
				    'fb2-reader-image-params size-raw)))
    (insert prefix-str)
    (insert-image img-adj fill-str)
    (insert "\n\n")
    ))

(defun fb2-reader--find-binary (book id)
  (fb2-reader--find-subitem book 'binary 'id id))

(defun fb2-reader--extract-image-data (item)
  (when-let* ((type-str (alist-get 'content-type (cl-second item)))
	      (type-char (alist-get type-str
				    '(("image/jpeg" . jpeg) ("image/png" . png))
				    nil nil 'equal))
	      (data (base64-decode-string (cl-third item))))
    (cons data type-char)))

;; In case I'll need seamlessly switch image backend to imagemagick or something
(defun fb2-reader--create-image (data type &rest props)
  (apply 'create-image data type 't props))


(defun fb2-reader--parse-a-link (book attributes body tags face curr-tag)
  (let ((id (replace-regexp-in-string "#" "" (cdr (car attributes))))
	(start (point))
	(link-face (cons (cons :inherit (list 'link)) face)))
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) link-face))
    (add-text-properties start (point)
			 (list 'fb2-reader-target id
				'follow-link t
				 'keymap fb2-reader-link-map
				 'mouse-face 'highlight))))

(defun fb2-reader-follow-link ()
  (interactive)
  (push-mark)
  (let* ((target (get-text-property (point) 'fb2-reader-target))
	 (position (alist-get (intern target) fb2-reader-ids)))
    (goto-char position)))

(defvar fb2-reader-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\r" 'fb2-reader-follow-link)
    (define-key map [mouse-2] 'fb2-reader-follow-link)
    map))

(defun fb2-reader--find-subitem (item tag &optional property value)
  (if (listp item)
      (catch 'subitem (dolist (subitem item)
			(if (and
			     (listp subitem)
			     (equal (cl-first subitem) tag)
			     (or (not property)
				 (and (listp (cl-second subitem))
				      (equal value (alist-get property (cl-second subitem))))))
			    (throw 'subitem subitem))))))

(defun fb2-reader--find-subitem-recursively (item &rest tags)
  (let (curr-item)
    (setq curr-item item)
    (dolist (tag tags curr-item)
      (setq curr-item (fb2-reader--find-subitem curr-item tag))
    )))

(defun fb2-reader--get-bodies (book)
  (let (bodies)
    (dolist (item (cddr book))
      (if (equal (cl-first item) 'body)
	  (push item bodies)))
    (reverse bodies)))

(defun fb2-reader--get-title (book)
  (cl-third (fb2-reader--find-subitem-recursively (cddr book) 'description 'title-info 'book-title)))

(defun fb2-reader-render (book)
  (dolist (body (fb2-reader--get-bodies book))
    (fb2-reader-parse book body)))


;; Imenu support

(defun fb2-reader-imenu-create-index ()
  (let (index)
    (dolist (item fb2-reader-toc)
      (push (cons (cl-first item) (cl-third item)) index))
      index))

(defun fb2-reader-imenu-setup ()
  (setq imenu-create-index-function 'fb2-reader-imenu-create-index))


;; Header line

(defun fb2-reader-toc-bisect (toc pos)
  (let* ((first (caar toc))
	 (last (caar (last toc)))
	 (toc-length (length toc))
	 (mid (car (nth (/ toc-length 2) toc))))
    (if (<= toc-length 2)
	(if (> pos last)
	    (cdadr toc)
	(cadar toc))
      (if (< pos mid)
	  (fb2-reader-toc-bisect (butlast toc (1- (- toc-length (/ toc-length 2))))
		  pos)
	(fb2-reader-toc-bisect (seq-drop toc (/ toc-length 2)) pos)))))

(defun fb2-reader-current-chapter ()
  (save-excursion
    (goto-char (window-start))
    (fb2-reader-toc-bisect fb2-reader-cot (point))))


(defun fb2-reader-set-up-header-line ()
  (setf header-line-format 'fb2-reader-header-line-format))

;; TOC sidebar

;; Caching

(defvar fb2-reader-cache-index nil
  "Contains alist (filename modification-time cache-filename)")
(defvar fb2-reader--cache-initialized nil)

(defun fb2-reader-init-cache ()
  (unless (f-exists-p fb2-reader-cache-dir)
    (make-directory fb2-reader-cache-dir))
  (let ((idx-path (f-join fb2-reader-cache-dir
			  fb2-reader-index-filename)))
    (if (f-exists-p idx-path)
	(setq fb2-reader-cache-index (fb2-reader-load-cache-index idx-path))))
  (setq fb2-reader--cache-initialized 't))

(defun fb2-reader-load-cache-index (file)
  (if (f-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(read (current-buffer)))))

(defun fb2-reader-save-cache-index (file)
  (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
	(insert ";; fb2-reader.el -- read fb2 books  ")
	(insert "file contains cache index, don't edit.\n")
	(insert (prin1-to-string fb2-reader-cache-index))
	(insert "\n")
    ))

(defun fb2-reader-cache-avail-p (file &optional actual-only)
  (when (not fb2-reader--cache-initialized)
    (error "Cache index not read"))
  (let ((idx-entry (alist-get file fb2-reader-cache-index nil nil 'equal)))
    (if actual-only
	(time-equal-p (car idx-entry)
		      (file-attribute-modification-time
		       (file-attributes file)))
      't)))


(defun fb2-reader-get-cache (file)
  (let ((cache-file (cl-second (alist-get file fb2-reader-cache-index nil nil 'equal))))
    (if (f-exists-p cache-file)
	(with-temp-buffer
	  (insert-file-contents cache-file)
	  (goto-char (point-min))
	  (read (current-buffer))))))

(defun fb2-reader-cache-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (fb2-reader-remove-from-cache (buffer-file-name))
  (with-current-buffer buffer
    (let ((idx-filename (f-join fb2-reader-cache-dir fb2-reader-index-filename))
	  (cache-filename (f-join fb2-reader-cache-dir
				  (fb2-reader-gen-cache-file-name buffer-file-name)))
	  (book-content (buffer-substring (point-min) (point-max))))
      (with-temp-file cache-filename
	(set-buffer-file-coding-system 'utf-8)
	(insert ";; fb2-reader.el -- read fb2 books  ")
	(insert "file contains fb2-reader book cache, don't edit.\n")
	(insert (prin1-to-string (list fb2-reader-ids fb2-reader-toc fb2-reader-cot  book-content)))
	(insert "\n"))
      (push (list buffer-file-name
		  (file-attribute-modification-time
		   (file-attributes buffer-file-name))
		  cache-filename) fb2-reader-cache-index)
      (fb2-reader-save-cache-index idx-filename))))

(defun fb2-reader-remove-from-cache (file)
  (when-let ((cache-file (cl-second
			  (alist-get file fb2-reader-cache-index nil nil 'equal))))
    (f-delete cache-file)
    (remove file fb2-reader-cache-index)
    (fb2-reader-save-cache-index
      (f-join fb2-reader-cache-dir fb2-reader-index-filename))))

(defun fb2-reader-restore-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((inhibit-null-byte-detection t))
  (with-current-buffer buffer
    (let ((book-cache (fb2-reader-get-cache buffer-file-name))
	  (inhibit-null-byte-detection t))
      (erase-buffer)
      (insert (cl-fourth book-cache))
      (setq fb2-reader-ids (cl-first book-cache)
	    fb2-reader-toc (cl-second book-cache)
	    fb2-reader-cot (cl-third book-cache))))))

(defun fb2-reader-gen-cache-file-name (filepath)
  (let ((fname (f-base filepath))
	(chars "abcdefghijklmnopqrstuvwxyz0123456789")
	(randstr "")
	randchar
	randnum)
    (while (length< randstr 7)
      (setq randnum (% (abs (random)) (length chars))
	    randchar (substring chars randnum (1+ randnum))
	    randstr (concat randstr randchar)))
    (format "%s%s.el" fname randstr)))

(defun fb2-reader-reload-book ()
  (interactive))

(defun fb2-reader-read-fb2-zip (file)
  (let ((tmpdir (concat (make-temp-file
			 (concat (f-base file) "-")
			 'directory) (f-path-separator))))
    (call-process "unzip" nil nil nil "-d" tmpdir file)
    (with-current-buffer
	(find-file-noselect (f-join tmpdir (f-base file)))
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun fb2-reader-read-fb2 (file)
  (with-current-buffer (find-file-noselect file)
    (libxml-parse-xml-region (point-min) (point-max))))


(defun fb2-reader-read ()
  (interactive)
  (fb2-reader-init-cache)
  (let (book title filename bodies)
    (setq book (if (equal "zip" (f-ext (buffer-file-name)))
	(fb2-reader-read-fb2-zip (buffer-file-name))
      (fb2-reader-read-fb2 (buffer-file-name))))
    ;; (setq book (libxml-parse-xml-region (point-min) (point-max))
    ;; 	  filename buffer-file-name)
    ;; (kill-buffer)
    (setq filename buffer-file-name)
    (setq title (fb2-reader--get-title book))
    (get-buffer-create title)
    (switch-to-buffer title)
    (setq buffer-file-name filename)
    ;; Parse fb2
    (setq-local fb2-reader-ids '())
    (setq-local fb2-reader-toc '())
    (setq-local fb2-reader-cot '())
      (fb2-reader-render book)
    ;; (fb2-reader-read-book book)
    ;; (if (fb2-reader-cache-avail-p filename)
    ;; 	(fb2-reader-restore-buffer)

    ;;   (fb2-reader-cache-buffer))
    (setq-local fb2-reader-cot (reverse fb2-reader-cot))
    (fb2-reader-imenu-setup)
    (fb2-reader-set-up-header-line)
    ))

(define-derived-mode fb2-reader-mode view-mode "FB2-reader"
  "Major mode for reading FB2 books
\\{fb2-reader-mode-map}"
  (let (book title filename bodies)
    (setq buffer-read-only nil)
    (setq book (libxml-parse-xml-region (point-min) (point-max)))
    (erase-buffer)
    (setq title (fb2-reader--get-title book))
    (rename-buffer title)
    ;; (get-buffer-create title)
    ;; (switch-to-buffer title)
    (auto-save-mode 0)
    (setq truncate-lines 1)
    (buffer-disable-undo)
    (make-local-variable 'fb2-reader-ids)
    (make-local-variable 'fb2-reader-toc)
    (fb2-reader-read-book book)
    (fb2-reader-imenu-setup)
    (setq buffer-read-only 't)
    (set-buffer-modified-p nil)))

;; (add-to-list 'auto-mode-alist '("\\.fb2$" . fb2-reader-mode))

(provide 'fb2-reader)

      ;; Вскоре костер уже ревел. Гарв нарубил столько дров, чтобы
