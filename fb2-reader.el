;;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-lib)
(require 'imenu)
(require 'dash)
(require 's)

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
	     (insert "\n"))
	    ;; ((equal current-tag 'image)
	    ;; (fb2-reader--parse-image book attributes))
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

    (insert prefix-first)
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) face alignment indent))
    (if append-newline
	(insert "\n"))
    (setq fill-prefix prefix)
    (fill-region point-start (point) alignment)
    ))

(defun fb2-reader--insert-newline-maybe ()
  "Insert newline if there is no newline (except \"empty-line\" tag) inserted before."
  (let (prev-empty-line-p)
    (if (equal (char-before) 10)		;char 10 is newline
	(progn (save-excursion
		 (backward-char)
		 (setq prev-empty-line-p (alist-get 'empty-line (get-text-property (point) 'fb2-reader-tags))))
	       (if prev-empty-line-p (insert "\n")))
      (insert "\n"))))

(defun fb2-reader--parse-title (book body tags face curr-tag)
  (let* ((title-level (--count (equal it 'section) tags))
	 (font-height (max 1.2 (- 1.8 (* title-level 0.2))))
	 (title-width (round (/ fill-column font-height)))
	 (title-face (cons (cons :height (list font-height)) face))
	 (fill-column-backup fill-column)
	 (start (point))
	 title-point)
    (insert "\n\n")
    (setq title-point (point))
    (setq-local fill-column title-width)
    (fb2-reader--format-string book body tags title-face curr-tag 'center 2)
    (setq-local fill-column fill-column-backup)
    ;; Add title and position to table of contents
    (let* ((title (s-replace "\n" " " (s-trim (s-collapse-whitespace (buffer-substring-no-properties start (point))))))
	   (toc-elt (list title title-level title-point)))
      (push  toc-elt fb2-reader-toc))
    ))

(defun fb2-reader--parse-cite (book body tags face current-tag)
  (let* ((indent 4)
	 (fill-column-backup fill-column)
	 (new-fill-column (- fill-column indent)))
    (fb2-reader--insert-newline-maybe)
    (setq-local fill-column new-fill-column)
    (dolist (subitem body)
    (fb2-reader-parse book subitem tags face 'left indent))
    (setq-local fill-column fill-column-backup)
    (insert "\n")
    )
  )

(defun fb2-reader--parse-poem (book body tags face current-tag)
  (dolist (subitem body)
    (let ((subtags (cons current-tag tags))
	  (subtag (cl-first subitem))
	  (subbody (cddr subitem)))
      (if (equal subtag 'stanza)
	  (fb2-reader--insert-newline-maybe))
      (fb2-reader-parse book subitem (cons subtag subtags) face)))
  (insert "\n")
  )

(defun fb2-reader--parse-image (book attributes)
  (message "starting image parsing, attrs: %s" (cdr (car attributes)))

  (let* ((id (replace-regexp-in-string "#" "" (cdr (car attributes))))
	 (binary (fb2-reader--find-binary book id))
	 (image (fb2-reader--generate-image binary))
    (insert-image image)
    (insert "\n\n"))))

(defun fb2-reader--find-binary (book id)
  (fb2-reader--find-subitem book 'binary 'id id))

(defun fb2-reader--generate-image (binary-item)
  (when-let* ((img-type (alist-get 'content-type (cl-second binary-item)))
	      (img-supported (member img-type '("image/jpeg" "image/png"))))
    (create-image (base64-encode-string (cl-third binary-item)) 'imagemagick t :height 500 :background "white")))

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

(defun fb2-reader-read-book (book)
  (dolist (body (fb2-reader--get-bodies book))
    (fb2-reader-parse book body))
  )


;; Imenu support

(defun fb2-reader-imenu-create-index ()
  (let (index)
    (dolist (item fb2-reader-toc)
      (push (cons (cl-first item) (cl-third item)) index))
      index))

(defun fb2-reader-imenu-setup ()
  (setq imenu-create-index-function 'fb2-reader-imenu-create-index))

(defun fb2-reader-read ()
  (interactive)
  (let (book title filename bodies)
    (setq book (libxml-parse-xml-region (point-min) (point-max)))
    ;; (kill-buffer)
    (setq title (fb2-reader--get-title book))
    (get-buffer-create title)
    (switch-to-buffer title)
    ;; Parse fb2
    (setq-local fb2-reader-ids '())
    (setq-local fb2-reader-toc '())
    (fb2-reader-read-book book)
    (fb2-reader-imenu-setup)
    ))

;; (define-derived-mode fb2-reader-mode view-mode "FB2-reader"

  ;; )

;; (add-to-list 'auto-mode-alist '("\\.fb2$" . fb2-reader-mode))

(provide 'fb2-reader)

