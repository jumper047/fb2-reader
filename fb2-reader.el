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
;; TODO: metadata screen
;; TODO: function to reload book
;; TODO: cleanup comments

(defcustom fb2-reader-settings-dir (expand-file-name "fb2-reader" user-emacs-directory)
  "Path to directory with cached books, saved places etc."
  :type 'directory
  :group 'fb2-reader)

(defcustom fb2-reader-title-in-headerline t
  "Show current chapter's title in headerline."
  :type 'boolean
  :group 'fb2-reader)

(defcustom fb2-reader-restore-position t
  "Restore last viewed position on book's opening."
  :type 'boolean
  :group 'fb2-reader)

(defcustom fb2-reader-page-width 120
  "Width of the rendered text."
  :type 'integer
  :group 'fb2-reader)

(defcustom fb2-reader-show-images 't
  "Show images."
  :type 'boolean)

(defcustom fb2-reader-image-max-width 400
  "Maximum width of the displayed image."
  :type 'integer
  :group 'fb2-reader)

(defcustom fb2-reader-image-max-height 400
  "Maximum height of the displayed image."
  :type 'integer
  :group 'fb2-reader)

(defvar fb2-reader-index-filename "index.el")
(defvar fb2-reader-position-filename "positions.el")

(defvar-local fb2-reader-last-saved-position nil)

(defvar-local fb2-reader-file-name nil
  "Book's filename.")

(defconst fb2-reader-header-line-format
  '(:eval (list (propertize " " 'display '((space :align-to 0)))
		(fb2-reader-current-chapter))))

(defun fb2-reader-parse (book item &optional tags face alignment indent)
  "Recursively parse ITEM (part of the BOOK) and insert it into the buffer."

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

      (cond ((equal current-tag 'text-author)
	     (fb2-reader--format-string book body tags face current-tag 'right indent))
	    ((equal current-tag 'section)
	     (fb2-reader--parse-section book attributes body tags face current-tag))
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
	     ;; Disabled due new async rendering function
	     ;; (fb2-reader--parse-image book attributes tags)
	     (fb2-reader--pickle-image book attributes tags)
	     )
	     
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
  "Format BODY (part of the BOOK) into string and insert it."

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

(defun fb2-reader--parse-section (book attributes body tags face curr-tag)
  (let ((start (point))
	(id (alist-get 'id attributes)))
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) face))
    (when id
      (add-text-properties start (point) (list 'fb2-reader-id (intern id))))))

(defun fb2-reader--parse-title (book body tags face curr-tag)
  "Parse and insert BODY (BOOK 's part) as title."

  (let* ((title-level (--count (equal it 'section) tags))
	 (font-height (max 1.2 (- 1.8 (* title-level 0.2))))
	 (title-width (round (/ fill-column font-height)))
	 (title-face (cons (cons :height (list font-height)) face))
	 (fill-column-backup fill-column)
	 start
	 end)


    (when (> (line-number-at-pos) 1)	;don't insert separator if this is first title
      (insert "\n\n"))
    (setq start (point))
    (setq-local fill-column title-width)
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) title-face  'center 2))
    (setq-local fill-column fill-column-backup)
    (setq end (point))
    (insert "\n")
    (add-text-properties start end '(fb2-reader-title t))))


(defun fb2-reader--parse-cite (book body tags face current-tag)
  "Parse and insert BODY (BOOK 's part) as cite."

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
  "Parse and insert BODY (BOOK 's part) as poem."

  (dolist (subitem body)
    (let ((subtags (cons current-tag tags))
	  (subtag (cl-first subitem))
	  (subbody (cddr subitem)))
      (if (equal subtag 'stanza)
	  (fb2-reader--insert-newline-maybe))
      (fb2-reader-parse book subitem (cons subtag subtags) face)))
  (insert (propertize "\n" 'fb2-reader-tags '('empty-line-special)))
  )



(defun fb2-reader--pickle-image (book attributes tags)
  "Save all image-related info from BOOK ATTRIBUTES and TAGS to text property.

It should be rendered when propertized text will be inserted into buffer."
  (when-let* ((imgdata (fb2-reader--extract-image-data book attributes tags))
	      (type-str (cl-first imgdata))
	      (data-str (cl-second imgdata))
	      (tags (cl-third imgdata)))
    (insert (propertize " "
			'fb2-reader-image-type type-str
			'fb2-reader-image-data data-str
			'fb2-reader-tags tags))

    ))

(defun fb2-reader--extract-image-data (book attributes tags)
  (when-let* ((id (replace-regexp-in-string "#" "" (cdr (car attributes))))
	      (binary (fb2-reader--find-binary book id))
	      (type-str (alist-get 'content-type (cl-second binary)))
	      (data-str (cl-third binary)))
    (list type-str data-str tags))
  )

(defun fb2-reader--insert-image (data type tags)
  "Generate image from DATA of type TYPE and insert it at point.

 Property fb2-reader-tags will be set to TAGS and appended
to placeholder."

  (when-let* ((type-char (alist-get type
				    '(("image/jpeg" . jpeg) ("image/png" . png))
				    nil nil 'equal))
	      (data-decoded (base64-decode-string data))
	      (img-raw (fb2-reader--create-image data-decoded type-char))
	      (size-raw (image-size img-raw 't))
	      (img-adj (fb2-reader--create-image data-decoded type-char
						 ;; TODO: should be customizable
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

(defun fb2-reader-restore-images (&optional buffer)
  "Find all images pickled in BUFFER and restore them."

  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (not (eobp))
      (let ((next-change (or (next-single-property-change (point) 'fb2-reader-image-data)
			     (point-max)))
	    (plist (text-properties-at (point)))
	    image-data
	    image-type
	    tags)
	(when (plist-member plist 'fb2-reader-image-data)
	  (setq image-data (plist-get plist 'fb2-reader-image-data)
		image-type (plist-get plist 'fb2-reader-image-type)
		tags (plist-get plist 'fb2-reader-tags))
	  (delete-char 1)
	  (fb2-reader--insert-image image-data image-type tags))
	(goto-char next-change)))))

(defun fb2-reader--find-binary (book id)
  "Find binary with ID in BOOK."

  (fb2-reader--find-subitem book 'binary 'id id))


;; In case I'll need seamlessly switch image backend to imagemagick or something
(defun fb2-reader--create-image (data type &rest props)
  "Create image of type TYPE from image DATA."

  (apply 'create-image data type 't props))


;; Links

(defun fb2-reader--parse-a-link (book attributes body tags face curr-tag)
  "Parse and insert link described with ATTRIBUTES from BOOK."

  (let ((id (intern (replace-regexp-in-string "#" "" (cdr (car attributes)))))
	(start (point))
	(link-face (cons (cons :inherit (list 'link)) face)))
    (dolist (subitem body)
      (fb2-reader-parse book subitem (cons curr-tag tags) link-face))
    (add-text-properties start (point)
			 (list 'fb2-reader-target id
				'follow-link t
				 'keymap fb2-reader-link-map
				 'mouse-face 'highlight))))

(defun fb2-reader--get-target-pos (id)
  (save-excursion
    (goto-char (point-min))
    
    (let (link-found next-change plist target-pos)
      (while (not (or link-found (eobp)))
	(setq next-change (or (next-single-property-change (point) 'fb2-reader-id)
			      (point-max))
	      plist (text-properties-at (point))
	      link-found (equal id (plist-get plist 'fb2-reader-id))
	      target-pos (point))
	(goto-char next-change))
      (if link-found target-pos))))
      

(defun fb2-reader-follow-link ()
  "Follow link under point."
  
  (interactive)
  (push-mark)
  (when-let* ((target-id (get-text-property (point) 'fb2-reader-target))
	      (position (fb2-reader--get-target-pos target-id)))
    (goto-char position)))

(defvar fb2-reader-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\r" 'fb2-reader-follow-link)
    (define-key map [mouse-2] 'fb2-reader-follow-link)
    map))

(defun fb2-reader--find-subitem (item tag &optional property value)
  "Find first ITEM 's child with TAG.

Founded item should have PROPERTY with certain VALUE,
if these parameters are set."
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
  "Find ITEM 's subitem with first tag from TAGS, then subitem's subitem with second tag and so on."

  (let (curr-item)
    (setq curr-item item)
    (dolist (tag tags curr-item)
      (setq curr-item (fb2-reader--find-subitem curr-item tag))
    )))

(defun fb2-reader--get-bodies (book)
  "Get list of all bodies from the BOOK."

  (let (bodies)
    (dolist (item (cddr book))
      (if (equal (cl-first item) 'body)
	  (push item bodies)))
    (reverse bodies)))

(defun fb2-reader--get-title (book)
  "Get title from BOOK."

  (cl-third (fb2-reader--find-subitem-recursively (cddr book) 'description 'title-info 'book-title)))

(defun fb2-reader-render (book)
  "Render BOOK and insert it into the current buffer."

  (dolist (body (fb2-reader--get-bodies book))
    (fb2-reader-parse book body)))


;; Imenu support

(defun fb2-reader-imenu-create-index ()
  (goto-char (point-min))
  (let (next-change plist index)
  (while (not (eobp))
    (setq next-change (or (next-single-property-change (point) 'fb2-reader-title)
			  (point-max))
	  plist (text-properties-at (point)))
    (when (plist-member plist 'fb2-reader-title)
      (push (cons (s-replace "\n" " " (s-trim (s-collapse-whitespace
					       (buffer-substring-no-properties
						(point) next-change)))) (point))
	    index))
    (goto-char next-change))
  index))

(defun fb2-reader-imenu-setup ()
  (setq imenu-create-index-function 'fb2-reader-imenu-create-index))


;; Header line

(defun fb2-reader-toc-bisect (toc pos)
  "Get from TOC element with position right before POS."

  (let* ((first (caar toc))
	 (last (caar (last toc)))
	 (toc-length (length toc))
	 (mid (car (nth (/ toc-length 2) toc))))
    (if (<= toc-length 2)
	(if (> pos last)
	    (cadadr toc)
	(cadar toc))
      (if (< pos mid)
	  (fb2-reader-toc-bisect (butlast toc (1- (- toc-length (/ toc-length 2))))
		  pos)
	(fb2-reader-toc-bisect (seq-drop toc (/ toc-length 2)) pos)))))

(defun fb2-reader-current-chapter ()
  "Get current chapter's title."

  (save-excursion
    (goto-char (window-start))
    ;; step forward for one char because when cursor appends exactly on title's
    ;; border prev/next single property function skips current change
    (forward-char 1)
    (setq title-end (funcall (if (plist-member (text-properties-at (point)) 'fb2-reader-title)
				 'next-single-property-change
			       'previous-single-property-change)
			     (point) 'fb2-reader-title))
    (if title-end (goto-char title-end))

    (setq title-begin (previous-single-property-change (point) 'fb2-reader-title))

    (when (and title-begin title-end)
      (s-replace "\n" " " (s-trim (s-collapse-whitespace
				   (buffer-substring-no-properties
				    title-begin title-end)))))))


(defun fb2-reader-set-up-header-line ()
  "Set up header line in current buffer."

  (setf header-line-format 'fb2-reader-header-line-format))

;; Reading from settings

(defun fb2-reader-ensure-settingsdir ()
  "Create settings directory if necessary."
  (unless (f-exists-p fb2-reader-settings-dir)
    (make-directory fb2-reader-settings-dir)))

(defun fb2-reader-load-settings (loadfn filename)
  "Open .el file from settings with function LOADFN.

FILENAME located in settings directory. Returns nil if file not found.
LOADFN should receive only one argument - full path to file."
  (let ((fullpath (f-join fb2-reader-settings-dir
			  filename)))
    (if (f-exists-p fullpath)
	(funcall loadfn fullpath))))

(defun fb2-reader-load-file (file)
  "Load text from FILE as elisp."

  (if (f-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(read (current-buffer)))))


;; Navigation

(defun fb2-reader--jump-chapter (chapter)
  (unless (eq chapter 0)	;do nothing if chapter is 0.
    (let* ((fwd (> chapter 0))
	   (obp (if fwd 'eobp 'bobp))
	   (point-end (if fwd 'point-max 'point-min))
	   (search-prop (if fwd 'next-single-property-change
			  'previous-single-property-change))
	   (step (if fwd 1 -1))
	   found)
      (message "chapter is %s" chapter)
      (while (and  (not (eq 0 chapter)) (not (funcall obp)))
	;; If we already inside title, skip it and go to next one
	(if (eq chapter 0)
	(message "chapter is %s" chapter))
	(unless (plist-member (text-properties-at (point)) 'fb2-reader-title)
	  (setq found 't))
	(goto-char (or (funcall search-prop (point) 'fb2-reader-title)
		       (funcall point-end)))
	(if found (setq chapter (- chapter step)
			found nil))))
  (recenter 0)
  ))

(defun fb2-reader-forward-chapter (&optional n)
  "Go N chapters forward."
  (interactive "p")
  (or n (setq n 1))
  (fb2-reader--jump-chapter n))

(defun fb2-reader-backward-chapter (&optional n)
  "Go N chapters backward."
  (interactive "p")
  (or n (setq n -1))
  (fb2-reader--jump-chapter n))

;; Caching

(defun fb2-reader-cache-index ()
  "Read cache index."

  (fb2-reader-load-settings 'fb2-reader-load-file
			    fb2-reader-index-filename))




(defun fb2-reader-save-cache-index (file index)
  "Serialize given cache INDEX and save it to FILE."
  
  (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
	(insert ";; fb2-reader.el -- read fb2 books  ")
	(insert "file contains cache index, don't edit.\n")
	(insert (prin1-to-string index))
	(insert "\n")
    ))

(defun fb2-reader-cache-avail-p (file &optional actual-only)
  "Check if cache for FILE available.

If ACTUAL-ONLY return 't if cache is existed and actual."
  
  (when-let ((idx-entry (alist-get file (fb2-reader-cache-index) nil nil 'equal)))
    (if actual-only
	(time-equal-p (car idx-entry)
		      (file-attribute-modification-time
		       (file-attributes file)))
      't)))


(defun fb2-reader-get-cache (file)
  "Load cache for FILE if it exists."

  (let ((cache-file (cl-second (alist-get file (fb2-reader-cache-index) nil nil 'equal))))
    (if (and cache-file (f-exists-p cache-file))
	(with-temp-buffer
	  (insert-file-contents cache-file)
	  (goto-char (point-min))
	  (read (current-buffer))))))

(defun fb2-reader-add-to-cache (filename data)
  "Add to cache rendered DATA for FILENAME.

Replace already added data if presented."

  (fb2-reader-remove-from-cache filename)
  (let ((idx-filename (f-join fb2-reader-settings-dir fb2-reader-index-filename))
	(cache-filename (f-join fb2-reader-settings-dir
				(fb2-reader-gen-cache-file-name filename)))
	(index (fb2-reader-cache-index)))
    (with-temp-file cache-filename
      (set-buffer-file-coding-system 'utf-8)
      (insert ";; fb2-reader.el -- read fb2 books  ")
      (insert "file contains fb2-reader book cache, don't edit.\n")
      (insert "\n")
      (insert (prin1-to-string data))
      )
    
    (push (list fb2-reader-file-name
		(file-attribute-modification-time
		 (file-attributes fb2-reader-file-name))
 		cache-filename)
	  index)
    (fb2-reader-save-cache-index idx-filename index)))


(defun fb2-reader-remove-from-cache (filename)
  "Remove FILENAME from cache."

  (when-let ((cache-file (cl-second
			  (alist-get filename (fb2-reader-cache-index) nil nil 'equal)))
	     (index (fb2-reader-cache-index)))
    (f-delete cache-file)
    (remove filename index)
    (fb2-reader-save-cache-index
     (f-join fb2-reader-settings-dir fb2-reader-index-filename)
     index)))

(defun fb2-reader-restore-buffer (&optional buffer)
  "Restore BUFFER from cache. Restore current if arg missed."

  (or buffer (setq buffer (current-buffer)))
  (when (fb2-reader-cache-avail-p
	 (buffer-local-value 'fb2-reader-file-name buffer) 't)
    (let ((inhibit-null-byte-detection t))
      (with-current-buffer buffer
  (setq buffer-read-only 't)
  (set-buffer-modified-p nil)
	(setq buffer-read-only nil)
	(erase-buffer)
	(set-buffer-file-coding-system 'utf-8)
	(insert (fb2-reader-get-cache fb2-reader-file-name))
	(if fb2-reader-show-images
	    (fb2-reader-restore-images))
	(if fb2-reader-restore-position
	    (fb2-reader-restore-pos)
	  (goto-char (point-min)))
	(fb2-reader-imenu-setup)
	(setq buffer-read-only nil)
	(set-buffer-modified-p nil)))))

(defun fb2-reader-gen-cache-file-name (filepath)
  "Generate file name for FILEPATH."

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


(defun fb2-reader-positions ()
  "Read file with saved positions and return alist"

  (fb2-reader-load-settings 'fb2-reader-load-file
			    fb2-reader-position-filename))

(defun fb2-reader-save-pos (&optional buffer)
  "Save current position in BUFFER."

  (or buffer (setq buffer (current-buffer)))
  (let ((pos-path (f-join fb2-reader-settings-dir
			  fb2-reader-position-filename))
	(filename (buffer-local-value 'fb2-reader-file-name buffer)))
    (with-temp-file pos-path
      (insert (prin1-to-string
	       (cons (list fb2-reader-file-name (point))
		     (assoc-delete-all filename (fb2-reader-positions))))))))

(defun fb2-reader-save-all-pos ()
  "Save positions in all fb2-reader buffers."

  (dolist (buffer (buffer-list))
    (if (eq (buffer-local-value 'major-mode buffer) 'fb2-reader-mode)
	(fb2-reader-save-pos buffer))))

(defun fb2-reader-restore-pos (&optional buffer)
  "Restore position in current buffer or BUFFER."

  (or buffer (setq buffer (current-buffer)))
  (let* ((filename (buffer-local-value 'fb2-reader-file-name buffer))
	      (pos (car (alist-get filename (fb2-reader-positions) nil nil 'equal))))
    (with-current-buffer buffer (goto-char (or pos (point-min))))))


;; TODO: Delete temp directory
(defun fb2-reader-read-fb2-zip (file)
  "Read book from fb2.zip FILE.

Book name should be the same as archive except .zip extension."

  (let ((tmpdir (concat (make-temp-file
			 (concat (f-base file) "-")
			 'directory) (f-path-separator)))
	fb2-buffer
	parsed)
    (call-process "unzip" nil nil nil "-d" tmpdir file)
    (with-current-buffer
	(setq fb2-buffer (find-file-noselect (f-join tmpdir (f-base file))))
      (setq parsed (libxml-parse-xml-region (point-min) (point-max))))
    (kill-buffer fb2-buffer)
    (f-delete tmpdir 't)
    parsed
    ))

(defun fb2-reader-read-fb2 (file)
  "Read book from .fb2 FILE."
  
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-xml-region (point-min) (point-max))))


(defun fb2-reader-read ()

  (interactive)
  (fb2-reader-init-cache)
  (let (book title filename bodies rendered)
    ;; (setq book (if (equal "zip" (f-ext (buffer-file-name)))
    ;; 	(fb2-reader-read-fb2-zip (buffer-file-name))
    ;;   (fb2-reader-read-fb2 (buffer-file-name))))

    (setq book (libxml-parse-xml-region (point-min) (point-max))
	  filename buffer-file-name)
    ;; (kill-buffer)
    (setq filename buffer-file-name)
    (setq title (fb2-reader--get-title book))
    (get-buffer-create title)
    (switch-to-buffer title)
    (setq buffer-file-name filename)
    ;; Parse fb2
;; (push "~/Src/Linux/_my/fb2-reader" load-path)
    (message "title is %s" title)
    (async-start
     `(lambda ()
	,(async-inject-variables "\\`\\(fb2-reader\\)-")
	,(async-inject-variables "book")
	(setq load-path (quote ,load-path))
	      ;; window-system (quote ,window-system))
	(require 'fb2-reader)
	(with-temp-buffer
	  (fb2-reader-render (quote ,book))
	  (prin1-to-string (buffer-substring (point-min) (point-max)))
	  )
	)
     (lambda (result)
       ;; (fb2-reader-cache-rendered (file rendered)
       (setq fb2-reader-rendered-tmp result)
       (with-current-buffer title
       ;; For some reason propertized string returned from async process
       ;; loses hash at it's beginning. 
	 (insert (read (concat "#" fb2-reader-rendered-tmp)))
	 (fb2-reader-restore-images))))
    ;; (fb2-reader-imenu-setup)
    ;; (fb2-reader-set-up-header-line)
    ))

(defvar fb2-reader-mode-map
  (let ((map (make-sparse-keymap)))
  (define-key map (kbd "n") 'fb2-reader-forward-chapter)
  (define-key map (kbd "]") 'fb2-reader-forward-chapter)
  (define-key map (kbd "p") 'fb2-reader-backward-chapter)
  (define-key map (kbd "[") 'fb2-reader-backward-chapter)
  map)
  )

 
(define-derived-mode fb2-reader-mode special-mode "FB2"
  "Major mode for reading FB2 books
\\{fb2-reader-mode-map}"

  (setq fb2-reader-file-name (buffer-file-name)
	buffer-read-only nil
	truncate-lines 1)
  (buffer-disable-undo)
  (set-visited-file-name nil t) ; disable autosaves and save questions
  (add-hook 'kill-buffer-hook 'fb2-reader-save-pos nil t)
  ;; (add-hook 'change-major-mode-hook 'fb2-reader-save-curr-buffer nil t)
  (add-hook 'kill-emacs-hook 'fb2-reader-save-all-pos)
  (fb2-reader-ensure-settingsdir)
  (erase-buffer)
  (let ((bufname (buffer-name))
	book)
    ;; (push "~/Src/Linux/_my/fb2-reader" load-path)
    (if (fb2-reader-cache-avail-p fb2-reader-file-name 't)
	(fb2-reader-restore-buffer)
      (setq book (if (equal "zip" (f-ext fb2-reader-file-name))
		     (fb2-reader-read-fb2-zip fb2-reader-file-name)
		   (fb2-reader-read-fb2 fb2-reader-file-name)))
      (async-start
       `(lambda ()
	  ,(async-inject-variables "\\`\\(fb2-reader\\)-")
	  ,(async-inject-variables "book")
	  (setq load-path (quote ,load-path))
	  (require 'fb2-reader)
	  (with-temp-buffer
	    (fb2-reader-render (quote ,book))
	    (prin1-to-string (buffer-substring (point-min) (point-max)))))
       (lambda (result)
	 (with-current-buffer bufname
	   ;; For some reason propertized string returned from async process
	   ;; loses hash at it's beginning.
	   (fb2-reader-add-to-cache fb2-reader-file-name
				    (read (concat "#" result)))
	   (fb2-reader-restore-buffer)))))))

(add-to-list 'auto-mode-alist '("\\.fb2\\(.zip\\|\\)$" . fb2-reader-mode))

(provide 'fb2-reader) 
