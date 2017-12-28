(require 'widget)
(require 'cl-lib)
(require 'wid-edit)
(require 'emacsql-sqlite)

(switch-to-buffer "*kanjidic*")
(buffer-disable-undo "*kanjidic*")

(defvar kanjidic-result-list)
(defvar kanjidic-db (emacsql-sqlite "~/workspace/KanjiDatabaseCopy.sqlite"))

(defgroup kanjidic-faces nil
  "todo"
  :group 'faces)

(defface good-match-face '((((class color)
                             (background dark))
                            (:background "lime green"))
                           (((class color)
                             (background light))
                            (:background "lime green"))
                           (t nil))
  "todo"
  :group 'kanjidic-faces)

(defface partial-match-face '((((class color)
                                (background dark))
                               (:background "light gray"))
                              (((class color)
                                (background light))
                               (:background "light gray"))
                              (t nil))
  "todo"
  :group 'kanjidic-faces)


(define-widget 'display-text 'string "todo"
  :format "%v\n"
  :tag "display-text"
  :indent 10
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'definition 'string "todo"
  :format "%v\n"
  :tag "def"
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'imm-str 'string "todo"
  :format "%v"
  :tag "imm-str"
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'search-result-list 'editable-list "todo"
  :format "%v\n"
  :entry-format "%v")

(define-widget 'definition-list 'editable-list "todo"
  :args (list 'definition)
  :format "%v\n"
  :entry-format "%n %v"
  :value-create 'definition-list-value-create)

(defun definition-list-value-create (widget)
  (let* ((value (widget-get widget :value))
	 (type (nth 0 (widget-get widget :args)))
         (count 0)
	 children)
    (widget-put widget :value-pos (point-marker))
    (set-marker-insertion-type (widget-get widget :value-pos) t)
    (while value
      (setq count (+ 1 count))
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq children (cons (definition-list-entry-create
				  widget
				  (if (widget-get type :inline)
				      (car answer)
				    (car (car answer)))
				  t
                                  count)
				 children)
		  value (cdr answer))
	  (setq value nil))))
    (widget-put widget :children (nreverse children))))


(defun definition-list-entry-create (widget value conv idx)
  (let ((type (nth 0 (widget-get widget :args)))
	child delete insert)
    (widget-specify-insert
     (save-excursion
       (and (widget-get widget :indent)
	    (insert-char ?\s (widget-get widget :indent)))
       (insert (widget-get widget :entry-format)))
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-char -2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?i)
		(setq insert (apply 'widget-create-child-and-convert
				    widget 'insert-button
				    (widget-get widget :insert-button-args))))
	       ((eq escape ?d)
		(setq delete (apply 'widget-create-child-and-convert
				    widget 'delete-button
				    (widget-get widget :delete-button-args))))
	       ((eq escape ?v)
		(if conv
		    (setq child (widget-create-child-value
				 widget type value))
		  (setq child (widget-create-child-value
			       widget type (widget-default-get type)))))
               ((eq escape ?n)
                (insert (format "%d. " idx)))
	       (t
		(error "Unknown escape `%c'" escape)))))
     (let ((buttons (widget-get widget :buttons)))
       (if insert (push insert buttons))
       (if delete (push delete buttons))
       (widget-put widget :buttons buttons))
     (let ((entry-from (point-min-marker))
	   (entry-to (point-max-marker)))
       (set-marker-insertion-type entry-from t)
       (set-marker-insertion-type entry-to nil)
       (widget-put child :entry-from entry-from)
       (widget-put child :entry-to entry-to)))
    (if insert (widget-put insert :widget child))
    (if delete (widget-put delete :widget child))
    child))

(define-widget 'search-result 'group "todo"
  ;     (list display text / definitions / match-quality / kana)
  :args (list 'display-text 'definition-list 'symbol 'string)
  :value-create 'search-result-value-create)

(defmacro consume-widget-group-element (widget args value store)
  `(setq arg (car ,args)
         ,(intern (concat (symbol-name store) "-type")) arg
         ,args (cdr ,args)
         answer (widget-match-inline arg ,value)
         ,store (car (car answer))
         ,value (cdr answer)))

(defun search-result-value-create (widget)
  (let ((args (widget-get widget :args))
        (from (point))
	(value (widget-get widget :value))
	children)
      (consume-widget-group-element widget args value display-text)
      (consume-widget-group-element widget args value definition-list)
      (consume-widget-group-element widget args value match-symbol)
      (push (widget-create-child-value widget display-text-type display-text) children)
      (push (widget-create-child-value widget definition-list-type definition-list) children)
      (widget-put widget :children (nreverse children))
      (let ((overlay (make-overlay from (point) nil t nil)))
        (overlay-put overlay 'face (search-result-face-for-match match-symbol)))))

(defun search-result-face-for-match (sym)
  (cond ((eq sym 'g) 'good-match-face)
        ((eq sym 'p) 'partial-match-face)))

(defun kanjidic-ui-setup ()
  (kill-all-local-variables)
  (make-local-variable 'kanjidic-searchbar)
  (let ((inhibit-read-only t))
         (erase-buffer))
  (remove-overlays)
  (widget-create 'editable-field
                 :size 30
                 :format "Query: %v %[Search%]"  
                 :action 'kanjidic-handle-search)
  (widget-insert (concat "\n" (make-string 50 ?-) "\n"))
  (setq kanjidic-result-list (widget-create 'search-result-list
                                            :entry-format "%v"
                                            'search-result))
  (use-local-map widget-keymap)
  (widget-setup))

(defun kanjidic-handle-search (a b)
  (let ((txt (widget-value a)))
    (widget-value-set kanjidic-result-list (kanjidic-search txt)))
  (widget-setup))

;; (defun kanjidic-search (query)
;;   (list (list query (list (concat query "1") (concat query "2")) 'g)
;;         (list query (list (concat query "1") (concat query "2")) 'p)))

(defvar exact-kana-match [:select [VocabSet:ID
                                   VocabSet:KanjiWriting
                                   VocabSet:KanaWriting
                                   VocabMeaningSet:Meaning]
                          :from VocabSet
                          :join VocabEntityVocabMeaning
                          :on (= VocabSet:ID VocabEntityVocabMeaning:VocabEntity_ID)
                          :join VocabMeaningSet
                          :on (= VocabMeaningSet:ID VocabEntityVocabMeaning:Meanings_ID)
                          :where (like VocabSet:KanaWriting $R0)
                          :limit 1000])

(defun kanjidic-search (query)
  (let* ((templated-query (kanjidic-templating exact-kana-match 'vconcat query))
         (results (emacsql kanjidic-db templated-query))
         (by-id (-group-by 'car results)))
    (-map 'collect-search-result by-id)))

(defun collect-search-result (id-group)
  (let* ((id (car id-group))
         (group (cdr id-group))
         (kanji (cadar group))
         (kana (caddar group))
         (display-text (or kanji kana))
         (definitions (-map 'cadddr group)))
    (list display-text definitions 'g kana)))

(cl-defun kanjidic-templating (query typef &rest strings)
  (funcall typef (-map (lambda (token)
                   (cond ((symbolp token)
                          (let* ((name (symbol-name token))
                                 (match (string-match-p "^\\$[R][0-9]+$" name)))
                            (if match
                                (let ((type (substring name 1 2))
                                      (idx (string-to-int (substring name 2))))
                                  (cond ((equal type "R")
                                         (eval (list 'quote (nth idx (car strings)))))
                                        (t (error "Definitely should not happen"))))
                              token)))
                         ((stringp token) token)
                         ((vectorp token) (kanjidic-templating token 'vconcat strings))
                         ((sequencep token) (kanjidic-templating token 'identity strings))
                         (t token)))
                 query)))

(kanjidic-ui-setup)
