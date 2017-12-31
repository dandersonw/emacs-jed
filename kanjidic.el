(require 'cl-lib)
(require 'eieio)
(require 'emacsql-sqlite)
(require 'dash-functional)
(require 'widget)
(require 'wid-edit)

(switch-to-buffer "*kanjidic*")
(buffer-disable-undo "*kanjidic*")

(defvar kanjidic-db (emacsql-sqlite "~/workspace/KanjiDatabaseCopy.sqlite"))

(defgroup kanjidic-faces nil
  "todo"
  :group 'faces)

(defface good-match-face '((((class color)
                             (background dark))
                            (:background "lime green"))
                           (((class color)
                             (background light))
                            (:background "light green"))
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

(defface badge-face '((((class color)
                                (background dark))
                               (:background "yellow"))
                              (((class color)
                                (background light))
                               (:background "yellow"
                                            :box t))
                              (t nil))
  "todo"
  :group 'kanjidic-faces)

(defface display-text-face '((t (:height 1.0)))
  "todo"
  :group 'kanjidic-faces)

(defface search-result-separator-face '((t (:height .1 :background "dark gray")))
  "todo"
  :group 'kanjidic-faces)

(defface search-result-separator-face-2 '((t (:height .1)))
  "todo"
  :group 'kanjidic-faces)

(defvar display-text-width 15)

(define-widget 'display-text 'string "todo"
  :format "%v%p"
  :tag "display-text"
  :indent display-text-width
  :format-handler 'pad-handler
  :value-create 'display-text-value-create)

(define-widget 'definition 'string "todo"
  :format "%v\n"
  :tag "def"
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'badge 'group "todo"
  :args (list 'imm-str 'symbol)
  :value-create 'badge-value-create)

(define-widget 'badge-list 'editable-list "todo"
  :args (list 'badge)
  :indent 1
  :format "%v\n"
  :entry-format "%v")

(define-widget 'imm-str 'string "todo"
  :format "%v"
  :tag "imm-str"
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'search-result-list 'editable-list "todo"
  :format "%v\n"
  :entry-format "%v")

(define-widget 'definition-list 'editable-list "todo"
  :args (list 'definition)
  :indent display-text-width
  :format "%v"
  :entry-format "%n %v"
  :value-create 'definition-list-value-create)

(define-widget 'search-result 'group "todo"
  ;     (list display text / definitions / match-quality / kana)
  :args (list 'display-text 'badge-list 'definition-list 'symbol 'string)
  :value-create 'search-result-value-create)

(defun display-text-value-create (widget)
  (let ((text (widget-get widget :value))
        (from (point)))
    (insert text)
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face 'display-text-face))))

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

(defun badge-value-create (widget)
  (let ((args (widget-get widget :args))
        (from (point))
	(value (widget-get widget :value))
        children text facesym)
    (consume-widget-group-element widget args value text)
    (consume-widget-group-element widget args value facesym)
    (push (widget-create-child-value widget text-type text) children)
    (widget-put widget :children (nreverse children))
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face facesym))))

(defmacro consume-widget-group-element (widget args value store)
  `(setq arg (car ,args)
         ,(intern (concat (symbol-name store) "-type")) arg
         ,args (cdr ,args)
         answer (widget-match-inline arg ,value)
         ,store (car (car answer))
         ,value (cdr answer)))

(defun search-result-separator-line (face)
  (let* ((from (point))
         (to (prog2 (insert "\n") (point)))
         (overlay (make-overlay from (point) nil t nil)))
    (overlay-put overlay 'priority 2)
    (overlay-put overlay 'face face)))

(defun search-result-value-create (widget)
  (let ((args (widget-get widget :args))
        (from (point))
	(value (widget-get widget :value))
	children facesym display-text badge-list definition-list)
    (consume-widget-group-element widget args value display-text)
    (consume-widget-group-element widget args value badge-list)
    (consume-widget-group-element widget args value definition-list)
    (consume-widget-group-element widget args value facesym)
    (search-result-separator-line 'search-result-separator-face-2)
    (push (widget-create-child-value widget display-text-type display-text) children)
    (push (widget-create-child-value widget badge-list-type badge-list) children)
    (push (widget-create-child-value widget definition-list-type definition-list) children)
    (widget-put widget :children (nreverse children))
    (search-result-separator-line 'search-result-separator-face-2)
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'face facesym))
        (search-result-separator-line 'search-result-separator-face)))

(defun pad-handler (widget c)
  (unless (equal c ?p)
    (error "Bad escape"))
  (let* ((txt (widget-get widget :value))
        (len (length txt))
        (padlen (- display-text-width len)))
    (when (> padlen 0)
      (insert (make-string padlen ? )))))

(defun kanjidic-ui-setup ()
  (kill-all-local-variables)
  (make-local-variable 'reading-field)
  (let ((inhibit-read-only t))
         (erase-buffer))
  (remove-overlays)
  (setq reading-field (widget-create 'editable-field
                                     :size 30
                                     :format "Reading: %v"  
                                     :action 'kanjidic-handle-search))
  (widget-insert (concat "\n" (make-string 50 ?-) "\n"))
  (setq kanjidic-result-list (widget-create 'search-result-list
                                            'search-result))
  (use-local-map widget-keymap)
  (widget-setup))

(defun kanjidic-handle-search (a b)
  (let* ((reading-query (widget-value reading-field))
        (search-results (kanjidic-search reading-query))
        (widget-data (-map 'sr-to-widget-data search-results)))
    (widget-value-set kanjidic-result-list widget-data))
  (widget-setup))

(defun kanjidic-templating (query &rest strings)
  (kanjidic-templating-h query 'vconcat strings))

(defun kanjidic-templating-h (query typef strings)
  (funcall typef (-map (lambda (token)
                   (cond ((symbolp token)
                          (let* ((name (symbol-name token))
                                 (match (string-match-p "^\\$[R][0-9]+$" name)))
                            (if match
                                (let ((type (substring name 1 2))
                                      (idx (string-to-int (substring name 2))))
                                  (cond ((equal type "R") (nth idx strings))
                                        (t (error "Definitely should not happen"))))
                              token)))
                         ((vectorp token) (kanjidic-templating-h token 'vconcat strings))
                         ((listp token) (kanjidic-templating-h token 'identity strings))
                         (t token)))
                 query)))

(defvar search-result-fields [VocabSet:ID
                              VocabSet:KanjiWriting
                              VocabSet:KanaWriting
                              VocabMeaningSet:Meaning
                              VocabSet:FrequencyRank
                              VocabSet:IsCommon])

(defvar exact-kana-match-cond `(like VocabSet:KanaWriting $R0))

(defvar kanjidic-query-template `[:select ,search-result-fields
                                  :from VocabSet
                                  :join VocabEntityVocabMeaning
                                  :on (= VocabSet:ID VocabEntityVocabMeaning:VocabEntity_ID)
                                  :join VocabMeaningSet
                                  :on (= VocabMeaningSet:ID VocabEntityVocabMeaning:Meanings_ID)
                                  :where $R0
                                  :limit 1000])
(defvar exact-kana-match-query (kanjidic-templating kanjidic-query-template exact-kana-match-cond))

(defclass raw-search-result ()
  ((vocab-id :initarg :vocab-id
             :type integer)
   (kanji-form :initarg :kanji-form
               :type (or null string))
   (reading :initarg :reading
            :type string
            :custom string)
   (definitions :initarg :definitions
     :type list
     :custom list)
   (frequency-rank :initarg :frequency-rank
                   :type (or null integer))
   (is-common :initarg :is-common
              :type boolean)))

(defclass scored-search-result (raw-search-result)
  ((score :initarg :score
         :type float)))

(defclass featurized-search-result (raw-search-result)
  ((ranking-features :initarg :ranking-features
                    :type list
                    :custom list)))

(defmethod sr-to-widget-data ((sr raw-search-result))
  (let ((display-text (or (oref sr :kanji-form) (oref sr :reading)))
        (badges (create-badges sr)))
    (list display-text badges (oref sr :definitions) (determine-sr-face sr) (oref sr :reading))))

(defun determine-sr-face (search-result)
  'good-match-face)

(defmethod featurized-sr-from-raw ((sr raw-search-result) features)
  (featurized-search-result :vocab-id (oref sr :vocab-id)
                            :kanji-form (oref sr :kanji-form)
                            :reading (oref sr :reading)
                            :definitions (oref sr :definitions)
                            :frequency-rank (oref sr :frequency-rank)
                            :is-common (oref sr :is-common)
                            :ranking-features features))

(defmethod scored-search-result-from-other ((sr raw-search-result) score)
  (scored-search-result :vocab-id (oref sr :vocab-id)
                        :kanji-form (oref sr :kanji-form)
                        :reading (oref sr :reading)
                        :definitions (oref sr :definitions)
                        :frequency-rank (oref sr :frequency-rank)
                        :is-common (oref sr :is-common)
                        :score score))

(defun kanjidic-search (reading-query)
  (let* ((reading-results (kanjidic-search-reading reading-query))
         (all-results (append reading-results))
         (featurized-results (kanjidic-combine-queries all-results))
         (ranked-results (kanjidic-rank-results featurized-results)))
    ranked-results))

(defun kanjidic-rank-results (featurized-results)
  (let ((scored (-map (lambda (sr)
                       (let ((score (kanjidic-score-result sr)))
                         (scored-search-result-from-other sr score)))
                       featurized-results)))
    (--sort (< (oref it :score) (oref other :score)) scored)))

(defvar kanjidic-feature-file "./features")

(defun kanjidic-features-from-file ()
  (let ((table (make-hash-table :test 'eq)))
    (with-temp-buffer
      (insert-file-contents kanjidic-feature-file)
      (goto-char 0)
      (while (prog2 (skip-chars-forward " \n") (not (eobp)))
        (let ((key (read (current-buffer)))
              (value (read (current-buffer))))
          (puthash key value table))))
    table))

(defvar kanjidic-feature-values (kanjidic-features-from-file))

(kanjidic-features-from-file)

(defun kanjidic-score-result (featurized-result)
  (-sum (-map (lambda (f)
                (pcase f
                  (`(,feature . ,feature-quantity)
                   (* feature-quantity (gethash feature kanjidic-feature-values)))
                  (feature (gethash feature kanjidic-feature-values))))
              (oref featurized-result :ranking-features))))

(defun kanjidic-combine-queries (all-results)
  (let ((by-id (-group-by (lambda (r) (oref r :vocab-id)) all-results)))
    (-map (lambda (group)
            (let ((example (cadr group))
                  (features (-distinct (-mapcat (lambda (r) (oref r :ranking-features)) (cdr group)))))
              (oset example :ranking-features features)
              example))
          by-id)))

(defun kanjidic-search-reading (reading-query)
  (let ((exact-results (kanjidic-search-single-query
                        (kanjidic-templating exact-kana-match-query reading-query))))
        (setq exact-results (-map (lambda (r)
                                    (featurized-sr-from-raw r '(exact-reading reading)))
                                  exact-results))
        (append exact-results)))

(defun kanjidic-search-single-query (query)
  (let* ((results (emacsql kanjidic-db query))
         (by-id (-group-by 'car results)))
    (-map 'collect-database-search-result by-id)))

;    (list display-text (create-badges (car group)) definitions 'g kana)))
(defun collect-database-search-result (id-group)
  (let* ((example (cadr id-group))
         (definitions (-map 'cadddr (cdr id-group))))
    (pcase example
      (`(,id ,kanji ,kana ,_ ,frequency ,is-common)
       (raw-search-result :vocab-id id
                          :kanji-form kanji
                          :reading kana
                          :definitions definitions
                          :frequency-rank frequency
                          :is-common (= 1 is-common))))))

(defun create-badges (result)
  (-filter 'identity (funcall (-juxt 'common-badge
                                     'frequency-badge)
                              result)))

(defun frequency-badge (result)
  (let ((frequency-rank (oref result :frequency-rank)))
    (and frequency-rank
         (list (format " W %dth most used " frequency-rank) 'badge-face))))

(defun common-badge (result)
  (and (oref result :is-common)
       (list " 本 Common " 'badge-face)))

(kanjidic-ui-setup)
