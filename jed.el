(require 'cl-lib)
(require 'eieio)
(require 'emacsql-sqlite)
(require 'dash-functional)
(require 'widget)
(require 'wid-edit)

(switch-to-buffer "*jed*")
(buffer-disable-undo "*jed*")

(defvar jed-db (emacsql-sqlite "~/workspace/KanjiDatabaseCopy.sqlite"))

(defgroup jed-faces nil
  "todo"
  :group 'faces)

(defface jed-badge-face '((((class color)
                        (background dark))
                       (:background "yellow"))
                      (((class color)
                        (background light))
                       (:background "yellow"
                                    :box t))
                      (t nil))
  "todo"
  :group 'jed-faces)

(defface jed-obsolete-badge-face '((t (:background "gray" :box t)))
  "todo"
  :group 'jed-faces)

(defvar jed-display-text-height 1.5)

(defface jed-display-text-face '((t (:height 1.5)))
  "todo"
  :group 'jed-faces)

(defface jed-sr-separator-face '((t (:height .1 :background "dark gray")))
  "todo"
  :group 'jed-faces)

(defface jed-sr-pad-face '((t (:height .1)))
  "todo"
  :group 'jed-faces)

(defface jed-field-face '((t (:box t :inherit 'widget-field-face)))
  "todo"
  :group 'jed-faces)

(defvar jed-display-text-width 15)

(define-widget 'jed-display-text 'group
  "Written form of a vocab item"
  :args (list 'string '(choice string symbol))
  :format "%v\n%p"
  :tag "display-text"
  :indent jed-display-text-width
  :format-handler 'jed-pad-handler
  :value-create 'jed-display-text-value-create)

(define-widget 'jed-meaning-categories 'choice
  "Categories a meaning (and subsequent meanings) belong to"
  :args (list 'symbol 'string)
  :format "%v "
  :value-create 'jed-meaning-categories-value-create)

(define-widget 'jed-definition 'list
  "Definitions for a vocabulary item"
  :args (list 'jed-meaning-categories 'jed-str)
  :format "%v\n"
  :tag "def"
  :value-create 'jed-definition-value-create)

(define-widget 'jed-badge 'group
  "Widget indicating the presence of some notable property"
  :args (list 'jed-str 'symbol)
  :value-create 'jed-badge-value-create)

(define-widget 'jed-badge-list 'editable-list
  "Badges aggregation"
  :args (list 'jed-badge)
  :indent 1
  :format "%v\n"
  :entry-format "%v")

(define-widget 'jed-str 'string "todo"
  :format "%v"
  :tag "jed-str"
  :value-create (lambda (w) (insert (widget-value w))))

(define-widget 'jed-sr-result-list 'editable-list
  "Highest level widget"
  :format "%v\n"
  :entry-format "%v")

(define-widget 'jed-deflist 'editable-list
  "Definition aggregation"
  :args (list 'jed-definition)
  :indent jed-display-text-width
  :format "%v"
  :entry-format "%n %v"
  :value-create 'jed-deflist-value-create)

(define-widget 'jed-search-result 'group "todo"
  ;     (list display text / definitions / match-quality / kana)
  :args (list 'jed-display-text 'jed-badge-list 'jed-deflist 'symbol 'string)
  :value-create 'jed-sr-value-create)

(defmacro jed-match-widget-element (widget args value store)
  `(let (answer arg)
     (setq arg (car ,args)
           ,(intern (concat (symbol-name store) "-type")) arg
           ,args (cdr ,args)
           answer (widget-match-inline arg ,value)
           ,store (car (car answer))
           ,value (cdr answer))))

(defun jed-definition-value-create (widget)
  (let ((args (widget-get widget :args))
        (value (widget-get widget :value))
        children
        def-categories def-categories-type
        text text-type)
    (jed-match-widget-element widget args value def-categories)
    (jed-match-widget-element widget args value text)
    (when def-categories (push (widget-create-child-value widget def-categories-type def-categories) children))
    (push (widget-create-child-value widget text-type text) children)
    (widget-put widget :children (nreverse children))))

(defun jed-meaning-categories-value-create (widget)
  (let ((text (widget-get widget :value))
        (from (point)))
    (when text
        (insert (format "[%s]" text))
      (let ((overlay (make-overlay from (point) nil t nil)))
        (overlay-put overlay 'priority 2)
        (overlay-put overlay 'face 'shadow)))))

(defun jed-display-text-value-create (widget)
  (let ((args (widget-get widget :args))
        (value (widget-get widget :value))
        from furigana-advice this-advice
        text text-type furigana furigana-type)
    (jed-match-widget-element widget args value text)
    (jed-match-widget-element widget args value furigana)
    (setq furigana-advice (jed-typeset-furigana furigana))
    (setq from (point))
    (dotimes (i (length text))
      (setq this-advice (assoc i furigana-advice))
      (when this-advice (insert (cdr this-advice)))
      (insert (aref text i)))
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face 'jed-display-text-face))))

(defvar spc-per-kan 1.7)
(defvar top-bottom-ratio 2.0)

(defun jed-probe-display-settings ()
  (if window-system
      (jed-graphical-display-settings)
    (jed-terminal-display-settings)))

(defun jed-default-font-width (c) 
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert c)
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))

(defun jed-graphical-display-settings ()
  (setq spc-per-kan (/ (float (jed-default-font-width "漢"))
                       (jed-default-font-width " ")))
  (setq top-bottom-ratio 2.0))

(defun jed-terminal-display-settings ()
  (setq spc-per-kan 2.0)
  (setq top-bottom-ratio 1.0))

(defun jed-typeset-furigana (furigana-spec)
  (defun top-kan-per-btm (s)
    (let ((bottom-width (float (- (cdar s) (caar s))))
          (top-width (length (cdr s))))
      (/ top-width bottom-width)))
  (let* ((parsed (jed-parse-furigana furigana-spec))
         (max-furigana-ratio (and parsed (-max (-map 'top-kan-per-btm parsed))))
         (furigana-height (/ jed-display-text-height top-bottom-ratio))
         (from (point))
         (result-advice nil)
         (remaining parsed)
         (running-top-pos 0)
         (extra-bottom-width 0.0)
         curr curr-len curr-text curr-bottom-pos is-long curr-advice
         curr-bottom-l curr-bottom-r curr-bottom-width
         target-bottom-center target-top-l)
    (while remaining
      (setq curr-advice nil)
      (setq curr (car remaining))
      (setq remaining (cdr remaining))
      (setq curr-bottom-pos (car curr))
      (setq curr-bottom-r (cdr curr-bottom-pos))
      (setq curr-bottom-l (car curr-bottom-pos))
      (setq curr-bottom-width (- curr-bottom-r curr-bottom-l))
      (setq curr-text (cdr curr))
      (setq curr-len (length curr-text))
      ;; (setq is-long (> (top-kan-per-btm curr) top-bottom-ratio))
      ;; (when is-long
      ;;   (let* ((extra-kw (- curr-len (* top-bottom-ratio curr-bottom-width)))
      ;;          (extra-bottom-spw (ceiling (/ (* extra-kw spc-per-kan) top-bottom-ratio 2)))
      ;;          (pad-str (make-string extra-bottom-spw ? )))
      ;;     (setq curr-advice (cons pad-str pad-str))
      ;;     (setq extra-bottom-width (+ (/ extra-bottom-spw spc-per-kan)
      ;;                                 extra-bottom-width))))
      (setq target-bottom-center (+ extra-bottom-width
                                    (/ (+ curr-bottom-l curr-bottom-r) 2.0)))
      (setq target-top-l (- (* top-bottom-ratio target-bottom-center) (/ curr-len 2.0)))
      (when (> running-top-pos target-top-l)
        (let* ((extra-bottom-spw (ceiling (* spc-per-kan
                                             (/ (- running-top-pos target-top-l)
                                                top-bottom-ratio))))
               (pad-str (make-string extra-bottom-spw ? )))
          (setq curr-advice (cons pad-str nil))
          (setq extra-bottom-width (+ (/ extra-bottom-spw spc-per-kan) extra-bottom-width))))
      (setq target-bottom-center (+ extra-bottom-width
                                    (/ (+ curr-bottom-l curr-bottom-r) 2.0)))
      (setq target-top-l (- (* top-bottom-ratio target-bottom-center) (/ curr-len 2.0)))
      (when (< running-top-pos target-top-l)
        (let ((extra-top-spw (round (* spc-per-kan (- target-top-l running-top-pos)))))
          (insert (make-string extra-top-spw ? ))
          (setq running-top-pos (+ running-top-pos (/ extra-top-spw spc-per-kan)))))
      (insert curr-text)
      (setq running-top-pos (+ running-top-pos curr-len))
      (when (and curr-advice (cdr curr-advice))
        (setq extra-bottom-width (+ (/ (length (cdr curr-advice)) spc-per-kan)
                                    extra-bottom-width)))
      (when (and curr-advice (car curr-advice))
        (setq result-advice (cons (cons curr-bottom-l (car curr-advice)) result-advice)))
      (when (and curr-advice (cdr curr-advice))
        (setq result-advice (cons (cons curr-bottom-r (cdr curr-advice)) result-advice))))
    (insert "\n")
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face (list :height furigana-height)))
    result-advice))

(defun jed-parse-furigana (furigana-spec)
  (let* ((split (and furigana-spec (split-string furigana-spec ";"))))
    (-map (lambda (s)
            (let* ((splita (split-string s ":"))
                   (pos-spec (split-string (car splita) "-"))
                   (pos (if (= (length pos-spec) 1)
                            (let ((l (string-to-int (car pos-spec))))
                              (cons l (+ 1 l)))
                          (let ((l (string-to-int (car pos-spec)))
                                (r (+ 1 (string-to-int (cadr pos-spec)))))
                            (cons l r)))))
              (cons pos (cadr splita))))
          split)))


(defun jed-deflist-value-create (widget)
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
	    (setq children (cons (jed-deflist-entry-create
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

(defun jed-deflist-entry-create (widget value conv idx)
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

(defun jed-badge-value-create (widget)
  (let ((args (widget-get widget :args))
        (from (point))
	(value (widget-get widget :value))
        children text facesym text-type facesym-type)
    (jed-match-widget-element widget args value text)
    (jed-match-widget-element widget args value facesym)
    (push (widget-create-child-value widget text-type text) children)
    (widget-put widget :children (nreverse children))
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (and facesym (overlay-put overlay 'face facesym)))))

(defun jed-sr-separator-line ()
  (when window-system (jed-sr-separator-or-pad-line 'search-result-separator-face)))

(defun jed-sr-pad-line ()
  (jed-sr-separator-or-pad-line 'jed-sr-pad-face))

(defun jed-sr-separator-or-pad-line (face)
  (let* ((from (point))
         (to (prog2 (insert "\n") (point)))
         (overlay (make-overlay from (point) nil t nil)))
    (overlay-put overlay 'priority 2)
    (overlay-put overlay 'face face)))

(defun jed-sr-value-create (widget)
  (let ((args (widget-get widget :args))
        (from (point))
	(value (widget-get widget :value))
	children facesym display-text badge-list definition-list
        display-text-type badge-list-type facesym-type definition-list-type)
    (jed-match-widget-element widget args value display-text)
    (jed-match-widget-element widget args value badge-list)
    (jed-match-widget-element widget args value definition-list)
    (jed-match-widget-element widget args value facesym)
    (jed-sr-pad-line)
    (push (widget-create-child-value widget display-text-type display-text) children)
    (push (widget-create-child-value widget badge-list-type badge-list) children)
    (push (widget-create-child-value widget definition-list-type definition-list) children)
    (widget-put widget :children (nreverse children))
    (jed-sr-pad-line)
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'face facesym))
        (jed-sr-separator-line)))

(defun jed-pad-handler (widget c)
  (unless (equal c ?p)
    (error "Bad escape"))
  (let* ((txt (widget-get widget :value))
         (len (length txt))
         (padlen (- jed-display-text-width len)))
    (when (> padlen 0)
      (insert (make-string padlen ? )))))

(defvar jed-reading-field nil)
(defvar jed-meaning-field nil)
(defvar jed-result-list nil)

(defun jed-ui-setup ()
  (kill-all-local-variables)
  (make-local-variable 'jed-reading-field)
  (make-local-variable 'jed-meaning-field)
  (let ((inhibit-read-only t))
         (erase-buffer))
  (remove-overlays)
  (setq jed-reading-field (widget-create 'editable-field
                                     :size 30
                                     :format "Reading: %v"  
                                     :action 'jed-handle-search
                                     :value-face 'jed-field-face))
  (widget-insert "\n")
  (setq jed-meaning-field (widget-create 'editable-field
                                     :size 30
                                     :format "Meaning: %v"  
                                     :action 'jed-handle-search
                                     :value-face 'jed-field-face))
  (widget-insert (concat "\n" (make-string 50 ?-) "\n"))
  (setq jed-result-list (widget-create 'jed-sr-result-list
                                            'jed-search-result))
  (use-local-map widget-keymap)
  (widget-setup))

(defun jed-handle-search (a b)
  (let* ((reading-query (widget-value jed-reading-field))
         (meaning-query (widget-value jed-meaning-field))
         (search-results (jed-search reading-query meaning-query))
         (widget-data (-map 'jed-sr-to-widget-data search-results)))
    (widget-value-set jed-result-list widget-data))
  (widget-setup))

(defun jed-templating (query &rest strings)
  (jed-templating-h query 'vconcat strings))

(defun jed-templating-h (query typef strings)
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
                         ((vectorp token) (jed-templating-h token 'vconcat strings))
                         ((listp token) (jed-templating-h token 'identity strings))
                         (t token)))
                 query)))

(defvar jed-sr-fields [VocabSet:ID
                       VocabSet:KanjiWriting
                       VocabSet:KanaWriting
                       VocabSet:Furigana
                       VocabMeaningSet:ID
                       VocabMeaningSet:Meaning
                       VocabSet:FrequencyRank
                       VocabSet:IsCommon
                       VocabSet:WikiRank])

(defvar jed-kana-match-cond `(like VocabSet:KanaWriting $R0))

(defvar jed-query-template `[:select ,jed-sr-fields
                             :from VocabSet
                             :join VocabEntityVocabMeaning
                             :on (= VocabSet:ID VocabEntityVocabMeaning:VocabEntity_ID)
                             :join VocabMeaningSet
                             :on (= VocabMeaningSet:ID VocabEntityVocabMeaning:Meanings_ID)
                             :where $R0
                             :limit 100])

(defvar jed-kana-match-query (jed-templating jed-query-template jed-kana-match-cond))

;; Steps for relevant text search
;; 1) change the database migration script to create a DB with FTS5
;; 2) use it here

(defvar jed-meaning-match-cond `(releva))

(defvar jed-meaning-match-query (jed-templating jed-query-template meaning-match-cond))

(defvar jed-suboptimal-result-categories '("ok" "oK" "arch"))

(defun jed-vocab-category-id-from-short (name)
  (let* ((query-temp `[:select ID :from VocabCategorySet :where (= ShortName $R0)])
         (templated (jed-templating query-temp name)))
    (caar (emacsql jed-db templated))))

(defun jed-get-vocab-categories (vocab-entity-id)
  (car (emacsql jed-db
                [:select ShortName
                 :from VocabCategoryVocabEntity
                 :join VocabCategorySet
                 :on (= VocabCategorySet:ID Categories_ID)
                 :where (= VocabCategoryVocabEntity_VocabCategory_ID $s0)]
                vocab-entity-id)))

(defun jed-get-definition-categories (meaning-ids)
  (emacsql jed-db
           [:select [VocabMeaningVocabCategory_VocabCategory_ID ShortName]
            :from VocabMeaningVocabCategory
            :join VocabCategorySet
            :on (= VocabCategorySet:ID Categories_ID)
            :where (in VocabMeaningVocabCategory_VocabCategory_ID $v0)]
           (vconcat meaning-ids)))

(defclass jed-search-result ()
           ((vocab-id :initarg :vocab-id
                      :type integer)
            (kanji-form :initarg :kanji-form
                        :type (or null string))
            (reading :initarg :reading
                     :type string
                     :custom string)
            (furigana :initarg :furigana
                      :type (or null string))
            (definitions :initarg :definitions
              :type list
              :custom list)
            (frequency-rank :initarg :frequency-rank
                            :type (or null integer))
            (is-common :initarg :is-common
                       :type boolean)
            (wiki-rank :initarg :wiki-rank
                       :type (or null integer))
            (vocab-categories :initarg :vocab-categories
                              :type list)
            (ranking-features :initarg :ranking-features
                              :type list
                              :custom list)
            (score :initarg :score
                   :type float)))

(defmethod jed-sr-to-widget-data ((sr jed-search-result))
  (let ((display-text (or (oref sr :kanji-form) (oref sr :reading)))
        (badges (jed-create-badges sr)))
    (list (list display-text (oref sr :furigana))
          badges
          (oref sr :definitions)
          (jed-determine-sr-face sr)
          (oref sr :reading))))

(defmethod jed-sr-add-feature ((sr jed-search-result) feature)
  (oset sr :ranking-features (cons feature (oref sr :ranking-features))))

(defun jed-determine-sr-face (search-result)
  (cond ((oref search-result :is-common) 'highlight)
        ((-intersection jed-suboptimal-result-categories (oref search-result :vocab-categories)) 'shadow)
        (t nil)))

(defun jed-search (reading-query meaning-query)
  (let* ((do-reading-query (> (length reading-query) 0))
         (do-meaning-query (> (length meaning-query) 0))
         (reading-results (when do-reading-query (list (jed-search-reading reading-query))))
         (meaning-results (when do-meaning-query (list (jed-search-meaning meaning-query))))
         (all-results (-flatten-n 1 (list reading-results meaning-results)))
         (combined-results (jed-combine-queries all-results))
         (featurized-results (jed-query-independent-featurization combined-results))
         (ranked-results (jed-rank-results featurized-results)))
    ranked-results))

(defun jed-rank-results (featurized-results)
  (-each featurized-results (lambda (r) (oset r :score (jed-score-result r))))
  (--sort (> (oref it :score) (oref other :score)) featurized-results))

(defvar jed-feature-file "./features")

(defun jed-load-hash-table-from-file (path)
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char 0)
      (while (prog2 (skip-chars-forward " \n") (not (eobp)))
        (let ((key (read (current-buffer)))
              (value (read (current-buffer))))
          (puthash key value table))))
    table))

(defvar jed-feature-values nil) ; load at runtime

(defun jed-query-independent-featurization (results)
  (-each results (lambda (r)
                   (when (oref r :is-common) (jed-sr-add-feature r 'is-common))
                   (when (let ((freq (oref r :frequency-rank))) (and freq (> freq 35000)))
                     (jed-sr-add-feature r 'very-common))))
  results)

(defun jed-score-result (featurized-result)
  (-sum (-map (lambda (f)
                (pcase f
                  (`(,feature . ,feature-quantity)
                   (* feature-quantity (gethash feature jed-feature-values)))
                  (feature (gethash feature jed-feature-values))))
              (oref featurized-result :ranking-features))))

(defun jed-combine-queries (all-results)
  (let ((target-count (length all-results))
        (by-id (--group-by (oref it :vocab-id) (-flatten all-results))))
    (-keep (lambda (group)
             (when (= target-count (- (length group) 1))
               (let ((example (cadr group))
                     (features (-distinct (-mapcat (lambda (r) (oref r :ranking-features)) (cdr group)))))
                 (oset example :ranking-features features)
                 example)))
           by-id)))

(defun jed-search-reading (reading-query)
  (let* ((do-prefix-search t)
         (reading (if (string-match-p "[$＄]$" reading-query)
                      (prog2 (setq do-prefix-search nil)
                          (substring reading-query 0 -1))
                    reading-query))
         (exact-results (jed-search-single-query
                         (jed-templating jed-kana-match-query reading))))
    (-each exact-results (lambda (r) (oset r :ranking-features '(exact-reading reading))))
    (append exact-results
            (and do-prefix-search (jed-search-reading-prefix reading)))))

(defun jed-search-reading-prefix (reading-prefix)
  (let ((results (jed-search-single-query
                  (jed-templating jed-kana-match-query (concat reading-prefix "%"))))
        (q-len (length reading-prefix)))
    (-each results (lambda (r) (oset r :ranking-features
                            (list 'prefix-reading
                                  'reading
                                  (cons 'extra-reading-len (- (length (oref r :reading))
                                                              q-len))))))
    results))

(defun jed-search-meaning (meaning-query)
  nil)

(defun jed-search-single-query (query)
  (let* ((results (emacsql jed-db query))
         (by-id (-group-by 'car results)))
    (-map 'jed-collect-database-sr by-id)))

(defun jed-collect-database-sr (id-group)
  (let* ((example (cadr id-group))
         (categories (jed-get-vocab-categories (car id-group)))
         (definitions (jed-collect-definitions id-group)))
    (pcase example
      (`(,id ,kanji ,kana ,furigana ,_ ,__ ,frequency ,is-common ,wiki-rank)
       (jed-search-result :vocab-id id
                               :kanji-form kanji
                               :reading kana
                               :furigana (jed-resolve-furigana furigana kanji kana)
                               :definitions definitions
                               :frequency-rank frequency
                               :is-common (= 1 is-common)
                               :wiki-rank wiki-rank
                               :vocab-categories categories))
      (_ (error "bad database result")))))

(defvar meaning-category-alias-path "./category-names")
(defvar meaning-category-aliases nil) ; load at runtime

(defun jed-collect-definitions (id-group)
  (let* ((texts (-select-column 5 (cdr id-group)))
         (ids (-select-column 4 (cdr id-group)))
         (categories (jed-get-definition-categories ids)))
    (mapcar* (lambda (text id)
               (let* ((for-this-id (--map (and (= (car it) id) (cdr it)) categories))
                      (aliased (--map (gethash it meaning-category-aliases) (-flatten for-this-id)))
                      (cat-str (and aliased (mapconcat 'identity aliased " ; "))))
                 (list cat-str text)))
             texts ids)))

(defun jed-resolve-furigana (furigana kanji kana)
  (or furigana
      ;; Patch missing furigana for 3+ character 熟字訓
      (and kanji (let ((kanji-len (length kanji)))
                   (format "%d-%d:%s" 0 (- kanji-len 1) kana)))))

(defun jed-create-badges (result)
  (-filter 'identity (funcall (-juxt 'jed-common-badge
                                    'jed-wikirank-badge
                                    'jed-obsolete-reading-badge)
                             result)))

(defun jed-wikirank-badge (result)
  (let ((wiki-rank (oref result :wiki-rank)))
    (and wiki-rank
         (list (format " W %dth most used " wiki-rank) 'jed-badge-face))))

(defun jed-common-badge (result)
  (let* ((freq (oref result :frequency-rank))
         (rank (cond ((not freq) nil)
                     ((> freq 35000) "Very Common")
                     ((> freq 6000)  "Common")
                     ((> freq 600)   "Unusual")
                     ((> freq 100)   "Rare")
                     (t nil))))
    (and rank (list (format " 本 %s " rank) 'jed-badge-face))))

(defun jed-obsolete-reading-badge (result)
  (and (-contains? (oref result :vocab-categories) "ok")
       (list "Outdated reading" 'jed-obsolete-badge-face)))

(defun jed-load-data-files ()
  (or meaning-category-aliases
      (setq meaning-category-aliases (jed-load-hash-table-from-file meaning-category-alias-path)))
  (or jed-feature-values
      (setq jed-feature-values (jed-load-hash-table-from-file jed-feature-file))))

;; Run
(jed-load-data-files)
(jed-probe-display-settings)
(jed-ui-setup)
