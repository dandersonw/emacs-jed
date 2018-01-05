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

(defface common-match-face '((((class color)
                             (background dark))
                            (:background "lime green"))
                           (((class color)
                             (background light))
                            (:background "light green"))
                           (t nil))
  "todo"
  :group 'kanjidic-faces)

(defface uncommon-match-face '((((class color)
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

(defvar display-text-height 1.5)

(defface display-text-face '((t (:height 1.5)))
  "todo"
  :group 'kanjidic-faces)

(defface search-result-separator-face '((t (:height .1 :background "dark gray")))
  "todo"
  :group 'kanjidic-faces)

(defface search-result-separator-face-2 '((t (:height .1)))
  "todo"
  :group 'kanjidic-faces)

(defvar display-text-width 15)

(define-widget 'display-text 'group "todo"
  :args (list 'string 'string)
  :format "%v\n%p"
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
  (let ((args (widget-get widget :args))
        (value (widget-get widget :value))
        from furigana-advice this-advice
        text text-type furigana furigana-type)
    (consume-widget-group-element widget args value text)
    (consume-widget-group-element widget args value furigana)
    (setq furigana-advice (typeset-furigana furigana))
    (setq from (point))
    (dotimes (i (length text))
      (setq this-advice (assq i furigana-advice))
      (when this-advice (insert (cdr this-advice)))
      (insert (aref text i))
      (when this-advice (insert (cdr this-advice))))
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face 'display-text-face))))

(defvar spc-per-kan 1.7)
(defvar top-bottom-ratio 2.0)

(defun typeset-furigana (furigana-spec)
  (defun top-kan-per-btm (s)
    (let ((bottom-width (float (- (cdar s) (caar s))))
          (top-width (length (cdr s))))
      (/ top-width bottom-width)))
  (let* ((parsed (parse-furigana-spec furigana-spec))
         (max-furigana-ratio (and parsed (-max (-map 'top-kan-per-btm parsed))))
         (furigana-height (and parsed (/ 1.5 top-bottom-ratio)))
         (from (point))
         (result-advice nil)
         (remaining parsed)
         (running-top-pos 0)
         (extra-bottom-width 0.0)
         curr curr-len curr-txt curr-bottom-pos is-long curr-advice
         curr-bottom-l curr-bottom-r curr-bottom-width )
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
      (setq is-long (> (top-kan-per-btm curr) top-bottom-ratio))
      (when is-long
        (let* ((extra-kw (- curr-len (* top-bottom-ratio curr-bottom-width)))
               (extra-bottom-spw (if (> extra-kw 2) 2 1)))
          (setq curr-advice (make-string extra-bottom-spw ? ))
          (setq extra-bottom-width (+ (/ extra-bottom-spw spc-per-kan)
                                      extra-bottom-width))))
      (setq target-bottom-center (+ extra-bottom-width
                                    (/ (+ curr-bottom-l curr-bottom-r) 2.0)))
      (setq target-top-l (- (* top-bottom-ratio target-bottom-center) (/ curr-len 2.0)))
      (when (< running-top-pos target-top-l)
        (let ((extra-top-spw (round (* spc-per-kan (- target-top-l running-top-pos)))))
          (insert (make-string extra-top-spw ? ))
          (setq running-top-pos (+ running-top-pos (/ extra-top-spw spc-per-kan)))))
      (insert curr-text)
      (setq running-top-pos (+ running-top-pos curr-len))
      (when curr-advice (setq extra-bottom-width (+ (/ (length curr-advice) spc-per-kan)
                                                    extra-bottom-width)))
      (when curr-advice (setq result-advice (cons (cons curr-bottom-l curr-advice) result-advice))))
    (insert "\n")
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (overlay-put overlay 'face (list :height furigana-height)))
    result-advice))

(defun parse-furigana-spec (furigana-spec)
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
        children text facesym text-type facesym-type)
    (consume-widget-group-element widget args value text)
    (consume-widget-group-element widget args value facesym)
    (push (widget-create-child-value widget text-type text) children)
    (widget-put widget :children (nreverse children))
    (let ((overlay (make-overlay from (point) nil t nil)))
      (overlay-put overlay 'priority 2)
      (and facesym (overlay-put overlay 'face facesym)))))

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
	children facesym display-text badge-list definition-list
        display-text-type badge-list-type facesym-type definition-list-type)
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
                              VocabSet:Furigana
                              VocabMeaningSet:Meaning
                              VocabSet:FrequencyRank
                              VocabSet:IsCommon
                              VocabSet:WikiRank])

(defvar exact-kana-match-cond `(like VocabSet:KanaWriting $R0))

(defvar kanjidic-query-template `[:select ,search-result-fields
                                  :from VocabSet
                                  :join VocabEntityVocabMeaning
                                  :on (= VocabSet:ID VocabEntityVocabMeaning:VocabEntity_ID)
                                  :join VocabMeaningSet
                                  :on (= VocabMeaningSet:ID VocabEntityVocabMeaning:Meanings_ID)
                                  :where $R0
                                  :limit 100])

(defvar exact-kana-match-query (kanjidic-templating kanjidic-query-template exact-kana-match-cond))

(defvar suboptimal-result-categories '("ok" "oK" "arch"))

(defun vocab-category-id-from-short (name)
  (let* ((query-temp `[:select ID :from VocabCategorySet :where (= ShortName $R0)])
         (templated (kanjidic-templating query-temp name)))
    (caar (emacsql kanjidic-db templated))))

(defvar suboptimal-result-category-ids nil)
(setq suboptimal-result-category-ids (-map 'vocab-category-id-from-short suboptimal-result-categories))

(defun get-vocab-category-ids (vocab-entity-id)
  (car (emacsql kanjidic-db
                [:select Categories_ID
                 :from VocabCategoryVocabEntity
                 :where (= VocabCategoryVocabEntity_VocabCategory_ID $s0)]
                vocab-entity-id)))

(defclass kanjidic-search-result ()
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

(defmethod sr-to-widget-data ((sr kanjidic-search-result))
  (let ((display-text (or (oref sr :kanji-form) (oref sr :reading)))
        (badges (create-badges sr)))
    (list (list display-text (oref sr :furigana))
          badges
          (oref sr :definitions)
          (determine-sr-face sr)
          (oref sr :reading))))

(defmethod sr-add-feature ((sr kanjidic-search-result) feature)
  (oset sr :ranking-features (cons feature (oref sr :ranking-features))))

(defun determine-sr-face (search-result)
  (cond ((oref search-result :is-common) 'common-match-face)
        ((-intersection suboptimal-result-category-ids (oref search-result :vocab-categories)) 'uncommon-match-face)
        (t nil)))

(defun kanjidic-search (reading-query)
  (let* ((reading-results (kanjidic-search-reading reading-query))
         (all-results (append reading-results))
         (combined-results (kanjidic-combine-queries all-results))
         (featurized-results (kanjidic-query-independent-featurization combined-results))
         (ranked-results (kanjidic-rank-results featurized-results)))
    ranked-results))

(defun kanjidic-rank-results (featurized-results)
  (-each featurized-results (lambda (r) (oset r :score (kanjidic-score-result r))))
  (--sort (> (oref it :score) (oref other :score)) featurized-results))

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

(setq kanjidic-feature-values (kanjidic-features-from-file))

(defun kanjidic-query-independent-featurization (results)
  (-each results (lambda (r)
                   (when (oref r :is-common) (sr-add-feature r 'is-common))
                   (when (let ((freq (oref r :frequency-rank))) (and freq (> freq 35000)))
                     (sr-add-feature r 'very-common))))
  results)

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
  (let* ((do-prefix-search t)
         (reading (if (string-match-p "[$＄]$" reading-query)
                      (prog2 (setq do-prefix-search nil)
                          (substring reading-query 0 -1))
                    reading-query))
         (exact-results (kanjidic-search-single-query
                         (kanjidic-templating exact-kana-match-query reading))))
    (-each exact-results (lambda (r) (oset r :ranking-features '(exact-reading reading))))
    (append exact-results
            (and do-prefix-search (kanjidic-search-reading-prefix reading)))))

(defun kanjidic-search-reading-prefix (reading-prefix)
  (let ((results (kanjidic-search-single-query
                  (kanjidic-templating exact-kana-match-query (concat reading-prefix "%"))))
        (q-len (length reading-prefix)))
    (-each results (lambda (r) (oset r :ranking-features
                            (list 'prefix-reading
                                  'reading
                                  (cons 'extra-reading-len (- (length (oref r :reading))
                                                              q-len))))))
    results))

(defun kanjidic-search-single-query (query)
  (let* ((results (emacsql kanjidic-db query))
         (by-id (-group-by 'car results)))
    (-map 'collect-database-search-result by-id)))

;    (list display-text (create-badges (car group)) definitions 'g kana)))
(defun collect-database-search-result (id-group)
  (let* ((example (cadr id-group))
         (categories (get-vocab-category-ids (car id-group)))
         (definitions (-map (lambda (g) (nth 4 g)) (cdr id-group))))
    (pcase example
      (`(,id ,kanji ,kana ,furigana ,_ ,frequency ,is-common ,wiki-rank)
       (kanjidic-search-result :vocab-id id
                               :kanji-form kanji
                               :reading kana
                               :furigana furigana
                               :definitions definitions
                               :frequency-rank frequency
                               :is-common (= 1 is-common)
                               :wiki-rank wiki-rank
                               :vocab-categories categories))
      (_ (error "bad database result")))))

(defun create-badges (result)
  (-filter 'identity (funcall (-juxt 'common-badge
                                     'wikirank-badge)
                              result)))

(defun wikirank-badge (result)
  (let ((wiki-rank (oref result :wiki-rank)))
    (and wiki-rank
         (list (format " W %dth most used " wiki-rank) 'badge-face))))

(defun common-badge (result)
  (let* ((freq (oref result :frequency-rank))
         (rank (cond ((not freq) nil)
                     ((> freq 35000) "Very Common")
                     ((> freq 6000)  "Common")
                     ((> freq 600)   "Unusual")
                     ((> freq 100)   "Rare")
                     (t nil))))
    (and rank (list (format " 本 %s " rank) 'badge-face))))

(kanjidic-ui-setup)
