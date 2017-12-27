(require 'widget)
(require 'cl-lib)
(require 'wid-edit)
; (require 'emacsql-sqlite)

(switch-to-buffer "*kanjidic*")
(buffer-disable-undo "*kanjidic*")

(defvar kanjidic-result-list)

(define-widget 'btext 'string "todo"
  :format "%v\n"
  :tag "btext"
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
  ;; Insert all values
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
  :args (list 'btext 'definition-list)
  :value-create 'search-result-value-create)

(defun search-result-value-create (widget)
  (let ((args (widget-get widget :args))
	(value (widget-get widget :value))
	arg answer children)
      (setq arg (car args)
	    args (cdr args)
	    answer (widget-match-inline arg value)
            btext (car (car answer))
	    value (cdr answer))
      (and (eq (preceding-char) ?\n)
	   (widget-get widget :indent)
	   (insert-char ?\s (widget-get widget :indent)))
      (push (widget-create-child-value widget arg btext)
	    children)
;      (insert-char ?\t 2)
      (setq arg (car args)
	    args (cdr args)
	    answer (widget-match-inline arg value)
	    value (cdr answer))
      (push (widget-create-child-value widget arg (car (car answer)))
	    children)
      (widget-put widget :children (nreverse children))))

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

(defun kanjidic-search (query)
  (list (list query (list (concat query "1") (concat query "2")))));'(item :value query)))

(kanjidic-ui-setup)

