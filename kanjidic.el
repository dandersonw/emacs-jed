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
  :entry-format "%n %v")

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

