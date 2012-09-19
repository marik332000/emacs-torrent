(require 'rdp)
(eval-when-compile (require 'cl))

(defun bdecode-buffer ()
  "Decode the bencoding in the current buffer."
  (flet ((rdp-skip-whitespace () t))
    (rdp-parse bencode-tokens bencode-funcs 'value)))

(defun bdecode-string (string)
  "Decode the bencoding in a string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert string)
    (goto-char (point-min))
    (bdecode-buffer)))

(defun bdecode-file (file)
  "Decode the bencoding in a file."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (bdecode-buffer)))

(defvar bencode-tokens
  '((end    . "e")
    (value  . [int string list dict])
    (int      "i" "-?[0-9]+" end)
    (string   "[0-9]+" ":" "")
    (list     "l" [end values])
    (values   value [end values])
    (dict     "d" [end entries])
    (entry    string value)
    (entries  entry [end entries]))
  "Grammar for bencode.")

(defvar bencode-funcs
  `((end     . ,(lambda (end) ()))
    (value   . ,#'identity)
    (int     . ,(lambda (match) (string-to-number (nth 1 match))))
    (string  . ,(lambda (match)
                  (let* ((len (string-to-number (car match)))
                         (string (buffer-substring (point) (+ (point) len))))
                    (forward-char (length string))
                    string)))
    (list    . ,#'cadr)
    (values  . ,(apply-partially #'apply #'cons))
    (dict    . ,#'cadr)
    (entry   . ,(apply-partially #'apply #'cons))
    (entries . ,(apply-partially #'apply #'cons)))
  "Functions to convert parsed bencoding into a nice s-exp.")

(defun improperp (list)
  "Return t if the list is improper."
  (cond ((null list) nil)
        ((not (listp (cdr list))) t)
        (t (improperp (cdr list)))))

(defun bencode (sexp)
  "Encode an s-expression into a bencoded string."
  (cond ((integerp sexp) (bencode--int sexp))
         ((stringp sexp) (bencode--string sexp))
         ((and (listp sexp) (listp (car sexp)) (improperp (car sexp)))
          (bencode--dict sexp))
         ((listp sexp) (bencode--list sexp))
         (t (signal 'wrong-type-argument sexp))))

(defun bencode--int (int)
  (if (integerp int)
      (concat "i" (number-to-string int) "e")
    (signal 'wrong-type-argument `(integerp ,int))))

(defun bencode--string (string)
  (concat (number-to-string (string-bytes string)) ":" string))

(defun bencode--list (list)
  (concat "l" (mapconcat #'bencode list "") "e"))

(defun bencode--dict (dict)
  (labels ((dict-sort (a b) (string< (car a) (car b)))
           (paircode (p) (concat (bencode--string (car p)) (bencode (cdr p)))))
    (let ((sdict (sort (copy-list dict) #'dict-sort)))
      (concat "d" (mapconcat #'paircode sdict "") "e"))))
