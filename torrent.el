(require 'cl)
(require 'bencode)

(defun torrent-info-hash (file)
  "Compute the info_hash for a torrent."
  (sha1 (bencode (cdr (assoc "info" (bdecode-file file))))))

(defun file-size (file)
  "Return the size of a file."
  (nth 7 (file-attributes file)))

(defun torrent-compute-pieces (file piece-length)
  "Compute the piece hashes for a file."
  (let ((length (file-size file)))
    (mapconcat 'identity
               (loop for i from 0 below length by piece-length collect
                     (with-temp-buffer
                       (insert-file-contents-literally
                        file nil i (min length (+ i piece-length)))
                       (sha1 (buffer-string) nil nil t))) "")))

(defun* torrent-create-info (file &key (piece-length (ash 1 18)) (name file))
  "Create an info section for a single-file torrent."
  `(("length"       . ,(file-size file))
    ("name"         . ,name)
    ("piece length" . ,piece-length)
    ("pieces"       . ,(torrent-compute-pieces file piece-length))))
