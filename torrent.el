(require 'bencode)

(defun torrent-info-hash (file)
  "Compute the info_hash for a torrent."
  (sha1 (bencode (cdr (assoc "info" (bdecode-file file))))))

(defun torrent-compute-pieces (file piece-length)
  "Compute the piece hashes for a file."
  (let ((length (nth 7 (file-attributes file))))
    (mapconcat 'identity
               (loop for i from 0 below length by piece-length collect
                     (with-temp-buffer
                       (insert-file-contents-literally
                        file nil i (min length (+ i piece-length)))
                       (sha1 (buffer-string) nil nil t))) "")))
