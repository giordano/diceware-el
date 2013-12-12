;;; diceware.el --- Emacs Lisp implementation of Diceware

;; Copyright (c) 2013  Mosè Giordano

;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Homepage: https://github.com/giordano/diceware-el
;; Keywords: diceware passphrase emacs

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; diceware.el is an Emacs Lisp implementation of Diceware.  Save the
;; file in a `load-path' directory and add
;;   (autoload 'diceware "diceware" nil t)
;; to your init file.  Then, you will be able to insert a diceware
;; passphrase at point with `M-x diceware RET'.  With a numeric prefix
;; argument, that number of passphrases will be inserted.

;;; Code:

(defun diceware (wordlist number &optional arg)
  "Insert a diceware passphrase at point.
Use WORDLIST file to create the passphrase made up of NUMBER words.

If optional ARG argument is non nil, insert that number of
passphrases.  When the function is called interactively, ARG is
given by the numeric prefix argument."
  (interactive "*fWord list file: \nnNumber of words: \np")
  ;; By default insert just one passphrase.
  (or arg (setq arg 1))
  (mapc
   (lambda (k)
     (apply 'insert
	    (with-temp-buffer
	      (insert-file-contents wordlist)
	      (save-match-data
		;; Repeat `number' times.
		(mapcar
		 (lambda (n)
		   (save-excursion
		     ;; Search for the next word.
		     (re-search-forward
		      (concat "^"
			      ;; Roll 5 dice.
			      (apply 'concat (mapcar
					      (lambda (d)
						(number-to-string (1+ (random 5))))
					      (number-sequence 1 5)))
			      "[ \t]*\\(.*\\)$") nil t))
		   ;; Print the new word, followed by a space.
		   (concat (match-string-no-properties 1) (and (< n number) " ")))
		 (number-sequence 1 number)))))
     ;; Go to a new line to insert a new passphrase, if needed.
     (and (> arg 1) (< k arg) (newline)))
   (number-sequence 1 arg)))

(provide 'diceware)

;;; diceware.el ends here
