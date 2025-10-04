;;; unicode-esc.el --- font-lock Unicode \N and \U escapes   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael R. Mauger

;; Author: Michael R. Mauger <michael@mauger.com>
;; Maintainer: Michael R. Mauger <michael@mauger.com>
;; Keywords: lisp, faces, tools
;; Package-Type: simple
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0.251003

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display \N{…} and \Ux…x Unicode literals in Lisp as the underlying
;; Unicode character.  This relies upon `prettify-symbols-mode' to
;; display and hide the Unicode representation.

;;; Code:

(require 'prog-mode)
(require 'font-lock)
(require 'rx)

(defgroup unicode-esc nil
  "Display and update Unicode character references."
  :group 'lisp
  :prefix "unicode-esc-")

;;; Regular expressions to match Unicode escape strings

;; \N{name} or \N{U+x…x}
(rx-define ue/-N-escape      (seq ?\\ ?N ?{ (group-n 1 (+ (not (in ?})))) ?}))
(rx-define ue/-N-U+xdigits   (seq string-start ?U ?+ (+ xdigit) string-end))
;; \Uxxxx or \Uxxxxxxxx
(rx-define ue/-U-escape      (seq ?\\ ?U (group-n 2 (= 4 xdigit) (? (= 4 xdigit)))))
(rx-define ue/-U-xdigits     (seq string-start (+ xdigit) string-end))
;; c (> 127)
(rx-define ue/-literal       (seq (group-n 3 (not ascii))))
(rx-define ue/-notascii      (seq string-start (not ascii) string-end))

(rx-define ue/-char-re       (seq (or ue/-N-escape
                                      ue/-U-escape
                                      ue/-literal)))

(defvar ue/font-lock-keywords)

;;; ============================================================
;;; Minor mode to fontify Unicode escape strings

(defvar-keymap ue/mode-map
  :doc "Mode map used for `unicode-esc-mode'."
  :parent nil
  "C-x 8 %"  #'ue/update-escapes
  "C-x 8 @"  #'ue/update-escape-at-point)

;;;###autoload
(define-minor-mode ue/mode
  "Display Unicode characters and maintain literal escapes."
  :init-value nil
  :lighter nil
  :keymap ue/mode-map
  :group 'unicode-esc
  ;;
  ;; Examples:
  ;;   \N{Duck} \N{DUCK} \N{duck}
  ;;   \N{U+1f986}
  ;;   \U0001f986
  ;;   🦆
  ;;
  (if ue/mode
      (progn
        (unless prettify-symbols-mode
          (message "`unicode-esc-mode' requires enabling `prettify-symbols-mode'; now enabled.")
          (prettify-symbols-mode +1))
        (font-lock-add-keywords nil ue/font-lock-keywords))
    (font-lock-remove-keywords nil ue/font-lock-keywords))
  (font-lock-update))

;;; ============================================================
;;; Font-lock Unicode escapes using `prettify-symbols-mode'

(defvar ue/font-lock-keywords
  `( ;; \N{name} or \N{U+xx..xx}
     ( ,(rx ue/-N-escape) 0 (ue/prettify) )
     ;; \Uxxxx or \Uxxxxxxxx
     ( ,(rx ue/-U-escape) 0 (ue/prettify) ))
  "Define `font-lock' keywords to match Lisp \\N and \\U literals.")

(defun ue/prettify ()
  "Font lock Unicode escape with the Unicode character."
  (when-let* (ue/mode
              (esc  (match-string-no-properties 0))
              (ucs  (ue/-get-char)))
    ;; ;; For debugging
    ;; (add-text-properties (match-beginning 0) (match-end 0)
    ;;                      (list 'help-echo (format "\N{Eyes} unicode-esc: \`%s\' %c" esc ucs)))
    (let ((prettify-symbols--current-symbol-bounds nil)
          (prettify-symbols-compose-predicate
           (lambda (_start _end _match) t)))
           ;; (lambda (start end match)
           ;;   (and (string= match esc)
           ;;        (string= (buffer-substring-no-properties start end) match)))))
      (prettify-symbols--compose-symbol `((,esc . ,ucs))))))

;;; ============================================================
;;; Settings for default generation of Unicode escapes

;;;###autoload
(defcustom ue/char-name-default-case #'capitalize
  "Default letter case for Unicode character names in `\\N{name}' escapes."
  :type '(choice (const :tag "Leave as is" identity)
                 (const :tag "Title Case"  capitalize)
                 (const :tag "lower case"  downcase)
                 (const :tag "UPPER CASE"  upcase)))

;;;###autoload
(defcustom ue/default-escape-style :\\N
  "Default Unicode escape format."
  :type '(choice (const :tag "\\N{name}"       :\\N)
                 (const :tag "\\Uxxxx"         :\\U)
                 (const :tag "Literal Unicode" :literal)))

;;; ============================================================
;;; Update Unicode escape at point

;;;###autoload
(defun ue/update-escape-at-point (&optional style)
  "Update Unicode expression at point with STYLE escape syntax.

When called from elisp, the optional STYLE is a value permitted in
`ue/default-escape-style'.  If omitted, it will use the style defined in
`ue/default-escape-style'.

When called interactively, STYLE is the prefix argument.  If omitted it
 uses `ue/default-escape-style', but with a prefix argument it will
 prompt for the format."

  (interactive "p")
  (if (looking-at (rx ue/-char-re))
      (let* ((save-md    (match-data))
             (end-esc    (match-end 0))
             (old-esc    (match-string-no-properties 0))
             (old-style  (ue/-get-style))
             (ucs        (ue/-get-char))
             (new-style  (ue/-style-argument style old-esc old-style ucs)))
        (pcase new-style
          ((or `:\\N `:\\U `:literal)
            (let ((new-esc (ue/-replacement-string new-style ucs)))
              (set-match-data save-md)
              (if (string= old-esc new-esc)
                  (goto-char end-esc)
                (replace-match new-esc t t))))
          (`quit
           (goto-char end-esc))))
    (forward-char 1)))

(defun ue/-style-argument (style old-esc old-style ucs)
  "Calculate the STYLE for UCS or revert to OLD-ESC style (OLD-STYLE)."
  (cond
   ((or (not style)
        (and (numberp style)
             (= style 1)))
    ue/default-escape-style)
   ((and (keywordp style)
         (memq style '(:\\N :\\U :literal)))
    style)
   ((numberp style)
    (let (new-style)
      (while (not new-style)
        (setq new-style (ue/-ask-style old-esc old-style ucs)))
      new-style))))

(defun ue/-ask-style (old-esc old-style ucs)
  "Ask for style of replacement for OLD-ESC (in OLD-STYLE) which is UCS."
  (let* ((prompt   (concat "Replace \"%s\" with: ("
                           (ue/-format-style-prompt old-esc old-style ucs)
                           "; Q/C-g: Quit) "))
         answer)
    (while (not answer)
      (setq answer
            (pcase (read-key (format prompt old-esc))
              ((or `?n `?N)         :\\N)
              ((or `?u `?U)         :\\U)
              ((or `?l `?L)         :literal)
              (`?\s                 old-style)
              ((or `?q `?Q `?\C-g)  'quit)
              (_                    (ding) nil))))
    answer))

(defun ue/-format-style-prompt (old-esc old-style ucs)
  "Format prompt for styles for UCS that are different from OLD-ESC in OLD-STYLE."
  (string-join
   (mapcan
    (lambda (selector-and-style)
      (let ((new-esc (ue/-replacement-string (cadr selector-and-style) ucs)))
        (unless (string= new-esc old-esc)
          (list (format "%s: %s" (car selector-and-style)
                        (or (caddr selector-and-style) (concat "`" new-esc "'")))))))
    `(("N" :\\N)
      ("U" :\\U)
      ("L" :literal)
      ("SPC" ,old-style "Same style")))
   "; "))

;;; ============================================================
;;; Update all valid Unicode escape sequences in buffer

;;;###autoload
(defun ue/update-escapes (&optional style)
  "Update each Unicode character or escape with STYLE escape syntax.

When called from elisp, the optional STYLE is a value permitted in
`ue/default-escape-style'.  If omitted, it will use the style defined in
`ue/default-escape-style'.  This style will be used for all
replacements.

When called interactively, STYLE is the prefix argument.  If omitted it
 uses `ue/default-escape-style' for all replacements.  With the prefix
 argument, the user may choose the style for each replacement string."

  (interactive "p")
  (let (do-all do-quit do-replace)
    (save-excursion
      (while (and (not do-quit)
                  (re-search-forward (rx ue/-char-re) nil t))
        (let* ((save-md    (match-data))
               (end-esc    (match-end 0))
               (old-esc    (match-string-no-properties 0))
               (old-style  (ue/-get-style))
               (ucs        (ue/-get-char)))
          (when ucs
            (setq style (ue/-style-argument style old-esc old-style ucs))
            (unless (called-interactively-p 'interactive)
              (setq do-all 'all))
            (pcase (or do-all (ue/-query-update old-esc ucs style))
              (`t            (setq do-replace t))
              (`skip         (setq do-replace nil))
              (`one-and-quit (setq do-replace t
                                   do-quit t))
              (`all          (setq do-replace t
                                   do-all 'all))
              (`quit         (setq do-replace nil
                                   do-quit t)))
            (if do-replace
                (let ((new-esc (ue/-replacement-string style ucs)))
                  (set-match-data save-md)
                  (if (string= old-esc new-esc)
                      (goto-char end-esc)
                    (undo-boundary)
                    (replace-match new-esc t t)))
              (goto-char end-esc))))))))

(defun ue/-query-update (old-esc ucs style)
  "Should we update UCS as OLD-ESC with STYLE?

Key  Returns  Description
---  -------  ----------------------------------------------
Y/y  t        Do update
N/n  nil      Skip update
 .   one-and-quit
              Update this one and then quit
 !   all      Go ahead and update all
Q/q  quit     Update no more"
  (let* ((prompt    (ue/-format-update-prompt old-esc ucs style))
         (answer    nil))
    (while (not answer)
      (setq answer
            (pcase (read-key prompt)
              ((or `?y `?Y)         t)
              ((or `?n `?N)         'skip)
              (`?.                  'one-and-quit)
              (`?!                  'all)
              ((or `?q `?Q `?\C-g)  'quit)
              (_                    (ding) nil))))
    answer))

(defun ue/-format-update-prompt (old-esc ucs style)
  "Format query to update OLD-ESC with UCS in STYLE."
  (format "Replace \"%s\" with \"%s\": (y, n, ., !, q) "
          old-esc
          (ue/-replacement-string style ucs)))

(defun ue/-get-style ()
  "Parse the current match and return the escape style.

This function recognizes the following strings:
 + \\N{name}
 + \\N{U+x...x}
 + \\Uxxxx
 + \\Uxxxxxxxx
 + a literal Unicode (non-ASCII) character."
  (let* ((\\N-name (match-string-no-properties 1))
         (\\U-name (match-string-no-properties 2))
         (lit-name (match-string-no-properties 3)))
    (cond
     (\\N-name  :\\N)
     (\\U-name  :\\U)
     (lit-name  :literal))))

(defun ue/-get-char ()
  "Parse the current match and return the character code.

This function recognizes the following strings:
 + \\N{name}
 + \\N{U+x...x}
 + \\Uxxxx
 + \\Uxxxxxxxx
 + a literal Unicode (non-ASCII) character."

  (let* ((\\N-name (match-string-no-properties 1))
         (\\U-name (match-string-no-properties 2))
         (lit-name (match-string-no-properties 3)))
    (cond
     (\\N-name
      (if (string-match-p (rx ue/-N-U+xdigits) \\N-name)
          (string-to-number (substring \\N-name 1) 16)
        (char-from-name \\N-name t)))

     (\\U-name
      (when (string-match-p (rx ue/-U-xdigits) \\U-name)
        (string-to-number \\U-name 16)))

     (lit-name
      (when (string-match-p (rx ue/-notascii) lit-name)
        (aref lit-name 0))))))

(defun ue/-replacement-string (style ucs)
  "Calculate the replacement string based on the STYLE escape of UCS."
  (let* ((name-case-param (assq ue/char-name-default-case
                                '((identity   "u" "x")
                                  (capitalize "U" "x")
                                  (upcase     "U" "X")
                                  (downcase   "u" "x"))))
         (style-char      (nth 1 name-case-param))
         (format-char     (nth 2 name-case-param)))
    (pcase style
      (:\\N
       (if-let* ((name (char-to-name ucs)))
           (concat "\\N{" (funcall ue/char-name-default-case name) "}")
         (concat "\\N{U+"
                 (format (concat "%" format-char) ucs)
                 "}")))

      (:\\U
       (format (concat "\\" style-char "%"
                       (if (> ucs #xffff)
                           "08"
                         "04")
                       format-char)
               ucs))

      (:literal
       (string ucs)))))

(provide 'unicode-esc)
;;; unicode-esc.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ue/" . "unicode-esc-"))
;; time-stamp-pattern: "20/Version:[\s\t]*\\(?:[[:digit:]]+[.]\\)+%y%m%d\n"
;; End:
