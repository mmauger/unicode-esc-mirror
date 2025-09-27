;;; unicode-escape.el --- font-lock Unicode \N and \U escapes   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael R. Mauger

;; Author: Michael R. Mauger <michael@mauger.com>
;; Maintainer: Michael R. Mauger <michael@mauger.com>
;; Keywords: lisp, faces, tools
;; Package-Type: simple
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0

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

(defvar ue/font-lock-keywords
  `( ;; \N{name} or \N{U+xx..xx}
     ( ,(rx ue/-N-escape) (0 (ue/prettify)) )
     ;; \Uxxxx or \Uxxxxxxxx
     ( ,(rx ue/-U-escape) (0 (ue/prettify)) ))
  "Define `font-lock' keywords to match Lisp \\N and \\U literals.")

(defvar-keymap ue/mode-map
  :doc "Mode map used for `unicode-escape-mode'."
  :parent nil
  "C-x 8 %"  #'ue/update-escapes
  "C-x 8 @"  #'ue/update-escape-at-point)

(defgroup unicode-escape nil
  "Display and update Unicode character references."
  :group 'lisp
  :prefix "unicode-escape-")

;;;###autoload
(define-minor-mode ue/mode
  "Display Unicode characters and maintain literal escapes."
  :init-value nil
  :lighter nil
  :keymap ue/mode-map
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
          (message "`unicode-escape-mode' requires that `prettify-symbols-mode' is enabled")
          (prettify-symbols-mode +1))
        (font-lock-add-keywords nil ue/font-lock-keywords))
    (font-lock-remove-keywords nil ue/font-lock-keywords))
  (font-lock-update))

;;;###autoload
(defcustom ue/char-name-default-case #'capitalize
  "Default letter case for Unicode character names in `\\N{name}' escapes."
  :type '(choice (const :tag "Leave as is" identity)
                 (const :tag "Title Case"  capitalize)
                 (const :tag "lower case"  downcase)
                 (const :tag "UPPER CASE"  upcase)))

;;;###autoload
(defcustom ue/default-escape-style :\N
  "Default Unicode escape format."
  :type '(choice (const :tag "\\N{name}"       :\N)
                 (const :tag "\\Uxxxx"         :\U)
                 (const :tag "Literal Unicode" :literal)))

;;;###autoload
(defun ue/update-escape-at-point (&optional style)
  "Update Unicode expression at point with STYLE escape syntax.

When called from elisp, the optional STYLE is a value permitted in
`ue/default-escape-style'.

When called interactively, STYLE is the prefix argument.  If omitted or
nil, it defaults to 1.  The prefix argument is translated into a escape
style keyword as follows:
 + None: The value of `unicode-escape-default-escape-style'
 + \\[universal-argument] (4): `:literal'
 + \\[universal-argument] \\[universal-argument] (16): `:\\N'
 + \\[universal-argument] \\[universal-argument] \\[universal-argument] (64): `:\\U'"

  (interactive "p")
  (setq style (ue/-style-argument style))
  (if (looking-at (rx ue/-char-re))
      (let* ((old-esc (match-string-no-properties 0))
             (ucs     (ue/-get-char))
             (new-esc (ue/-replacement-string style ucs)))
        (if (and ucs new-esc
                 (not (string= new-esc old-esc)))
            (replace-match new-esc t t)
          (goto-char (match-end 0))))
    (forward-char 1)))

;;;###autoload
(defun ue/update-escapes (&optional style)
  "Update each Unicode character or escape with STYLE escape syntax.

When called from elisp, the optional STYLE is a value permitted in
`ue/default-escape-style'.

When called interactively, STYLE is the prefix argument.  If omitted or
nil, it defaults to 1.  The prefix argument is translated into a escape
style keyword as follows:
 + None: The value of `unicode-escape-default-escape-style'
 + \\[universal-argument] (4): `:literal'
 + \\[universal-argument] \\[universal-argument] (16): `:\\N'
 + \\[universal-argument] \\[universal-argument] \\[universal-argument] (64): `:\\U'"

  (interactive "p")
  (setq style (ue/-style-argument style))
  (let (ask-all do-quit)
    (while (and (not do-quit)
                (re-search-forward (rx ue/-char-re) nil t))
      (let* ((old-esc (match-string-no-properties 0))
             (ucs     (ue/-get-char))
             (new-esc (ue/-replacement-string style old-esc))
             do-replace)
        (if (and ucs new-esc)
            (progn
              (pcase (or ask-all
                         (read-key (format "Replace `%s' with `%s'? %s "
					   old-esc new-esc "(y, n, ., !, or q)")))
                ((or ?y ?Y)  (setq do-replace t))
                ((or ?n ?N)  (setq do-replace nil))
                (?.          (setq do-replace t
                                   do-quit    t))
                (?!          (setq do-replace t
                                   ask-all    ?!))
                ((or ?q ?Q ?\C-g)
                             (setq do-replace nil
                                   do-quit    t))
                (_           (beep)))
              (when do-replace
                (replace-match new-esc t t)))
          (goto-char (match-end 0)))))))

(defun ue/-style-argument (style)
  "Translate style argument into supported STYLE keywords."
  (cond
   ((not style)
    ue/default-escape-style)
   ((and (keywordp style)
         (memq style '(:\N :\U :literal)))
    style)
   ((numberp style)
    (assoc-default style
                   '((1  . ue/default-escape-style)
                     (4  . :literal)
                     (16 . :\N)
                     (64 . :\U))
                   #'=
                   ue/default-escape-style))
   (t
    (user-error "Unrecognized style specification %S" style))))

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
      (:\N
       (if-let* ((name (char-to-name ucs)))
           (concat "\\N{" (funcall ue/char-name-default-case name) "}")
         (concat "\\N{U+"
                 (format (concat "%" format-char) ucs)
                 "}")))

      (:\U
           (format (concat "\\" style-char "%"
                           (if (> ucs #xffff)
                               "08"
                             "04")
                           format-char)
                   ucs))

      (:literal
       (string ucs)))))

(defun ue/prettify ()
  "Font lock Unicode escape with the Unicode character."
  (when-let* (ue/mode
              (ucs      (ue/-get-char))
              (old-esc  (match-string-no-properties 0))
	      (alist    `((,old-esc . ,ucs))))
    (let ((prettify-symbols-compose-predicate
	   (lambda (_start _end _match) t)))
      (prettify-symbols--compose-symbol alist))))

(provide 'unicode-escape)
;;; unicode-escape.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ue/" . "unicode-escape-"))
;; End:
