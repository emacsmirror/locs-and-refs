;;; locs-and-refs.el --- Define locations and references for files and buffers  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Pierre-Henry FRÖHRING
;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Maintainer: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Homepage: https://github.com/phf-1/locs-and-refs
;; Package-Version: 0.17
;; Package-Requires: ((emacs "27.1") (pcre2el "1.12"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Locations and References for Emacs
;;
;; If there is a string like "[[ref:1234]]" in some buffer, then this minor mode will
;; turn it into a "Reference". A reference may be viewed as a button such that a
;; click will search for the matching "Location" in files' content, file names and
;; buffers. A matching location may be a string "[[id:1234]]" or a file named "1234".
;;
;; More precisely:
;;
;; - A location is defined as:
;;   - or :ID: <ID>
;;   - or [[id:<ID>]]
;;   - or [[id:<ID>][<name>]]
;;
;; - A reference is defined as:
;;   - or :REF: <ID>
;;   - or [[ref:<ID>]]
;;   - or [[ref:<ID>][<name>]]
;;
;; This package requires 'ripgrep' and 'fd' to be installed on your system for full
;; functionality.
;;
;; - Ripgrep: For fast text search.
;; - fd: For fast file search.
;;
;; The code structure lives at:
;; - https://github.com/phf-1/.emacs.d/blob/master/elisp/locs-and-refs.el.org
;;
;;; Code:

;; Dependencies
;; This section lists this package's dependencies.
;; - rxt (a.k.a. pcre2el) provides `rxt-elisp-to-pcre' that builds a PCRE expression
;;   from an ELisp RegEx. Useful to talk to Ripgrep from ELisp.
;; - org provides `org-link-set-parameters' that gives control over click behavior on
;;   links.


(require 'pcre2el)
(require 'org)

;; Configuration
;; This section lists the user's defined custom parameters.


(defgroup locs-and-refs nil
  "Customization options for locs-and-refs mode.
This mode provides functionality for handling locations and
references within text and programming buffers."
  :group 'convenience
  :prefix "locs-and-refs-")

(defcustom locs-and-refs-delay 1
  "Time in seconds after which locs-and-refs operations are applied.
This comes after buffer creation or modification."
  :type 'integer
  :group 'locs-and-refs)

(defcustom locs-and-refs-ripgrep-cmd "rg"
  "The name or path of the Ripgrep executable used for searching."
  :type 'string
  :group 'locs-and-refs)

(defcustom locs-and-refs-fd-cmd "fd"
  "The name or path of the fd executable used for searching."
  :type 'string
  :group 'locs-and-refs)

(defcustom locs-and-refs-root-dir (expand-file-name "~")
  "The root directory where Ripgrep searches for matches."
  :type 'string
  :group 'locs-and-refs)

(defcustom locs-and-refs-location-tag "id"
  "The tag used to identify locations."
  :type 'string
  :group 'locs-and-refs)

(defface locs-and-refs-location-face
  '((t (:foreground "blue"
        :underline t)))
  "A custom face with customizable colors."
  :group 'locs-and-refs
  :tag "Face for locations")

(defcustom locs-and-refs-reference-tag "ref"
  "The tag used to identify references."
  :type 'string
  :group 'locs-and-refs)

(defface locs-and-refs-reference-face
  '((t (:foreground "red"
        :underline t)))
  "A custom face with customizable colors."
  :group 'locs-and-refs
  :tag "Face for references")

;; LineFileMatch
;; An instance represents a match at a given line in some file.



;; mk
;; λ : Path Line → LineFileMatch


(defun locs-and-refs--line-file-match-mk (path line)
  "Create a LineFileMatch object with PATH and LINE.
PATH should be a valid file path, and LINE should be a
non-negative integer."
  (unless (file-exists-p path) (error "PATH does not exist"))
  (unless (and (integerp line) (<= 0 line)) (error "LINE is not a positive integer"))
  (list :line-file-match path line))

;; p
;; λ : Any → Boolean


(defun locs-and-refs--line-file-match-p (any)
  "Check if ANY is a LineFileMatch object."
  (eq (car-safe any) :line-file-match))

;; use
;; λ : (Path Line → C) → LineFileMatch → C


(defun locs-and-refs--line-file-match-use (func)
  "Apply FUNC to the path and line of a LineFileMatch object."
  (lambda (match)
    (unless (locs-and-refs--line-file-match-p match) (error "MATCH is not a LineFileMatch"))
    (apply func (cdr match))))

;; path
;; λ : LineFileMatch → Path


(defun locs-and-refs--line-file-match-path (match)
  "Extract the path from a MATCH object."
  (funcall (locs-and-refs--line-file-match-use (lambda (&rest params) (car params))) match))

;; line
;; λ : LineFileMatch → Line


(defun locs-and-refs--line-file-match-line (match)
  "Extract the line number from a MATCH object."
  (funcall (locs-and-refs--line-file-match-use (lambda (&rest params) (cadr params))) match))

;; LineBufferMatch
;; An instance represents a match at a given line in some buffer.



;; mk
;; λ : Buffer Line → LineBufferMatch


(defun locs-and-refs--line-buffer-match-mk (buffer line)
  "Create a LineBufferMatch object with BUFFER and LINE.
BUFFER should be a valid buffer, and LINE should be a
non-negative integer."
  (unless (bufferp buffer) (error "BUFFER does not exist"))
  (unless (and (integerp line) (<= 0 line)) (error "LINE is not a positive integer"))
  (list :line-buffer-match buffer line))

;; p
;; λ : Any → Boolean


(defun locs-and-refs--line-buffer-match-p (any)
  "Check if ANY is a LineBufferMatch object."
  (eq (car-safe any) :line-buffer-match))

;; use
;; λ : (Buffer Line → C) → LineBufferMatch → C


(defun locs-and-refs--line-buffer-match-use (func)
  "Apply FUNC to the buffer and line of a LineBufferMatch object."
  (lambda (match)
    (unless (locs-and-refs--line-buffer-match-p match) (error "MATCH is not a LineBufferMatch"))
    (apply func (cdr match))))

;; buffer
;; λ : LineBufferMatch → Buffer


(defun locs-and-refs--line-buffer-match-buffer (match)
  "Extract the buffer from a MATCH object."
  (funcall (locs-and-refs--line-buffer-match-use (lambda (&rest params) (car params))) match))

;; line
;; λ : LineBufferMatch → Line


(defun locs-and-refs--line-buffer-match-line (match)
  "Extract the line number from a MATCH object."
  (funcall (locs-and-refs--line-buffer-match-use (lambda (&rest params) (cadr params))) match))

;; FileMatch
;; An instance represents a file match.



;; mk
;; λ : Path → FileMatch


(defun locs-and-refs--file-match-mk (path)
  "Create a FileMatch object with PATH.
PATH should be a valid file path."
  (unless (file-exists-p path) (error "PATH does not exist"))
  (list :file-match path))

;; p
;; λ : Any → Boolean


(defun locs-and-refs--file-match-p (any)
  "Check if ANY is a FileMatch object."
  (eq (car-safe any) :file-match))

;; use
;; λ : (Path → C) → FileMatch → C


(defun locs-and-refs--file-match-use (func)
  "Apply FUNC to the path of a FileMatch object."
  (lambda (match)
    (unless (locs-and-refs--file-match-p match) (error "MATCH is not a FileMatch"))
    (apply func (cdr match))))

;; path
;; λ : FileMatch → Path


(defun locs-and-refs--file-match-path (match)
  "Extract the path from a MATCH object."
  (funcall (locs-and-refs--file-match-use (lambda (&rest params) (car params))) match))

;; Match
;; An instance represents either a FileMatch, LineFileMatch or a LineBufferMatch.



;; use
;; λ : (FileMatch → C) (LineFileMatch → C) (LineBufferMatch → C) → Match → C


(defun locs-and-refs--match-use (file-func line-file-func line-buffer-func)
  "Apply different functions to different types of Matches.
FILE-FUNC is applied to FileMatch, LINE-FILE-FUNC to
LineFileMatch, and LINE-BUFFER-FUNC to LineBufferMatch."
  (lambda (match)
    (cond
     ((locs-and-refs--file-match-p match) (funcall file-func match))
     ((locs-and-refs--line-file-match-p match) (funcall line-file-func match))
     ((locs-and-refs--line-buffer-match-p match) (funcall line-buffer-func match))
     (t (error "MATCH is not a FileMatch or a LineFileMatch or a LineBufferMatch")))))

;; name
;; λ : Match → String


(defun locs-and-refs--match-name (match)
  "Return the name of the file or buffer from a MATCH object."
  (funcall
   (locs-and-refs--match-use
    (lambda (file-match)
      (let ((name (file-name-nondirectory
                   (locs-and-refs--file-match-path
                    file-match)))
            (type "File"))
        (format "%s: %s" type name)))

    (lambda (line-file-match)
      (let ((name (file-name-nondirectory
                   (locs-and-refs--line-file-match-path
                    line-file-match)))
            (type "Line in file"))
        (format "%s: %s" type name)))

    (lambda (line-buffer-match)
      (let ((name (buffer-name
                   (locs-and-refs--line-buffer-match-buffer line-buffer-match)))
            (type "Line in buffer"))
        (format "%s: %s" type name))))

   match))

;; action
;; λ : Match → ∅ → ∅


(defun locs-and-refs--match-action (match)
  "Create an action based on the type of MATCH.
This action will open the file or switch to the buffer at the
specified location."
  (funcall
   (locs-and-refs--match-use
    (lambda (file-match)
      (lambda ()
        (let ((path (locs-and-refs--file-match-path file-match)))
          (find-file path)
          (recenter))))

    (lambda (line-file-match)
      (lambda ()
        (let ((path (locs-and-refs--line-file-match-path line-file-match))
              (line (locs-and-refs--line-file-match-line line-file-match)))
          (find-file path)
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter))))

    (lambda (line-buffer-match)
      (lambda ()
        (let ((buffer (locs-and-refs--line-buffer-match-buffer line-buffer-match))
              (line (locs-and-refs--line-buffer-match-line line-buffer-match)))
          (switch-to-buffer-other-window buffer)
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter)))))
   match))

;; Search
;; Given a RegEx in the form of an Rx expression,
;; search matching files/buffers/filenames.



;; files
;; λ : RegEx → List(Match)


(defun locs-and-refs--search-files (regex)
  "Search for REGEX in files under `locs-and-refs-root-dir' using Ripgrep.
Returns a list of LineFileMatch objects."
  (let* ((home-directory locs-and-refs-root-dir)
         (pattern (rxt-elisp-to-pcre (rx-to-string regex)))
         (command (format "%s -i --no-heading -n --color=never '%s' %s" locs-and-refs-ripgrep-cmd pattern home-directory))
         matches)
    (with-temp-buffer
      (call-process-shell-command command nil `(,(current-buffer) nil) nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (match (split-string line ":")))
          (push (locs-and-refs--line-file-match-mk (nth 0 match) (string-to-number (nth 1 match))) matches))
        (forward-line 1)))
    matches))

;; buffers
;; λ : RegEx → List(Match)


(defun locs-and-refs--line-number ()
  "Return the current line number in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos))))

(defun locs-and-refs--search-buffers (regex)
  "Search for REGEX in all buffers.
Returns a list of LineBufferMatch objects."
  (let ((case-fold-search t) matches)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (rx-to-string regex) nil t)
            (push (locs-and-refs--line-buffer-match-mk buffer (locs-and-refs--line-number)) matches)))))
    matches))

;; filenames
;; λ : RegEx → List(Match)


(defun locs-and-refs--search-filenames (regex)
  "Search for REGEX in filenames under `locs-and-refs-root-dir' using fd.
Returns a list of FileMatch objects."
  (let* ((home-directory locs-and-refs-root-dir)
         (pattern (rxt-elisp-to-pcre (rx-to-string regex)))
         (command (format "%s -a '%s' '%s'" locs-and-refs-fd-cmd pattern home-directory))
         matches)
    (with-temp-buffer
      (call-process-shell-command command nil `(,(current-buffer) nil) nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (push (locs-and-refs--file-match-mk line) matches))
        (forward-line 1)))
    matches))

;; RegEx
;; A few utilities.



;; content
;; λ : Tag Optional(id) → Rx


(defun locs-and-refs--regex-content (tag &optional id)
  "Generate a regex pattern for matching content with TAG and optional ID.
ID can be either a string or a regex pattern."
  (let* ((this-id (or id '(1+ (not (or "\n" "]")))))
         (property `(seq ":" (group ,tag) ":" (1+ space) (group ,this-id)))
         (org-link `(seq "[[" (group ,tag) ":" (group ,this-id) "]" (opt "[" (group (0+ (not "]"))) "]") "]")))
    `(or ,property ,org-link)))

;; Location
;; An instance defines a place that can be referenced.
;; A click on an instance shows all references to it.



;; mk
;; λ : String Buffer Start End Name → Location


(defun locs-and-refs--location-mk (id buffer start end name)
  "Create a Location object with ID, BUFFER, START, END and NAME.
ID is a string, BUFFER must be a buffer object, START and END are
integer positions."
  (unless (stringp id) (error "ID is not a string"))
  (unless (bufferp buffer) (error "BUFFER is not a buffer"))
  (unless (integerp start) (error "START is not an integer"))
  (unless (integerp end) (error "END is not an integer"))
  (let (button loc display-name)
    (setq display-name
          (apply #'propertize
                 `(,(substring-no-properties (or name (concat locs-and-refs-location-tag ":" id)))
                   face locs-and-refs-location-face)))
    (setq button
          (with-current-buffer buffer
            (let ((inhibit-modification-hooks t))
              (make-button start end
                           'action (lambda (_button) (locs-and-refs--location-click loc))
                           'lar t
                           'display display-name
                           'help-echo (format "Click to open %s" id)
                           'follow-link t))))
    (setq loc (list :location id buffer start end button name))
    loc))

;; p
;; λ : Any → Boolean


(defun locs-and-refs--location-p (loc)
  "Check if LOC is a Location object."
  (eq (car-safe loc) :location))

;; use
;; λ : (String → C) → Location → C


(defun locs-and-refs--location-use (func)
  "Apply FUNC to the ID of a Location object."
  (lambda (loc)
    (unless (locs-and-refs--location-p loc) (error "LOC is not a Location"))
    (apply func (cdr loc))))

;; id
;; λ : Location → String


(defun locs-and-refs--location-id (loc)
  "Extract the ID from a LOC object."
  (funcall (locs-and-refs--location-use (lambda (id &rest _args) id)) loc))

;; tag
;; λ : String


(defun locs-and-refs--location-tag ()
  "Return the tag used for identifying locations."
  locs-and-refs-location-tag)

;; content-regex
;; λ : Location → RegEx


(defun locs-and-refs--location-content-regex (loc)
  "Generate a regex for content of a Location with LOC's ID."
  (locs-and-refs--regex-content (locs-and-refs--reference-tag) (locs-and-refs--location-id loc)))

;; regex
;; λ : RegEx


(defun locs-and-refs--location-regex ()
  "Return the regex pattern for matching locations."
  (locs-and-refs--regex-content (locs-and-refs--location-tag)))

;; click
;; λ : Location → Buffer
;; λ loc :≡
;;   content-regex  :≡ content-regex(loc)
;;   file-matches   :≡ search-files(content-regex)
;;   buffer-matches :≡ search-buffers(content-regex)
;;   matches        :≡ file-matches + buffer-matches
;;   ui-matches(matches id(loc) tag(loc))


(defun locs-and-refs--location-click (loc)
  "Click on a location LOC shows matching references in a new buffer."
  (let* ((content-regex (locs-and-refs--location-content-regex loc))
         (file-matches (locs-and-refs--search-files content-regex))
         (buffer-matches (locs-and-refs--search-buffers content-regex))
         (matches (append file-matches buffer-matches))
         (id (locs-and-refs--location-id loc)))
    (locs-and-refs--ui-matches matches id locs-and-refs-location-tag)))

;; Reference
;; An instance define a reference to a Location.
;; A click on an instance shows all locations that it refers to.



;; mk
;; λ : Id Buffer Start End Name → Reference


(defun locs-and-refs--reference-mk (id buffer start end name)
  "Create a Reference object with ID, BUFFER, START, END and NAME.
ID is a string, BUFFER must be a buffer object, START and END are
integer positions."
  (unless (stringp id) (error "ID is not a string"))
  (unless (bufferp buffer) (error "BUFFER is not a buffer"))
  (unless (integerp start) (error "START is not an integer"))
  (unless (integerp end) (error "END is not an integer"))
  (let (button ref display-name)
    (setq display-name
          (apply #'propertize
                   `(,(substring-no-properties (or name (concat locs-and-refs-reference-tag ":" id)))
                     face locs-and-refs-reference-face)))
    (setq button
          (with-current-buffer buffer
            (let ((inhibit-modification-hooks t))
              (make-button start end
                         'action (lambda (_button) (locs-and-refs--reference-click ref))
                         'lar t
                         'display display-name
                         'help-echo (format "Click to open %s" id)
                         'follow-link t))))
    (setq ref (list :reference id buffer start end button name))
    ref))

;; p
;; λ : Any → Boolean


(defun locs-and-refs--reference-p (ref)
  "Check if REF is a Reference object."
  (eq (car-safe ref) :reference))

;; use
;; λ : (String → C) → Reference → C


(defun locs-and-refs--reference-use (func)
  "Apply FUNC to the ID of a Reference object."
  (lambda (ref)
    (unless (locs-and-refs--reference-p ref) (error "REF is not a Reference"))
    (apply func (cdr ref))))

;; id
;; λ : Reference → String


(defun locs-and-refs--reference-id (ref)
  "Extract the ID from a REF object."
  (funcall (locs-and-refs--reference-use (lambda (id &rest _args) id)) ref))

;; tag
;; λ : String


(defun locs-and-refs--reference-tag ()
  "Return the tag used for identifying references."
  locs-and-refs-reference-tag)

;; content-regex
;; λ : Reference → RegEx


(defun locs-and-refs--reference-content-regex (ref)
  "Generate a regex for content of a Reference with REF's ID."
  (locs-and-refs--regex-content (locs-and-refs--location-tag) (locs-and-refs--reference-id ref)))

;; regex
;; λ : Regex


(defun locs-and-refs--reference-regex ()
  "Return the regex pattern for matching references."
  (locs-and-refs--regex-content (locs-and-refs--reference-tag)))

;; filename-regex
;; λ : Reference → RegEx


(defun locs-and-refs--reference-filename-regex (ref)
  "Generate a regex for matching filenames with REF's ID."
  `(seq ,(locs-and-refs--reference-id ref)))

;; click
;; λ : Reference → Buffer
;; click ref :≡
;;   content-regex    :≡ content-regex(ref)
;;   file-matches     :≡ search-files(content-regex)
;;   buffer-matches   :≡ search-buffers(content-regex)
;;   filename-matches :≡ search-filenames(filename-regex)
;;   matches          :≡ file-matches + buffer-matches + filename-matches
;;   ui-matches(matches id(ref) tag(ref))


(defun locs-and-refs--reference-click (ref)
  "Click on a reference REF shows matching locations in a new buffer."
  (let* ((content-regex (locs-and-refs--reference-content-regex ref))
         (file-matches (locs-and-refs--search-files content-regex))
         (buffer-matches (locs-and-refs--search-buffers content-regex))
         (filename-matches (locs-and-refs--search-filenames (locs-and-refs--reference-filename-regex ref)))
         (matches (append file-matches buffer-matches filename-matches))
         (id (locs-and-refs--reference-id ref)))
    (locs-and-refs--ui-matches matches id locs-and-refs-reference-tag)))

;; UI



;; insert-button
;; λ : Name Action → Button


(defun locs-and-refs--ui-insert-button (name action tag)
  "Insert a clickable button with NAME and ACTION in the current buffer.
The face depends on TAG."
  (insert-button name
                 'action (lambda (_button) (funcall action))
                 'lar t
                 'face (cond
                        ((string= tag (locs-and-refs--location-tag))
                         'locs-and-refs-reference-face)
                        ((string= tag (locs-and-refs--reference-tag))
                         'locs-and-refs-location-face))
                 'help-echo (format "Click to open %s" name)
                 'follow-link t))

;; matches
;; λ : List(Match) Id Tag → Buffer


(defun locs-and-refs--ui-matches (matches id tag)
  "Display MATCHES for ID from TAG in a buffer.
Opens a new dedicated frame and switches to the buffer in that frame.
For each match, a button is inserted in the buffer.
A click on a button opens the associated file."
  (let* ((buffer (with-current-buffer (get-buffer-create "Search results")
                   (setq buffer-read-only nil)
                   (erase-buffer)
                   (insert
                    (cond
                     ((string= tag (locs-and-refs--location-tag))
                      (format "List of references to the location: %S\n\n" (substring-no-properties id)))
                     ((string= tag (locs-and-refs--reference-tag))
                      (format "List of locations with id: %S\n\n" (substring-no-properties id)))))
                   (setq buffer-read-only t)
                   (current-buffer)))
         (insert-button
          (lambda (match)
            (with-current-buffer buffer
              (locs-and-refs--ui-insert-button
               (locs-and-refs--match-name match)
               (locs-and-refs--match-action match)
               tag)
              (insert "\n")))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (let (line-buffer-matches line-file-matches file-matches)
        (setq file-matches
              (alist-get t (seq-group-by #'locs-and-refs--file-match-p matches)))

        (setq line-file-matches
              (alist-get t (seq-group-by #'locs-and-refs--line-file-match-p matches)))

        (setq line-buffer-matches
              (alist-get t (seq-group-by #'locs-and-refs--line-buffer-match-p matches)))

        (when line-buffer-matches
          (insert "* Line buffer matches\n\n")
          (mapc insert-button line-buffer-matches)
          (insert "\n"))

        (when line-file-matches
          (insert "* Line file matches\n\n")
          (mapc insert-button line-file-matches)
          (insert "\n"))

        (when file-matches
          (insert "* File matches\n\n")
          (mapc insert-button file-matches)
          (insert "\n")))
      (setq buffer-read-only t))
    (let* ((frame (make-frame `((name . "Search Results")
                                (dedicated . t))))
           (window (frame-root-window frame)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t))
    buffer))

;; Minor mode
;; Make sure that locations and references are activated in all buffers at all times
;; as long as they derive from `text-mode' or `prog-mode'.



(defvar locs-and-refs--timer nil
  "Record the last time the buffer has been modified.")
(put 'locs-and-refs--timer 'permanent-local t)

(defun locs-and-refs--check-ripgrep ()
  "Check if Ripgrep (rg) is installed and available."
  (unless (executable-find locs-and-refs-ripgrep-cmd)
    (user-error "Ripgrep (rg) is not installed. Please install it to use this package")))

(defun locs-and-refs--check-fd ()
  "Check if fd is installed and available."
  (unless (executable-find locs-and-refs-fd-cmd)
    (user-error "Fd (fd) is not installed. Please install it to use this package")))

(defun locs-and-refs--mutated (_a _b _c)
  "Handle buffer mutations for locs-and-refs mode."
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (when locs-and-refs--timer (cancel-timer locs-and-refs--timer))
      (setq-local locs-and-refs--timer
                  (run-with-idle-timer
                   locs-and-refs-delay
                   nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (locs-and-refs--activate-buffer buffer))))))))

(defun locs-and-refs--activate ()
  "Activate the main functionality of locs-and-refs mode."
  (locs-and-refs--check-ripgrep)
  (locs-and-refs--check-fd)
  (locs-and-refs--activate-buffers (buffer-list))
  (add-hook 'after-change-major-mode-hook #'locs-and-refs--activate-buffer)
  (add-hook 'after-change-functions #'locs-and-refs--mutated))

(defun locs-and-refs--deactivate ()
  "Deactivate the main functionality of locs-and-refs mode."
  (remove-hook 'after-change-major-mode-hook #'locs-and-refs--activate-buffer)
  (remove-hook 'after-change-functions #'locs-and-refs--mutated)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (remove-overlays nil nil 'lar t)
      (when locs-and-refs--timer
        (cancel-timer locs-and-refs--timer)
        (kill-local-variable 'locs-and-refs--timer)))))

(defun locs-and-refs--regex-groups ()
  "Extract groups from the last regex match."
  (let (groups)
    (dotimes (i (/ (length (match-data)) 2))
      (when (> i 0)
        (let ((group (match-string i)))
          (when group (push group groups)))))
    (nreverse groups)))

(defun locs-and-refs--activate-buffer (&optional buffer)
  "Activate locs-and-refs functionality for BUFFER or the current buffer."
  (let* ((this-buffer (or buffer (current-buffer)))
         (loc-regex (locs-and-refs--location-regex))
         (loc-tag (locs-and-refs--location-tag))
         (ref-tag (locs-and-refs--reference-tag))
         (ref-regex (locs-and-refs--reference-regex))
         (regex (rx-to-string `(or ,loc-regex ,ref-regex)))
         (case-fold-search t)
         groups tag id name)
    (with-current-buffer this-buffer
      (when (derived-mode-p 'text-mode 'prog-mode)
        (save-excursion
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (overlay-get ov 'lar) (delete-overlay ov)))
          (goto-char (point-min))
          (save-match-data
            (while (re-search-forward regex nil t)
              (setq groups (locs-and-refs--regex-groups))
              (setq tag (car groups))
              (setq id (seq-find #'identity (cdr groups)))
              (setq name (nth 2 groups))
              (cond
               ((string= (downcase tag) loc-tag)
                (locs-and-refs--location-mk
                 id
                 this-buffer
                 (match-beginning 0)
                 (match-end 0)
                 name))
               ((string= (downcase tag) ref-tag)
                (locs-and-refs--reference-mk
                 id
                 this-buffer
                 (match-beginning 0)
                 (match-end 0)
                 name))))))))))

(defun locs-and-refs--activate-buffers (buffers)
  "Activate locs-and-refs functionality for all buffers in BUFFERS."
  (mapcar #'locs-and-refs--activate-buffer buffers))

;;;###autoload
(define-minor-mode locs-and-refs-mode
  "Locations and References for Emacs.

If there is a string like \"[[ref:1234]]\" in some buffer, then
this minor mode will turn it into a \"Reference\". A reference
may be viewed as a button such that a click will search for the
matching \"Location\" in files' content, file names and
buffers. A matching location may be a string \"[[id:1234]]\" or a
file named \"1234\".

More precisely:

- A location is defined as:
  - or :ID: <UUID>
  - or [[id:<UUID>]]
  - or [[id:<UUID>][<name>]]

- A reference is defined as:
  - or :REF: <UUID>
  - or [[ref:<UUID>]]
  - or [[ref:<UUID>][<name>]]"
  :init-value nil
  :lighter " L&R"
  :keymap nil
  :group 'locs-and-refs
  :global t
  (if locs-and-refs-mode
      (locs-and-refs--activate)
    (locs-and-refs--deactivate)))

;; Footer


(provide 'locs-and-refs)

;;; locs-and-refs.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
