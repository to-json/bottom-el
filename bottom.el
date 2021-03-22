(defvar bottom-seperator "👉👈")

(defun bottom-ify (str)
  "translate text to bottom"
  (mapconcat 'bottom--char str ""))

(defun bottom-ify-controlled (str)
  "a well trained bottom can still understand control characters"
  (mapconcat 'bottom--char-controlled str ""))

;; TODO: macro out this basic "replace region" shape

(defun bottom-ify-interactive (beg end)
  "bottomify a region"
  (interactive "r")
  (save-excursion
    (let ((bottomified (bottom-ify (buffer-substring-no-properties beg end))))
	 (kill-region beg end)
	 (insert bottomified))))

(defun bottom-ify-interactive-controlled (beg end)
  "bottomify a region. uses controlled bottomify"
  (interactive "r")
  (save-excursion
    (let ((bottomified (bottom-ify-controlled (buffer-substring-no-properties beg end))))
	 (kill-region beg end)
	 (insert bottomified))))

(defun bottom-regress (str)
  "translate bottom to human-readable text (futile)"
  (concat (mapcar 'bottom--regress-char
                  (butlast (split-string str bottom-seperator)))))

(defun bottom-regress-controlled (str)
  "translate controlled-bottom to human-readable text (slightly less futile)"
  (concat (mapcar 'bottom--regress-char-controlled
                  (butlast (split-string str bottom-seperator)))))

(defun bottom-regress-interactive (beg end)
  "regress a region."
  (interactive "r")
  (save-excursion
    (let ((bottomified (bottom-regress (buffer-substring-no-properties beg end))))
	 (kill-region beg end)
	 (insert bottomified))))

(defun bottom-regress-interactive-controlled (beg end)
  "regress a region. uses controlled bottomify"
  (interactive "r")
  (save-excursion
    (let ((bottomified (bottom-regress-controlled (buffer-substring-no-properties beg end))))
	 (kill-region beg end)
	 (insert bottomified))))

;; ex: (global-set-key (kbd "C-c b") 'bottom-ify-interactive)
;; ex: (global-set-key (kbd "C-c r") 'bottom-regress-interactive)

;; "private" functions
(defun bottom--char (c)
  "Map a single character to it's bottom-encoded alternative"
  (concat (if (eq c 0) "❤" (bottom--inner-char c)) bottom-seperator))

(defun bottom--inner-char (c)
  "Inner loop for bottom-char on non-null characters"
  (cond ((eq 0 c) "")
        ((>= c 200) (concat "🫂" (bottom--inner-char (- c 200))))
	((>= c 50)  (concat "💖" (bottom--inner-char (- c 50))))
	((>= c 10)  (concat "✨" (bottom--inner-char (- c 10))))
	((>= c 5)   (concat "🥺" (bottom--inner-char (- c 5))))
	((>= c 1)   (concat ","   (bottom--inner-char (- c 1))))))

(defun bottom--char-controlled (c)
  "an exception to the bottomify rules to permit linewise transformation"
    (concat (cond ((eq c 0) "❤")
	           ((< c 32) (char-to-string c))
	           (t (bottom--inner-char c)))
	    bottom-seperator))

(defun bottom--regress-char (c)
  "regress a bottom-encoded character"
  (if (eq "❤" c) 0
    (apply '+ (mapcar 'bottom--regress-char-inner c))))

(defun bottom--regress-char-controlled (c)
  "regress a bottom-encoded character in a controlled fashion"
  (cond ((eq "❤" c) 0)
	((< (string-to-char c) 32) (string-to-char c))
        (t (apply '+ (mapcar 'bottom--regress-char-inner c)))))

(defun bottom--regress-char-inner (c)
  "convert an individual bottom codepoint"
  (cond ((eq c ?🫂) 200)
	((eq c ?💖)  50)
	((eq c ?✨)  10)
	((eq c ?🥺)   5)
	((eq c ?,)   1)
	(t c)))

;; tests commented out because i like having it all in one file, but,
;; emacs will happily load these and make shit confusing
;; if you're actually using ert

;; (ert-deftest bottom-ify-test-basic ()
;;   "tests core bottomification functionality"
;;   (should (equal (bottom-ify "Hello world!")
;; 		 (concat "💖✨✨,,👉👈💖💖,👉👈💖💖🥺,,,👉👈💖💖🥺,,,👉👈"
;;                          "💖💖✨,👉👈✨✨✨,,👉👈💖💖✨🥺,,,,👉👈"
;; 			 "💖💖✨,👉👈💖💖✨,,,,👉👈💖💖🥺,,,👉👈"
;;                          "💖💖👉👈✨✨✨,,,👉👈"))))
;; 
;; (ert-deftest bottom-ify-controlled-test-basic ()
;;   "tests collared bottomification functionality"
;;   (should (equal (bottom-ify-controlled "ab\ncd")
;;                  (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
;;                          "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈"))))
;; 
;; (ert-deftest bottom-regress-test-basic ()
;;   "tests core bottomification functionality"
;;   (should (equal (bottom-regress (concat "💖✨✨,,👉👈💖💖,👉👈💖💖🥺,,,👉👈"
;; 			                 "💖💖🥺,,,👉👈💖💖✨,👉👈✨✨✨,,👉👈"
;; 			                 "💖💖✨🥺,,,,👉👈💖💖✨,👉👈💖💖✨,,,,"
;; 			                 "👉👈💖💖🥺,,,👉👈💖💖👉👈✨✨✨,,,👉👈"))
;; 		 "Hello world!")))
;; 
;; (ert-deftest bottom-regress-controlled-test-basic ()
;;   "tests collared bottomification functionality"
;;   (should (equal (bottom-regress-controlled (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
;;                                                     "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈"))
;; 		 "ab\ncd")))

;; when making modifications, these are convenient to have around
;; (defvar test-data "💖💖✨,👉👈💖💖✨,,,,👉👈💖💖🥺,,,👉👈") ;; -> orl
;; (defvar multiline-test-data (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
;;                                    "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈")) ;; -> "ab\ncd"
