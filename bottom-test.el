(ert-deftest bottom-ify-test-basic ()
  "tests core bottomification functionality"
  (should (equal (bottom-ify "Hello world!")
		 (concat "💖✨✨,,👉👈💖💖,👉👈💖💖🥺,,,👉👈💖💖🥺,,,👉👈"
                         "💖💖✨,👉👈✨✨✨,,👉👈💖💖✨🥺,,,,👉👈"
			 "💖💖✨,👉👈💖💖✨,,,,👉👈💖💖🥺,,,👉👈"
                         "💖💖👉👈✨✨✨,,,👉👈"))))

(ert-deftest bottom-ify-controlled-test-basic ()
  "tests collared bottomification functionality"
  (should (equal (bottom-ify-controlled "ab\ncd")
                 (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
                         "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈"))))

(ert-deftest bottom-regress-test-basic ()
  "tests core regression functionality"
  (should (equal (bottom-regress (concat "💖✨✨,,👉👈💖💖,👉👈💖💖🥺,,,👉👈"
			                 "💖💖🥺,,,👉👈💖💖✨,👉👈✨✨✨,,👉👈"
			                 "💖💖✨🥺,,,,👉👈💖💖✨,👉👈💖💖✨,,,,"
			                 "👉👈💖💖🥺,,,👉👈💖💖👉👈✨✨✨,,,👉👈"))
		 "Hello world!")))

(ert-deftest bottom-regress-controlled-test-basic ()
  "tests collared regression functionality"
  (should (equal (bottom-regress-controlled (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
                                                    "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈"))
		 "ab\ncd")))

(ert-deftest bottom-regress-test-cyrillic ()
  "tests core regression functionality on cyrillic multi-codepoint chars"
  (should (equal (bottom-regress bottom--cyrillic-test-data)
		 "Я тут бота на коленке сделала")))

(ert-deftest bottom-ify-test-cyrillic ()
  "tests core bottomification functionality on cyrillic multi-codepoint chars"
  (should (equal (bottom-ify "Я тут бота на коленке сделала")
		 bottom--cyrillic-test-data)))

;; when making modifications, these are convenient to have around
(defvar bottom--test-data "💖💖✨,👉👈💖💖✨,,,,👉👈💖💖🥺,,,👉👈") ;; -> orl
(defvar bottom--multiline-test-data (concat "💖✨✨✨✨🥺,,👉👈💖✨✨✨✨🥺,,,👉👈\n"
                                   "👉👈💖✨✨✨✨🥺,,,,👉👈💖💖👉👈")) ;; -> "ab\ncd"
(defvar bottom--cyrillic-test-data (concat
  "🫂🫂🫂🫂🫂💖✨✨,👉👈✨✨✨,,👉👈🫂🫂🫂🫂🫂💖✨✨✨✨👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨✨,👉👈🫂🫂🫂🫂🫂💖✨✨✨✨👉👈✨✨✨,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨,,,👉👈🫂🫂🫂🫂🫂💖✨✨✨🥺,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨✨👉👈🫂🫂🫂🫂🫂💖✨✨,,👉👈✨✨✨,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨🥺👉👈🫂🫂🫂🫂🫂💖✨✨,,👉👈✨✨✨,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨,,👉👈🫂🫂🫂🫂🫂💖✨✨✨🥺,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨,,,👉👈🫂🫂🫂🫂🫂💖✨✨🥺,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨🥺👉👈🫂🫂🫂🫂🫂💖✨✨✨,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨🥺,,👉👈✨✨✨,,👉👈🫂🫂🫂🫂🫂💖✨✨✨🥺,,,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨🥺,👉👈🫂🫂🫂🫂🫂💖✨✨🥺,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨,,,👉👈🫂🫂🫂🫂🫂💖✨✨,,👉👈"
  "🫂🫂🫂🫂🫂💖✨✨✨,,,👉👈🫂🫂🫂🫂🫂💖✨✨,,👉👈")) ;; -> "Я тут бота на коленке сделала"
