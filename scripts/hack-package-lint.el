;; -*- lexical-binding: t -*-

;; Loading this file will hack package-lint so it does not report
;; certain warnings which are not relevant to Apheleia.
;;
;; See https://github.com/purcell/package-lint/issues/125 for
;; discussion about how package-lint will apparently never support
;; ignoring specific warnings, by maintainer fiat.

(advice-add #'package-lint--check-autoloads-on-private-functions
            :override #'ignore)

(advice-add #'package-lint--check-globalized-minor-mode
            :override #'ignore)
