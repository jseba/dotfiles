;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun nx/swiper-at-point (sym)
  "Use `swiper' to search for the symbol at point."
  (interactive (list (thing-at-point 'symbol)))
  (swiper sym))

;;;###autoload
(defun nx/enable-ivy-flx-matching ()
  "Make `ivy' matching work more like IDO."
  (interactive)
  (require 'flx)
  (setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

