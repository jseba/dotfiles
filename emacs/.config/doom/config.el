;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq 
  user-full-name "Josh Seba"
  user-mail-address "sebajosh@outlook.com"
  display-line-numbers-type nil
  confirm-kill-emacs nil
  ;; Doom exposes five (optional) variables for controlling fonts in Doom:
  ;;
  ;; - `doom-font' -- the primary font to use
  ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
  ;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.
  ;; - `doom-unicode-font' -- for unicode glyphs
  ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
  doom-font (font-spec :family "Fira Code" :size 14)
  doom-big-font (font-spec :family "Fira Code" :size 18)
  doom-theme 'doom-one

  evil-escape-key-sequence "kj"
  evil-cross-lines t)

;; TODO keybindings
(map!
 :n "C-t" #'pop-tag-mark)
