;;; init.el -*- lexical-binding: t; -*-

(doom! :feature
       ;debugger
       eval
       (evil +everywhere)
       file-templates
       ;services
       snippets
       spellcheck
       syntax-checker
       workspaces

       :completion
       (company +childframe)
       (ivy +childframe)

       :editor
       rotate-text

       :ui
       doom
       (doom-modeline +new)
       hl-todo
       popup
       (pretty-code +fira)
       vc-gutter
       window-select

       :tools
       make
       magit
       pdf

       :emacs
       dired
       electric
       eshell
       imenu
       term
       vc

       :lang
       assembly
       data
       emacs-lisp
       markdown
       (org +attach +babel +capture +export +publish)
       perl
       python
       sh

       :app
      ;(email +gmail)
      ;irc
      ;(rss +org)
       :config
       (default +evil-commands))
