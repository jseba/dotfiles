;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       ;;helm
       ;;ido
       ;;ivy
       vertico

       :ui
       ;;deft
       doom
       doom-dashboard
       ;;doom-quit
       (emoji +unicode)
       hl-todo
       ;;hydra
       indent-guides
       ;;ligatures
       ;;minimap
       modeline
       ;;nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;tabs
       ;;treemacs
       ;;unicode
       vc-gutter
       vi-tilde-fringe
       ;;window-select
       workspaces
       ;;zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       ;;god
       ;;lispy
       ;;multiple-cursors
       ;;objed
       ;;parinfer
       rotate-text
       snippets
       ;;word-wrap

       :emacs
       dired
       electric
       ;;ibuffer
       undo
       vc

       :term
       ;;eshell
       ;;shell
       ;;term
       ;;vterm

       :checkers
       syntax
       (spell +flyspell)
       ;;grammar

       :tools
       ;;ansible
       ;;biblio
       ;;debugger
       ;;direnv
       docker
       editorconfig
       ;;ein
       (eval +overlay)
       ;;gist
       lookup
       lsp
       magit
       make
       ;;pass
       ;;pdf
       ;;prodigy
       ;;rgb
       ;;taskrunner
       ;;terraform
       ;;tmux
       tramp
       ;;upload

       :os
       (:if IS-MAC macos)
       ;;tty

       :lang
       ;;agda
       ;;beancount
       ;;(cc +lsp)
       ;;clojure
       ;;common-lisp
       ;;coq
       ;;crystal
       ;;csharp
       data
       ;;(dart +flutter)
       ;;dhall
       ;;elixir
       ;;elm
       emacs-lisp
       ;;erlang
       ;;ess
       ;;factor
       ;;faust
       ;;fortran
       ;;fsharp
       ;;fstar
       ;;gdscript
       (go +lsp)
       ;;(graphql +lsp)
       ;;(haskell +lsp)
       ;;hy
       ;;idris
       json
       ;;(java +lsp)
       ;;javascript
       ;;julia
       ;;kotlin
       ;;latex
       ;;lean
       ;;ledger
       ;;lua
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       ;;org
       ;;php
       ;;plantuml
       ;;purescript
       python
       ;;qt
       ;;racket
       ;;raku
       ;;rest
       rst
       ;;(ruby +rails)
       (rust +lsp)
       ;;scala
       ;;(scheme +guile)
       sh
       ;;sml
       ;;solidity
       ;;swift
       ;;terra
       ;;web
       yaml
       ;;zig

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere
       ;;irc
       ;;(rss +org)
       ;;twitter

       :config
       ;;literate
       (default +bindings +smartparens))
