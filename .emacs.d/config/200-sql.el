;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'func)
  (require 'sql-indent nil t))

(use-package sql-indent
  :ensure t
  :defer t
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (setq-default sqlind-basic-offset 4))
