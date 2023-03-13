;;; emacs-nerd-icons.el --- Emacs Nerd Font Icons Library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package was inspired by

;; - `vim-devicons' for Vim, found at https://github.com/ryanoasis/vim-devicons
;; - `nvim-web-devicons' for NeoVim, found at https://github.com/nvim-tree/nvim-web-devicons

;; This package provides an interface to the Nerd Fonts

;; - `nerd-fonts', found at https://github.com/ryanoasis/nerd-fonts

;;; Code:

(require 'data-iec-power-symbols "./data/data-iec-power-symbols")
(require 'data-octicons "./data/data-octicons")
(require 'data-pomicons "./data/data-pomicons")
(require 'data-powerline "./data/data-powerline")
(require 'data-font-awesome "./data/data-font-awesome")
(require 'data-weather-icons "./data/data-weather-icons")
(require 'data-seti-ui-custom "./data/data-seti-ui-custom")
(require 'data-devicons "./data/data-devicons")
(require 'data-codicons "./data/data-codicons")
(require 'data-font-logos "./data/data-font-logos")
(require 'data-material-design "./data/data-material-design")

(require 'emacs-nerd-icons-faces)

(defgroup emacs-nerd-icons nil
  "Manage how Nerd Fonts formats icons."
  :prefix "emacs-nerd-icons-"
  :group 'appearance
  :group 'convenience)

;; TODO: implementation

(provide 'emacs-nerd-icons)
;;; emacs-nerd-icons.el ends here
