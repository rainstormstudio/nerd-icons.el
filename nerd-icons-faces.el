;;; nerd-icons-faces.el --- A module of faces for nerd-icons -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/rainstormstudio/nerd-icons.el
;; Keywords: convenient, lisp

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

;; This file contains all of the faces used by the package for
;; colouring icons

;;; Code:

(defgroup nerd-icons-faces nil
  "Manage how Nerd Font icons are coloured and themed."
  :prefix "nerd-icons-"
  :group 'tools
  :group 'nerd-icons)

;; red
(defface nerd-icons-red
  '((((background dark)) :foreground "#AC4142")
    (((background light)) :foreground "#AC4142"))
  "Face for red icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lred
  '((((background dark)) :foreground "#EB595A")
    (((background light)) :foreground "#EB595A"))
  "Face for lred icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dred
  '((((background dark)) :foreground "#843031")
    (((background light)) :foreground "#843031"))
  "Face for dred icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-red-alt
  '((((background dark)) :foreground "#ce5643")
    (((background light)) :foreground "#843031"))
  "Face for dred icons."
  :group 'nerd-icons-faces)

;; green
(defface nerd-icons-green
  '((((background dark)) :foreground "#90A959")
    (((background light)) :foreground "#90A959"))
  "Face for green icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lgreen
  '((((background dark)) :foreground "#C6E87A")
    (((background light)) :foreground "#3D6837"))
  "Face for lgreen icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dgreen
  '((((background dark)) :foreground "#6D8143")
    (((background light)) :foreground "#6D8143"))
  "Face for dgreen icons."
  :group 'nerd-icons-faces)

;; yellow
(defface nerd-icons-yellow
  '((((background dark)) :foreground "#FFD446")
    (((background light)) :foreground "#FFCC0E"))
  "Face for yellow icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lyellow
  '((((background dark)) :foreground "#FFC16D")
    (((background light)) :foreground "#FF9300"))
  "Face for lyellow icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dyellow
  '((((background dark)) :foreground "#B48D56")
    (((background light)) :foreground "#B48D56"))
  "Face for dyellow icons."
  :group 'nerd-icons-faces)

;; blue
(defface nerd-icons-blue
  '((((background dark)) :foreground "#6A9FB5")
    (((background light)) :foreground "#6A9FB5"))
  "Face for blue icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-blue-alt
  '((((background dark)) :foreground "#2188b6")
    (((background light)) :foreground "#2188b6"))
  "Face for blue icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lblue
  '((((background dark)) :foreground "#8FD7F4")
    (((background light)) :foreground "#677174"))
  "Face for lblue icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dblue
  '((((background dark)) :foreground "#446674")
    (((background light)) :foreground "#446674"))
  "Face for dblue icons."
  :group 'nerd-icons-faces)

;; maroon
(defface nerd-icons-maroon
  '((((background dark)) :foreground "#8F5536")
    (((background light)) :foreground "#8F5536"))
  "Face for maroon icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lmaroon
  '((((background dark)) :foreground "#CE7A4E")
    (((background light)) :foreground "#CE7A4E"))
  "Face for lmaroon icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dmaroon
  '((((background dark)) :foreground "#72584B")
    (((background light)) :foreground "#72584B"))
  "Face for dmaroon icons."
  :group 'nerd-icons-faces)

;; purple
(defface nerd-icons-purple
  '((((background dark)) :foreground "#AA759F")
    (((background light)) :foreground "#68295B"))
  "Face for purple icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-purple-alt
  '((((background dark)) :foreground "#5D54E1")
    (((background light)) :foreground "#5D54E1"))
  "Face for purple icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lpurple
  '((((background dark)) :foreground "#E69DD6")
    (((background light)) :foreground "#E69DD6"))
  "Face for lpurple icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dpurple
  '((((background dark)) :foreground "#694863")
    (((background light)) :foreground "#694863"))
  "Face for dpurple icons."
  :group 'nerd-icons-faces)

;; orange
(defface nerd-icons-orange
  '((((background dark)) :foreground "#D4843E")
    (((background light)) :foreground "#D4843E"))
  "Face for orange icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lorange
  '((((background dark)) :foreground "#FFA500")
    (((background light)) :foreground "#FFA500"))
  "Face for lorange icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dorange
  '((((background dark)) :foreground "#915B2D")
    (((background light)) :foreground "#915B2D"))
  "Face for dorange icons."
  :group 'nerd-icons-faces)

;; cyan
(defface nerd-icons-cyan
  '((((background dark)) :foreground "#75B5AA")
    (((background light)) :foreground "#75B5AA"))
  "Face for cyan icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-cyan-alt
  '((((background dark)) :foreground "#61dafb")
    (((background light)) :foreground "#0595bd"))
  "Face for cyan icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lcyan
  '((((background dark)) :foreground "#A5FDEC")
    (((background light)) :foreground "#2C7D6E"))
  "Face for lcyan icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dcyan
  '((((background dark)) :foreground "#48746D")
    (((background light)) :foreground "#48746D"))
  "Face for dcyan icons."
  :group 'nerd-icons-faces)

;; pink
(defface nerd-icons-pink
  '((((background dark)) :foreground "#F2B4B8")
    (((background light)) :foreground "#FC505B"))
  "Face for pink icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lpink
  '((((background dark)) :foreground "#FFBDC1")
    (((background light)) :foreground "#FF505B"))
  "Face for lpink icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dpink
  '((((background dark)) :foreground "#B18286")
    (((background light)) :foreground "#7E5D5F"))
  "Face for dpink icons."
  :group 'nerd-icons-faces)

;; silver
(defface nerd-icons-silver
  '((((background dark)) :foreground "#716E68")
    (((background light)) :foreground "#716E68"))
  "Face for silver icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-lsilver
  '((((background dark)) :foreground "#B9B6AA")
    (((background light)) :foreground "#7F7869"))
  "Face for lsilver icons."
  :group 'nerd-icons-faces)
(defface nerd-icons-dsilver
  '((((background dark)) :foreground "#838484")
    (((background light)) :foreground "#838484"))
  "Face for dsilver icons."
  :group 'nerd-icons-faces)

(provide 'nerd-icons-faces)
;;; nerd-icons-faces.el ends here
