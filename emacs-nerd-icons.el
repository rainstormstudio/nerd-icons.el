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

(require 'emacs-nerd-icons-data-iec-power-symbols "./data/emacs-nerd-icons-data-iec-power-symbols")
(require 'emacs-nerd-icons-data-pomicons "./data/emacs-nerd-icons-data-pomicons")
(require 'emacs-nerd-icons-data-octicons "./data/emacs-nerd-icons-data-octicons")
(require 'emacs-nerd-icons-data-powerline "./data/emacs-nerd-icons-data-powerline")
(require 'emacs-nerd-icons-data-font-awesome "./data/emacs-nerd-icons-data-font-awesome")
(require 'emacs-nerd-icons-data-weather-icons "./data/emacs-nerd-icons-data-weather-icons")
(require 'emacs-nerd-icons-data-seti-ui-custom "./data/emacs-nerd-icons-data-seti-ui-custom")
(require 'emacs-nerd-icons-data-devicons "./data/emacs-nerd-icons-data-devicons")
(require 'emacs-nerd-icons-data-codicons "./data/emacs-nerd-icons-data-codicons")
(require 'emacs-nerd-icons-data-font-logos "./data/emacs-nerd-icons-data-font-logos")
(require 'emacs-nerd-icons-data-material-design "./data/emacs-nerd-icons-data-material-design")

(require 'emacs-nerd-icons-faces)

(defgroup emacs-nerd-icons nil
  "Manage how Nerd Fonts formats icons."
  :prefix "emacs-nerd-icons-"
  :group 'appearance
  :group 'convenience)

(defcustom emacs-nerd-icons-color-icons t
  "Whether or not to include a foreground color when formatting the icon."
  :group 'emacs-nerd-icons
  :type 'boolean)

(defcustom emacs-nerd-icons-scale-factor 1.0
  "The base Scale Factor for the `height' face property of an icon."
  :group 'emacs-nerd-icons
  :type 'number)

(defcustom emacs-nerd-icons-default-adjust 0.0
  "The default adjustment to be made to the `raise' display property of an icon."
  :group 'emacs-nerd-icons
  :type 'number)

(defvar emacs-nerd-icons-font-family "FiraCode Nerd Font") ;; temporary font

(defvar emacs-nerd-icons-weather-icon-alist
  '(
    ("tornado"               emacs-nerd-icons-wicon "nf-weather-tornado")
    ("hurricane"             emacs-nerd-icons-wicon "nf-weather-hurricane")
    ("thunderstorms"         emacs-nerd-icons-wicon "nf-weather-thunderstorm")
    ("sunny"                 emacs-nerd-icons-wicon "nf-weather-day_sunny")
    ("rain.*snow"            emacs-nerd-icons-wicon "nf-weather-rain_mix")
    ("rain.*hail"            emacs-nerd-icons-wicon "nf-weather-rain_mix")
    ("sleet"                 emacs-nerd-icons-wicon "nf-weather-sleet")
    ("hail"                  emacs-nerd-icons-wicon "nf-weather-hail")
    ("drizzle"               emacs-nerd-icons-wicon "nf-weather-sprinkle")
    ("rain"                  emacs-nerd-icons-wicon "nf-weather-showers")
    ("showers"               emacs-nerd-icons-wicon "nf-weather-showers")
    ("blowing.*snow"         emacs-nerd-icons-wicon "nf-weather-snow_wind")
    ("snow"                  emacs-nerd-icons-wicon "nf-weather-snow")
    ("dust"                  emacs-nerd-icons-wicon "nf-weather-dust")
    ("fog"                   emacs-nerd-icons-wicon "nf-weather-fog")
    ("haze"                  emacs-nerd-icons-wicon "nf-weather-day_haze")
    ("smoky"                 emacs-nerd-icons-wicon "nf-weather-smoke")
    ("blustery"              emacs-nerd-icons-wicon "nf-weather-cloudy_windy")
    ("windy"                 emacs-nerd-icons-wicon "nf-weather-cloudy_gusts")
    ("cold"                  emacs-nerd-icons-wicon "nf-weather-snowflake_cold")
    ("partly.*cloudy.*night" emacs-nerd-icons-wicon "nf-weather-night_alt_partly_cloudy")
    ("partly.*cloudy"        emacs-nerd-icons-wicon "nf-weather-day_cloudy_high")
    ("cloudy.*night"         emacs-nerd-icons-wicon "nf-weather-night_alt_cloudy")
    ("cxloudy.*day"          emacs-nerd-icons-wicon "nf-weather-day_cloudy")
    ("cloudy"                emacs-nerd-icons-wicon "nf-weather-cloudy")
    ("clear.*night"          emacs-nerd-icons-wicon "nf-weather-night_clear")
    ("fair.*night"           emacs-nerd-icons-wicon "nf-weather-stars")
    ("fair.*day"             emacs-nerd-icons-wicon "nf-weather-horizon")
    ("hot"                   emacs-nerd-icons-wicon "nf-weather-hot")
    ("not.*available"        emacs-nerd-icons-wicon "nf-weather-na")
    ))

(defun emacs-nerd-icons-auto-mode-match? (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (emacs-nerd-icons-match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun emacs-nerd-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (cl-find-if (lambda (it) (string-match (car it) file)) alist)))

(defun emacs-nerd-icons--read-candidates ()
  "Helper to build a list of candidates for all families."
  (cl-reduce 'append (mapcar (lambda (it) (emacs-nerd-icons--read-candidates-for-family (car it) t)) emacs-nerd-icons-alist)))

(defun emacs-nerd-icons--read-candidates-for-family (family &optional show-family)
  "Helper to build read candidates for FAMILY.
If SHOW-FAMILY is non-nil, displays the icons family in the candidate string."
  (let ((data   (cdr (assoc family emacs-nerd-icons-alist)))
        (icon-f (emacs-nerd-icons--function-name family)))
    (mapcar
     (lambda (it)
       (let* ((icon-name (car it))
              (icon-name-head (substring icon-name 0 1))
              (icon-name-tail (substring icon-name 1))

              (icon-display (propertize icon-name-head 'display (format "%s\t%s" (funcall icon-f icon-name) icon-name-head)))
              (icon-family (if show-family (format "\t[%s]" family) ""))

              (candidate-name (format "%s%s%s" icon-display icon-name-tail icon-family))
              (candidate-icon (funcall (emacs-nerd-icons--function-name family) icon-name)))

         (cons candidate-name candidate-icon)))
     data)))

;;;###autoload
(defun emacs-nerd-icons-insert (&optional arg family)
  "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it."
  (interactive "P")
  (let* ((standard-output (current-buffer))
         (candidates (if family
                         (emacs-nerd-icons--read-candidates-for-family family)
                       (emacs-nerd-icons--read-candidates)))
         (prompt     (if family
                         (format "%s Icon: " (funcall (emacs-nerd-icons--family-name family)))
                       "Icon : "))

         (selection (completing-read prompt candidates nil t))
         (result    (cdr (assoc selection candidates))))

    (if arg (prin1 result) (insert result))))

;; Weather icons
(defun emacs-nerd-icons-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (emacs-nerd-icons-match-to-alist weather emacs-nerd-icons-weather-icon-alist)))
    (if icon (apply (car icon) (cdr icon)) weather)))

(eval-and-compile
  (defun emacs-nerd-icons--function-name (name)
    "Get the symbol for an icon function name for icon set NAME."
    (intern (concat "emacs-nerd-icons-" (downcase (symbol-name name)))))

  (defun emacs-nerd-icons--family-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "emacs-nerd-icons-" (downcase (symbol-name name)) "-family")))

  (defun emacs-nerd-icons--data-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "emacs-nerd-icons-" (downcase (symbol-name name)) "-data")))

  (defun emacs-nerd-icons--insert-function-name (name)
    "Get the symbol for an icon insert function for icon set NAME."
    (intern (concat "emacs-nerd-icons-insert-" (downcase (symbol-name name)))))
  )

(defun emacs-nerd-icons-insert-icons-for (family &optional height duration)
  "Insert all of the available icons associated with FAMILY.
If a HEIGHT is provided it will render the icons at this height.
This is useful both to see the icons more clearly and to test
different height rendering.  If DURATION is provided, it will
pause for DURATION seconds between printing each character."
  (let* ((data-f    (emacs-nerd-icons--data-name family))
         (insert-f  (emacs-nerd-icons--function-name family))

         (height (or height 1.0))
         (data (funcall data-f)))
    (mapc
     (lambda (it)
       (insert (format "%s - %s\n" (funcall insert-f (car it) :height height) (car it)))
       (when duration (sit-for duration 0)))
     data)))

(defmacro emacs-nerd-icons-define-icon (name alist family)
  "Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `emacs-nerd-icon-NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
FONT-NAME is the name of the .ttf file providing the font, defaults to FAMILY."
  `(progn
     (defun ,(emacs-nerd-icons--family-name name) () ,family)
     (defun ,(emacs-nerd-icons--data-name name) () ,alist)
     (defun ,(emacs-nerd-icons--function-name name) (icon-name &rest args)
       (let ((icon (cdr (assoc icon-name ,alist)))
             (other-face (when emacs-nerd-icons-color-icons (plist-get args :face)))
             (height (* emacs-nerd-icons-scale-factor (or (plist-get args :height) 1.0)))
             (v-adjust (* emacs-nerd-icons-scale-factor (or (plist-get args :v-adjust) emacs-nerd-icons-default-adjust)))
             (family ,family))
         (unless icon
           (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name))))
         (let ((face (if other-face
                         `(:family ,family :height ,height :inherit ,other-face)
                       `(:family ,family :height ,height))))
           (propertize icon
                       'face face
                       'font-lock-face face
                       'display `(raise ,v-adjust)
                       'rear-nonsticky t))))
     (defun ,(emacs-nerd-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." family)
       (interactive "P")
       (emacs-nerd-icons-insert arg (quote ,name))))
  )

(emacs-nerd-icons-define-icon ipsicon emacs-nerd-icons/ipsicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon octicon emacs-nerd-icons/octicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon pomicon emacs-nerd-icons/pomicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon powerline emacs-nerd-icons/powerline-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon faicon emacs-nerd-icons/faicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon wicon emacs-nerd-icons/wicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon sucicon emacs-nerd-icons/sucicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon devicon emacs-nerd-icons/devicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon codicon emacs-nerd-icons/codicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon flicon emacs-nerd-icons/flicon-alist emacs-nerd-icons-font-family)
(emacs-nerd-icons-define-icon mdicon emacs-nerd-icons/mdicon-alist emacs-nerd-icons-font-family)

(provide 'emacs-nerd-icons)
;;; emacs-nerd-icons.el ends here
