;;; nerd-icons-data-flicon.el --- glyphset flicon -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp

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

;; flicon
;; from Nerd Font Version: 3.1.1

;;; Code:

(defvar nerd-icons/flicon-alist
  '(
    ("nf-linux-almalinux" . "f31d")
    ("nf-linux-alpine" . "f300")
    ("nf-linux-aosc" . "f301")
    ("nf-linux-apple" . "f302")
    ("nf-linux-archcraft" . "f345")
    ("nf-linux-archlabs" . "f31e")
    ("nf-linux-archlinux" . "f303")
    ("nf-linux-arcolinux" . "f346")
    ("nf-linux-arduino" . "f34b")
    ("nf-linux-artix" . "f31f")
    ("nf-linux-awesome" . "f354")
    ("nf-linux-biglinux" . "f347")
    ("nf-linux-bspwm" . "f355")
    ("nf-linux-budgie" . "f320")
    ("nf-linux-centos" . "f304")
    ("nf-linux-cinnamon" . "f35f")
    ("nf-linux-codeberg" . "f330")
    ("nf-linux-coreos" . "f305")
    ("nf-linux-crystal" . "f348")
    ("nf-linux-debian" . "f306")
    ("nf-linux-deepin" . "f321")
    ("nf-linux-devuan" . "f307")
    ("nf-linux-docker" . "f308")
    ("nf-linux-dwm" . "f356")
    ("nf-linux-elementary" . "f309")
    ("nf-linux-endeavour" . "f322")
    ("nf-linux-enlightenment" . "f357")
    ("nf-linux-fdroid" . "f36a")
    ("nf-linux-fedora" . "f30a")
    ("nf-linux-fedora_inverse" . "f30b")
    ("nf-linux-ferris" . "f323")
    ("nf-linux-flathub" . "f324")
    ("nf-linux-fluxbox" . "f358")
    ("nf-linux-forgejo" . "f335")
    ("nf-linux-fosdem" . "f36b")
    ("nf-linux-freebsd" . "f30c")
    ("nf-linux-freecad" . "f336")
    ("nf-linux-freedesktop" . "f360")
    ("nf-linux-garuda" . "f337")
    ("nf-linux-gentoo" . "f30d")
    ("nf-linux-gimp" . "f338")
    ("nf-linux-gitea" . "f339")
    ("nf-linux-gnome" . "f361")
    ("nf-linux-gnu_guix" . "f325")
    ("nf-linux-gtk" . "f362")
    ("nf-linux-hyperbola" . "f33a")
    ("nf-linux-hyprland" . "f359")
    ("nf-linux-i3" . "f35a")
    ("nf-linux-illumos" . "f326")
    ("nf-linux-inkscape" . "f33b")
    ("nf-linux-jwm" . "f35b")
    ("nf-linux-kali_linux" . "f327")
    ("nf-linux-kde_neon" . "f331")
    ("nf-linux-kde_plasma" . "f332")
    ("nf-linux-kdenlive" . "f33c")
    ("nf-linux-kicad" . "f34c")
    ("nf-linux-krita" . "f33d")
    ("nf-linux-kubuntu" . "f333")
    ("nf-linux-kubuntu_inverse" . "f334")
    ("nf-linux-linuxmint" . "f30e")
    ("nf-linux-linuxmint_inverse" . "f30f")
    ("nf-linux-locos" . "f349")
    ("nf-linux-lxde" . "f363")
    ("nf-linux-lxle" . "f33e")
    ("nf-linux-lxqt" . "f364")
    ("nf-linux-mageia" . "f310")
    ("nf-linux-mandriva" . "f311")
    ("nf-linux-manjaro" . "f312")
    ("nf-linux-mate" . "f365")
    ("nf-linux-mpv" . "f36e")
    ("nf-linux-mxlinux" . "f33f")
    ("nf-linux-neovim" . "f36f")
    ("nf-linux-nixos" . "f313")
    ("nf-linux-octoprint" . "f34d")
    ("nf-linux-openbsd" . "f328")
    ("nf-linux-openscad" . "f34e")
    ("nf-linux-opensuse" . "f314")
    ("nf-linux-osh" . "f34f")
    ("nf-linux-oshwa" . "f350")
    ("nf-linux-osi" . "f36c")
    ("nf-linux-parabola" . "f340")
    ("nf-linux-parrot" . "f329")
    ("nf-linux-pop_os" . "f32a")
    ("nf-linux-prusaslicer" . "f351")
    ("nf-linux-puppy" . "f341")
    ("nf-linux-qtile" . "f35c")
    ("nf-linux-qubesos" . "f342")
    ("nf-linux-raspberry_pi" . "f315")
    ("nf-linux-redhat" . "f316")
    ("nf-linux-reprap" . "f352")
    ("nf-linux-riscv" . "f353")
    ("nf-linux-rocky_linux" . "f32b")
    ("nf-linux-sabayon" . "f317")
    ("nf-linux-slackware" . "f318")
    ("nf-linux-slackware_inverse" . "f319")
    ("nf-linux-snappy" . "f32c")
    ("nf-linux-solus" . "f32d")
    ("nf-linux-sway" . "f35d")
    ("nf-linux-tails" . "f343")
    ("nf-linux-thunderbird" . "f370")
    ("nf-linux-tor" . "f371")
    ("nf-linux-trisquel" . "f344")
    ("nf-linux-tux" . "f31a")
    ("nf-linux-ubuntu" . "f31b")
    ("nf-linux-ubuntu_inverse" . "f31c")
    ("nf-linux-vanilla" . "f366")
    ("nf-linux-void" . "f32e")
    ("nf-linux-vscodium" . "f372")
    ("nf-linux-wayland" . "f367")
    ("nf-linux-wikimedia" . "f36d")
    ("nf-linux-xerolinux" . "f34a")
    ("nf-linux-xfce" . "f368")
    ("nf-linux-xmonad" . "f35e")
    ("nf-linux-xorg" . "f369")
    ("nf-linux-zorin" . "f32f")
    ))

(provide 'nerd-icons-data-flicon)
;;; nerd-icons-data-flicon.el ends here