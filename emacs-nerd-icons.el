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

(require 'cl-lib)

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

(defcustom emacs-nerd-icons-font-family "Symbols Nerd Font Mono"
  "The Nerd Font for display icons."
  :group 'emacs-nerd-icons
  :type 'string)

(defcustom emacs-nerd-icons-fonts-subdirectory nil
  "The subdirectory within the system fonts folder where the icons are installed."
  :group 'emacs-nerd-icons
  :type 'directory)

(defvar emacs-nerd-icons-font-names '() "List of defined font file names.")

(defvar emacs-nerd-icons-extension-icon-alist
  '(
    ("fish" emacs-nerd-icons-devicon "nf-dev-terminal" :face emacs-nerd-icons-lpink)
    ("zsh" emacs-nerd-icons-devicon "nf-dev-terminal" :face emacs-nerd-icons-lcyan)
    ("sh" emacs-nerd-icons-devicon "nf-dev-terminal" :face emacs-nerd-icons-purple)
    ;; Meta
    ("tags" emacs-nerd-icons-octicon "nf-oct-tag" :face emacs-nerd-icons-blue)
    ("log" emacs-nerd-icons-octicon "nf-oct-bug" :face emacs-nerd-icons-maroon)
    ;; Config
    ("node" emacs-nerd-icons-devicon "nf-dev-nodejs_small" :face emacs-nerd-icons-green)
    ("babelrc" emacs-nerd-icons-mdicon "nf-md-babel" :face emacs-nerd-icons-yellow)
    ("bashrc" emacs-nerd-icons-mdicon "nf-md-bash" :face emacs-nerd-icons-dpink)
    ("bowerrc" emacs-nerd-icons-devicon "nf-dev-bower" :face emacs-nerd-icons-silver)
    ("cr" emacs-nerd-icons-sucicon "nf-seti-crystal" :face emacs-nerd-icons-yellow)
    ("ecr" emacs-nerd-icons-sucicon "nf-seti-crystal" :face emacs-nerd-icons-yellow)
    ("ini" emacs-nerd-icons-octicon "nf-oct-settings" :face emacs-nerd-icons-yellow)
    ("eslintignore" emacs-nerd-icons-mdicon "nf-md-eslint" :face emacs-nerd-icons-purple)
    ("eslint" emacs-nerd-icons-mdicon "nf-md-eslint" :face emacs-nerd-icons-lpurple)
    ("git" emacs-nerd-icons-devicon "nf-dev-git" :face emacs-nerd-icons-lred)
    ("mk" emacs-nerd-icons-devicon "nf-dev-gnu" :face emacs-nerd-icons-dorange)
    ;;    ("cmake" emacs-nerd-icons-devicon "cmake") TODO: cmake
    ("dockerignore" emacs-nerd-icons-devicon "nf-dev-docker" :face emacs-nerd-icons-dblue)
    ("xml" emacs-nerd-icons-faicon "nf-fa-file_code_o" :face emacs-nerd-icons-lorange)
    ("json" emacs-nerd-icons-octicon "nf-oct-settings" :face emacs-nerd-icons-yellow)
    ("cson" emacs-nerd-icons-octicon "nf-oct-settings" :face emacs-nerd-icons-yellow)
    ("yml" emacs-nerd-icons-octicon "nf-oct-settings" :face emacs-nerd-icons-dyellow)
    ("yaml" emacs-nerd-icons-octicon "nf-oct-settings" :face emacs-nerd-icons-dyellow)
    ;; ?
    ("pkg" emacs-nerd-icons-octicon "nf-oct-package" :face emacs-nerd-icons-dsilver)
    ("rpm" emacs-nerd-icons-octicon "nf-oct-package" :face emacs-nerd-icons-dsilver)
    ("pkgbuild" emacs-nerd-icons-octicon "nf-oct-package" :face emacs-nerd-icons-dsilver)
    ("elc" emacs-nerd-icons-octicon "nf-oct-file_binary" :face emacs-nerd-icons-dsilver)
    ("gz" emacs-nerd-icons-octicon "nf-oct-file_binary" :face emacs-nerd-icons-lmaroon)
    ("zip" emacs-nerd-icons-octicon "nf-oct-file_zip" :face emacs-nerd-icons-lmaroon)
    ("7z" emacs-nerd-icons-octicon "nf-oct-file_zip" :face emacs-nerd-icons-lmaroon)
    ("dat" emacs-nerd-icons-faicon "nf-fa-bar_chart" :face emacs-nerd-icons-cyan)
    ("dmg" emacs-nerd-icons-octicon "nf-oct-tools" :face emacs-nerd-icons-lsilver)
    ("dll" emacs-nerd-icons-faicon "nf-fa-cogs" :face emacs-nerd-icons-silver)
    ("ds_store" emacs-nerd-icons-faicon "nf-fa-cogs" :face emacs-nerd-icons-silver)
    ;; Source Codes
    ("scpt"         emacs-nerd-icons-devicon "nf-dev-apple" :face emacs-nerd-icons-pink)
    ;; ("aup"          emacs-nerd-icons-fileicon "audacity") TODO: audacity
    ("elm"          emacs-nerd-icons-sucicon "nf-seti-elm" :face emacs-nerd-icons-blue)
    ("erl"          emacs-nerd-icons-devicon "nf-dev-erlang" :face emacs-nerd-icons-red)
    ("hrl"          emacs-nerd-icons-devicon "nf-dev-erlang" :face emacs-nerd-icons-dred)
    ("eex"          emacs-nerd-icons-sucicon "nf-seti-elixir" :face emacs-nerd-icons-lorange)
    ("leex"         emacs-nerd-icons-sucicon "nf-seti-elixir" :face emacs-nerd-icons-lorange)
    ("heex"         emacs-nerd-icons-sucicon "nf-seti-elixir" :face emacs-nerd-icons-lorange)
    ("ex"           emacs-nerd-icons-sucicon "nf-seti-elixir" :face emacs-nerd-icons-lpurple)
    ("exs"          emacs-nerd-icons-sucicon "nf-seti-elixir" :face emacs-nerd-icons-lred)
    ("java"         emacs-nerd-icons-devicon "nf-dev-java" :face emacs-nerd-icons-purple)
    ("gradle"       emacs-nerd-icons-sucicon "nf-seti-gradle" :face emacs-nerd-icons-silver)
    ("ebuild"       emacs-nerd-icons-mdicon "nf-md-gentoo" :face emacs-nerd-icons-cyan)
    ("eclass"       emacs-nerd-icons-mdicon "nf-md-gentoo" :face emacs-nerd-icons-blue)
    ("go"           emacs-nerd-icons-devicon "nf-dev-go" :face emacs-nerd-icons-blue)
    ("jl"           emacs-nerd-icons-sucicon "nf-seti-julia" :face emacs-nerd-icons-purple)
    ("magik" emacs-nerd-icons-faicon "nf-fa-magic" :face emacs-nerd-icons-blue)
    ;; ("matlab"       emacs-nerd-icons-devicon "matlab") TODO: matlab
    ("nix"          emacs-nerd-icons-mdicon "nf-md-nix" :face emacs-nerd-icons-blue)
    ("pl"           emacs-nerd-icons-sucicon "nf-seti-perl" :face emacs-nerd-icons-lorange)
    ("pm"           emacs-nerd-icons-sucicon "nf-seti-perl" :face emacs-nerd-icons-lorange)
    ;; ("pl6"          emacs-nerd-icons-devicon"raku") TODO: raku
    ;; ("pm6"          emacs-nerd-icons-devicon "raku") TODO: raku
    ("pod"          emacs-nerd-icons-devicon "nf-dev-perl" :face emacs-nerd-icons-lgreen)
    ("php"          emacs-nerd-icons-devicon "nf-dev-php" :face emacs-nerd-icons-lsilver)
    ;; ("pony"         emacs-nerd-icons-devicon "pony") TODO: pony
    ("ps1"          emacs-nerd-icons-mdicon "nf-md-powershell" :face emacs-nerd-icons-blue)
    ("pro"          emacs-nerd-icons-sucicon "nf-seti-prolog" :face emacs-nerd-icons-lmaroon)
    ("proog"        emacs-nerd-icons-sucicon "nf-seti-prolog" :face emacs-nerd-icons-lmaroon)
    ("py"           emacs-nerd-icons-devicon "nf-dev-python" :face emacs-nerd-icons-dblue)
    ;; ("idr"          emacs-nerd-icons-devicon "idris") TODO: idris
    ;; ("ipynb"        emacs-nerd-icons-devicon "jupyter") TODO: jupyter
    ("gem"          emacs-nerd-icons-devicon "nf-dev-ruby" :face emacs-nerd-icons-red)
    ;; ("raku"         emacs-nerd-icons-devicon "raku") TODO: raku
    ;; ("rakumod"      emacs-nerd-icons-devicon "raku") TODO: raku
    ("rb" emacs-nerd-icons-octicon "nf-oct-ruby" :face emacs-nerd-icons-lred)
    ("rs"           emacs-nerd-icons-devicon "nf-dev-rust" :face emacs-nerd-icons-maroon)
    ("rlib"         emacs-nerd-icons-devicon "nf-dev-rust" :face emacs-nerd-icons-dmaroon)
    ("r"            emacs-nerd-icons-sucicon "nf-seti-r" :face emacs-nerd-icons-lblue)
    ("rd"           emacs-nerd-icons-sucicon "nf-seti-r" :face emacs-nerd-icons-lblue)
    ("rdx"          emacs-nerd-icons-sucicon "nf-seti-r" :face emacs-nerd-icons-lblue)
    ("rsx"          emacs-nerd-icons-sucicon "nf-seti-r" :face emacs-nerd-icons-lblue)
    ("svelte"       emacs-nerd-icons-sucicon "nf-seti-svelte" :face emacs-nerd-icons-red)
    ("gql"          emacs-nerd-icons-mdicon "nf-md-graphql" :face emacs-nerd-icons-dpink)
    ("graphql"      emacs-nerd-icons-mdicon "nf-md-graphql" :face emacs-nerd-icons-dpink)
    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("c"            emacs-nerd-icons-sucicon "nf-custom-c" :face emacs-nerd-icons-blue)
    ("h"            emacs-nerd-icons-faicon "nf-fa-h_square" :face emacs-nerd-icons-purple)
    ("m"            emacs-nerd-icons-devicon "nf-dev-apple" )
    ("mm"           emacs-nerd-icons-devicon "nf-dev-apple" )
    ;;
    ("cc"           emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-blue)
    ("cpp"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-blue)
    ("cxx"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-blue)
    ("hh"           emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-purple)
    ("hpp"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-purple)
    ("hxx"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face emacs-nerd-icons-purple)
    ;; Lisps
    ;; ("cl"           emacs-nerd-icons-devicon "clisp") TODO: clisp
    ;; ("l"            emacs-nerd-icons-devicon "lisp") TODO: lisp
    ;; ("lisp"         emacs-nerd-icons-devicon "lisp") TODO: lisp
    ;; ("hy"           emacs-nerd-icons-devicon "hy") TODO: hy
    ("el"           emacs-nerd-icons-sucicon "nf-custom-emacs" :face emacs-nerd-icons-purple)
    ("clj"          emacs-nerd-icons-devicon "nf-dev-clojure" :face emacs-nerd-icons-blue)
    ("cljc"         emacs-nerd-icons-devicon "nf-dev-clojure" :face emacs-nerd-icons-blue)
    ;; ("cljs"         emacs-nerd-icons-devicon "cljs") TODO: cljs
    ("coffee"       emacs-nerd-icons-devicon "nf-dev-coffeescript" :face emacs-nerd-icons-maroon)
    ("iced"         emacs-nerd-icons-devicon "nf-dev-coffeescript" :face emacs-nerd-icons-lmaroon)
    ("dart"         emacs-nerd-icons-devicon "nf-dev-dart" :face emacs-nerd-icons-blue)
    ;; ("rkt"          emacs-nerd-icons-devicon "racket") TODO: racket
    ;; ("scrbl"        emacs-nerd-icons-devicon "racket") TODO: racket
    ;; Stylesheeting
    ("css"          emacs-nerd-icons-devicon "nf-dev-css3" :face emacs-nerd-icons-yellow)
    ("scss"         emacs-nerd-icons-mdicon "nf-md-sass" :face emacs-nerd-icons-pink)
    ("sass"         emacs-nerd-icons-mdicon "nf-md-sass" :face emacs-nerd-icons-dpink)
    ("less"         emacs-nerd-icons-devicon "nf-dev-less" :face emacs-nerd-icons-dyellow)
    ;; ("postcss"      emacs-nerd-icons-devicon "postcss") TODO: postcss
    ;; ("sss"          emacs-nerd-icons-devicon "postcss") TODO: postcss
    ("styl"         emacs-nerd-icons-devicon "nf-dev-stylus" :face emacs-nerd-icons-lgreen)
    ("csv" emacs-nerd-icons-octicon "nf-oct-graph" :face emacs-nerd-icons-dblue)
    ;; haskell
    ("hs"           emacs-nerd-icons-devicon "nf-dev-haskell" :face emacs-nerd-icons-red)
    ("chs"          emacs-nerd-icons-devicon "nf-dev-haskell" :face emacs-nerd-icons-red)
    ("lhs"          emacs-nerd-icons-devicon "nf-dev-haskell" :face emacs-nerd-icons-red)
    ("hsc"          emacs-nerd-icons-devicon "nf-dev-haskell" :face emacs-nerd-icons-red)
    ;; Web modes
    ("inky-haml"    emacs-nerd-icons-sucicon "nf-seti-haml" :face emacs-nerd-icons-lyellow)
    ("haml"         emacs-nerd-icons-sucicon "nf-seti-haml" :face emacs-nerd-icons-lyellow)
    ("htm"          emacs-nerd-icons-devicon "nf-dev-html5" :face emacs-nerd-icons-orange)
    ("html"         emacs-nerd-icons-devicon "nf-dev-html5" :face emacs-nerd-icons-orange)
    ("inky-er"      emacs-nerd-icons-devicon "nf-dev-html5" :face emacs-nerd-icons-lred)
    ("inky-erb"     emacs-nerd-icons-devicon "nf-dev-html5" :face emacs-nerd-icons-lred)
    ("erb"          emacs-nerd-icons-devicon "nf-dev-html5" :face emacs-nerd-icons-lred)
    ;; ("hbs"          emacs-nerd-icons-fileicon "moustache") TODO: moustache
    ("inky-slim" emacs-nerd-icons-octicon "nf-oct-dashboard" :face emacs-nerd-icons-yellow)
    ("slim" emacs-nerd-icons-octicon "nf-oct-dashboard" :face emacs-nerd-icons-yellow)
    ("jade"         emacs-nerd-icons-sucicon "nf-seti-jade" :face emacs-nerd-icons-red)
    ("pug"          emacs-nerd-icons-sucicon "nf-seti-pug" :face emacs-nerd-icons-red)
    ;; Javascript
    ;; ("d3js"         emacs-nerd-icons-devicon "d3") TODO: d3
    ("re"           emacs-nerd-icons-sucicon "nf-seti-reasonml" :face emacs-nerd-icons-red-alt)
    ("rei"          emacs-nerd-icons-sucicon "nf-seti-reasonml" :face emacs-nerd-icons-dred)
    ("ml"           emacs-nerd-icons-sucicon "nf-seti-ocaml" :face emacs-nerd-icons-lpink)
    ("mli"          emacs-nerd-icons-sucicon "nf-seti-ocaml" :face emacs-nerd-icons-dpink)
    ("react"        emacs-nerd-icons-devicon "nf-dev-react" :face emacs-nerd-icons-lblue)
    ("ts"           emacs-nerd-icons-sucicon "nf-seti-typescript" :face emacs-nerd-icons-blue-alt)
    ("js"           emacs-nerd-icons-devicon "nf-dev-javascript" :face emacs-nerd-icons-yellow)
    ("es"           emacs-nerd-icons-devicon "nf-dev-javascript" :face emacs-nerd-icons-yellow)
    ("jsx"          emacs-nerd-icons-devicon "nf-dev-javascript" :face emacs-nerd-icons-cyan-alt)
    ("tsx"          emacs-nerd-icons-devicon "nf-dev-javascript" :face emacs-nerd-icons-cyan-alt)
    ("njs"          emacs-nerd-icons-mdicon "nf-md-nodejs" :face emacs-nerd-icons-lgreen)
    ("vue"          emacs-nerd-icons-sucicon "nf-seti-vue" :face emacs-nerd-icons-lgreen)

    ("sbt"          emacs-nerd-icons-sucicon   "nf-seti-sbt" :face emacs-nerd-icons-red)
    ("scala"        emacs-nerd-icons-devicon "nf-dev-scala" :face emacs-nerd-icons-red)
    ("scm"          emacs-nerd-icons-mdicon   "nf-md-lambda" :face emacs-nerd-icons-red)
    ("swift"        emacs-nerd-icons-devicon "nf-dev-swift" :face emacs-nerd-icons-green)

    ("tcl"          emacs-nerd-icons-mdicon "nf-md-feather" :face emacs-nerd-icons-dred)

    ("tf"           emacs-nerd-icons-mdicon "nf-md-terraform" :face emacs-nerd-icons-purple-alt)
    ("tfvars"       emacs-nerd-icons-mdicon "nf-md-terraform" :face emacs-nerd-icons-purple-alt)
    ("tfstate"      emacs-nerd-icons-mdicon "nf-md-terraform" :face emacs-nerd-icons-purple-alt)

    ("asm"          emacs-nerd-icons-sucicon "nf-seti-asm" :face emacs-nerd-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS     ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ;; ("v"            emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("vams"         emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("sv"           emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("sva"          emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("svh"          emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("svams"        emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; VHDL(-AMS     ;; VHDL(-AMS)
    ("vhd"          emacs-nerd-icons-mdicon "nf-md-xml" :face emacs-nerd-icons-blue)
    ("vhdl"         emacs-nerd-icons-mdicon "nf-md-xml" :face emacs-nerd-icons-blue)
    ("vhms"         emacs-nerd-icons-mdicon "nf-md-xml" :face emacs-nerd-icons-blue)
    ;; Cabal
    ;; ("cabal"        emacs-nerd-icons-devicon "cabal") TODO: cabal
    ;; Kotlin
    ("kt"           emacs-nerd-icons-sucicon "nf-seti-kotlin" :face emacs-nerd-icons-orange)
    ("kts"          emacs-nerd-icons-sucicon "nf-seti-kotlin" :face emacs-nerd-icons-orange)
    ;; Nimrod
    ("nim"          emacs-nerd-icons-sucicon "nf-seti-nim" :face emacs-nerd-icons-yellow)
    ("nims"         emacs-nerd-icons-sucicon "nf-seti-nim" :face emacs-nerd-icons-yellow)
    ;; SQL
    ("sql" emacs-nerd-icons-octicon "nf-oct-database" :face emacs-nerd-icons-silver)
    ;; Styles
    ;; ("styles"       emacs-nerd-icons-devicon "style") TODO: style
    ;; Lua
    ("lua"          emacs-nerd-icons-sucicon "nf-seti-lua" :face emacs-nerd-icons-dblue)
    ;; ASCII doc
    ;; ("adoc"         emacs-nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; ("asciidoc"     emacs-nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; Puppet
    ("pp"           emacs-nerd-icons-sucicon "nf-seti-puppet" :face emacs-nerd-icons-yellow)
    ;; Jinja
    ("j2"           emacs-nerd-icons-sucicon "nf-seti-jinja" :face emacs-nerd-icons-silver)
    ("jinja2"       emacs-nerd-icons-sucicon "nf-seti-jinja" :face emacs-nerd-icons-silver)
    ;; Docker
    ("dockerfile"   emacs-nerd-icons-sucicon "nf-seti-docker" :face emacs-nerd-icons-cyan)
    ;; Vagrant
    ;; ("vagrantfile"  emacs-nerd-icons-fileicon "vagrant") TODO: vagrant
    ;; GLSL
    ("glsl"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-blue)
    ("vert"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-blue)
    ("tesc"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-purple)
    ("tese"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-dpurple)
    ("geom"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-green)
    ("frag"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-red)
    ("comp"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face emacs-nerd-icons-dblue)
    ;; CUDA
    ("cu"       emacs-nerd-icons-sucicon "nf-custom-c" :face emacs-nerd-icons-green)
    ("cuh"      emacs-nerd-icons-faicon "nf-fa-h_square" :face emacs-nerd-icons-green)
    ;; Fortran
    ("f90"      emacs-nerd-icons-mdicon "nf-md-language_fortran" :face emacs-nerd-icons-purple)
    ;; C#
    ("cs"           emacs-nerd-icons-mdicon "nf-md-language_csharp" :face emacs-nerd-icons-dblue)
    ("csx"          emacs-nerd-icons-mdicon "nf-md-language_csharp" :face emacs-nerd-icons-dblue)
    ;; F#
    ("fs" emacs-nerd-icons-devicon "nf-dev-fsharp" :face emacs-nerd-icons-blue-alt)
    ("fsi" emacs-nerd-icons-devicon "nf-dev-fsharp" :face emacs-nerd-icons-blue-alt)
    ("fsx" emacs-nerd-icons-devicon "nf-dev-fsharp" :face emacs-nerd-icons-blue-alt)
    ("fsscript" emacs-nerd-icons-devicon "nf-dev-fsharp" :face emacs-nerd-icons-blue-alt)
    ;; zig
    ("zig"          emacs-nerd-icons-sucicon "nf-seti-zig" :face emacs-nerd-icons-orange)
    ;; odin
    ;; ("odin"         emacs-nerd-icons-fileicon "odin") TODO: odin
    ;; File Types
    ("ico" emacs-nerd-icons-octicon "nf-oct-file_media" :face emacs-nerd-icons-blue)
    ("png" emacs-nerd-icons-mdicon "nf-md-file_png_box" :face emacs-nerd-icons-orange)
    ("gif" emacs-nerd-icons-mdicon "nf-md-file_gif_box" :face emacs-nerd-icons-green)
    ("jpeg" emacs-nerd-icons-mdicon "nf-md-file_jpg_box" :face emacs-nerd-icons-dblue)
    ("jpg" emacs-nerd-icons-mdicon "nf-md-file_jpg_box" :face emacs-nerd-icons-dblue)
    ("webp" emacs-nerd-icons-octicon "nf-oct-file_media" :face emacs-nerd-icons-dblue)
    ;; Audio
    ("mp3" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("wav" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("m4a" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("ogg" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("flac" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("opus" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("au" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("aif" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("aifc" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("aiff" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-dred)
    ("svg" emacs-nerd-icons-sucicon "nf-seti-svg" :face emacs-nerd-icons-lgreen)
    ;; Video
    ("mov" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("mp4" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("ogv" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-dblue)
    ("mpg" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("mpeg" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("flv" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("ogv" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-dblue)
    ("mkv" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ("webm" emacs-nerd-icons-faicon "nf-fa-film" :face emacs-nerd-icons-blue)
    ;; Fonts
    ("ttf"          emacs-nerd-icons-faicon "nf-fa-font" :face emacs-nerd-icons-dcyan)
    ("woff"         emacs-nerd-icons-faicon "nf-fa-font" :face emacs-nerd-icons-cyan)
    ("woff2"        emacs-nerd-icons-faicon "nf-fa-font" :face emacs-nerd-icons-cyan)
    ;; Archives
    ("tar" emacs-nerd-icons-mdicon "nf-md-zip_box" :face emacs-nerd-icons-orange)
    ("rar" emacs-nerd-icons-mdicon "nf-md-zip_box" :face emacs-nerd-icons-orange)
    ("tgz" emacs-nerd-icons-mdicon "nf-md-zip_box" :face emacs-nerd-icons-orange)
    ;; Doc
    ("pdf" emacs-nerd-icons-octicon "nf-oct-file_pdf" :face emacs-nerd-icons-dred)
    ("text" emacs-nerd-icons-octicon "nf-oct-file_text" :face emacs-nerd-icons-cyan)
    ("txt" emacs-nerd-icons-octicon "nf-oct-file_text" :face emacs-nerd-icons-cyan)
    ("doc"          emacs-nerd-icons-mdicon "nf-md-file_word" :face emacs-nerd-icons-blue)
    ("docx"         emacs-nerd-icons-mdicon "nf-md-file_word" :face emacs-nerd-icons-blue)
    ("docm"         emacs-nerd-icons-mdicon "nf-md-file_word" :face emacs-nerd-icons-blue)
    ("texi"         emacs-nerd-icons-mdicon "nf-md-format_text" :face emacs-nerd-icons-lred)
    ("tex"          emacs-nerd-icons-mdicon "nf-md-format_text" :face emacs-nerd-icons-lred)
    ("md" emacs-nerd-icons-octicon "nf-oct-markdown" :face emacs-nerd-icons-lblue)
    ;; ("bib"          emacs-nerd-icons-fileicon "bib") TODO: bib
    ("org"          emacs-nerd-icons-sucicon "nf-custom-orgmode" :face emacs-nerd-icons-lgreen)
    ("pps"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face emacs-nerd-icons-orange)
    ("ppt"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face emacs-nerd-icons-orange)
    ("pptsx"        emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face emacs-nerd-icons-orange)
    ("ppttx"        emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face emacs-nerd-icons-orange)
    ("knt"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face emacs-nerd-icons-cyan)
    ("xlsx"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face emacs-nerd-icons-dgreen)
    ("xlsm"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face emacs-nerd-icons-dgreen)
    ("xlsb"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face emacs-nerd-icons-dgreen)
    ("xltx"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face emacs-nerd-icons-dgreen)
    ("xltm"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face emacs-nerd-icons-dgreen)
    ("ly" emacs-nerd-icons-faicon "nf-fa-music" :face emacs-nerd-icons-green)
    ;;
    ("key" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-lblue)
    ("pem" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-orange)
    ("p12" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-dorange)
    ("crt" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-lblue)
    ("pub" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-blue)
    ("gpg" emacs-nerd-icons-octicon "nf-oct-key" :face emacs-nerd-icons-lblue)
    ("cache" emacs-nerd-icons-octicon "nf-oct-database" :face emacs-nerd-icons-green)
    ))

(defvar emacs-nerd-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"           emacs-nerd-icons-octicon "nf-oct-tag"                     :face emacs-nerd-icons-blue)
    ("^TODO$"           emacs-nerd-icons-octicon "nf-oct-checklist"               :face emacs-nerd-icons-lyellow)
    ("^LICENSE$"        emacs-nerd-icons-octicon "nf-oct-book"                    :face emacs-nerd-icons-blue)
    ("^readme"          emacs-nerd-icons-octicon "nf-oct-book"                    :face emacs-nerd-icons-lcyan)

    ;; Config
    ("nginx$"            emacs-nerd-icons-devicon "nf-dev-nginx"                 :face emacs-nerd-icons-dgreen)
    ;; ("apache$"           emacs-nerd-icons-alltheicon "apache") TODO: apache

    ;; C
    ("^Makefile$"       emacs-nerd-icons-sucicon "nf-seti-makefile"                  :face emacs-nerd-icons-dorange)
    ("^CMakeLists.txt$" emacs-nerd-icons-sucicon "nf-seti-makefile"                  :face emacs-nerd-icons-red) ;; TODO: cmake
    ("^CMakeCache.txt$" emacs-nerd-icons-sucicon "nf-seti-makefile"                  :face emacs-nerd-icons-blue) ;; TODO: cmakecache
    ("^meson.build$"    emacs-nerd-icons-sucicon "nf-seti-makefile"                  :face emacs-nerd-icons-purple) ;; TODO: meson
    ("^meson_options.txt$" emacs-nerd-icons-sucicon "nf-seti-makefile"               :face emacs-nerd-icons-purple) ;; TODO: meson

    ;; Docker
    ("^\\.?Dockerfile"  emacs-nerd-icons-sucicon "nf-seti-docker"             :face emacs-nerd-icons-blue)

    ;; Homebrew
    ("^Brewfile$"       emacs-nerd-icons-faicon "nf-fa-beer"                     :face emacs-nerd-icons-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"   emacs-nerd-icons-devicon "nf-dev-aws"                  :face emacs-nerd-icons-orange)
    ("^serverless\\.yml$" emacs-nerd-icons-faicon "nf-fa-bolt"                   :face emacs-nerd-icons-yellow)

    ;; lock files
    ("~$"               emacs-nerd-icons-octicon "nf-oct-lock"                    :face emacs-nerd-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"       emacs-nerd-icons-sucicon "nf-seti-elixir"               :face emacs-nerd-icons-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$" emacs-nerd-icons-octicon "nf-oct-ruby"      :face emacs-nerd-icons-red)
    ("_?test\\.rb$"        emacs-nerd-icons-octicon "nf-oct-ruby"           :face emacs-nerd-icons-red)
    ("_?test_helper\\.rb$" emacs-nerd-icons-octicon "nf-oct-ruby"           :face emacs-nerd-icons-dred)
    ("_?spec\\.rb$"        emacs-nerd-icons-octicon "nf-oct-ruby"           :face emacs-nerd-icons-red)
    ("_?spec_helper\\.rb$" emacs-nerd-icons-octicon "nf-oct-ruby"           :face emacs-nerd-icons-dred)

    ("-?spec\\.ts$"     emacs-nerd-icons-mdicon "nf-md-language_typescript"        :face emacs-nerd-icons-blue)
    ("-?test\\.ts$"     emacs-nerd-icons-mdicon "nf-md-language_typescript"        :face emacs-nerd-icons-blue)
    ("-?spec\\.js$"     emacs-nerd-icons-mdicon "nf-md-language_javascript"                :face emacs-nerd-icons-lpurple)
    ("-?test\\.js$"     emacs-nerd-icons-mdicon "nf-md-language_javascript"                :face emacs-nerd-icons-lpurple)
    ("-?spec\\.jsx$"    emacs-nerd-icons-mdicon "nf-md-react"             :face emacs-nerd-icons-blue-alt)
    ("-?test\\.jsx$"    emacs-nerd-icons-mdicon "nf-md-react"             :face emacs-nerd-icons-blue-alt)

    ;; Git
    ("^MERGE_"          emacs-nerd-icons-octicon "nf-oct-git_merge"               :face emacs-nerd-icons-red)
    ("^COMMIT_EDITMSG"  emacs-nerd-icons-octicon "nf-oct-git_commit"              :face emacs-nerd-icons-red)

    ;; Stylesheeting
    ("stylelint"        emacs-nerd-icons-sucicon "nf-seti-stylelint"              :face emacs-nerd-icons-lyellow)

    ;; JavaScript
    ("^package.json$"   emacs-nerd-icons-devicon "nf-dev-npm"                    :face emacs-nerd-icons-red)
    ("^package.lock.json$" emacs-nerd-icons-devicon "nf-dev-npm"                 :face emacs-nerd-icons-dred)
    ("^yarn\\.lock"     emacs-nerd-icons-sucicon "nf-seti-yarn"                   :face emacs-nerd-icons-blue-alt)
    ("\\.npmignore$"    emacs-nerd-icons-devicon "nf-dev-npm"                    :face emacs-nerd-icons-dred)
    ("^bower.json$"     emacs-nerd-icons-devicon "nf-dev-bower"                :face emacs-nerd-icons-lorange)
    ("^gulpfile"        emacs-nerd-icons-devicon "nf-dev-gulp"                 :face emacs-nerd-icons-lred)
    ("^gruntfile"       emacs-nerd-icons-devicon "nf-dev-grunt"                :face emacs-nerd-icons-lyellow)
    ("^webpack"         emacs-nerd-icons-mdicon "nf-md-webpack"                :face emacs-nerd-icons-lblue)

    ;; Go
    ("^go.mod$"         emacs-nerd-icons-sucicon "nf-seti-config"              :face emacs-nerd-icons-blue-alt)
    ("^go.work$"        emacs-nerd-icons-sucicon "nf-seti-config"              :face emacs-nerd-icons-blue-alt)

    ;; Emacs
    ("bookmark"         emacs-nerd-icons-octicon "nf-oct-bookmark"                :face emacs-nerd-icons-lpink)

    ("^\\*scratch\\*$"  emacs-nerd-icons-faicon "nf-fa-sticky_note"              :face emacs-nerd-icons-lyellow)
    ("^\\*scratch.*"    emacs-nerd-icons-faicon "nf-fa-sticky_note"              :face emacs-nerd-icons-yellow)
    ("^\\*new-tab\\*$"  emacs-nerd-icons-mdicon "nf-md-star"                   :face emacs-nerd-icons-cyan)

    ("^\\."             emacs-nerd-icons-octicon "nf-oct-gear"                 )
    ))

(defvar emacs-nerd-icons-default-file-icon
  '(emacs-nerd-icons-faicon "nf-fa-file_o")
  )

(defvar emacs-nerd-icons-dir-icon-alist
  '(
    ("trash"            emacs-nerd-icons-faicon "nf-fa-trash_o")
    ("dropbox"          emacs-nerd-icons-faicon "nf-fa-dropbox")
    ("google[ _-]drive" emacs-nerd-icons-mdicon "nf-md-folder_google_drive")
    ("^atom$"           emacs-nerd-icons-devicon "nf-dev-atom")
    ("documents"        emacs-nerd-icons-mdicon "nf-md-folder_file")
    ("download"         emacs-nerd-icons-mdicon "nf-md-folder_download")
    ("desktop"          emacs-nerd-icons-octicon "nf-oct-device_desktop")
    ("pictures"         emacs-nerd-icons-mdicon "nf-md-folder_image")
    ("photos"           emacs-nerd-icons-faicon "nf-fa-camera_retro")
    ("music"            emacs-nerd-icons-mdicon "nf-md-folder_music")
    ("movies"           emacs-nerd-icons-faicon "nf-fa-film")
    ("code"             emacs-nerd-icons-octicon "nf-oct-code")
    ("workspace"        emacs-nerd-icons-octicon "nf-oct-code")
    ;; ("test"             emacs-nerd-icons-devicon "test-dir")
    ("\\.git"           emacs-nerd-icons-devicon "nf-dev-git")
    (".?"               emacs-nerd-icons-octicon "nf-oct-file_directory")
    ))

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

(defvar emacs-nerd-icons-mode-icon-alist
  '(
    (emacs-lisp-mode           emacs-nerd-icons-sucicon "nf-custom-emacs"                :face emacs-nerd-icons-purple)
    (circe-server-mode         emacs-nerd-icons-faicon "nf-fa-commenting_o"          )
    (circe-channel-mode        emacs-nerd-icons-faicon "nf-fa-commenting_o"          )
    (circe-query-mode          emacs-nerd-icons-faicon "nf-fa-commenting_o"          )
    (crystal-mode              emacs-nerd-icons-sucicon "nf-custom-crystal"             :face emacs-nerd-icons-yellow)
    (erc-mode                  emacs-nerd-icons-faicon "nf-fa-commenting_o"          )
    (inferior-emacs-lisp-mode  emacs-nerd-icons-sucicon "nf-custom-emacs"                :face emacs-nerd-icons-lblue)
    (dired-mode                emacs-nerd-icons-octicon "nf-oct-file_directory"      )
    ;; (lisp-interaction-mode     emacs-nerd-icons-fileicon "lisp"                :face emacs-nerd-icons-orange)
    ;; (sly-mrepl-mode            emacs-nerd-icons-fileicon "clisp"                :face emacs-nerd-icons-orange)
    ;; (slime-repl-mode           emacs-nerd-icons-fileicon "clisp"                :face emacs-nerd-icons-orange)
    (org-mode                  emacs-nerd-icons-sucicon "nf-custom-orgmode"                 :face emacs-nerd-icons-lgreen)
    (typescript-mode           emacs-nerd-icons-mdicon "nf-md-language_typescript"          :face emacs-nerd-icons-blue-alt)
    (typescript-ts-mode        emacs-nerd-icons-mdicon "nf-md-language_typescript"          :face emacs-nerd-icons-blue-alt)
    (typescript-tsx-mode       emacs-nerd-icons-sucicon "nf-md-language_typescript"                 :face emacs-nerd-icons-cyan-alt)
    (tsx-ts-mode               emacs-nerd-icons-sucicon "nf-md-language_typescript"                 :face emacs-nerd-icons-cyan-alt)
    (js-mode                   emacs-nerd-icons-devicon "nf-dev-javascript"        :face emacs-nerd-icons-yellow)
    (js-jsx-mode               emacs-nerd-icons-devicon "nf-dev-javascript"        :face emacs-nerd-icons-yellow)
    (js2-mode                  emacs-nerd-icons-devicon "nf-dev-javascript"        :face emacs-nerd-icons-yellow)
    (js3-mode                  emacs-nerd-icons-devicon "nf-dev-javascript"        :face emacs-nerd-icons-yellow)
    (rjsx-mode                 emacs-nerd-icons-devicon "nf-dev-javascript"               :face emacs-nerd-icons-cyan-alt)
    (term-mode                 emacs-nerd-icons-devicon "nf-dev-terminal"            )
    (vterm-mode                emacs-nerd-icons-devicon "nf-dev-terminal"            )
    (eshell-mode               emacs-nerd-icons-devicon "nf-dev-terminal"             :face emacs-nerd-icons-purple)
    (magit-refs-mode           emacs-nerd-icons-devicon "nf-dev-git_branch"           :face emacs-nerd-icons-red)
    (magit-process-mode        emacs-nerd-icons-octicon "nf-oct-mark_github"         )
    (magit-diff-mode           emacs-nerd-icons-devicon "nf-dev-git_compare"          :face emacs-nerd-icons-lblue)
    (ediff-mode                emacs-nerd-icons-devicon "nf-dev-git_compare"          :Face emacs-nerd-icons-red)
    (comint-mode               emacs-nerd-icons-faicon "nf-fa-terminal"              :face emacs-nerd-icons-lblue)
    (eww-mode                  emacs-nerd-icons-faicon "nf-fa-firefox"               :face emacs-nerd-icons-red)
    (org-agenda-mode           emacs-nerd-icons-octicon "nf-oct-checklist"            :face emacs-nerd-icons-lgreen)
    (cfw:calendar-mode         emacs-nerd-icons-octicon "nf-oct-calendar"            )
    (ibuffer-mode              emacs-nerd-icons-faicon "nf-fa-files_o"               :face emacs-nerd-icons-dsilver)
    (messages-buffer-mode      emacs-nerd-icons-faicon "nf-fa-file_o"                :face emacs-nerd-icons-dsilver)
    (help-mode                 emacs-nerd-icons-faicon "nf-fa-info"                  :face emacs-nerd-icons-purple)
    (benchmark-init/tree-mode  emacs-nerd-icons-octicon "nf-oct-dashboard"           )
    (jenkins-mode              emacs-nerd-icons-devicon "nf-dev-jenkins"            :face emacs-nerd-icons-blue)
    (magit-popup-mode          emacs-nerd-icons-sucicon "nf-seti-git"              :face emacs-nerd-icons-red)
    (magit-status-mode         emacs-nerd-icons-sucicon "nf-seti-git"              :face emacs-nerd-icons-lred)
    (magit-log-mode            emacs-nerd-icons-sucicon "nf-seti-git"              :face emacs-nerd-icons-green)
    (mu4e-compose-mode         emacs-nerd-icons-octicon "nf-oct-pencil"              )
    (mu4e-headers-mode         emacs-nerd-icons-octicon "nf-oct-mail"                )
    (mu4e-main-mode            emacs-nerd-icons-octicon "nf-oct-mail"                )
    (mu4e-view-mode            emacs-nerd-icons-octicon "nf-oct-mail_read"           )
    (sieve-mode                emacs-nerd-icons-octicon "nf-oct-mail"                )
    (gnus-group-mode           emacs-nerd-icons-octicon "nf-oct-mail"                )
    (gnus-summary-mode         emacs-nerd-icons-octicon "nf-oct-mail"                )
    (gnus-article-mode         emacs-nerd-icons-octicon "nf-oct-mail_read"           )
    (message-mode              emacs-nerd-icons-octicon "nf-oct-pencil"              )
    (package-menu-mode         emacs-nerd-icons-faicon "nf-fa-archive"                :face emacs-nerd-icons-silver)
    (paradox-menu-mode         emacs-nerd-icons-faicon "nf-fa-archive"                :face emacs-nerd-icons-silver)
    (Custom-mode               emacs-nerd-icons-octicon "nf-oct-settings"            )

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode             emacs-nerd-icons--web-mode-icon)

    (fundamental-mode                   emacs-nerd-icons-sucicon "nf-custom-emacs"              :face emacs-nerd-icons-dsilver)
    (special-mode                       emacs-nerd-icons-sucicon "nf-custom-emacs"              :face emacs-nerd-icons-yellow)
    (text-mode                          emacs-nerd-icons-octicon "nf-oct-file_text"          :face emacs-nerd-icons-cyan)
    (enh-ruby-mode                      emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-lred)
    (ruby-mode                          emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-lred)
    (ruby-ts-mode                       emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-lred)
    (inf-ruby-mode                      emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-red)
    (projectile-rails-compilation-mode  emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-red)
    (rspec-compilation-mode             emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-red)
    (rake-compilation-mode              emacs-nerd-icons-devicon "nf-dev-ruby"       :face emacs-nerd-icons-red)
    (sh-mode                            emacs-nerd-icons-devicon "nf-dev-terminal"       :face emacs-nerd-icons-purple)
    (shell-mode                         emacs-nerd-icons-devicon "nf-dev-terminal"       :face emacs-nerd-icons-purple)
    (fish-mode                          emacs-nerd-icons-devicon "nf-dev-terminal"       :face emacs-nerd-icons-lpink)
    (nginx-mode                         emacs-nerd-icons-devicon "nf-dev-nginx"              :face emacs-nerd-icons-dgreen)
    ;; (apache-mode                        emacs-nerd-icons-alltheicon "apache"           :face emacs-nerd-icons-dgreen)
    (makefile-mode                      emacs-nerd-icons-devicon "nf-dev-gnu"              :face emacs-nerd-icons-dorange)
    ;; (cmake-mode                         emacs-nerd-icons-fileicon "cmake"            :face emacs-nerd-icons-red)
    ;; (cmake-ts-mode                      emacs-nerd-icons-fileicon "cmake"            :face emacs-nerd-icons-red)
    (dockerfile-mode                    emacs-nerd-icons-sucicon "nf-seti-docker"       :face emacs-nerd-icons-blue)
    (dockerfile-ts-mode                 emacs-nerd-icons-sucicon "nf-seti-docker"       :face emacs-nerd-icons-blue)
    (docker-compose-mode                emacs-nerd-icons-sucicon "nf-seti-docker"       :face emacs-nerd-icons-lblue)
    (nxml-mode                          emacs-nerd-icons-faicon "nf-fa-file_code_o"         :face emacs-nerd-icons-lorange)
    (json-mode                          emacs-nerd-icons-octicon "nf-oct-settings"          :face emacs-nerd-icons-yellow)
    (json-ts-mode                       emacs-nerd-icons-octicon "nf-oct-settings"          :face emacs-nerd-icons-yellow)
    (jsonian-mode                       emacs-nerd-icons-octicon "nf-oct-settings"          :face emacs-nerd-icons-yellow)
    (yaml-mode                          emacs-nerd-icons-octicon "nf-oct-settings"           :face emacs-nerd-icons-dyellow)
    (yaml-ts-mode                       emacs-nerd-icons-octicon "nf-oct-settings"           :face emacs-nerd-icons-dyellow)
    (elisp-byte-code-mode               emacs-nerd-icons-octicon "nf-oct-file_binary"        :face emacs-nerd-icons-dsilver)
    (archive-mode                       emacs-nerd-icons-octicon "nf-oct-file_zip"           :face emacs-nerd-icons-lmaroon)
    (elm-mode                           emacs-nerd-icons-sucicon "nf-custom-elm"              :face emacs-nerd-icons-blue)
    (erlang-mode                        emacs-nerd-icons-devicon "nf-dev-erlang"         :face emacs-nerd-icons-red  )
    (elixir-mode                        emacs-nerd-icons-sucicon "nf-custom-elixir"         :face emacs-nerd-icons-lorange  )
    (java-mode                          emacs-nerd-icons-devicon "nf-dev-java"            :face emacs-nerd-icons-purple)
    (java-ts-mode                       emacs-nerd-icons-devicon "nf-dev-java"            :face emacs-nerd-icons-purple)
    (go-mode                            emacs-nerd-icons-devicon "nf-dev-go"                :face emacs-nerd-icons-blue)
    (go-ts-mode                         emacs-nerd-icons-devicon "nf-dev-go"                :face emacs-nerd-icons-blue)
    (go-dot-mod-mode                    emacs-nerd-icons-sucicon "nf-seti-config"         :face emacs-nerd-icons-blue-alt)
    (go-dot-work-mode                   emacs-nerd-icons-sucicon "nf-seti-config"         :face emacs-nerd-icons-blue-alt)
    (graphql-mode                       emacs-nerd-icons-sucicon "nf-seti-graphql"          :face emacs-nerd-icons-dpink)
    ;; (matlab-mode                        emacs-nerd-icons-fileicon "matlab"           :face emacs-nerd-icons-orange)
    (nix-mode                           emacs-nerd-icons-mdicon "nf-md-nix"              :face emacs-nerd-icons-blue)
    (perl-mode                          emacs-nerd-icons-devicon "nf-dev-perl"           :face emacs-nerd-icons-lorange)
    (cperl-mode                         emacs-nerd-icons-devicon "nf-dev-perl"           :face emacs-nerd-icons-lorange)
    (php-mode                           emacs-nerd-icons-devicon "nf-dev-php"              :face emacs-nerd-icons-lsilver)
    (prolog-mode                        emacs-nerd-icons-devicon "nf-dev-prolog"           :face emacs-nerd-icons-lmaroon)
    (python-mode                        emacs-nerd-icons-devicon "nf-dev-python"           :face emacs-nerd-icons-dblue)
    (python-ts-mode                     emacs-nerd-icons-devicon "nf-dev-python"           :face emacs-nerd-icons-dblue)
    (inferior-python-mode               emacs-nerd-icons-devicon "nf-dev-python"           :face emacs-nerd-icons-dblue)
    ;; (racket-mode                        emacs-nerd-icons-fileicon "racket"            :face emacs-nerd-icons-red)
    (rust-mode                          emacs-nerd-icons-devicon "nf-dev-rust"             :face emacs-nerd-icons-maroon)
    (rustic-mode                        emacs-nerd-icons-devicon "nf-dev-rust"             :face emacs-nerd-icons-maroon)
    (rust-ts-mode                       emacs-nerd-icons-devicon "nf-dev-rust"             :face emacs-nerd-icons-maroon)
    (scala-mode                         emacs-nerd-icons-devicon "nf-dev-scala"          :face emacs-nerd-icons-red)
    ;; (scheme-mode                        emacs-nerd-icons-fileicon   "scheme"          :face emacs-nerd-icons-red)
    (swift-mode                         emacs-nerd-icons-devicon "nf-dev-swift"            :face emacs-nerd-icons-green)
    (svelte-mode                        emacs-nerd-icons-sucicon "nf-seti-svelte"            :face emacs-nerd-icons-red)
    (c-mode                             emacs-nerd-icons-sucicon "nf-custom-c"         :face emacs-nerd-icons-blue)
    (c-ts-mode                          emacs-nerd-icons-sucicon "nf-custom-c"         :face emacs-nerd-icons-blue)
    (c++-mode                           emacs-nerd-icons-sucicon "nf-custom-cpp"  :face emacs-nerd-icons-blue)
    (c++-ts-mode                        emacs-nerd-icons-sucicon "nf-custom-cpp"  :face emacs-nerd-icons-blue)
    (csharp-mode                        emacs-nerd-icons-mdicon "nf-md-language_csharp"    :face emacs-nerd-icons-dblue)
    (csharp-ts-mode                     emacs-nerd-icons-mdicon "nf-md-language_csharp"    :face emacs-nerd-icons-dblue)
    (clojure-mode                       emacs-nerd-icons-devicon "nf-dev-clojure_alt"          :face emacs-nerd-icons-blue)
    (cider-repl-mode                    emacs-nerd-icons-devicon "nf-dev-clojure_alt"          :face emacs-nerd-icons-green)
    (clojurescript-mode                 emacs-nerd-icons-devicon "nf-dev-clojure"               :face emacs-nerd-icons-dblue)
    (coffee-mode                        emacs-nerd-icons-devicon "nf-dev-coffeescript"     :face emacs-nerd-icons-maroon)
    ;; (lisp-mode                          emacs-nerd-icons-fileicon "lisp"             :face emacs-nerd-icons-orange)
    (css-mode                           emacs-nerd-icons-devicon "nf-dev-css3"           :face emacs-nerd-icons-yellow)
    (css-ts-mode                        emacs-nerd-icons-devicon "nf-dev-css3"           :face emacs-nerd-icons-yellow)
    (scss-mode                          emacs-nerd-icons-mdicon "nf-md-sass"           :face emacs-nerd-icons-pink)
    (sass-mode                          emacs-nerd-icons-mdicon "nf-md-sass"           :face emacs-nerd-icons-dpink)
    (less-css-mode                      emacs-nerd-icons-devicon "nf-dev-less"             :face emacs-nerd-icons-dyellow)
    (stylus-mode                        emacs-nerd-icons-devicon "nf-dev-stylus"         :face emacs-nerd-icons-lgreen)
    (csv-mode                           emacs-nerd-icons-octicon "nf-oct-graph"              :face emacs-nerd-icons-dblue)
    (haskell-mode                       emacs-nerd-icons-devicon "nf-dev-haskell"          :face emacs-nerd-icons-red)
    (haskell-c2hs-mode                  emacs-nerd-icons-devicon "nf-dev-haskell"          :face emacs-nerd-icons-red)
    (literate-haskell-mode              emacs-nerd-icons-devicon "nf-dev-haskell"          :face emacs-nerd-icons-red)
    (haml-mode                          emacs-nerd-icons-sucicon "nf-seti-haml"             :face emacs-nerd-icons-lyellow)
    (html-mode                          emacs-nerd-icons-devicon "nf-dev-html5"          :face emacs-nerd-icons-orange)
    (html-ts-mode                       emacs-nerd-icons-devicon "nf-dev-html5"          :face emacs-nerd-icons-orange)
    (rhtml-mode                         emacs-nerd-icons-devicon "nf-dev-html5"          :face emacs-nerd-icons-lred)
    ;; (mustache-mode                      emacs-nerd-icons-fileicon "moustache"        :face emacs-nerd-icons-green)
    (slim-mode                          emacs-nerd-icons-octicon "nf-oct-dashboard"          :face emacs-nerd-icons-yellow)
    (jade-mode                          emacs-nerd-icons-sucicon "nf-seti-jade"             :face emacs-nerd-icons-red)
    (pug-mode                           emacs-nerd-icons-sucicon "nf-seti-pug"              :face emacs-nerd-icons-red)
    (react-mode                         emacs-nerd-icons-devicon "nf-dev-react"            :face emacs-nerd-icons-lblue)
    (image-mode                         emacs-nerd-icons-octicon "nf-oct-file_media"         :face emacs-nerd-icons-blue)
    ;; (texinfo-mode                       emacs-nerd-icons-fileicon "tex"              :face emacs-nerd-icons-lred)
    (markdown-mode                      emacs-nerd-icons-octicon "nf-oct-markdown"           :face emacs-nerd-icons-lblue)
    ;; (bibtex-mode                        emacs-nerd-icons-fileicon "bib"              :face emacs-nerd-icons-maroon)
    (org-mode                           emacs-nerd-icons-sucicon "nf-custom-orgmode"              :face emacs-nerd-icons-lgreen)
    (compilation-mode                   emacs-nerd-icons-faicon "nf-fa-cogs"                )
    (objc-mode                          emacs-nerd-icons-faicon "nf-fa-apple"               )
    (tuareg-mode                        emacs-nerd-icons-sucicon "nf-seti-ocaml"             )
    (purescript-mode                    emacs-nerd-icons-sucicon "nf-seti-purescript"        )
    ;; (verilog-mode                       emacs-nerd-icons-fileicon "verilog"            :face emacs-nerd-icons-red)
    ;; (vhdl-mode                          emacs-nerd-icons-fileicon "vhdl"             :face emacs-nerd-icons-blue)
    ;; (haskell-cabal-mode                 emacs-nerd-icons-fileicon "cabal"            :face emacs-nerd-icons-lblue)
    (kotlin-mode                        emacs-nerd-icons-sucicon "nf-custom-kotlin"           :face emacs-nerd-icons-orange)
    (kotlin-ts-mode                     emacs-nerd-icons-sucicon "nf-custom-kotlin"           :face emacs-nerd-icons-orange)
    (nim-mode                           emacs-nerd-icons-sucicon "nf-seti-nim"           :face emacs-nerd-icons-yellow)
    (sql-mode                           emacs-nerd-icons-devicon  "nf-dev-database"         :face emacs-nerd-icons-silver)
    (lua-mode                           emacs-nerd-icons-sucicon "nf-seti-lua"              :face emacs-nerd-icons-dblue)
    ;; (adoc-mode                          emacs-nerd-icons-fileicon "asciidoc"         :face emacs-nerd-icons-lblue)
    (puppet-mode                        emacs-nerd-icons-sucicon "nf-custom-puppet"           :face emacs-nerd-icons-yellow)
    (jinja2-mode                        emacs-nerd-icons-sucicon "nf-seti-jinja"            :face emacs-nerd-icons-silver)
    (powershell-mode                    emacs-nerd-icons-mdicon "nf-md-powershell"       :face emacs-nerd-icons-blue)
    ;; (tex-mode                           emacs-nerd-icons-fileicon "tex"              :face emacs-nerd-icons-lred)
    ;; (latex-mode                         emacs-nerd-icons-fileicon "tex"              :face emacs-nerd-icons-lred)
    (dart-mode                          emacs-nerd-icons-devicon "nf-dev-dart"               :face emacs-nerd-icons-blue)
    (fsharp-mode                        emacs-nerd-icons-devicon "nf-dev-fsharp"             :face emacs-nerd-icons-blue)
    (asm-mode                           emacs-nerd-icons-sucicon "nf-seti-asm"           :face emacs-nerd-icons-blue)
    (nasm-mode                          emacs-nerd-icons-sucicon "nf-seti-asm"           :face emacs-nerd-icons-blue)
    ;; (tcl-mode                           emacs-nerd-icons-fileicon "tcl"                :face emacs-nerd-icons-dred)
    ;; (cuda-mode                          emacs-nerd-icons-fileicon "nvidia"           :face emacs-nerd-icons-green)
    (f90-mode                           emacs-nerd-icons-mdicon "nf-md-language_fortran"          :face emacs-nerd-icons-purple)
    ;; (hy-mode                            emacs-nerd-icons-fileicon "hy"               :face emacs-nerd-icons-blue)
    (glsl-mode                          emacs-nerd-icons-faicon "nf-fa-paint_brush"    :face emacs-nerd-icons-green)
    (zig-mode                           emacs-nerd-icons-sucicon "nf-seti-zig"              :face emacs-nerd-icons-orange)
    ;; (odin-mode                          emacs-nerd-icons-fileicon "odin"              :face emacs-nerd-icons-lblue)
    (pdf-view-mode                      emacs-nerd-icons-octicon  "nf-oct-file_pdf"          :face emacs-nerd-icons-dred)
    (spacemacs-buffer-mode              emacs-nerd-icons-sucicon "nf-custom-emacs"              :face emacs-nerd-icons-purple)
    (elfeed-search-mode                 emacs-nerd-icons-faicon   "nf-fa-rss-square"       :face emacs-nerd-icons-orange)
    (elfeed-show-mode                   emacs-nerd-icons-faicon   "nf-fa-rss"              :face emacs-nerd-icons-orange)
    (emms-browser-mode                  emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (emms-lyrics-mode                   emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (emms-show-all-mode                 emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (emms-metaplaylist-mode             emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (emms-tag-editor-mode               emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (emms-playlist-mode                 emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-silver)
    (lilypond-mode                      emacs-nerd-icons-faicon   "nf-fa-music"            :face emacs-nerd-icons-green)
    (magik-session-mode                 emacs-nerd-icons-devicon "nf-dev-terminal"       :face emacs-nerd-icons-blue)
    (magik-cb-mode                      emacs-nerd-icons-faicon "nf-fa-book"               :face emacs-nerd-icons-blue)
    ;; (meson-mode                         emacs-nerd-icons-fileicon "meson"            :face emacs-nerd-icons-purple)
    ;; (man-common                         emacs-nerd-icons-fileicon "man-page"         :face emacs-nerd-icons-blue)
    ))

(defvar emacs-nerd-icons-url-alist
  '(
    ;; Social media and communities
    ("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us" emacs-nerd-icons-faicon "nf-fa-delicious")
    ("^\\(https?://\\)?\\(www\\.\\)?behance\\.net" emacs-nerd-icons-faicon "nf-fa-behance")
    ("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com" emacs-nerd-icons-faicon "nf-fa-dribbble")
    ("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com" emacs-nerd-icons-faicon "nf-fa-facebook_official")
    ("^\\(https?://\\)?\\(www\\.\\)?glide\\.me" emacs-nerd-icons-faicon "nf-fa-glide_g")
    ("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com" emacs-nerd-icons-faicon "nf-fa-google_plus")
    ("linkedin\\.com" emacs-nerd-icons-faicon "nf-fa-linkedin")
    ("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru" emacs-nerd-icons-faicon "nf-fa-odnoklassniki")
    ("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com" emacs-nerd-icons-faicon "nf-fa-reddit_alien")
    ("^\\(https?://\\)?\\(www\\.\\)?slack\\.com" emacs-nerd-icons-faicon "nf-fa-slack")
    ("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com" emacs-nerd-icons-faicon "nf-fa-snapchat_ghost")
    ("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com" emacs-nerd-icons-faicon "nf-fa-weibo")
    ("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com" emacs-nerd-icons-faicon "nf-fa-twitter")
    ;; Blogging
    ("joomla\\.org" emacs-nerd-icons-faicon "nf-fa-joomla")
    ("^\\(https?://\\)?\\(www\\.\\)?medium\\.com" emacs-nerd-icons-faicon "nf-fa-medium")
    ("tumblr\\.com" emacs-nerd-icons-faicon "nf-fa-tumblr")
    ("^wordpress\\.com" emacs-nerd-icons-faicon "nf-fa-wordpress")
    ;; Programming
    ("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org" emacs-nerd-icons-faicon "nf-fa-bitbucket")
    ("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io" emacs-nerd-icons-faicon "nf-fa-codepen")
    ("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com" emacs-nerd-icons-faicon "nf-fa-codiepie")
    ("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com" emacs-nerd-icons-octicon "nf-oct-gist")
    ("^\\(https?://\\)?\\(www\\.\\)?github\\.com" emacs-nerd-icons-octicon "nf-oct-mark_github")
    ("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com" emacs-nerd-icons-faicon "nf-fa-gitlab")
    ("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" emacs-nerd-icons-faicon "nf-fa-hacker_news")
    ("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net" emacs-nerd-icons-faicon "nf-fa-jsfiddle")
    ("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com" emacs-nerd-icons-faicon "nf-fa-maxcdn")
    ("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com" emacs-nerd-icons-faicon "nf-fa-stack_overflow")
    ;; Video
    ("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv" emacs-nerd-icons-faicon "nf-fa-twitch")
    ("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com" emacs-nerd-icons-faicon "nf-fa-vimeo")
    ("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com" emacs-nerd-icons-faicon "nf-fa-youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be" emacs-nerd-icons-faicon "nf-fa-youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?vine\\.co" emacs-nerd-icons-faicon "nf-fa-vine")
    ;; Sound
    ("^\\(https?://\\)?\\(www\\.\\)?last\\.fm" emacs-nerd-icons-faicon "nf-fa-lastfm")
    ("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com" emacs-nerd-icons-faicon "nf-fa-mixcloud")
    ("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com" emacs-nerd-icons-faicon "nf-fa-soundcloud")
    ("spotify\\.com" emacs-nerd-icons-faicon "nf-fa-spotify")
    ;; Shopping
    ("^\\(https?://\\)?\\(www\\.\\)?amazon\\." emacs-nerd-icons-faicon "nf-fa-amazon")
    ("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com" emacs-nerd-icons-faicon "nf-fa-opencart")
    ("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com" emacs-nerd-icons-faicon "nf-fa-paypal")
    ("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com" emacs-nerd-icons-faicon "nf-fa-shitsinbulk")
    ;; Images
    ("^\\(https?://\\)?\\(www\\.\\)?500px\\.com" emacs-nerd-icons-faicon "nf-fa-500px")
    ("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com" emacs-nerd-icons-faicon "nf-fa-deviantart")
    ("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com" emacs-nerd-icons-faicon "nf-fa-flickr")
    ("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com" emacs-nerd-icons-faicon "nf-fa-instagram")
    ("^\\(https?://\\)?\\(www\\.\\)?pinterest\\." emacs-nerd-icons-faicon "nf-fa-pinterest")
    ;; Information and books
    ("^\\(https?://\\)?\\(www\\.\\)?digg\\.com" emacs-nerd-icons-faicon "nf-fa-digg")
    ("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com" emacs-nerd-icons-faicon "nf-fa-foursquare")
    ("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com" emacs-nerd-icons-faicon "nf-fa-get_pocket")
    ("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com" emacs-nerd-icons-faicon "nf-fa-scribd")
    ("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net" emacs-nerd-icons-faicon "nf-fa-slideshare")
    ("stackexchange\\.com" emacs-nerd-icons-faicon "nf-fa-stack_exchange")
    ("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com" emacs-nerd-icons-faicon "nf-fa-stumbleupon")
    ("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\." emacs-nerd-icons-faicon "nf-fa-tripadvisor")
    ("^\\(https?://\\)?\\(www\\.\\)?yelp\\." emacs-nerd-icons-faicon "nf-fa-yelp")

    ("wikipedia\\.org" emacs-nerd-icons-faicon "nf-fa-wikipedia_w")
    ;; Various companies and tools
    ("^\\(https?://\\)?\\(www\\.\\)?angel\\.co" emacs-nerd-icons-faicon "nf-fa-angellist")
    ("^\\(https?://\\)?\\(www\\.\\)?apple\\.com" emacs-nerd-icons-faicon "nf-fa-apple")
    ("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com" emacs-nerd-icons-faicon "nf-fa-buysellads")
    ("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com" emacs-nerd-icons-faicon "nf-fa-connectdevelop")
    ("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com" emacs-nerd-icons-faicon "nf-fa-dashcube")
    ("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com" emacs-nerd-icons-faicon "nf-fa-dropbox")
    ("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com" emacs-nerd-icons-faicon "nf-fa-envira")
    ("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com" emacs-nerd-icons-faicon "nf-fa-fort_awesome")
    ("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com" emacs-nerd-icons-faicon "nf-fa-forumbee")
    ("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com" emacs-nerd-icons-faicon "nf-fa-gratipay")
    ("^\\(https?://\\)?\\(www\\.\\)?modx\\.com" emacs-nerd-icons-faicon "nf-fa-modx")
    ("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com" emacs-nerd-icons-faicon "nf-fa-pagelines")
    ("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com" emacs-nerd-icons-faicon "nf-fa-product_hunt")
    ("sellsy\\.com" emacs-nerd-icons-faicon "nf-fa-sellsy")
    ("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com" emacs-nerd-icons-faicon "nf-fa-simplybuilt")
    ("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com" emacs-nerd-icons-faicon "nf-fa-skyatlas")
    ("^\\(https?://\\)?\\(www\\.\\)?skype\\.com" emacs-nerd-icons-faicon "nf-fa-skype")
    ("steampowered\\.com" emacs-nerd-icons-faicon "nf-fa-steam")
    ("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com" emacs-nerd-icons-faicon "nf-fa-themeisle")
    ("^\\(https?://\\)?\\(www\\.\\)?trello\\.com" emacs-nerd-icons-faicon "nf-fa-trello")
    ("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com" emacs-nerd-icons-faicon "nf-fa-whatsapp")
    ("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com" emacs-nerd-icons-faicon "nf-fa-y_combinator")
    ("yahoo\\.com" emacs-nerd-icons-faicon "nf-fa-yahoo")
    ("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com" emacs-nerd-icons-faicon "nf-fa-yoast")
    ;; Catch all
    ("android" emacs-nerd-icons-faicon "nf-fa-android")
    ("creativecommons" emacs-nerd-icons-faicon "nf-fa-creative_commons")
    ("forums?" emacs-nerd-icons-octicon "nf-oct-comment_discussion")
    ("\\.pdf$" emacs-nerd-icons-octicon "nf-oct-file_pdf" :face emacs-nerd-icons-dred)
    ("google" emacs-nerd-icons-faicon "nf-fa-google")
    ("\\.rss" emacs-nerd-icons-faicon "nf-fa-rss")
    ))

(defun emacs-nerd-icons-auto-mode-match? (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (emacs-nerd-icons-match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun emacs-nerd-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (cl-find-if (lambda (it) (string-match (car it) file)) alist)))

(defun emacs-nerd-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))

    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

(defun emacs-nerd-icons--read-candidates ()
  "Helper to build a list of candidates for all families."
  (cl-reduce 'append (mapcar (lambda (it) (emacs-nerd-icons--read-candidates-for-family it t)) emacs-nerd-icons-font-family)))

(defun emacs-nerd-icons--read-candidates-for-family (family &optional show-family)
  "Helper to build read candidates for FAMILY.
If SHOW-FAMILY is non-nil, displays the icons family in the candidate string."
  (let ((data   (funcall (emacs-nerd-icons--data-name family)))
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
(defun emacs-nerd-icons-install-fonts (&optional pfx)
  "Helper function to download and install the latests fonts based on OS.
The provided Nerd Font is Symbols Nerd Font Mono.
When PFX is non-nil, ignore the prompt and just install"
  (interactive "P")
  (when (or pfx (yes-or-no-p "This will download and install fonts, are you sure you want to do this?"))
    (let* ((url-format "https://raw.githubusercontent.com/rainstormstudio/emacs-nerd-icons/fonts/%s")
           (font-dest (cond
                       ;; Default Linux install directories
                       ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                        (concat (or (getenv "XDG_DATA_HOME")
                                    (concat (getenv "HOME") "/.local/share"))
                                "/fonts/"
                                emacs-nerd-icons-fonts-subdirectory))
                       ;; Default MacOS install directory
                       ((eq system-type 'darwin)
                        (concat (getenv "HOME")
                                "/Library/Fonts/"
                                emacs-nerd-icons-fonts-subdirectory))))
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            emacs-nerd-icons-font-names)
      (when known-dest?
        (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "%s Successfully %s `emacs-nerd-icons' fonts to `%s'!"
               (emacs-nerd-icons-wicon "nf-weather-stars" :v-adjust 0.0)
               (if known-dest? "installed" "downloaded")
               font-dest))))

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

;;;###autoload
(defun emacs-nerd-icons-icon-for-dir (dir &rest arg-overrides)
  "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.
Note: You want chevron, please use `emacs-nerd-icons-icon-for-dir-with-chevron'."
  (let* ((dirname (file-name-base (directory-file-name dir)))
         (path (expand-file-name dir))
         (icon (emacs-nerd-icons-match-to-alist dirname emacs-nerd-icons-dir-icon-alist))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (cond
     ((file-remote-p path)
      (apply #'emacs-nerd-icons-octicon "nf-oct-terminal" (cdr args)))
     ((file-symlink-p path)
      (apply #'emacs-nerd-icons-octicon "nf-oct-file_symlink_directory" (cdr args)))
     ((emacs-nerd-icons-dir-is-submodule path)
      (apply #'emacs-nerd-icons-octicon "nf-oct-file_submodule" (cdr args)))
     ((file-exists-p (format "%s/.git" path))
      (apply #'emacs-nerd-icons-octicon "nf-oct-repo" (cdr args)))
     (t (apply (car icon) args)))))

;;;###autoload
(defun emacs-nerd-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((ext (file-name-extension file))
         (icon (or (emacs-nerd-icons-match-to-alist file emacs-nerd-icons-regexp-icon-alist)
                   (and ext
                        (cdr (assoc (downcase ext)
                                    emacs-nerd-icons-extension-icon-alist)))
                   emacs-nerd-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun emacs-nerd-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (cdr (or (assoc mode emacs-nerd-icons-mode-icon-alist)
                        (assoc (get mode 'derived-mode-parent) emacs-nerd-icons-mode-icon-alist))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

;;;###autoload
(defun emacs-nerd-icons-icon-for-url (url &rest arg-overrides)
  "Get the formatted icon for URL.
If an icon for URL isn't found in `emacs-nerd-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (emacs-nerd-icons-match-to-alist url emacs-nerd-icons-url-alist))
         (args (cdr icon)))
    (unless icon
      (setq icon '(emacs-nerd-icons-faicon "globe"))
      (setq args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

(defcustom emacs-nerd-icons--cache-limit 2048
  "Maximum cache size for functions cached by `emacs-nerd-icons-cache'."
  :type 'integer)

(defun emacs-nerd-icons-cache (func)
  "Set a cache for FUNC.  Does not work on interactive functions."
  (unless (get func 'emacs-nerd-icons--cached)
    (let ((cache (make-hash-table :test #'equal
                                  :size emacs-nerd-icons--cache-limit))
          (orig-fn (symbol-function func)))
      (fset func
            (lambda (&rest args)
              (or (gethash args cache)
                  (progn
                    (when (> (hash-table-count cache)
                             emacs-nerd-icons--cache-limit)
                      (clrhash cache))
                    (puthash args (apply orig-fn args) cache)))))))

  (put func 'emacs-nerd-icons--cached t))

(emacs-nerd-icons-cache #'emacs-nerd-icons-icon-for-dir)
(emacs-nerd-icons-cache #'emacs-nerd-icons-icon-for-file)
(emacs-nerd-icons-cache #'emacs-nerd-icons-icon-for-mode)
(emacs-nerd-icons-cache #'emacs-nerd-icons-icon-for-url)

(defun emacs-nerd-icons--icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.
When F is provided, the info function is calculated with the format
`emacs-nerd-icons-icon-%s-for-file' or `emacs-nerd-icons-icon-%s-for-mode'."
  (let* ((base-f (concat "emacs-nerd-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (emacs-nerd-icons-auto-mode-match?))
        (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

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
