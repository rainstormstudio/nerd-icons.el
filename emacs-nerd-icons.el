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

(defvar emacs-nerd-icons-extension-icon-alist
  '(
    ("fish" emacs-nerd-icons-devicon "nf-dev-terminal" :face all-the-icons-lpink)
    ("zsh" emacs-nerd-icons-devicon "nf-dev-terminal" :face all-the-icons-lcyan)
    ("sh" emacs-nerd-icons-devicon "nf-dev-terminal" :face all-the-icons-purple)
    ;; Meta
    ("tags" emacs-nerd-icons-octicon "nf-oct-tag" :face all-the-icons-blue)
    ("log" emacs-nerd-icons-octicon "nf-oct-bug" :face all-the-icons-maroon)
    ;; Config
    ("node" emacs-nerd-icons-devicon "nf-dev-nodejs_small" :face all-the-icons-green)
    ("babelrc" emacs-nerd-icons-mdicon "nf-md-babel" :face all-the-icons-yellow)
    ("bashrc" emacs-nerd-icons-mdicon "nf-md-bash" :face all-the-icons-dpink)
    ("bowerrc" emacs-nerd-icons-devicon "nf-dev-bower" :face all-the-icons-silver)
    ("cr" emacs-nerd-icons-sucicon "nf-seti-crystal" :face all-the-icons-yellow)
    ("ecr" emacs-nerd-icons-sucicon "nf-seti-crystal" :face all-the-icons-yellow)
    ("ini" emacs-nerd-icons-octicon "nf-oct-settings" :face all-the-icons-yellow)
    ("eslintignore" emacs-nerd-icons-mdicon "nf-md-eslint" :face all-the-icons-purple)
    ("eslint" emacs-nerd-icons-mdicon "nf-md-eslint" :face all-the-icons-lpurple)
    ("git" emacs-nerd-icons-devicon "nf-dev-git" :face all-the-icons-lred)
    ("mk" emacs-nerd-icons-devicon "nf-dev-gnu" :face all-the-icons-dorange)
    ;;    ("cmake" emacs-nerd-icons-devicon "cmake") TODO: cmake
    ("dockerignore" emacs-nerd-icons-devicon "nf-dev-docker" :face all-the-icons-dblue)
    ("xml" emacs-nerd-icons-faicon "nf-fa-file_code_o" :face all-the-icons-lorange)
    ("json" emacs-nerd-icons-octicon "nf-oct-settings" :face all-the-icons-yellow)
    ("cson" emacs-nerd-icons-octicon "nf-oct-settings" :face all-the-icons-yellow)
    ("yml" emacs-nerd-icons-octicon "nf-oct-settings" :face all-the-icons-dyellow)
    ("yaml" emacs-nerd-icons-octicon "nf-oct-settings" :face all-the-icons-dyellow)
    ;; ?
    ("pkg" emacs-nerd-icons-octicon "nf-oct-package" :face all-the-icons-dsilver)
    ("rpm" emacs-nerd-icons-octicon "nf-oct-package" :face all-the-icons-dsilver)
    ("pkgbuild" emacs-nerd-icons-octicon "nf-oct-package" :face all-the-icons-dsilver)
    ("elc" emacs-nerd-icons-octicon "nf-oct-file_binary" :face all-the-icons-dsilver)
    ("gz" emacs-nerd-icons-octicon "nf-oct-file_binary" :face all-the-icons-lmaroon)
    ("zip" emacs-nerd-icons-octicon "nf-oct-file_zip" :face all-the-icons-lmaroon)
    ("7z" emacs-nerd-icons-octicon "nf-oct-file_zip" :face all-the-icons-lmaroon)
    ("dat" emacs-nerd-icons-faicon "nf-fa-bar_chart" :face all-the-icons-cyan)
    ("dmg" emacs-nerd-icons-octicon "nf-oct-tools" :face all-the-icons-lsilver)
    ("dll" emacs-nerd-icons-faicon "nf-fa-cogs" :face all-the-icons-silver)
    ("ds_store" emacs-nerd-icons-faicon "nf-fa-cogs" :face all-the-icons-silver)
    ;; Source Codes
    ("scpt"         emacs-nerd-icons-devicon "nf-dev-apple" :face all-the-icons-pink)
    ;; ("aup"          emacs-nerd-icons-fileicon "audacity") TODO: audacity
    ("elm"          emacs-nerd-icons-sucicon "nf-seti-elm" :face all-the-icons-blue)
    ("erl"          emacs-nerd-icons-devicon "nf-dev-erlang" :face all-the-icons-red)
    ("hrl"          emacs-nerd-icons-devicon "nf-dev-erlang" :face all-the-icons-dred)
    ("eex"          emacs-nerd-icons-sucicon "nf-seti-elixir" :face all-the-icons-lorange)
    ("leex"         emacs-nerd-icons-sucicon "nf-seti-elixir" :face all-the-icons-lorange)
    ("heex"         emacs-nerd-icons-sucicon "nf-seti-elixir" :face all-the-icons-lorange)
    ("ex"           emacs-nerd-icons-sucicon "nf-seti-elixir" :face all-the-icons-lpurple)
    ("exs"          emacs-nerd-icons-sucicon "nf-seti-elixir" :face all-the-icons-lred)
    ("java"         emacs-nerd-icons-devicon "nf-dev-java" :face all-the-icons-purple)
    ("gradle"       emacs-nerd-icons-sucicon "nf-seti-gradle" :face all-the-icons-silver)
    ("ebuild"       emacs-nerd-icons-mdicon "nf-md-gentoo" :face all-the-icons-cyan)
    ("eclass"       emacs-nerd-icons-mdicon "nf-md-gentoo" :face all-the-icons-blue)
    ("go"           emacs-nerd-icons-devicon "nf-dev-go" :face all-the-icons-blue)
    ("jl"           emacs-nerd-icons-sucicon "nf-seti-julia" :face all-the-icons-purple)
    ("magik" emacs-nerd-icons-faicon "nf-fa-magic" :face all-the-icons-blue)
    ;; ("matlab"       emacs-nerd-icons-devicon "matlab") TODO: matlab
    ("nix"          emacs-nerd-icons-mdicon "nf-md-nix" :face all-the-icons-blue)
    ("pl"           emacs-nerd-icons-sucicon "nf-seti-perl" :face all-the-icons-lorange)
    ("pm"           emacs-nerd-icons-sucicon "nf-seti-perl" :face all-the-icons-lorange)
    ;; ("pl6"          emacs-nerd-icons-devicon"raku") TODO: raku
    ;; ("pm6"          emacs-nerd-icons-devicon "raku") TODO: raku
    ("pod"          emacs-nerd-icons-devicon "nf-dev-perl" :face all-the-icons-lgreen)
    ("php"          emacs-nerd-icons-devicon "nf-dev-php" :face all-the-icons-lsilver)
    ;; ("pony"         emacs-nerd-icons-devicon "pony") TODO: pony
    ("ps1"          emacs-nerd-icons-mdicon "nf-md-powershell" :face all-the-icons-blue)
    ("pro"          emacs-nerd-icons-sucicon "nf-seti-prolog" :face all-the-icons-lmaroon)
    ("proog"        emacs-nerd-icons-sucicon "nf-seti-prolog" :face all-the-icons-lmaroon)
    ("py"           emacs-nerd-icons-devicon "nf-dev-python" :face all-the-icons-dblue)
    ;; ("idr"          emacs-nerd-icons-devicon "idris") TODO: idris
    ;; ("ipynb"        emacs-nerd-icons-devicon "jupyter") TODO: jupyter
    ("gem"          emacs-nerd-icons-devicon "nf-dev-ruby" :face all-the-icons-red)
    ;; ("raku"         emacs-nerd-icons-devicon "raku") TODO: raku
    ;; ("rakumod"      emacs-nerd-icons-devicon "raku") TODO: raku
    ("rb" emacs-nerd-icons-octicon "nf-oct-ruby" :face all-the-icons-lred)
    ("rs"           emacs-nerd-icons-devicon "nf-dev-rust" :face all-the-icons-maroon)
    ("rlib"         emacs-nerd-icons-devicon "nf-dev-rust" :face all-the-icons-dmaroon)
    ("r"            emacs-nerd-icons-sucicon "nf-seti-r" :face all-the-icons-lblue)
    ("rd"           emacs-nerd-icons-sucicon "nf-seti-r" :face all-the-icons-lblue)
    ("rdx"          emacs-nerd-icons-sucicon "nf-seti-r" :face all-the-icons-lblue)
    ("rsx"          emacs-nerd-icons-sucicon "nf-seti-r" :face all-the-icons-lblue)
    ("svelte"       emacs-nerd-icons-sucicon "nf-seti-svelte" :face all-the-icons-red)
    ("gql"          emacs-nerd-icons-mdicon "nf-md-graphql" :face all-the-icons-dpink)
    ("graphql"      emacs-nerd-icons-mdicon "nf-md-graphql" :face all-the-icons-dpink)
    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("c"            emacs-nerd-icons-sucicon "nf-custom-c" :face all-the-icons-blue)
    ("h"            emacs-nerd-icons-faicon "nf-fa-h_square" :face all-the-icons-purple)
    ("m"            emacs-nerd-icons-devicon "nf-dev-apple" )
    ("mm"           emacs-nerd-icons-devicon "nf-dev-apple" )
    ;;
    ("cc"           emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-blue)
    ("cpp"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-blue)
    ("cxx"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-blue)
    ("hh"           emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-purple)
    ("hpp"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-purple)
    ("hxx"          emacs-nerd-icons-sucicon "nf-custom-cpp" :face all-the-icons-purple)
    ;; Lisps
    ;; ("cl"           emacs-nerd-icons-devicon "clisp") TODO: clisp
    ;; ("l"            emacs-nerd-icons-devicon "lisp") TODO: lisp
    ;; ("lisp"         emacs-nerd-icons-devicon "lisp") TODO: lisp
    ;; ("hy"           emacs-nerd-icons-devicon "hy") TODO: hy
    ("el"           emacs-nerd-icons-sucicon "nf-custom-emacs" :face all-the-icons-purple)
    ("clj"          emacs-nerd-icons-devicon "nf-dev-clojure" :face all-the-icons-blue)
    ("cljc"         emacs-nerd-icons-devicon "nf-dev-clojure" :face all-the-icons-blue)
    ;; ("cljs"         emacs-nerd-icons-devicon "cljs") TODO: cljs
    ("coffee"       emacs-nerd-icons-devicon "nf-dev-coffeescript" :face all-the-icons-maroon)
    ("iced"         emacs-nerd-icons-devicon "nf-dev-coffeescript" :face all-the-icons-lmaroon)
    ("dart"         emacs-nerd-icons-devicon "nf-dev-dart" :face all-the-icons-blue)
    ;; ("rkt"          emacs-nerd-icons-devicon "racket") TODO: racket
    ;; ("scrbl"        emacs-nerd-icons-devicon "racket") TODO: racket
    ;; Stylesheeting
    ("css"          emacs-nerd-icons-devicon "nf-dev-css3" :face all-the-icons-yellow)
    ("scss"         emacs-nerd-icons-mdicon "nf-md-sass" :face all-the-icons-pink)
    ("sass"         emacs-nerd-icons-mdicon "nf-md-sass" :face all-the-icons-dpink)
    ("less"         emacs-nerd-icons-devicon "nf-dev-less" :face all-the-icons-dyellow)
    ;; ("postcss"      emacs-nerd-icons-devicon "postcss") TODO: postcss
    ;; ("sss"          emacs-nerd-icons-devicon "postcss") TODO: postcss
    ("styl"         emacs-nerd-icons-devicon "nf-dev-stylus" :face all-the-icons-lgreen)
    ("csv" emacs-nerd-icons-octicon "nf-oct-graph" :face all-the-icons-dblue)
    ;; haskell
    ("hs"           emacs-nerd-icons-devicon "nf-dev-haskell" :face all-the-icons-red)
    ("chs"          emacs-nerd-icons-devicon "nf-dev-haskell" :face all-the-icons-red)
    ("lhs"          emacs-nerd-icons-devicon "nf-dev-haskell" :face all-the-icons-red)
    ("hsc"          emacs-nerd-icons-devicon "nf-dev-haskell" :face all-the-icons-red)
    ;; Web modes
    ("inky-haml"    emacs-nerd-icons-sucicon "nf-seti-haml" :face all-the-icons-lyellow)
    ("haml"         emacs-nerd-icons-sucicon "nf-seti-haml" :face all-the-icons-lyellow)
    ("htm"          emacs-nerd-icons-devicon "nf-dev-html5" :face all-the-icons-orange)
    ("html"         emacs-nerd-icons-devicon "nf-dev-html5" :face all-the-icons-orange)
    ("inky-er"      emacs-nerd-icons-devicon "nf-dev-html5" :face all-the-icons-lred)
    ("inky-erb"     emacs-nerd-icons-devicon "nf-dev-html5" :face all-the-icons-lred)
    ("erb"          emacs-nerd-icons-devicon "nf-dev-html5" :face all-the-icons-lred)
    ;; ("hbs"          emacs-nerd-icons-fileicon "moustache") TODO: moustache
    ("inky-slim" emacs-nerd-icons-octicon "nf-oct-dashboard" :face all-the-icons-yellow)
    ("slim" emacs-nerd-icons-octicon "nf-oct-dashboard" :face all-the-icons-yellow)
    ("jade"         emacs-nerd-icons-sucicon "nf-seti-jade" :face all-the-icons-red)
    ("pug"          emacs-nerd-icons-sucicon "nf-seti-pug" :face all-the-icons-red)
    ;; Javascript
    ;; ("d3js"         emacs-nerd-icons-devicon "d3") TODO: d3
    ("re"           emacs-nerd-icons-sucicon "nf-seti-reasonml" :face all-the-icons-red-alt)
    ("rei"          emacs-nerd-icons-sucicon "nf-seti-reasonml" :face all-the-icons-dred)
    ("ml"           emacs-nerd-icons-sucicon "nf-seti-ocaml" :face all-the-icons-lpink)
    ("mli"          emacs-nerd-icons-sucicon "nf-seti-ocaml" :face all-the-icons-dpink)
    ("react"        emacs-nerd-icons-devicon "nf-dev-react" :face all-the-icons-lblue)
    ("ts"           emacs-nerd-icons-sucicon "nf-seti-typescript" :face all-the-icons-blue-alt)
    ("js"           emacs-nerd-icons-devicon "nf-dev-javascript" :face all-the-icons-yellow)
    ("es"           emacs-nerd-icons-devicon "nf-dev-javascript" :face all-the-icons-yellow)
    ("jsx"          emacs-nerd-icons-devicon "nf-dev-javascript" :face all-the-icons-cyan-alt)
    ("tsx"          emacs-nerd-icons-devicon "nf-dev-javascript" :face all-the-icons-cyan-alt)
    ("njs"          emacs-nerd-icons-mdicon "nf-md-nodejs" :face all-the-icons-lgreen)
    ("vue"          emacs-nerd-icons-sucicon "nf-seti-vue" :face all-the-icons-lgreen)

    ("sbt"          emacs-nerd-icons-sucicon   "nf-seti-sbt" :face all-the-icons-red)
    ("scala"        emacs-nerd-icons-devicon "nf-dev-scala" :face all-the-icons-red)
    ("scm"          emacs-nerd-icons-mdicon   "nf-md-lambda" :face all-the-icons-red)
    ("swift"        emacs-nerd-icons-devicon "nf-dev-swift" :face all-the-icons-green)

    ("tcl"          emacs-nerd-icons-mdicon "nf-md-feather" :face all-the-icons-dred)

    ("tf"           emacs-nerd-icons-mdicon "nf-md-terraform" :face all-the-icons-purple-alt)
    ("tfvars"       emacs-nerd-icons-mdicon "nf-md-terraform" :face all-the-icons-purple-alt)
    ("tfstate"      emacs-nerd-icons-mdicon "nf-md-terraform" :face all-the-icons-purple-alt)

    ("asm"          emacs-nerd-icons-sucicon "nf-seti-asm" :face all-the-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS     ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ;; ("v"            emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("vams"         emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("sv"           emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("sva"          emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("svh"          emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; ("svams"        emacs-nerd-icons-devicon "verilog") TODO: verilog
    ;; VHDL(-AMS     ;; VHDL(-AMS)
    ("vhd"          emacs-nerd-icons-mdicon "nf-md-xml" :face all-the-icons-blue)
    ("vhdl"         emacs-nerd-icons-mdicon "nf-md-xml" :face all-the-icons-blue)
    ("vhms"         emacs-nerd-icons-mdicon "nf-md-xml" :face all-the-icons-blue)
    ;; Cabal
    ;; ("cabal"        emacs-nerd-icons-devicon "cabal") TODO: cabal
    ;; Kotlin
    ("kt"           emacs-nerd-icons-sucicon "nf-seti-kotlin" :face all-the-icons-orange)
    ("kts"          emacs-nerd-icons-sucicon "nf-seti-kotlin" :face all-the-icons-orange)
    ;; Nimrod
    ("nim"          emacs-nerd-icons-sucicon "nf-seti-nim" :face all-the-icons-yellow)
    ("nims"         emacs-nerd-icons-sucicon "nf-seti-nim" :face all-the-icons-yellow)
    ;; SQL
    ("sql" emacs-nerd-icons-octicon "nf-oct-database" :face all-the-icons-silver)
    ;; Styles
    ;; ("styles"       emacs-nerd-icons-devicon "style") TODO: style
    ;; Lua
    ("lua"          emacs-nerd-icons-sucicon "nf-seti-lua" :face all-the-icons-dblue)
    ;; ASCII doc
    ;; ("adoc"         emacs-nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; ("asciidoc"     emacs-nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; Puppet
    ("pp"           emacs-nerd-icons-sucicon "nf-seti-puppet" :face all-the-icons-yellow)
    ;; Jinja
    ("j2"           emacs-nerd-icons-sucicon "nf-seti-jinja" :face all-the-icons-silver)
    ("jinja2"       emacs-nerd-icons-sucicon "nf-seti-jinja" :face all-the-icons-silver)
    ;; Docker
    ("dockerfile"   emacs-nerd-icons-sucicon "nf-seti-docker" :face all-the-icons-cyan)
    ;; Vagrant
    ;; ("vagrantfile"  emacs-nerd-icons-fileicon "vagrant") TODO: vagrant
    ;; GLSL
    ("glsl"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-blue)
    ("vert"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-blue)
    ("tesc"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-purple)
    ("tese"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-dpurple)
    ("geom"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-green)
    ("frag"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-red)
    ("comp"     emacs-nerd-icons-faicon "nf-fa-paint_brush" :face all-the-icons-dblue)
    ;; CUDA
    ("cu"       emacs-nerd-icons-sucicon "nf-custom-c") ;; TODO: green face
    ("cuh"      emacs-nerd-icons-faicon "nf-fa-h_square") ;; TODO: green face
    ;; Fortran
    ("f90"      emacs-nerd-icons-mdicon "nf-md-language_fortran" :face all-the-icons-purple)
    ;; C#
    ("cs"           emacs-nerd-icons-mdicon "nf-md-language_csharp" :face all-the-icons-dblue)
    ("csx"          emacs-nerd-icons-mdicon "nf-md-language_csharp" :face all-the-icons-dblue)
    ;; F#
    ("fs" emacs-nerd-icons-devicon "nf-dev-fsharp" :face all-the-icons-blue-alt)
    ("fsi" emacs-nerd-icons-devicon "nf-dev-fsharp" :face all-the-icons-blue-alt)
    ("fsx" emacs-nerd-icons-devicon "nf-dev-fsharp" :face all-the-icons-blue-alt)
    ("fsscript" emacs-nerd-icons-devicon "nf-dev-fsharp" :face all-the-icons-blue-alt)
    ;; zig
    ("zig"          emacs-nerd-icons-sucicon "nf-seti-zig" :face all-the-icons-orange)
    ;; odin
    ;; ("odin"         emacs-nerd-icons-fileicon "odin") TODO: odin
    ;; File Types
    ("ico" emacs-nerd-icons-octicon "nf-oct-file_media" :face all-the-icons-blue)
    ("png" emacs-nerd-icons-mdicon "nf-md-file_png_box" :face all-the-icons-orange)
    ("gif" emacs-nerd-icons-mdicon "nf-md-file_gif_box" :face all-the-icons-green)
    ("jpeg" emacs-nerd-icons-mdicon "nf-md-file_jpg_box" :face all-the-icons-dblue)
    ("jpg" emacs-nerd-icons-mdicon "nf-md-file_jpg_box" :face all-the-icons-dblue)
    ("webp" emacs-nerd-icons-octicon "nf-oct-file_media" :face all-the-icons-dblue)
    ;; Audio
    ("mp3" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("wav" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("m4a" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("ogg" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("flac" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("opus" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("au" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("aif" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("aifc" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("aiff" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-dred)
    ("svg" emacs-nerd-icons-sucicon "nf-seti-svg" :face all-the-icons-lgreen)
    ;; Video
    ("mov" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("mp4" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("ogv" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-dblue)
    ("mpg" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("mpeg" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("flv" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("ogv" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-dblue)
    ("mkv" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ("webm" emacs-nerd-icons-faicon "nf-fa-film" :face all-the-icons-blue)
    ;; Fonts
    ("ttf"          emacs-nerd-icons-faicon "nf-fa-font" :face all-the-icons-dcyan)
    ("woff"         emacs-nerd-icons-faicon "nf-fa-font" :face all-the-icons-cyan)
    ("woff2"        emacs-nerd-icons-faicon "nf-fa-font" :face all-the-icons-cyan)
    ;; Doc
    ("pdf" emacs-nerd-icons-octicon "nf-oct-file_pdf" :face all-the-icons-dred)
    ("text" emacs-nerd-icons-octicon "nf-oct-file_text" :face all-the-icons-cyan)
    ("txt" emacs-nerd-icons-octicon "nf-oct-file_text" :face all-the-icons-cyan)
    ("doc"          emacs-nerd-icons-mdicon "nf-md-file_word" :face all-the-icons-blue)
    ("docx"         emacs-nerd-icons-mdicon "nf-md-file_word" :face all-the-icons-blue)
    ("docm"         emacs-nerd-icons-mdicon "nf-md-file_word" :face all-the-icons-blue)
    ("texi"         emacs-nerd-icons-mdicon "nf-md-format_text" :face all-the-icons-lred)
    ("tex"          emacs-nerd-icons-mdicon "nf-md-format_text" :face all-the-icons-lred)
    ("md" emacs-nerd-icons-octicon "nf-oct-markdown" :face all-the-icons-lblue)
    ;; ("bib"          emacs-nerd-icons-fileicon "bib") TODO: bib
    ("org"          emacs-nerd-icons-sucicon "nf-custom-orgmode" :face all-the-icons-lgreen)
    ("pps"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face all-the-icons-orange)
    ("ppt"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face all-the-icons-orange)
    ("pptsx"        emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face all-the-icons-orange)
    ("ppttx"        emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face all-the-icons-orange)
    ("knt"          emacs-nerd-icons-mdicon "nf-md-file_powerpoint" :face all-the-icons-cyan)
    ("xlsx"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face all-the-icons-dgreen)
    ("xlsm"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face all-the-icons-dgreen)
    ("xlsb"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face all-the-icons-dgreen)
    ("xltx"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face all-the-icons-dgreen)
    ("xltm"         emacs-nerd-icons-mdicon "nf-md-file_excel" :face all-the-icons-dgreen)
    ("ly" emacs-nerd-icons-faicon "nf-fa-music" :face all-the-icons-green)
    ;;
    ("key" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-lblue)
    ("pem" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-orange)
    ("p12" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-dorange)
    ("crt" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-lblue)
    ("pub" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-blue)
    ("gpg" emacs-nerd-icons-octicon "nf-oct-key" :face all-the-icons-lblue)
    ("cache" emacs-nerd-icons-octicon "nf-oct-database" :face all-the-icons-green)
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
    ("google[ _-]drive" emacs-nerd-icons-devicon "nf-dev-google_drive")
    ("^atom$"           emacs-nerd-icons-devicon "nf-dev-atom")
    ("documents"        emacs-nerd-icons-faicon "nf-fa-book")
    ("download"         emacs-nerd-icons-faicon "nf-fa-cloud_download")
    ("desktop"          emacs-nerd-icons-octicon "nf-oct-device_desktop")
    ("pictures"         emacs-nerd-icons-faicon "nf-fa-picture_o")
    ("photos"           emacs-nerd-icons-faicon "nf-fa-camera_retro")
    ("music"            emacs-nerd-icons-faicon "nf-fa-music")
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
