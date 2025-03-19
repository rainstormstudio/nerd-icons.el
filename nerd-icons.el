;;; nerd-icons.el --- Emacs Nerd Font Icons Library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>, Vincent Zhang <seagle0128@gmail.com>
;; Keywords: lisp
;; Version: 0.1.0
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

;; This package was inspired by

;; - `all-the-icons', found at https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-all-the-icons.el
;; - `vim-devicons' for Vim, found at https://github.com/ryanoasis/vim-devicons
;; - `nvim-web-devicons' for NeoVim, found at https://github.com/nvim-tree/nvim-web-devicons

;; This package provides an interface to the Nerd Fonts

;; - `nerd-fonts', found at https://github.com/ryanoasis/nerd-fonts

;;; Code:

(require 'cl-lib)

(require 'nerd-icons-data)
(require 'nerd-icons-faces)

(declare-function set-fontset-font "src/fontset.c")

(defgroup nerd-icons nil
  "Manage how Nerd Fonts formats icons."
  :prefix "nerd-icons-"
  :group 'appearance
  :group 'convenience)

(defcustom nerd-icons-color-icons t
  "Whether or not to include a foreground color when formatting the icon."
  :group 'nerd-icons
  :type 'boolean)

(defcustom nerd-icons-scale-factor 1.0
  "The base Scale Factor for the `height' face property of an icon."
  :group 'nerd-icons
  :type 'number)

(defcustom nerd-icons-default-adjust 0.0
  "The default adjustment to be made to the `raise' display property of an icon."
  :group 'nerd-icons
  :type 'number)

(defcustom nerd-icons--cache-limit 2048
  "Maximum cache size for functions cached by `nerd-icons-cache'."
  :group 'nerd-icons
  :type 'integer)

(defcustom nerd-icons-font-family "Symbols Nerd Font Mono"
  "The Nerd Font for display icons."
  :group 'nerd-icons
  :type 'string)

(defcustom nerd-icons-fonts-subdirectory nil
  "The subdirectory within the system fonts folder where the icons are installed."
  :group 'nerd-icons
  :type 'directory)

(defvar nerd-icons-font-names '("NFM.ttf")
  "List of defined font file names.")

(defvar nerd-icons-glyph-sets '() "List of defined icon glyph sets.")

(defvar nerd-icons-extension-icon-alist
  '(
    ("fish"           nerd-icons-devicon "nf-dev-terminal"       :face nerd-icons-lpink)
    ("zsh"            nerd-icons-devicon "nf-dev-terminal"       :face nerd-icons-lcyan)
    ("sh"             nerd-icons-devicon "nf-dev-terminal"       :face nerd-icons-purple)
    ("bat"            nerd-icons-codicon "nf-cod-terminal_cmd"   :face nerd-icons-lsilver)
    ("cmd"            nerd-icons-codicon "nf-cod-terminal_cmd"   :face nerd-icons-lsilver)
    ;; Meta
    ("tags"           nerd-icons-octicon "nf-oct-tag"            :face nerd-icons-blue)
    ("log"            nerd-icons-octicon "nf-oct-log"            :face nerd-icons-maroon)
    ;; Config
    ("node"           nerd-icons-devicon "nf-dev-nodejs_small"   :face nerd-icons-green)
    ("babelrc"        nerd-icons-mdicon "nf-md-babel"            :face nerd-icons-yellow)
    ("bashrc"         nerd-icons-mdicon "nf-md-bash"             :face nerd-icons-dpink)
    ("bowerrc"        nerd-icons-devicon "nf-dev-bower"          :face nerd-icons-silver)
    ("cr"             nerd-icons-sucicon "nf-seti-crystal"       :face nerd-icons-yellow)
    ("ecr"            nerd-icons-sucicon "nf-seti-crystal"       :face nerd-icons-yellow)
    ("ini"            nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-yellow)
    ("properties"     nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-yellow)
    ("eslintignore"   nerd-icons-mdicon "nf-md-eslint"           :face nerd-icons-purple)
    ("eslint"         nerd-icons-mdicon "nf-md-eslint"           :face nerd-icons-lpurple)
    ("git"            nerd-icons-sucicon "nf-seti-git"            :face nerd-icons-lred)
    ("gitattributes"  nerd-icons-sucicon "nf-seti-git"            :face nerd-icons-lred)
    ("gitignore"      nerd-icons-sucicon "nf-seti-git"            :face nerd-icons-lred)
    ("gitmodules"     nerd-icons-sucicon "nf-seti-git"            :face nerd-icons-lred)
    ("mk"             nerd-icons-devicon "nf-dev-gnu"            :face nerd-icons-dorange)
    ;; ("cmake"    nerd-icons-devicon "cmake") TODO: cmake
    ("dockerignore"   nerd-icons-devicon "nf-dev-docker"         :face nerd-icons-dblue)
    ("plist"          nerd-icons-faicon "nf-fa-file_code"        :face nerd-icons-dsilver)
    ("xml"            nerd-icons-faicon "nf-fa-file_code_o"      :face nerd-icons-lorange)
    ("json"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-yellow)
    ("jsonl"          nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-yellow)
    ("cson"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-yellow)
    ("yml"            nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-dyellow)
    ("yaml"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-dyellow)
    ("toml"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-orange)
    ("conf"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-dorange)
    ("editorconfig"   nerd-icons-sucicon "nf-seti-editorconfig"  :face nerd-icons-silver)
    ("idekeybindings" nerd-icons-faicon "nf-fa-file_code_o"      :face nerd-icons-lblue)
    ;; ?
    ("pkg"            nerd-icons-octicon "nf-oct-package"        :face nerd-icons-dsilver)
    ("rpm"            nerd-icons-octicon "nf-oct-package"        :face nerd-icons-dsilver)
    ("pkgbuild"       nerd-icons-octicon "nf-oct-package"        :face nerd-icons-dsilver)
    ("elc"            nerd-icons-octicon "nf-oct-file_binary"    :face nerd-icons-dsilver)
    ("eln"            nerd-icons-octicon "nf-oct-file_binary"    :face nerd-icons-dsilver)
    ("gz"             nerd-icons-octicon "nf-oct-file_binary"    :face nerd-icons-lmaroon)
    ("zip"            nerd-icons-octicon "nf-oct-file_zip"       :face nerd-icons-lmaroon)
    ("7z"             nerd-icons-octicon "nf-oct-file_zip"       :face nerd-icons-lmaroon)
    ("zst"            nerd-icons-octicon "nf-oct-file_zip"       :face nerd-icons-lmaroon)
    ("dat"            nerd-icons-faicon "nf-fa-bar_chart"        :face nerd-icons-cyan)
    ("dmg"            nerd-icons-octicon "nf-oct-tools"          :face nerd-icons-lsilver)
    ("dll"            nerd-icons-faicon "nf-fa-cogs"             :face nerd-icons-silver)
    ("ds_store"       nerd-icons-faicon "nf-fa-cogs"             :face nerd-icons-silver)
    ("exe"            nerd-icons-octicon "nf-oct-file_binary"    :face nerd-icons-dsilver)
    ("msg"            nerd-icons-octicon "nf-oct-mail"           :face nerd-icons-dsilver)
    ;; Diff
    ("diff"           nerd-icons-octicon "nf-oct-file_diff"      :face nerd-icons-lred)
    ("patch"          nerd-icons-octicon "nf-oct-file_diff"      :face nerd-icons-lred)
    ;; Interface Builder
    ("nib"            nerd-icons-mdicon "nf-md-fountain_pen_tip" :face nerd-icons-lsilver)
    ("xib"            nerd-icons-mdicon "nf-md-fountain_pen_tip" :face nerd-icons-lsilver)
    ;; Source Codes
    ("ada"            nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-blue)
    ("adb"            nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-blue)
    ("adc"            nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-blue)
    ("ads"            nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-blue)
    ("gpr"            nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-green)
    ("cgpr"           nerd-icons-sucicon "nf-custom-ada"         :face nerd-icons-green)
    ("scpt"           nerd-icons-devicon "nf-dev-apple"          :face nerd-icons-pink)
    ("awk"            nerd-icons-devicon "nf-dev-awk"            :face nerd-icons-yellow)
    ;; ("aup"         nerd-icons-fileicon "audacity") TODO: audacity
    ("elm"            nerd-icons-sucicon "nf-seti-elm"           :face nerd-icons-blue)
    ("erl"            nerd-icons-devicon "nf-dev-erlang"         :face nerd-icons-red)
    ("hrl"            nerd-icons-devicon "nf-dev-erlang"         :face nerd-icons-dred)
    ("eex"            nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lorange)
    ("leex"           nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lorange)
    ("heex"           nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lorange)
    ("ex"             nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lpurple)
    ("exs"            nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lred)
    ("livemd"         nerd-icons-sucicon "nf-seti-elixir"        :face nerd-icons-lblue)
    ("java"           nerd-icons-devicon "nf-dev-java"           :face nerd-icons-purple)
    ("groovy"         nerd-icons-devicon "nf-dev-groovy"         :face nerd-icons-cyan)
    ("gradle"         nerd-icons-sucicon "nf-seti-gradle"        :face nerd-icons-silver)
    ("ebuild"         nerd-icons-mdicon "nf-md-gentoo"           :face nerd-icons-cyan)
    ("eclass"         nerd-icons-mdicon "nf-md-gentoo"           :face nerd-icons-blue)
    ("go"             nerd-icons-sucicon "nf-seti-go2"             :face nerd-icons-blue)
    ("jl"             nerd-icons-sucicon "nf-seti-julia"         :face nerd-icons-purple)
    ("magik"          nerd-icons-faicon "nf-fa-magic"            :face nerd-icons-blue)
    ("matlab"         nerd-icons-devicon "nf-dev-matlab"         :face nerd-icons-orange)
    ("nix"            nerd-icons-mdicon "nf-md-nix"              :face nerd-icons-blue)
    ("pl"             nerd-icons-sucicon "nf-seti-perl"          :face nerd-icons-lorange)
    ("pm"             nerd-icons-sucicon "nf-seti-perl"          :face nerd-icons-lorange)
    ;; ("pl6"         nerd-icons-devicon"raku") TODO: raku
    ;; ("pm6"         nerd-icons-devicon "raku") TODO: raku
    ("pod"            nerd-icons-devicon "nf-dev-perl"           :face nerd-icons-lgreen)
    ("php"            nerd-icons-devicon "nf-dev-php"            :face nerd-icons-lsilver)
    ;; ("pony"        nerd-icons-devicon "pony") TODO: pony
    ("ps1"            nerd-icons-mdicon "nf-md-powershell"       :face nerd-icons-blue)
    ("pro"            nerd-icons-sucicon "nf-seti-prolog"        :face nerd-icons-lmaroon)
    ("proog"          nerd-icons-sucicon "nf-seti-prolog"        :face nerd-icons-lmaroon)
    ("py"             nerd-icons-devicon "nf-dev-python"         :face nerd-icons-dblue)
    ;; ("idr"         nerd-icons-devicon "idris") TODO: idris
    ("ipynb"          nerd-icons-devicon "nf-dev-jupyter"        :face nerd-icons-orange)
    ("gem"            nerd-icons-devicon "nf-dev-ruby"           :face nerd-icons-red)
    ;; ("raku"        nerd-icons-devicon "raku") TODO: raku
    ;; ("rakumod"     nerd-icons-devicon "raku") TODO: raku
    ("rb"             nerd-icons-octicon "nf-oct-ruby"           :face nerd-icons-lred)
    ("rs"             nerd-icons-devicon "nf-dev-rust"           :face nerd-icons-maroon)
    ("rlib"           nerd-icons-devicon "nf-dev-rust"           :face nerd-icons-dmaroon)
    ("r"              nerd-icons-sucicon "nf-seti-r"             :face nerd-icons-lblue)
    ("rd"             nerd-icons-sucicon "nf-seti-r"             :face nerd-icons-lblue)
    ("rdx"            nerd-icons-sucicon "nf-seti-r"             :face nerd-icons-lblue)
    ("rsx"            nerd-icons-sucicon "nf-seti-r"             :face nerd-icons-lblue)
    ("svelte"         nerd-icons-sucicon "nf-seti-svelte"        :face nerd-icons-red)
    ("gql"            nerd-icons-mdicon "nf-md-graphql"          :face nerd-icons-dpink)
    ("graphql"        nerd-icons-mdicon "nf-md-graphql"          :face nerd-icons-dpink)
    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("c"              nerd-icons-sucicon "nf-custom-c"           :face nerd-icons-blue)
    ("h"              nerd-icons-faicon "nf-fa-h_square"         :face nerd-icons-purple)
    ("m"              nerd-icons-devicon "nf-dev-apple" )
    ("mm"             nerd-icons-devicon "nf-dev-apple" )
    ;;
    ("cc"             nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-blue)
    ("cpp"            nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-blue)
    ("cxx"            nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-blue)
    ("hh"             nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-purple)
    ("hpp"            nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-purple)
    ("hxx"            nerd-icons-sucicon "nf-custom-cpp"         :face nerd-icons-purple)
    ;; Lisps
    ("cl"             nerd-icons-sucicon "nf-custom-common_lisp" :face nerd-icons-lorange)
    ("l"              nerd-icons-sucicon "nf-custom-scheme"      :face nerd-icons-orange)
    ("lisp"           nerd-icons-sucicon "nf-custom-scheme"      :face nerd-icons-orange)
    ;; ("hy"             nerd-icons-sucicon "nf-custom-hy"          :face nerd-icons-blue)
    ("el"             nerd-icons-sucicon "nf-custom-emacs"       :face nerd-icons-purple)
    ("eld"            nerd-icons-sucicon "nf-custom-emacs"       :face nerd-icons-purple)
    ("clj"            nerd-icons-devicon "nf-dev-clojure"        :face nerd-icons-blue)
    ("cljc"           nerd-icons-devicon "nf-dev-clojure"        :face nerd-icons-blue)
    ("cljd"           nerd-icons-devicon "nf-dev-clojure"        :face nerd-icons-green)
    ("cljs"           nerd-icons-devicon "nf-dev-clojure"        :face nerd-icons-lyellow)
    ("coffee"         nerd-icons-devicon "nf-dev-coffeescript"   :face nerd-icons-maroon)
    ("iced"           nerd-icons-devicon "nf-dev-coffeescript"   :face nerd-icons-lmaroon)
    ("dart"           nerd-icons-devicon "nf-dev-dart"           :face nerd-icons-blue)
    ("ledger"         nerd-icons-mdicon  "nf-md-file_table_box_multiple" :face nerd-icons-green)
    ("rkt"            nerd-icons-sucicon "nf-custom-scheme"       :face nerd-icons-red)
    ("scrbl"          nerd-icons-sucicon "nf-custom-scheme"       :face nerd-icons-blue)
    ;; Stylesheeting
    ("css"            nerd-icons-devicon "nf-dev-css3"           :face nerd-icons-yellow)
    ("scss"           nerd-icons-mdicon "nf-md-sass"             :face nerd-icons-pink)
    ("sass"           nerd-icons-mdicon "nf-md-sass"             :face nerd-icons-dpink)
    ("less"           nerd-icons-devicon "nf-dev-less"           :face nerd-icons-dyellow)
    ;; ("postcss"     nerd-icons-devicon "postcss") TODO: postcss
    ;; ("sss"         nerd-icons-devicon "postcss") TODO: postcss
    ("styl"           nerd-icons-devicon "nf-dev-stylus"         :face nerd-icons-lgreen)
    ("csv"            nerd-icons-octicon "nf-oct-graph"          :face nerd-icons-dblue)
    ("tsv"            nerd-icons-octicon "nf-oct-graph"          :face nerd-icons-dblue)
    ;; haskell
    ("hs"             nerd-icons-devicon "nf-dev-haskell"        :face nerd-icons-red)
    ("hx"             nerd-icons-sucicon "nf-seti-haxe"          :face nerd-icons-orange)
    ("chs"            nerd-icons-devicon "nf-dev-haskell"        :face nerd-icons-red)
    ("lhs"            nerd-icons-devicon "nf-dev-haskell"        :face nerd-icons-red)
    ("hsc"            nerd-icons-devicon "nf-dev-haskell"        :face nerd-icons-red)
    ;; Web modes
    ("inky-haml"      nerd-icons-sucicon "nf-seti-haml"          :face nerd-icons-lyellow)
    ("haml"           nerd-icons-sucicon "nf-seti-haml"          :face nerd-icons-lyellow)
    ("htm"            nerd-icons-devicon "nf-dev-html5"          :face nerd-icons-orange)
    ("html"           nerd-icons-devicon "nf-dev-html5"          :face nerd-icons-orange)
    ("inky-er"        nerd-icons-devicon "nf-dev-html5"          :face nerd-icons-lred)
    ("inky-erb"       nerd-icons-devicon "nf-dev-html5"          :face nerd-icons-lred)
    ("erb"            nerd-icons-devicon "nf-dev-html5"          :face nerd-icons-lred)
    ;; ("hbs"         nerd-icons-fileicon "moustache") TODO: moustache
    ("inky-slim"      nerd-icons-codicon "nf-cod-dashboard"      :face nerd-icons-yellow)
    ("slim"           nerd-icons-codicon "nf-cod-dashboard"      :face nerd-icons-yellow)
    ("jade"           nerd-icons-sucicon "nf-seti-jade"          :face nerd-icons-red)
    ("pug"            nerd-icons-sucicon "nf-seti-pug"           :face nerd-icons-red)
    ;; Javascript
    ("d3js"           nerd-icons-devicon "nf-dev-d3js"           :face nerd-icons-orange)
    ("re"             nerd-icons-sucicon "nf-seti-reasonml"      :face nerd-icons-red-alt)
    ("rei"            nerd-icons-sucicon "nf-seti-reasonml"      :face nerd-icons-dred)
    ("ml"             nerd-icons-sucicon "nf-seti-ocaml"         :face nerd-icons-lpink)
    ("mli"            nerd-icons-sucicon "nf-seti-ocaml"         :face nerd-icons-dpink)
    ("react"          nerd-icons-devicon "nf-dev-react"          :face nerd-icons-lblue)
    ("ts"             nerd-icons-sucicon "nf-seti-typescript"    :face nerd-icons-blue-alt)
    ("js"             nerd-icons-devicon "nf-dev-javascript"     :face nerd-icons-yellow)
    ("es"             nerd-icons-devicon "nf-dev-javascript"     :face nerd-icons-yellow)
    ("jsx"            nerd-icons-devicon "nf-dev-javascript"     :face nerd-icons-cyan-alt)
    ("tsx"            nerd-icons-sucicon "nf-seti-typescript"    :face nerd-icons-blue-alt)
    ("njs"            nerd-icons-mdicon "nf-md-nodejs"           :face nerd-icons-lgreen)
    ("vue"            nerd-icons-sucicon "nf-seti-vue"           :face nerd-icons-lgreen)

    ("sbt"            nerd-icons-sucicon "nf-seti-sbt"           :face nerd-icons-red)
    ("scala"          nerd-icons-devicon "nf-dev-scala"          :face nerd-icons-red)
    ("scm"            nerd-icons-sucicon "nf-custom-scheme"      :face nerd-icons-red)
    ("swift"          nerd-icons-devicon "nf-dev-swift"          :face nerd-icons-lred)

    ("tcl"            nerd-icons-mdicon "nf-md-feather"          :face nerd-icons-dred)
    ("exp"            nerd-icons-mdicon "nf-md-feather"          :face nerd-icons-dred)

    ("tf"             nerd-icons-mdicon "nf-md-terraform"        :face nerd-icons-purple-alt)
    ("tfvars"         nerd-icons-mdicon "nf-md-terraform"        :face nerd-icons-purple-alt)
    ("tfstate"        nerd-icons-mdicon "nf-md-terraform"        :face nerd-icons-purple-alt)
    ("tfbackend"      nerd-icons-mdicon "nf-md-terraform"        :face nerd-icons-purple-alt)

    ("asm"            nerd-icons-sucicon "nf-seti-asm"           :face nerd-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS     ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"              nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ("vams"           nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ("sv"             nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ("sva"            nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ("svh"            nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ("svams"          nerd-icons-mdicon "nf-md-chip"             :face nerd-icons-red)
    ;; VHDL(-AMS     ;; VHDL(-AMS)
    ("vhd"            nerd-icons-octicon "nf-oct-cpu"            :face nerd-icons-blue)
    ("vhdl"           nerd-icons-octicon "nf-oct-cpu"            :face nerd-icons-blue)
    ("vhms"           nerd-icons-octicon "nf-oct-cpu"            :face nerd-icons-blue)
    ;; Cabal
    ;; ("cabal"       nerd-icons-devicon "cabal") TODO: cabal
    ;; Kotlin
    ("kt"             nerd-icons-sucicon "nf-seti-kotlin"        :face nerd-icons-orange)
    ("kts"            nerd-icons-sucicon "nf-seti-kotlin"        :face nerd-icons-orange)
    ;; Nimrod
    ("nim"            nerd-icons-sucicon "nf-seti-nim"           :face nerd-icons-yellow)
    ("nims"           nerd-icons-sucicon "nf-seti-nim"           :face nerd-icons-yellow)
    ;; SQL
    ("sql"            nerd-icons-octicon "nf-oct-database"       :face nerd-icons-silver)
    ;; Styles
    ;; ("styles"      nerd-icons-devicon "style") TODO: style
    ;; Lua
    ("lua"            nerd-icons-sucicon "nf-seti-lua"           :face nerd-icons-dblue)
    ;; ASCII doc
    ;; ("adoc"        nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; ("asciidoc"    nerd-icons-devicon "asciidoc") TODO: asciidoc
    ;; Puppet
    ("pp"             nerd-icons-sucicon "nf-seti-puppet"        :face nerd-icons-yellow)
    ;; Jinja
    ("j2"             nerd-icons-sucicon "nf-seti-jinja"         :face nerd-icons-silver)
    ("jinja2"         nerd-icons-sucicon "nf-seti-jinja"         :face nerd-icons-silver)
    ;; Docker
    ("dockerfile"     nerd-icons-sucicon "nf-seti-docker"        :face nerd-icons-cyan)
    ;; Vagrant
    ;; ("vagrantfile" nerd-icons-fileicon "vagrant") TODO: vagrant
    ;; GLSL
    ("glsl"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-blue)
    ("vert"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-blue)
    ("tesc"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-purple)
    ("tese"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-dpurple)
    ("geom"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-green)
    ("frag"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-red)
    ("comp"           nerd-icons-faicon "nf-fa-paint_brush"      :face nerd-icons-dblue)
    ;; CUDA
    ("cu"             nerd-icons-sucicon "nf-custom-c"           :face nerd-icons-green)
    ("cuh"            nerd-icons-faicon "nf-fa-h_square"         :face nerd-icons-green)
    ;; Fortran
    ("f90"            nerd-icons-mdicon "nf-md-language_fortran" :face nerd-icons-purple)
    ;; C#
    ("cs"             nerd-icons-mdicon "nf-md-language_csharp"  :face nerd-icons-dblue)
    ("csx"            nerd-icons-mdicon "nf-md-language_csharp"  :face nerd-icons-dblue)
    ;; F#
    ("fs"             nerd-icons-devicon "nf-dev-fsharp"         :face nerd-icons-blue-alt)
    ("fsi"            nerd-icons-devicon "nf-dev-fsharp"         :face nerd-icons-blue-alt)
    ("fsx"            nerd-icons-devicon "nf-dev-fsharp"         :face nerd-icons-blue-alt)
    ("fsscript"       nerd-icons-devicon "nf-dev-fsharp"         :face nerd-icons-blue-alt)
    ;; Godot / GDScript
    ("gd"             nerd-icons-sucicon "nf-seti-godot"         :face nerd-icons-blue)
    ("tscn"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-orange)
    ("tres"           nerd-icons-codicon "nf-cod-settings"       :face nerd-icons-orange)
    ;; zig
    ("zig"            nerd-icons-sucicon "nf-seti-zig"           :face nerd-icons-orange)
    ;; odin
    ;; ("odin"        nerd-icons-fileicon "odin") TODO: odin
    ;; File Types
    ("ico"            nerd-icons-octicon "nf-oct-file_media"     :face nerd-icons-blue)
    ("png"            nerd-icons-mdicon "nf-md-file_png_box"     :face nerd-icons-orange)
    ("gif"            nerd-icons-mdicon "nf-md-file_gif_box"     :face nerd-icons-green)
    ("jpeg"           nerd-icons-mdicon "nf-md-file_jpg_box"     :face nerd-icons-dblue)
    ("jpg"            nerd-icons-mdicon "nf-md-file_jpg_box"     :face nerd-icons-dblue)
    ("webp"           nerd-icons-octicon "nf-oct-file_media"     :face nerd-icons-dblue)
    ("xpm"            nerd-icons-octicon "nf-oct-file_media"     :face nerd-icons-dgreen)
    ("cb7"            nerd-icons-mdicon "nf-md-image_album"      :face nerd-icons-lmaroon)
    ("cba"            nerd-icons-mdicon "nf-md-image_album"      :face nerd-icons-lmaroon)
    ("cbr"            nerd-icons-mdicon "nf-md-image_album"      :face nerd-icons-lmaroon)
    ("cbt"            nerd-icons-mdicon "nf-md-image_album"      :face nerd-icons-lmaroon)
    ("cbz"            nerd-icons-mdicon "nf-md-image_album"      :face nerd-icons-lmaroon)
    ;; Audio
    ("mp3"            nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("wav"            nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("m4a"            nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("ogg"            nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("flac"           nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("opus"           nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("au"             nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("aif"            nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("aifc"           nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("aiff"           nerd-icons-faicon "nf-fa-music"                  :face nerd-icons-dred)
    ("svg"            nerd-icons-sucicon "nf-seti-svg"                 :face nerd-icons-lgreen)
    ("m3u"            nerd-icons-mdicon "nf-md-playlist_music_outline" :face nerd-icons-dred)
    ("m3u8"           nerd-icons-mdicon "nf-md-playlist_music_outline" :face nerd-icons-dred)
    ;; Video
    ("mov"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("mp4"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("ogv"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-dblue)
    ("mpg"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("mpeg"           nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("flv"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("ogv"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-dblue)
    ("mkv"            nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ("webm"           nerd-icons-faicon "nf-fa-film"             :face nerd-icons-blue)
    ;; Fonts
    ("ttf"            nerd-icons-faicon "nf-fa-font"             :face nerd-icons-dcyan)
    ("woff"           nerd-icons-faicon "nf-fa-font"             :face nerd-icons-cyan)
    ("woff2"          nerd-icons-faicon "nf-fa-font"             :face nerd-icons-cyan)
    ;; Archives
    ("iso"            nerd-icons-mdicon "nf-md-disc"            :face nerd-icons-orange)
    ("tar"            nerd-icons-mdicon "nf-md-zip_box"          :face nerd-icons-orange)
    ("rar"            nerd-icons-mdicon "nf-md-zip_box"          :face nerd-icons-orange)
    ("tgz"            nerd-icons-mdicon "nf-md-zip_box"          :face nerd-icons-orange)
    ("jar"            nerd-icons-devicon "nf-dev-java"           :face nerd-icons-dpurple)
    ;; Doc
    ("pdf"            nerd-icons-codicon "nf-cod-file_pdf"       :face nerd-icons-dred)
    ("text"           nerd-icons-faicon "nf-fa-file_text"        :face nerd-icons-cyan)
    ("txt"            nerd-icons-faicon "nf-fa-file_text"        :face nerd-icons-cyan)
    ("doc"            nerd-icons-mdicon "nf-md-file_word"        :face nerd-icons-blue)
    ("docx"           nerd-icons-mdicon "nf-md-file_word"        :face nerd-icons-blue)
    ("docm"           nerd-icons-mdicon "nf-md-file_word"        :face nerd-icons-blue)
    ("texi"           nerd-icons-sucicon "nf-seti-tex"           :face nerd-icons-lred)
    ("tex"            nerd-icons-sucicon "nf-seti-tex"           :face nerd-icons-lred)
    ("ltx"            nerd-icons-sucicon "nf-seti-tex"           :face nerd-icons-lred)
    ("dtx"            nerd-icons-sucicon "nf-seti-tex"           :face nerd-icons-lred)
    ("sty"            nerd-icons-sucicon "nf-seti-tex"           :face nerd-icons-lred)
    ("md"             nerd-icons-octicon "nf-oct-markdown"       :face nerd-icons-lblue)
    ("bib"            nerd-icons-mdicon "nf-md-book"             :face nerd-icons-lblue)
    ("org"            nerd-icons-sucicon "nf-custom-orgmode"     :face nerd-icons-lgreen)
    ("org_archive"    nerd-icons-sucicon "nf-custom-orgmode"     :face nerd-icons-lgreen)
    ("pps"            nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-orange)
    ("ppt"            nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-orange)
    ("pptx"           nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-orange)
    ("pptsx"          nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-orange)
    ("ppttx"          nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-orange)
    ("knt"            nerd-icons-mdicon "nf-md-file_powerpoint"  :face nerd-icons-cyan)
    ("xlsx"           nerd-icons-mdicon "nf-md-file_excel"       :face nerd-icons-dgreen)
    ("xlsm"           nerd-icons-mdicon "nf-md-file_excel"       :face nerd-icons-dgreen)
    ("xlsb"           nerd-icons-mdicon "nf-md-file_excel"       :face nerd-icons-dgreen)
    ("xltx"           nerd-icons-mdicon "nf-md-file_excel"       :face nerd-icons-dgreen)
    ("xltm"           nerd-icons-mdicon "nf-md-file_excel"       :face nerd-icons-dgreen)
    ("epub"           nerd-icons-mdicon "nf-md-book_open"        :face nerd-icons-green)
    ("ly"             nerd-icons-faicon "nf-fa-music"            :face nerd-icons-green)
    ;;
    ("key"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-lblue)
    ("pem"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-orange)
    ("p12"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-dorange)
    ("crt"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-lblue)
    ("pub"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-blue)
    ("gpg"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-lblue)
    ("kdb"            nerd-icons-octicon "nf-oct-key"            :face nerd-icons-green)
    ("kdbx"           nerd-icons-octicon "nf-oct-key"            :face nerd-icons-green)
    ("cache"          nerd-icons-octicon "nf-oct-database"       :face nerd-icons-green)
    ("db"             nerd-icons-octicon "nf-oct-database"       :face nerd-icons-dblue)
    ;; backup
    ("backup"         nerd-icons-mdicon "nf-md-file_restore"     :face nerd-icons-lblue)
    ("old"            nerd-icons-mdicon "nf-md-file_restore"     :face nerd-icons-lblue)
    ("bak"            nerd-icons-mdicon "nf-md-file_restore"     :face nerd-icons-lblue)))

(defvar                        nerd-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                  nerd-icons-octicon "nf-oct-tag"               :face nerd-icons-blue)
    ("^TODO$"                  nerd-icons-octicon "nf-oct-checklist"         :face nerd-icons-lyellow)
    ("^LICENSE\\(?:-\\|$\\)"   nerd-icons-octicon "nf-oct-book"              :face nerd-icons-blue)
    ("^COPYING$"               nerd-icons-octicon "nf-oct-book"              :face nerd-icons-blue)
    ("^readme"                 nerd-icons-octicon "nf-oct-book"              :face nerd-icons-lcyan)

    ;; Config
    ("nginx$"                  nerd-icons-devicon "nf-dev-nginx"             :face nerd-icons-dgreen)
    ("apache$"                 nerd-icons-devicon "nf-dev-apache"            :face nerd-icons-dgreen)

    ;; C
    ("^Makefile$"              nerd-icons-sucicon "nf-seti-makefile"         :face nerd-icons-dorange)
    ("^CMakeLists.txt$"        nerd-icons-sucicon "nf-seti-makefile"         :face nerd-icons-red)
    ("^CMakeCache.txt$"        nerd-icons-sucicon "nf-seti-makefile"         :face nerd-icons-blue)
    ("^meson.build$"           nerd-icons-sucicon "nf-seti-makefile"         :face nerd-icons-purple)
    ("^meson_options.txt$"     nerd-icons-sucicon "nf-seti-makefile"         :face nerd-icons-purple)

    ;; Docker
    ("^\\.?Dockerfile"         nerd-icons-sucicon "nf-seti-docker"           :face nerd-icons-blue)

    ;; Homebrew
    ("^Brewfile$"              nerd-icons-faicon "nf-fa-beer"                :face nerd-icons-lsilver)

    ;; Arch
    ("^PKGBUILD$"              nerd-icons-flicon "nf-linux-archlinux"        :face nerd-icons-cyan)
    ("^\\.SRCINFO$"            nerd-icons-flicon "nf-linux-archlinux"        :face nerd-icons-red)

    ;; ;; AWS
    ("^stack.*.json$"          nerd-icons-devicon "nf-dev-aws"               :face nerd-icons-orange)
    ("^serverless\\.yml$"      nerd-icons-faicon "nf-fa-bolt"                :face nerd-icons-yellow)

    ;; lock files
    ("~$"                      nerd-icons-octicon "nf-oct-lock"              :face nerd-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"              nerd-icons-sucicon "nf-seti-elixir"           :face nerd-icons-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$" nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-red)
    ("_?test\\.rb$"            nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-red)
    ("_?test_helper\\.rb$"     nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-dred)
    ("_?spec\\.rb$"            nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-red)
    ("_?spec_helper\\.rb$"     nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-dred)
    ;;; Cocoapods
    ("^Podfile\\(\\.lock\\)?$" nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-red)
    ;;; Danger
    ("^Dangerfile\\(\\.lock\\)?$" nerd-icons-octicon "nf-oct-ruby"            :face nerd-icons-red)
    ;;; Fastlane
    ("^Appfile\\(\\.lock\\)?$" nerd-icons-octicon "nf-oct-ruby"              :face nerd-icons-red)
    ("^Matchfile\\(\\.lock\\)?$" nerd-icons-octicon "nf-oct-ruby"            :face nerd-icons-red)

    ("-?spec\\.ts$"            nerd-icons-mdicon "nf-md-language_typescript" :face nerd-icons-blue)
    ("-?test\\.ts$"            nerd-icons-mdicon "nf-md-language_typescript" :face nerd-icons-blue)
    ("-?spec\\.js$"            nerd-icons-mdicon "nf-md-language_javascript" :face nerd-icons-lpurple)
    ("-?test\\.js$"            nerd-icons-mdicon "nf-md-language_javascript" :face nerd-icons-lpurple)
    ("-?spec\\.jsx$"           nerd-icons-mdicon "nf-md-react"               :face nerd-icons-blue-alt)
    ("-?test\\.jsx$"           nerd-icons-mdicon "nf-md-react"               :face nerd-icons-blue-alt)

    ;; Git
    ("^MERGE_"                 nerd-icons-octicon "nf-oct-git_merge"         :face nerd-icons-red)
    ("^COMMIT_EDITMSG"         nerd-icons-octicon "nf-oct-git_commit"        :face nerd-icons-red)

    ;; Stylesheeting
    ("^stylelint"               nerd-icons-sucicon "nf-seti-stylelint"        :face nerd-icons-lyellow)

    ;; JavaScript
    ("^package.json$"          nerd-icons-devicon "nf-dev-npm"               :face nerd-icons-red)
    ("^package.lock.json$"     nerd-icons-devicon "nf-dev-npm"               :face nerd-icons-dred)
    ("^yarn\\.lock"            nerd-icons-sucicon "nf-seti-yarn"             :face nerd-icons-blue-alt)
    ("\\.npmignore$"           nerd-icons-devicon "nf-dev-npm"               :face nerd-icons-dred)
    ("^bower.json$"            nerd-icons-devicon "nf-dev-bower"             :face nerd-icons-lorange)
    ("^gulpfile"               nerd-icons-devicon "nf-dev-gulp"              :face nerd-icons-lred)
    ("^gruntfile"              nerd-icons-devicon "nf-dev-grunt"             :face nerd-icons-lyellow)
    ("^webpack"                nerd-icons-mdicon "nf-md-webpack"             :face nerd-icons-lblue)

    ;; Go
    ("^go.mod$"                nerd-icons-sucicon "nf-seti-config"           :face nerd-icons-blue-alt)
    ("^go.work$"               nerd-icons-sucicon "nf-seti-config"           :face nerd-icons-blue-alt)

    ;; Groovy
    ("Jenkinsfile\\'"          nerd-icons-devicon "nf-dev-groovy"            :face nerd-icons-cyan-alt)

    ;; Emacs
    ("^bookmark"                nerd-icons-octicon "nf-oct-bookmark"          :face nerd-icons-lpink)

    ("^\\*scratch\\*$"         nerd-icons-faicon "nf-fa-sticky_note"         :face nerd-icons-lyellow)
    ("^\\*scratch.*"           nerd-icons-faicon "nf-fa-sticky_note"         :face nerd-icons-yellow)
    ("^\\*new-tab\\*$"         nerd-icons-mdicon "nf-md-star"                :face nerd-icons-cyan)
    ("Cask\\'"                 nerd-icons-sucicon "nf-custom-emacs"          :face nerd-icons-blue)
    ("Eask\\'"                 nerd-icons-sucicon "nf-custom-emacs"          :face nerd-icons-blue)

    ;; Rust
    ("^Cargo.toml$"            nerd-icons-devicon "nf-dev-rust"              :face nerd-icons-yellow)
    ("^Cargo.lock$"            nerd-icons-devicon "nf-dev-rust"              :face nerd-icons-dyellow)

    ("^\\."                    nerd-icons-octicon "nf-oct-gear")))

(defvar nerd-icons-default-file-icon
  '(nerd-icons-faicon "nf-fa-file_o"))

(defvar nerd-icons-dir-icon-alist
  '(
    ("trash"            nerd-icons-faicon "nf-fa-trash_o")
    ("dropbox"          nerd-icons-faicon "nf-fa-dropbox")
    ("google[ _-]drive" nerd-icons-mdicon "nf-md-folder_google_drive")
    ("github"           nerd-icons-sucicon "nf-custom-folder_github")
    ("^atom$"           nerd-icons-devicon "nf-dev-atom")
    ("documents"        nerd-icons-mdicon "nf-md-folder_file")
    ("download"         nerd-icons-mdicon "nf-md-folder_download")
    ("desktop"          nerd-icons-octicon "nf-oct-device_desktop")
    ("pictures"         nerd-icons-mdicon "nf-md-folder_image")
    ("photos"           nerd-icons-faicon "nf-fa-camera_retro")
    ("music"            nerd-icons-mdicon "nf-md-folder_music")
    ("movies"           nerd-icons-faicon "nf-fa-film")
    ("code"             nerd-icons-octicon "nf-oct-code")
    ("workspace"        nerd-icons-octicon "nf-oct-code")
    ;; ("test"             nerd-icons-devicon "test-dir")
    ("\\.git"           nerd-icons-sucicon "nf-custom-folder_git")
    ("\\.config"        nerd-icons-sucicon "nf-custom-folder_config")
    (".?"               nerd-icons-sucicon "nf-custom-folder_oct")))

(defvar nerd-icons-weather-icon-alist
  '(
    ("tornado"               nerd-icons-wicon "nf-weather-tornado")
    ("hurricane"             nerd-icons-wicon "nf-weather-hurricane")
    ("thunderstorms"         nerd-icons-wicon "nf-weather-thunderstorm")
    ("sunny"                 nerd-icons-wicon "nf-weather-day_sunny")
    ("rain.*snow"            nerd-icons-wicon "nf-weather-rain_mix")
    ("rain.*hail"            nerd-icons-wicon "nf-weather-rain_mix")
    ("sleet"                 nerd-icons-wicon "nf-weather-sleet")
    ("hail"                  nerd-icons-wicon "nf-weather-hail")
    ("drizzle"               nerd-icons-wicon "nf-weather-sprinkle")
    ("rain"                  nerd-icons-wicon "nf-weather-showers")
    ("showers"               nerd-icons-wicon "nf-weather-showers")
    ("blowing.*snow"         nerd-icons-wicon "nf-weather-snow_wind")
    ("snow"                  nerd-icons-wicon "nf-weather-snow")
    ("dust"                  nerd-icons-wicon "nf-weather-dust")
    ("fog"                   nerd-icons-wicon "nf-weather-fog")
    ("haze"                  nerd-icons-wicon "nf-weather-day_haze")
    ("smoky"                 nerd-icons-wicon "nf-weather-smoke")
    ("blustery"              nerd-icons-wicon "nf-weather-cloudy_windy")
    ("windy"                 nerd-icons-wicon "nf-weather-cloudy_gusts")
    ("cold"                  nerd-icons-wicon "nf-weather-snowflake_cold")
    ("partly.*cloudy.*night" nerd-icons-wicon "nf-weather-night_alt_partly_cloudy")
    ("partly.*cloudy"        nerd-icons-wicon "nf-weather-day_cloudy_high")
    ("cloudy.*night"         nerd-icons-wicon "nf-weather-night_alt_cloudy")
    ("cxloudy.*day"          nerd-icons-wicon "nf-weather-day_cloudy")
    ("cloudy"                nerd-icons-wicon "nf-weather-cloudy")
    ("clear.*night"          nerd-icons-wicon "nf-weather-night_clear")
    ("fair.*night"           nerd-icons-wicon "nf-weather-stars")
    ("fair.*day"             nerd-icons-wicon "nf-weather-horizon")
    ("hot"                   nerd-icons-wicon "nf-weather-hot")
    ("not.*available"        nerd-icons-wicon "nf-weather-na")))

(defvar nerd-icons-mode-icon-alist
  '(
    (emacs-lisp-mode                   nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-purple)
    (lisp-data-mode                    nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-purple)
    (circe-server-mode                 nerd-icons-faicon "nf-fa-commenting_o")
    (circe-channel-mode                nerd-icons-faicon "nf-fa-commenting_o")
    (circe-query-mode                  nerd-icons-faicon "nf-fa-commenting_o")
    (crystal-mode                      nerd-icons-sucicon "nf-custom-crystal"         :face nerd-icons-yellow)
    (erc-mode                          nerd-icons-faicon "nf-fa-commenting_o")
    (inferior-emacs-lisp-mode          nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-lblue)
    (dired-mode                        nerd-icons-octicon "nf-oct-file_directory")
    (lisp-interaction-mode             nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-orange)
    (sly-mrepl-mode                    nerd-icons-sucicon "nf-custom-common_lisp"     :face nerd-icons-orange)
    (slime-repl-mode                   nerd-icons-sucicon "nf-custom-common_lisp"     :face nerd-icons-orange)
    (org-mode                          nerd-icons-sucicon "nf-custom-orgmode"         :face nerd-icons-lgreen)
    (ledger-mode                       nerd-icons-mdicon "nf-md-file_table_box_multiple"   :face nerd-icons-green)
    (typescript-mode                   nerd-icons-mdicon "nf-md-language_typescript"  :face nerd-icons-blue-alt)
    (typescript-ts-mode                nerd-icons-mdicon "nf-md-language_typescript"  :face nerd-icons-blue-alt)
    (typescript-tsx-mode               nerd-icons-mdicon "nf-md-language_typescript"  :face nerd-icons-blue-alt)
    (tsx-ts-mode                       nerd-icons-mdicon "nf-md-language_typescript"  :face nerd-icons-blue-alt)
    (js-mode                           nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-yellow)
    (js-ts-mode                        nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-yellow)
    (js-jsx-mode                       nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-yellow)
    (js2-mode                          nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-yellow)
    (js3-mode                          nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-yellow)
    (rjsx-mode                         nerd-icons-devicon "nf-dev-javascript"         :face nerd-icons-cyan-alt)
    (term-mode                         nerd-icons-devicon "nf-dev-terminal")
    (vterm-mode                        nerd-icons-devicon "nf-dev-terminal")
    (eat-mode                          nerd-icons-devicon "nf-dev-terminal")
    (eshell-mode                       nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-purple)
    (magit-refs-mode                   nerd-icons-devicon "nf-dev-git_branch"         :face nerd-icons-red)
    (magit-process-mode                nerd-icons-octicon "nf-oct-mark_github")
    (magit-diff-mode                   nerd-icons-devicon "nf-dev-git_compare"        :face nerd-icons-lblue)
    (ediff-mode                        nerd-icons-devicon "nf-dev-git_compare"        :face nerd-icons-red)
    (diff-mode                         nerd-icons-octicon "nf-oct-file_diff"          :face nerd-icons-lred)
    (comint-mode                       nerd-icons-faicon "nf-fa-terminal"             :face nerd-icons-lblue)
    (eww-mode                          nerd-icons-faicon "nf-fa-firefox"              :face nerd-icons-red)
    (xwidget-webkit-mode               nerd-icons-faicon "nf-fa-chrome"               :face nerd-icons-blue)
    (org-agenda-mode                   nerd-icons-octicon "nf-oct-checklist"          :face nerd-icons-lgreen)
    (cfw:calendar-mode                 nerd-icons-octicon "nf-oct-calendar")
    (ibuffer-mode                      nerd-icons-faicon "nf-fa-files_o"              :face nerd-icons-dsilver)
    (messages-buffer-mode              nerd-icons-faicon "nf-fa-file_o"               :face nerd-icons-dsilver)
    (help-mode                         nerd-icons-faicon "nf-fa-info"                 :face nerd-icons-purple)
    (helpful-mode                      nerd-icons-faicon "nf-fa-info"                 :face nerd-icons-purple)
    (Info-mode                         nerd-icons-faicon "nf-fa-info"                 :face nerd-icons-blue)
    (benchmark-init/tree-mode          nerd-icons-faicon "nf-fa-dashboard")
    (jenkins-mode                      nerd-icons-devicon "nf-dev-jenkins"            :face nerd-icons-blue)
    (magit-popup-mode                  nerd-icons-sucicon "nf-seti-git"               :face nerd-icons-red)
    (magit-status-mode                 nerd-icons-sucicon "nf-seti-git"               :face nerd-icons-lred)
    (magit-log-mode                    nerd-icons-sucicon "nf-seti-git"               :face nerd-icons-green)
    (mu4e-compose-mode                 nerd-icons-octicon "nf-oct-pencil")
    (mu4e-headers-mode                 nerd-icons-octicon "nf-oct-mail")
    (mu4e-main-mode                    nerd-icons-octicon "nf-oct-mail")
    (mu4e-view-mode                    nerd-icons-codicon "nf-cod-mail_read")
    (notmuch-hello-mode                nerd-icons-octicon "nf-oct-mail")
    (notmuch-search-mode               nerd-icons-octicon "nf-oct-mail")
    (notmuch-tree-mode                 nerd-icons-octicon "nf-oct-mail")
    (notmuch-show-mode                 nerd-icons-codicon "nf-cod-mail_read")
    (sieve-mode                        nerd-icons-octicon "nf-oct-mail")
    (gnus-group-mode                   nerd-icons-octicon "nf-oct-mail")
    (gnus-summary-mode                 nerd-icons-octicon "nf-oct-mail")
    (gnus-article-mode                 nerd-icons-codicon "nf-cod-mail_read")
    (message-mode                      nerd-icons-octicon "nf-oct-pencil")
    (package-menu-mode                 nerd-icons-faicon "nf-fa-archive"              :face nerd-icons-silver)
    (paradox-menu-mode                 nerd-icons-faicon "nf-fa-archive"              :face nerd-icons-silver)
    (Custom-mode                       nerd-icons-codicon "nf-cod-settings")

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode                          nerd-icons--web-mode-icon)

    (fundamental-mode                  nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-dsilver)
    (special-mode                      nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-yellow)
    (cask-mode                         nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-blue)
    (eask-mode                         nerd-icons-sucicon "nf-custom-emacs"           :face nerd-icons-blue)
    (text-mode                         nerd-icons-faicon "nf-fa-file_text"            :face nerd-icons-cyan)
    (enh-ruby-mode                     nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-lred)
    (ruby-mode                         nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-lred)
    (ruby-ts-mode                      nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-lred)
    (inf-ruby-mode                     nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-red)
    (projectile-rails-compilation-mode nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-red)
    (rspec-compilation-mode            nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-red)
    (rake-compilation-mode             nerd-icons-devicon "nf-dev-ruby"               :face nerd-icons-red)
    (sh-mode                           nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-purple)
    (bash-ts-mode                      nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-purple)
    (shell-mode                        nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-purple)
    (fish-mode                         nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-lpink)
    (bat-mode                          nerd-icons-codicon "nf-cod-terminal_cmd"       :face nerd-icons-lsilver)
    (nginx-mode                        nerd-icons-devicon "nf-dev-nginx"              :face nerd-icons-dgreen)
    (apache-mode                       nerd-icons-devicon "nf-dev-apache"             :face nerd-icons-dgreen)
    (makefile-mode                     nerd-icons-devicon "nf-dev-gnu"                :face nerd-icons-dorange)
    (makefile-ts-mode                  nerd-icons-devicon "nf-dev-gnu"                :face nerd-icons-dorange)
    ;; (cmake-mode                     nerd-icons-fileicon "cmake"                    :face nerd-icons-red)
    ;; (cmake-ts-mode                  nerd-icons-fileicon "cmake"                    :face nerd-icons-red)
    (dockerfile-mode                   nerd-icons-sucicon "nf-seti-docker"            :face nerd-icons-blue)
    (dockerfile-ts-mode                nerd-icons-sucicon "nf-seti-docker"            :face nerd-icons-blue)
    (docker-compose-mode               nerd-icons-sucicon "nf-seti-docker"            :face nerd-icons-lblue)
    (nxml-mode                         nerd-icons-faicon "nf-fa-file_code_o"          :face nerd-icons-lorange)
    (conf-mode                         nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-lyellow)
    (json-mode                         nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-yellow)
    (json-ts-mode                      nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-yellow)
    (jsonian-mode                      nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-yellow)
    (yaml-mode                         nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-dyellow)
    (yaml-ts-mode                      nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-dyellow)
    (toml-mode                         nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-orange)
    (toml-ts-mode                      nerd-icons-codicon "nf-cod-settings"           :face nerd-icons-orange)
    (elisp-byte-code-mode              nerd-icons-octicon "nf-oct-file_binary"        :face nerd-icons-dsilver)
    (archive-mode                      nerd-icons-octicon "nf-oct-file_zip"           :face nerd-icons-lmaroon)
    (elm-mode                          nerd-icons-sucicon "nf-custom-elm"             :face nerd-icons-blue)
    (erlang-mode                       nerd-icons-devicon "nf-dev-erlang"             :face nerd-icons-red)
    (elixir-mode                       nerd-icons-sucicon "nf-custom-elixir"          :face nerd-icons-lorange)
    (elixir-ts-mode                    nerd-icons-sucicon "nf-custom-elixir"          :face nerd-icons-lorange)
    (java-mode                         nerd-icons-devicon "nf-dev-java"               :face nerd-icons-purple)
    (groovy-mode                       nerd-icons-devicon "nf-dev-groovy"             :face nerd-icons-cyan)
    (java-ts-mode                      nerd-icons-devicon "nf-dev-java"               :face nerd-icons-purple)
    (go-mode                           nerd-icons-sucicon "nf-seti-go2"               :face nerd-icons-blue)
    (go-ts-mode                        nerd-icons-sucicon "nf-seti-go2"               :face nerd-icons-blue)
    (go-dot-mod-mode                   nerd-icons-sucicon "nf-seti-config"            :face nerd-icons-blue-alt)
    (go-mod-ts-mode                    nerd-icons-sucicon "nf-seti-config"            :face nerd-icons-blue-alt)
    (go-dot-work-mode                  nerd-icons-sucicon "nf-seti-config"            :face nerd-icons-blue-alt)
    (go-work-ts-mode                   nerd-icons-sucicon "nf-seti-config"            :face nerd-icons-blue-alt)
    (graphql-mode                      nerd-icons-sucicon "nf-seti-graphql"           :face nerd-icons-dpink)
    (matlab-mode                       nerd-icons-devicon "nf-dev-matlab"             :face nerd-icons-orange)
    (nix-mode                          nerd-icons-mdicon  "nf-md-nix"                 :face nerd-icons-blue)
    (perl-mode                         nerd-icons-devicon "nf-dev-perl"               :face nerd-icons-lorange)
    (cperl-mode                        nerd-icons-devicon "nf-dev-perl"               :face nerd-icons-lorange)
    (php-mode                          nerd-icons-devicon "nf-dev-php"                :face nerd-icons-lsilver)
    (php-ts-mode                       nerd-icons-devicon "nf-dev-php"                :face nerd-icons-lsilver)
    (prolog-mode                       nerd-icons-devicon "nf-dev-prolog"             :face nerd-icons-lmaroon)
    (python-mode                       nerd-icons-devicon "nf-dev-python"             :face nerd-icons-dblue)
    (python-ts-mode                    nerd-icons-devicon "nf-dev-python"             :face nerd-icons-dblue)
    (inferior-python-mode              nerd-icons-devicon "nf-dev-python"             :face nerd-icons-dblue)
    ;; (racket-mode                    nerd-icons-fileicon "racket"                   :face nerd-icons-red)
    (rust-mode                         nerd-icons-devicon "nf-dev-rust"               :face nerd-icons-maroon)
    (rustic-mode                       nerd-icons-devicon "nf-dev-rust"               :face nerd-icons-maroon)
    (rust-ts-mode                      nerd-icons-devicon "nf-dev-rust"               :face nerd-icons-maroon)
    (scala-mode                        nerd-icons-devicon "nf-dev-scala"              :face nerd-icons-red)
    ;; (scheme-mode                    nerd-icons-fileicon   "scheme"                 :face nerd-icons-red)
    (swift-mode                        nerd-icons-devicon "nf-dev-swift"              :face nerd-icons-lred)
    (swift-ts-mode                     nerd-icons-devicon "nf-dev-swift"              :face nerd-icons-lred)
    (svelte-mode                       nerd-icons-sucicon "nf-seti-svelte"            :face nerd-icons-red)
    (ada-mode                          nerd-icons-sucicon "nf-custom-ada"             :face nerd-icons-blue)
    (ada-ts-mode                       nerd-icons-sucicon "nf-custom-ada"             :face nerd-icons-blue)
    (gpr-mode                          nerd-icons-sucicon "nf-custom-ada"             :face nerd-icons-green)
    (gpr-ts-mode                       nerd-icons-sucicon "nf-custom-ada"             :face nerd-icons-green)
    (c-mode                            nerd-icons-sucicon "nf-custom-c"               :face nerd-icons-blue)
    (c-ts-mode                         nerd-icons-sucicon "nf-custom-c"               :face nerd-icons-blue)
    (c++-mode                          nerd-icons-sucicon "nf-custom-cpp"             :face nerd-icons-blue)
    (c++-ts-mode                       nerd-icons-sucicon "nf-custom-cpp"             :face nerd-icons-blue)
    (csharp-mode                       nerd-icons-mdicon "nf-md-language_csharp"      :face nerd-icons-dblue)
    (csharp-ts-mode                    nerd-icons-mdicon "nf-md-language_csharp"      :face nerd-icons-dblue)
    (clojure-mode                      nerd-icons-devicon "nf-dev-clojure_alt"        :face nerd-icons-blue)
    (clojure-ts-mode                   nerd-icons-devicon "nf-dev-clojure_alt"        :face nerd-icons-blue)
    (cider-repl-mode                   nerd-icons-devicon "nf-dev-clojure_alt"        :face nerd-icons-green)
    (clojurec-mode                     nerd-icons-sucicon "nf-seti-clojure"           :face nerd-icons-blue)
    (clojurec-ts-mode                  nerd-icons-sucicon "nf-seti-clojure"           :face nerd-icons-blue)
    (clojurescript-mode                nerd-icons-devicon "nf-dev-clojure_alt"        :face nerd-icons-lyellow)
    (clojurescript-ts-mode             nerd-icons-devicon "nf-dev-clojure_alt"        :face nerd-icons-lyellow)
    (coffee-mode                       nerd-icons-devicon "nf-dev-coffeescript"       :face nerd-icons-maroon)
    (lisp-mode                         nerd-icons-sucicon "nf-custom-scheme"          :face nerd-icons-orange)
    (css-mode                          nerd-icons-devicon "nf-dev-css3"               :face nerd-icons-yellow)
    (css-ts-mode                       nerd-icons-devicon "nf-dev-css3"               :face nerd-icons-yellow)
    (scss-mode                         nerd-icons-mdicon "nf-md-sass"                 :face nerd-icons-pink)
    (sass-mode                         nerd-icons-mdicon "nf-md-sass"                 :face nerd-icons-dpink)
    (less-css-mode                     nerd-icons-devicon "nf-dev-less"               :face nerd-icons-dyellow)
    (stylus-mode                       nerd-icons-devicon "nf-dev-stylus"             :face nerd-icons-lgreen)
    (csv-mode                          nerd-icons-octicon "nf-oct-graph"              :face nerd-icons-dblue)
    (gdscript-mode                     nerd-icons-sucicon "nf-seti-godot"             :face nerd-icons-blue)
    (gdscript-ts-mode                  nerd-icons-sucicon "nf-seti-godot"             :face nerd-icons-blue)
    (haskell-mode                      nerd-icons-devicon "nf-dev-haskell"            :face nerd-icons-red)
    (haskell-c2hs-mode                 nerd-icons-devicon "nf-dev-haskell"            :face nerd-icons-red)
    (literate-haskell-mode             nerd-icons-devicon "nf-dev-haskell"            :face nerd-icons-red)
    (haml-mode                         nerd-icons-sucicon "nf-seti-haml"              :face nerd-icons-lyellow)
    (html-mode                         nerd-icons-devicon "nf-dev-html5"              :face nerd-icons-orange)
    (html-ts-mode                      nerd-icons-devicon "nf-dev-html5"              :face nerd-icons-orange)
    (rhtml-mode                        nerd-icons-devicon "nf-dev-html5"              :face nerd-icons-lred)
    ;; (mustache-mode                  nerd-icons-fileicon "moustache"                :face nerd-icons-green)
    (slim-mode                         nerd-icons-codicon "nf-cod-dashboard"          :face nerd-icons-yellow)
    (jade-mode                         nerd-icons-sucicon "nf-seti-jade"              :face nerd-icons-red)
    (pug-mode                          nerd-icons-sucicon "nf-seti-pug"               :face nerd-icons-red)
    (react-mode                        nerd-icons-devicon "nf-dev-react"              :face nerd-icons-lblue)
    (image-mode                        nerd-icons-octicon "nf-oct-file_media"         :face nerd-icons-blue)
    (texinfo-mode                      nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (markdown-mode                     nerd-icons-octicon "nf-oct-markdown"           :face nerd-icons-lblue)
    (markdown-ts-mode                  nerd-icons-octicon "nf-oct-markdown"           :face nerd-icons-lblue)
    ;; (bibtex-mode                    nerd-icons-fileicon "bib"                      :face nerd-icons-maroon)
    (compilation-mode                  nerd-icons-faicon "nf-fa-cogs")
    (objc-mode                         nerd-icons-faicon "nf-fa-apple")
    (tuareg-mode                       nerd-icons-sucicon "nf-seti-ocaml")
    (purescript-mode                   nerd-icons-sucicon "nf-seti-purescript")
    (verilog-mode                      nerd-icons-mdicon "nf-md-chip"                 :face nerd-icons-red)
    (verilog-ts-mode                   nerd-icons-mdicon "nf-md-chip"                 :face nerd-icons-red)
    (vhdl-mode                         nerd-icons-octicon "nf-oct-cpu"                :face nerd-icons-blue)
    (vhdl-ts-mode                      nerd-icons-octicon "nf-oct-cpu"                :face nerd-icons-blue)
    ;; (haskell-cabal-mode             nerd-icons-fileicon "cabal"                    :face nerd-icons-lblue)
    (kotlin-mode                       nerd-icons-sucicon "nf-custom-kotlin"          :face nerd-icons-orange)
    (kotlin-ts-mode                    nerd-icons-sucicon "nf-custom-kotlin"          :face nerd-icons-orange)
    (nim-mode                          nerd-icons-sucicon "nf-seti-nim"               :face nerd-icons-yellow)
    (sql-mode                          nerd-icons-devicon  "nf-dev-database"          :face nerd-icons-silver)
    (lua-mode                          nerd-icons-sucicon "nf-seti-lua"               :face nerd-icons-dblue)
    (lua-ts-mode                       nerd-icons-sucicon "nf-seti-lua"               :face nerd-icons-dblue)
    ;; (adoc-mode                      nerd-icons-fileicon "asciidoc"                 :face nerd-icons-lblue)
    (puppet-mode                       nerd-icons-sucicon "nf-custom-puppet"          :face nerd-icons-yellow)
    (jinja2-mode                       nerd-icons-sucicon "nf-seti-jinja"             :face nerd-icons-silver)
    (powershell-mode                   nerd-icons-mdicon "nf-md-powershell"           :face nerd-icons-blue)
    (tex-mode                          nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (latex-mode                        nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (latex-ts-mode                     nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (doctex-mode                       nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    ;; Modes provided by AUCTeX 14.1 and higher
    (TeX-mode                          nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (LaTeX-mode                        nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (docTeX-mode                       nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (ConTeXt-mode                      nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (AmSTeX-mode                       nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (plain-TeX-mode                    nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (Texinfo-mode                      nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (japanese-plain-TeX-mode           nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (japanese-LaTeX-mode               nerd-icons-sucicon "nf-seti-tex"               :face nerd-icons-lred)
    (dart-mode                         nerd-icons-devicon "nf-dev-dart"               :face nerd-icons-blue)
    (dart-ts-mode                      nerd-icons-devicon "nf-dev-dart"               :face nerd-icons-blue)
    (fsharp-mode                       nerd-icons-devicon "nf-dev-fsharp"             :face nerd-icons-blue)
    (asm-mode                          nerd-icons-sucicon "nf-seti-asm"               :face nerd-icons-blue)
    (fasm-mode                         nerd-icons-sucicon "nf-seti-asm"               :face nerd-icons-blue)
    (masm-mode                         nerd-icons-sucicon "nf-seti-asm"               :face nerd-icons-blue)
    (nasm-mode                         nerd-icons-sucicon "nf-seti-asm"               :face nerd-icons-blue)
    (gas-mode                          nerd-icons-sucicon "nf-seti-asm"               :face nerd-icons-blue)
    ;; (tcl-mode                       nerd-icons-fileicon "tcl"                      :face nerd-icons-dred)
    ;; (cuda-mode                      nerd-icons-fileicon "nvidia"                   :face nerd-icons-green)
    (f90-mode                          nerd-icons-mdicon "nf-md-language_fortran"     :face nerd-icons-purple)
    ;; (hy-mode                        nerd-icons-fileicon "hy"                       :face nerd-icons-blue)
    (glsl-mode                         nerd-icons-faicon "nf-fa-paint_brush"          :face nerd-icons-green)
    (zig-mode                          nerd-icons-sucicon "nf-seti-zig"               :face nerd-icons-orange)
    ;; (odin-mode                      nerd-icons-fileicon "odin"                     :face nerd-icons-lblue)
    (pdf-view-mode                     nerd-icons-codicon  "nf-cod-file_pdf"          :face nerd-icons-dred)
    (doc-view-mode                     nerd-icons-mdicon   "nf-md-file_document"      :face nerd-icons-lred)
    (calibre-library-mode              nerd-icons-codicon  "nf-cod-library"           :face nerd-icons-dblue)
    (calibre-edit-mode                 nerd-icons-codicon  "nf-cod-library"           :face nerd-icons-lred)
    (calibredb-search-mode             nerd-icons-codicon  "nf-cod-library"           :face nerd-icons-dblue)
    (calibredb-show-mode               nerd-icons-codicon  "nf-cod-library"           :face nerd-icons-lblue)
    (osm-mode                          nerd-icons-mdicon   "nf-md-map_search"         :face nerd-icons-lgreen)
    (spacemacs-buffer-mode             nerd-icons-sucicon  "nf-custom-emacs"          :face nerd-icons-purple)

    (elfeed-search-mode                nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-orange)
    (elfeed-summary-mode               nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-orange)
    (elfeed-show-mode                  nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-yellow)
    (newsticker-treeview-mode          nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-orange)
    (newsticker-treeview-list-mode     nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-lorange)
    (newsticker-treeview-item-mode     nerd-icons-faicon   "nf-fa-rss_square"         :face nerd-icons-yellow)

    (emms-browser-mode                 nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (emms-lyrics-mode                  nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (emms-show-all-mode                nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (emms-metaplaylist-mode            nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (emms-tag-editor-mode              nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (emms-playlist-mode                nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-silver)
    (lilypond-mode                     nerd-icons-faicon   "nf-fa-music"              :face nerd-icons-green)
    (bongo-playlist-mode               nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (bongo-library-mode                nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (mingus-playlist-mode              nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (mingus-help-mode                  nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (mingus-browse-mode                nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (mingus-burn-mode                  nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (simple-mpc-mode                   nerd-icons-faicon "nf-fa-music"                :face nerd-icons-silver)
    (telega-root-mode                  nerd-icons-faicon "nf-fae-telegram"            :face nerd-icons-purple)
    (telega-chat-mode                  nerd-icons-faicon "nf-fae-telegram"            :face nerd-icons-blue)
    (mastodon-mode                     nerd-icons-mdicon "nf-md-mastodon"             :face nerd-icons-blue)

    (fanyi-mode                        nerd-icons-codicon "nf-cod-library"            :face nerd-icons-lcyan)
    (osx-dictionary-mode               nerd-icons-codicon "nf-cod-library"            :face nerd-icons-lcyan)
    (youdao-dictionary-mode            nerd-icons-codicon "nf-cod-library"            :face nerd-icons-lcyan)

    (magik-mode                        nerd-icons-faicon "nf-fa-magic"                :face nerd-icons-blue)
    (magik-ts-mode                     nerd-icons-faicon "nf-fa-magic"                :face nerd-icons-blue)
    (magik-session-mode                nerd-icons-devicon "nf-dev-terminal"           :face nerd-icons-blue)
    (magik-cb-mode                     nerd-icons-faicon "nf-fa-book"                 :face nerd-icons-blue)
    ;; (meson-mode                     nerd-icons-fileicon "meson"                    :face nerd-icons-purple)
    ;; (man-common                     nerd-icons-fileicon "man-page"                 :face nerd-icons-blue)
    (heex-mode                         nerd-icons-sucicon "nf-seti-elixir"            :face nerd-icons-lorange)
    (heex-ts-mode                      nerd-icons-sucicon "nf-seti-elixir"            :face nerd-icons-lorange)
    (julia-mode                        nerd-icons-sucicon "nf-seti-julia"             :face nerd-icons-purple)
    (julia-ts-mode                     nerd-icons-sucicon "nf-seti-julia"             :face nerd-icons-purple)
    (flycheck-error-list               nerd-icons-faicon "nf-fa-list_alt"             :face nerd-icons-lred)
    (exwm-mode                         nerd-icons-flicon "nf-linux-xorg"              :face nerd-icons-dsilver)
    (proced-mode                       nerd-icons-codicon "nf-cod-dashboard"          :face nerd-icons-green)
    (bluetooth-mode                    nerd-icons-faicon "nf-fa-bluetooth"            :face nerd-icons-blue)
    (disk-usage                        nerd-icons-faicon "nf-fa-pie_chart"            :face nerd-icons-lred)
    (awk-mode                          nerd-icons-devicon "nf-dev-awk"                :face nerd-icons-yellow)
    (ement-directory-mode              nerd-icons-faicon "nf-fa-search"               :face nerd-icons-dpurple)
    (ement-describe-room-mode          nerd-icons-faicon "nf-fa-info"                 :face nerd-icons-dpurple)
    (ement-room-list-mode              nerd-icons-faicon "nf-fa-list"                 :face nerd-icons-dpurple)
    (ement-tabulated-room-list-mode    nerd-icons-faicon "nf-fa-table"                :face nerd-icons-dpurple)
    (ement-room-mode                   nerd-icons-faicon "nf-fa-comments_o"           :face nerd-icons-dpurple)
    (ement-notifications-mode          nerd-icons-faicon "nf-fa-inbox"                :face nerd-icons-dpurple)))

(defvar nerd-icons-url-alist
  '(
    ;; Social media and communities
    ("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us"          nerd-icons-faicon "nf-fa-delicious")
    ("^\\(https?://\\)?\\(www\\.\\)?behance\\.net"            nerd-icons-faicon "nf-fa-behance")
    ("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com"           nerd-icons-faicon "nf-fa-dribbble")
    ("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com"           nerd-icons-faicon "nf-fa-facebook_official")
    ("^\\(https?://\\)?\\(www\\.\\)?glide\\.me"               nerd-icons-faicon "nf-fa-glide_g")
    ("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com"      nerd-icons-faicon "nf-fa-google_plus")
    ("linkedin\\.com"                                         nerd-icons-faicon "nf-fa-linkedin")
    ("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru"                  nerd-icons-faicon "nf-fa-odnoklassniki")
    ("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com"             nerd-icons-faicon "nf-fa-reddit_alien")
    ("^\\(https?://\\)?\\(www\\.\\)?slack\\.com"              nerd-icons-faicon "nf-fa-slack")
    ("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com"           nerd-icons-faicon "nf-fa-snapchat_ghost")
    ("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com"              nerd-icons-faicon "nf-fa-weibo")
    ("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com"            nerd-icons-faicon "nf-fa-twitter")
    ;; Blogging
    ("joomla\\.org"                                           nerd-icons-faicon "nf-fa-joomla")
    ("^\\(https?://\\)?\\(www\\.\\)?medium\\.com"             nerd-icons-faicon "nf-fa-medium")
    ("tumblr\\.com"                                           nerd-icons-faicon "nf-fa-tumblr")
    ("^wordpress\\.com"                                       nerd-icons-faicon "nf-fa-wordpress")
    ;; Programming
    ("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org"          nerd-icons-faicon "nf-fa-bitbucket")
    ("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io"             nerd-icons-faicon "nf-fa-codepen")
    ("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com"           nerd-icons-faicon "nf-fa-codiepie")
    ("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com"      nerd-icons-octicon "nf-oct-logo_gist")
    ("^\\(https?://\\)?\\(www\\.\\)?github\\.com"             nerd-icons-octicon "nf-oct-mark_github")
    ("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com"             nerd-icons-faicon "nf-fa-gitlab")
    ("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" nerd-icons-faicon "nf-fa-hacker_news")
    ("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net"           nerd-icons-faicon "nf-fa-jsfiddle")
    ("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com"             nerd-icons-faicon "nf-fa-maxcdn")
    ("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com"      nerd-icons-faicon "nf-fa-stack_overflow")
    ;; Video
    ("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv"              nerd-icons-faicon "nf-fa-twitch")
    ("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com"              nerd-icons-faicon "nf-fa-vimeo")
    ("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com"            nerd-icons-faicon "nf-fa-youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be"               nerd-icons-faicon "nf-fa-youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?vine\\.co"                nerd-icons-faicon "nf-fa-vine")
    ;; Sound
    ("^\\(https?://\\)?\\(www\\.\\)?last\\.fm"                nerd-icons-faicon "nf-fa-lastfm")
    ("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com"           nerd-icons-faicon "nf-fa-mixcloud")
    ("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com"         nerd-icons-faicon "nf-fa-soundcloud")
    ("spotify\\.com"                                          nerd-icons-faicon "nf-fa-spotify")
    ;; Shopping
    ("^\\(https?://\\)?\\(www\\.\\)?amazon\\."                nerd-icons-faicon "nf-fa-amazon")
    ("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com"           nerd-icons-faicon "nf-fa-opencart")
    ("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com"             nerd-icons-faicon "nf-fa-paypal")
    ("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com"       nerd-icons-faicon "nf-fa-shitsinbulk")
    ;; Images
    ("^\\(https?://\\)?\\(www\\.\\)?500px\\.com"              nerd-icons-faicon "nf-fa-500px")
    ("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com"         nerd-icons-faicon "nf-fa-deviantart")
    ("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com"             nerd-icons-faicon "nf-fa-flickr")
    ("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com"          nerd-icons-faicon "nf-fa-instagram")
    ("^\\(https?://\\)?\\(www\\.\\)?pinterest\\."             nerd-icons-faicon "nf-fa-pinterest")
    ;; Information and books
    ("^\\(https?://\\)?\\(www\\.\\)?digg\\.com"               nerd-icons-faicon "nf-fa-digg")
    ("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com"         nerd-icons-faicon "nf-fa-foursquare")
    ("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com"          nerd-icons-faicon "nf-fa-get_pocket")
    ("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com"             nerd-icons-faicon "nf-fa-scribd")
    ("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net"         nerd-icons-faicon "nf-fa-slideshare")
    ("stackexchange\\.com"                                    nerd-icons-faicon "nf-fa-stack_exchange")
    ("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com"        nerd-icons-faicon "nf-fa-stumbleupon")
    ("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\."           nerd-icons-faicon "nf-fa-tripadvisor")
    ("^\\(https?://\\)?\\(www\\.\\)?yelp\\."                  nerd-icons-faicon "nf-fa-yelp")

    ("wikipedia\\.org"                                        nerd-icons-faicon "nf-fa-wikipedia_w")
    ;; Various companies and tools
    ("^\\(https?://\\)?\\(www\\.\\)?angel\\.co"               nerd-icons-faicon "nf-fa-angellist")
    ("^\\(https?://\\)?\\(www\\.\\)?apple\\.com"              nerd-icons-faicon "nf-fa-apple")
    ("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com"         nerd-icons-faicon "nf-fa-buysellads")
    ("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com"     nerd-icons-faicon "nf-fa-connectdevelop")
    ("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com"           nerd-icons-faicon "nf-fa-dashcube")
    ("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com"            nerd-icons-faicon "nf-fa-dropbox")
    ("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com"      nerd-icons-faicon "nf-fa-envira")
    ("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com"        nerd-icons-faicon "nf-fa-fort_awesome")
    ("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com"           nerd-icons-faicon "nf-fa-forumbee")
    ("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com"           nerd-icons-faicon "nf-fa-gratipay")
    ("^\\(https?://\\)?\\(www\\.\\)?modx\\.com"               nerd-icons-faicon "nf-fa-modx")
    ("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com"          nerd-icons-faicon "nf-fa-pagelines")
    ("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com"        nerd-icons-faicon "nf-fa-product_hunt")
    ("sellsy\\.com"                                           nerd-icons-faicon "nf-fa-sellsy")
    ("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com"        nerd-icons-faicon "nf-fa-simplybuilt")
    ("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com"           nerd-icons-faicon "nf-fa-skyatlas")
    ("^\\(https?://\\)?\\(www\\.\\)?skype\\.com"              nerd-icons-faicon "nf-fa-skype")
    ("steampowered\\.com"                                     nerd-icons-faicon "nf-fa-steam")
    ("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com"          nerd-icons-faicon "nf-fa-themeisle")
    ("^\\(https?://\\)?\\(www\\.\\)?trello\\.com"             nerd-icons-faicon "nf-fa-trello")
    ("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com"           nerd-icons-faicon "nf-fa-whatsapp")
    ("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com"        nerd-icons-faicon "nf-fa-y_combinator")
    ("yahoo\\.com"                                            nerd-icons-faicon "nf-fa-yahoo")
    ("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com"              nerd-icons-faicon "nf-fa-yoast")
    ;; Catch all
    ("android"                                                nerd-icons-faicon "nf-fa-android")
    ("creativecommons"                                        nerd-icons-faicon "nf-fa-creative_commons")
    ("forums?"                                                nerd-icons-codicon "nf-cod-comment_discussion")
    ("\\.pdf$"                                                nerd-icons-codicon "nf-cod-file_pdf" :face nerd-icons-dred)
    ("google"                                                 nerd-icons-faicon "nf-fa-google")
    ("\\.rss"                                                 nerd-icons-faicon "nf-fa-rss")))

(defun nerd-icons-auto-mode-match? (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (nerd-icons-match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun nerd-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (cl-find-if (lambda (it) (string-match (car it) file)) alist)))

(defun nerd-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))

    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

(defun nerd-icons--read-candidates ()
  "Helper to build a list of candidates for all glyph sets."
  (cl-reduce 'append (mapcar (lambda (it) (nerd-icons--read-candidates-for-glyph-set it t)) nerd-icons-glyph-sets)))

(defun nerd-icons--read-candidates-for-glyph-set (glyph-set &optional show-glyph-set)
  "Helper to build read candidates for GLYPH-SET.

If SHOW-GLYPH-SET is non-nil, displays the icons glyph set in the candidate
string."
  (let ((data   (funcall (nerd-icons--data-name glyph-set)))
        (icon-f (nerd-icons--function-name glyph-set)))
    (mapcar
     (lambda (it)
       (let* ((icon-name (car it))

              (icon-display (funcall icon-f icon-name))
              (icon-glyph-set (if show-glyph-set (format "\t[%s]" glyph-set) ""))

              (candidate-name (format "%s\t%s%s" icon-display icon-name icon-glyph-set))
              (candidate-icon (funcall (nerd-icons--function-name glyph-set) icon-name)))
         (cons candidate-name candidate-icon)))
     data)))

;;;###autoload
(defun nerd-icons-install-fonts (&optional pfx)
  "Helper function to download and install the latests fonts based on OS.
The provided Nerd Font is Symbols Nerd Font Mono.
When PFX is non-nil, ignore the prompt and just install"
  (interactive "P")
  (when (or pfx (yes-or-no-p "This will download and install fonts, are you sure you want to do this?"))
    (let* ((url-format "https://raw.githubusercontent.com/rainstormstudio/nerd-icons.el/main/fonts/%s")
           (font-dest (cond
                       ;; Default Linux install directories
                       ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                        (concat (or (getenv "XDG_DATA_HOME")
                                    (concat (getenv "HOME") "/.local/share"))
                                "/fonts/"
                                nerd-icons-fonts-subdirectory))
                       ;; Default MacOS install directory
                       ((eq system-type 'darwin)
                        (concat (getenv "HOME")
                                "/Library/Fonts/"
                                nerd-icons-fonts-subdirectory))))
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            nerd-icons-font-names)
      (when known-dest?
        (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "%s Successfully %s `nerd-icons' fonts to `%s'!"
               (nerd-icons-wicon "nf-weather-stars" :v-adjust 0.0)
               (if known-dest? "installed" "downloaded")
               font-dest))))

;;;###autoload
(defun nerd-icons-insert (&optional arg glyph-set)
  "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When GLYPH-SET is non-nil, limit the candidates to the icon set matching it."
  (interactive "P")
  (let* ((standard-output (current-buffer))
         (candidates (if glyph-set
                         (nerd-icons--read-candidates-for-glyph-set glyph-set)
                       (nerd-icons--read-candidates)))
         (prompt     (if glyph-set
                         (format "%s Icon: " (funcall (nerd-icons--glyph-set-name glyph-set)))
                       "Icon : "))
         (selection (completing-read prompt candidates nil t))
         (result    (cdr (assoc selection candidates))))

    (if arg (prin1 result) (insert result))))

;;;###autoload
(defun nerd-icons-icon-for-dir (dir &rest arg-overrides)
  "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((dir (or dir default-directory "/"))
         (dirname (file-name-base (directory-file-name dir)))
         (path (if (file-name-absolute-p dir) dir (expand-file-name dir)))
         (icon (nerd-icons-match-to-alist dirname nerd-icons-dir-icon-alist))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (cond
     ((file-remote-p path)
      (apply #'nerd-icons-codicon "nf-cod-remote" (cdr args)))
     ((file-symlink-p path)
      (apply #'nerd-icons-codicon "nf-cod-file_symlink_directory" (cdr args)))
     ((nerd-icons-dir-is-submodule path)
      (apply #'nerd-icons-codicon "nf-cod-file_submodule" (cdr args)))
     ((file-exists-p (format "%s/.git" path))
      (apply #'nerd-icons-octicon "nf-oct-repo" (cdr args)))
     (t (apply (car icon) args)))))

;;;###autoload
(defun nerd-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((name (file-name-nondirectory file))
         (ext (file-name-extension name))
         (icon (or (and ext
                        (cdr (assoc (downcase ext)
                                    nerd-icons-extension-icon-alist)))
                   (nerd-icons-match-to-alist name nerd-icons-regexp-icon-alist)                   
                   nerd-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun nerd-icons-icon-for-extension (ext &rest arg-overrides)
  "Get the formatted icon for EXT.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (or
                (and ext
                     (cdr (assoc (downcase ext)
                                 nerd-icons-extension-icon-alist)))
                nerd-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun nerd-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (or (cdr (or (assoc mode nerd-icons-mode-icon-alist)
                            (assoc (get mode 'derived-mode-parent) nerd-icons-mode-icon-alist)))
                   nerd-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

;;;###autoload
(defun nerd-icons-icon-for-url (url &rest arg-overrides)
  "Get the formatted icon for URL.
If an icon for URL isn't found in `nerd-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (nerd-icons-match-to-alist url nerd-icons-url-alist))
         (args (cdr icon)))
    (unless icon
      (setq icon '(nerd-icons-faicon "nf-fa-globe"))
      (setq args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun nerd-icons-icon-for-buffer ()
  "Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon."
  (nerd-icons--icon-info-for-buffer))

(defun nerd-icons-cache (func)
  "Set a cache for FUNC.  Does not work on interactive functions."
  (unless (get func 'nerd-icons--cached)
    (let ((cache (make-hash-table :test #'equal
                                  :size nerd-icons--cache-limit))
          (orig-fn (symbol-function func)))
      (fset func
            (lambda (&rest args)
              (or (gethash args cache)
                  (progn
                    (when (> (hash-table-count cache)
                             nerd-icons--cache-limit)
                      (clrhash cache))
                    (puthash args (apply orig-fn args) cache)))))))

  (put func 'nerd-icons--cached t))

(nerd-icons-cache #'nerd-icons-icon-for-dir)
(nerd-icons-cache #'nerd-icons-icon-for-file)
(nerd-icons-cache #'nerd-icons-icon-for-extension)
(nerd-icons-cache #'nerd-icons-icon-for-mode)
(nerd-icons-cache #'nerd-icons-icon-for-url)

(defun nerd-icons--icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.
When F is provided, the info function is calculated with the format
`nerd-icons-icon-%s-for-file' or `nerd-icons-icon-%s-for-mode'."
  (let* ((base-f (concat "nerd-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (nerd-icons-auto-mode-match?))
        (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

;; Weather icons
(defun nerd-icons-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (nerd-icons-match-to-alist weather nerd-icons-weather-icon-alist)))
    (if icon (apply (car icon) (cdr icon)) weather)))

;; For `web-mode'
(defun nerd-icons--web-mode-icon (&rest arg-overrides)
  "Get icon for a `web-mode' buffer with ARG-OVERRIDES."
  (nerd-icons--web-mode arg-overrides))
(defun nerd-icons--web-mode-icon-family ()
  "Get icon family for a `web-mode' buffer."
  (nerd-icons--web-mode t))

(defvar web-mode-content-type)          ; external
(defun nerd-icons--web-mode (&optional arg-overrides)
  "Return icon or FAMILY for `web-mode' based on `web-mode-content-type'.
Providing ARG-OVERRIDES will modify the creation of the icon."
  (let ((non-nil-args (cl-reduce (lambda (acc it) (if it (append acc (list it)) acc))
                                 arg-overrides :initial-value '())))
    (cond
     ((equal web-mode-content-type "jsx")
      (apply 'nerd-icons-devicon (append '("javascript") non-nil-args)))
     ((equal web-mode-content-type "javascript")
      (apply 'nerd-icons-devicon (append '("javascript") non-nil-args)))
     ((equal web-mode-content-type "json")
      (apply 'nerd-icons-devicon (append '("nf-dev-less") non-nil-args)))
     ((equal web-mode-content-type "xml")
      (apply 'nerd-icons-faicon (append '("nf-fa-file_code_o") non-nil-args)))
     ((equal web-mode-content-type "css")
      (apply 'nerd-icons-devicon (append '("nf-dev-css3") non-nil-args)))
     (t
      (apply 'nerd-icons-devicon (append '("nf-dev-html5") non-nil-args))))))

(eval-and-compile
  (defun nerd-icons--function-name (name)
    "Get the symbol for an icon function name for icon set NAME."
    (intern (concat "nerd-icons-" (downcase (symbol-name name)))))

  (defun nerd-icons--family-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "nerd-icons-" (downcase (symbol-name name)) "-family")))

  (defun nerd-icons--glyph-set-name (name)
    "Get the symbol for an icon glyph set function for icon set NAME."
    (intern (concat "nerd-icons-" (downcase (symbol-name name)) "-glyph-set")))

  (defun nerd-icons--data-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "nerd-icons-" (downcase (symbol-name name)) "-data")))

  (defun nerd-icons--insert-function-name (name)
    "Get the symbol for an icon insert function for icon set NAME."
    (intern (concat "nerd-icons-insert-" (downcase (symbol-name name))))))

(defun nerd-icons-insert-icons-for (family &optional height duration)
  "Insert all of the available icons associated with FAMILY.
If a HEIGHT is provided it will render the icons at this height.
This is useful both to see the icons more clearly and to test
different height rendering.  If DURATION is provided, it will
pause for DURATION seconds between printing each character."
  (let* ((data-f    (nerd-icons--data-name family))
         (insert-f  (nerd-icons--function-name family))

         (height (or height 1.0))
         (data (funcall data-f)))
    (mapc
     (lambda (it)
       (insert (format "%s - %s\n" (funcall insert-f (car it) :height height) (car it)))
       (when duration (sit-for duration)))
     data)))

(defun nerd-icons-set-font (&optional font-family frame)
  "Modify nerd font charsets to use FONT-FAMILY for FRAME."
  (let ((font-f (or font-family nerd-icons-font-family))
        (charsets '((#xe5fa . #xe6b2)  ;; Seti-UI + Custom
                    (#xe700 . #xe7c5)  ;; Devicons
                    (#xf000 . #xf2e0)  ;; Font Awesome
                    (#xe200 . #xe2a9)  ;; Font Awesome Extension
                    (#xf500 . #xfd46) (#xf0001 . #xf1af0) ;; Material Design Icons
                    (#xe300 . #xe3eb)  ;; Weather
                    (#xf400 . #xf4a8) #x2665 #x26a1 #xf27c  ;; Octicons
                    (#xe0a0 . #xe0a2) (#xe0b0 . #xe0b3)  ;; Powerline Symbols
                    #xe0a3 (#xe0b4 . #xe0c8) (#xe0cc . #xe0d2) #xe0d4  ;; Powerline Extra Symbols
                    (#x23fb . #x23fe) #x2b58  ;; IEC Power Symbols
                    (#xf300 . #xf372)  ;; Font Logos
                    (#xe000 . #xe00a)  ;; Pomicons
                    (#xea60 . #xebeb))))  ;; Codicons
    (cl-loop for charset in charsets do
             (set-fontset-font
              (frame-parameter nil 'font)
              charset
              (font-spec :family font-f
                         :weight nil
                         :size   nil)
              frame
              'prepend))))

(defmacro nerd-icons-define-icon (name alist family glyph-set)
  "Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `nerd-icon-NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
GLYPH-SET is the glyph set of the icon."
  `(progn
     (add-to-list 'nerd-icons-glyph-sets (quote ,name))
     (defun ,(nerd-icons--family-name name) () ,family)
     (defun ,(nerd-icons--glyph-set-name name) () ,glyph-set)
     (defun ,(nerd-icons--data-name name) () ,alist)
     (defun ,(nerd-icons--function-name name) (icon-name &rest args)
       (let ((icon (cdr (assoc icon-name ,alist)))
             (other-face (when nerd-icons-color-icons (plist-get args :face)))
             (height (* nerd-icons-scale-factor (or (plist-get args :height) 1.0)))
             (v-adjust (* nerd-icons-scale-factor (or (plist-get args :v-adjust) nerd-icons-default-adjust)))
             (family ,family))
         (unless icon
           (error "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name)))
         (let ((face (if other-face
                         `(:family ,family :height ,height :inherit ,other-face)
                       `(:family ,family :height ,height))))
           (propertize icon
                       'face face
                       'font-lock-face face
                       'display `(raise ,v-adjust)
                       'rear-nonsticky t))))
     (defun ,(nerd-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." glyph-set)
       (interactive "P")
       (nerd-icons-insert arg (quote ,name)))))

(nerd-icons-define-icon ipsicon nerd-icons/ipsicon-alist nerd-icons-font-family "IEC Power Symbols")
(nerd-icons-define-icon octicon nerd-icons/octicon-alist nerd-icons-font-family "Octicons")
(nerd-icons-define-icon pomicon nerd-icons/pomicon-alist nerd-icons-font-family "Pomicons")
(nerd-icons-define-icon powerline nerd-icons/powerline-alist nerd-icons-font-family "Powerline Symbols")
(nerd-icons-define-icon faicon nerd-icons/faicon-alist nerd-icons-font-family "Font Awesome")
(nerd-icons-define-icon wicon nerd-icons/wicon-alist nerd-icons-font-family "Weather")
(nerd-icons-define-icon sucicon nerd-icons/sucicon-alist nerd-icons-font-family "Seti-UI + Custom")
(nerd-icons-define-icon devicon nerd-icons/devicon-alist nerd-icons-font-family "Devicons")
(nerd-icons-define-icon codicon nerd-icons/codicon-alist nerd-icons-font-family "Codicons")
(nerd-icons-define-icon flicon nerd-icons/flicon-alist nerd-icons-font-family "Font Logos")
(nerd-icons-define-icon mdicon nerd-icons/mdicon-alist nerd-icons-font-family "Material Design Icons")

(provide 'nerd-icons)
;;; nerd-icons.el ends here
