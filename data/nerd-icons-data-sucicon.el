;;; nerd-icons-data-sucicon.el --- glyphset sucicon -*- lexical-binding: t -*-

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

;; sucicon
;; from Nerd Font Version: 3.1.1

;;; Code:

(defvar nerd-icons/sucicon-alist
  '(
    ("nf-custom-asm" . "e6ab")
    ("nf-custom-c" . "e61e")
    ("nf-custom-common_lisp" . "e6b0")
    ("nf-custom-cpp" . "e61d")
    ("nf-custom-crystal" . "e62f")
    ("nf-custom-default" . "e612")
    ("nf-custom-electron" . "e62e")
    ("nf-custom-elixir" . "e62d")
    ("nf-custom-elm" . "e62c")
    ("nf-custom-emacs" . "e632")
    ("nf-custom-fennel" . "e6af")
    ("nf-custom-folder" . "e5ff")
    ("nf-custom-folder_config" . "e5fc")
    ("nf-custom-folder_git" . "e5fb")
    ("nf-custom-folder_git_branch" . "e5fb")
    ("nf-custom-folder_github" . "e5fd")
    ("nf-custom-folder_npm" . "e5fa")
    ("nf-custom-folder_oct" . "e6ad")
    ("nf-custom-folder_open" . "e5fe")
    ("nf-custom-go" . "e626")
    ("nf-custom-home" . "e617")
    ("nf-custom-kotlin" . "e634")
    ("nf-custom-msdos" . "e629")
    ("nf-custom-neovim" . "e6ae")
    ("nf-custom-orgmode" . "e633")
    ("nf-custom-play_arrow" . "e602")
    ("nf-custom-puppet" . "e631")
    ("nf-custom-purescript" . "e630")
    ("nf-custom-scheme" . "e6b1")
    ("nf-custom-toml" . "e6b2")
    ("nf-custom-v_lang" . "e6ac")
    ("nf-custom-vim" . "e62b")
    ("nf-custom-windows" . "e62a")
    ("nf-seti-apple" . "e635")
    ("nf-seti-argdown" . "e636")
    ("nf-seti-asm" . "e637")
    ("nf-seti-audio" . "e638")
    ("nf-seti-babel" . "e639")
    ("nf-seti-bazel" . "e63a")
    ("nf-seti-bicep" . "e63b")
    ("nf-seti-bower" . "e61a")
    ("nf-seti-bsl" . "e63c")
    ("nf-seti-c" . "e649")
    ("nf-seti-c_sharp" . "e648")
    ("nf-seti-cake" . "e63e")
    ("nf-seti-cake_php" . "e63d")
    ("nf-seti-checkbox" . "e63f")
    ("nf-seti-checkbox_unchecked" . "e640")
    ("nf-seti-cjsx" . "e61b")
    ("nf-seti-clock" . "e641")
    ("nf-seti-clojure" . "e642")
    ("nf-seti-code_climate" . "e643")
    ("nf-seti-code_search" . "e644")
    ("nf-seti-coffee" . "e61b")
    ("nf-seti-coldfusion" . "e645")
    ("nf-seti-config" . "e615")
    ("nf-seti-cpp" . "e646")
    ("nf-seti-crystal" . "e62f")
    ("nf-seti-crystal_embedded" . "e647")
    ("nf-seti-css" . "e614")
    ("nf-seti-csv" . "e64a")
    ("nf-seti-cu" . "e64b")
    ("nf-seti-d" . "e651")
    ("nf-seti-dart" . "e64c")
    ("nf-seti-db" . "e64d")
    ("nf-seti-default" . "e64e")
    ("nf-seti-deprecation_cop" . "e64f")
    ("nf-seti-docker" . "e650")
    ("nf-seti-editorconfig" . "e652")
    ("nf-seti-ejs" . "e618")
    ("nf-seti-elixir" . "e62d")
    ("nf-seti-elixir_script" . "e653")
    ("nf-seti-elm" . "e62c")
    ("nf-seti-error" . "e654")
    ("nf-seti-eslint" . "e655")
    ("nf-seti-ethereum" . "e656")
    ("nf-seti-f_sharp" . "e65a")
    ("nf-seti-favicon" . "e623")
    ("nf-seti-firebase" . "e657")
    ("nf-seti-firefox" . "e658")
    ("nf-seti-folder" . "e613")
    ("nf-seti-font" . "e659")
    ("nf-seti-git" . "e65d")
    ("nf-seti-git_folder" . "e65d")
    ("nf-seti-git_ignore" . "e65d")
    ("nf-seti-github" . "e65b")
    ("nf-seti-gitlab" . "e65c")
    ("nf-seti-go" . "e627")
    ("nf-seti-go2" . "e65e")
    ("nf-seti-godot" . "e65f")
    ("nf-seti-gradle" . "e660")
    ("nf-seti-grails" . "e661")
    ("nf-seti-graphql" . "e662")
    ("nf-seti-grunt" . "e611")
    ("nf-seti-gulp" . "e610")
    ("nf-seti-hacklang" . "e663")
    ("nf-seti-haml" . "e664")
    ("nf-seti-happenings" . "e665")
    ("nf-seti-haskell" . "e61f")
    ("nf-seti-haxe" . "e666")
    ("nf-seti-heroku" . "e607")
    ("nf-seti-hex" . "e667")
    ("nf-seti-home" . "e617")
    ("nf-seti-html" . "e60e")
    ("nf-seti-ignored" . "e668")
    ("nf-seti-illustrator" . "e669")
    ("nf-seti-image" . "e60d")
    ("nf-seti-info" . "e66a")
    ("nf-seti-ionic" . "e66b")
    ("nf-seti-jade" . "e66c")
    ("nf-seti-java" . "e66d")
    ("nf-seti-javascript" . "e60c")
    ("nf-seti-jenkins" . "e66e")
    ("nf-seti-jinja" . "e66f")
    ("nf-seti-json" . "e60b")
    ("nf-seti-julia" . "e624")
    ("nf-seti-karma" . "e622")
    ("nf-seti-kotlin" . "e634")
    ("nf-seti-less" . "e60b")
    ("nf-seti-license" . "e60a")
    ("nf-seti-liquid" . "e670")
    ("nf-seti-livescript" . "e671")
    ("nf-seti-lock" . "e672")
    ("nf-seti-lua" . "e620")
    ("nf-seti-makefile" . "e673")
    ("nf-seti-markdown" . "e609")
    ("nf-seti-maven" . "e674")
    ("nf-seti-mdo" . "e675")
    ("nf-seti-mustache" . "e60f")
    ("nf-seti-new_file" . "e676")
    ("nf-seti-nim" . "e677")
    ("nf-seti-notebook" . "e678")
    ("nf-seti-npm" . "e616")
    ("nf-seti-npm_ignored" . "e616")
    ("nf-seti-nunjucks" . "e679")
    ("nf-seti-ocaml" . "e67a")
    ("nf-seti-odata" . "e67b")
    ("nf-seti-pddl" . "e67c")
    ("nf-seti-pdf" . "e67d")
    ("nf-seti-perl" . "e67e")
    ("nf-seti-photoshop" . "e67f")
    ("nf-seti-php" . "e608")
    ("nf-seti-pipeline" . "e680")
    ("nf-seti-plan" . "e681")
    ("nf-seti-platformio" . "e682")
    ("nf-seti-play_arrow" . "e602")
    ("nf-seti-powershell" . "e683")
    ("nf-seti-prisma" . "e684")
    ("nf-seti-project" . "e601")
    ("nf-seti-prolog" . "e685")
    ("nf-seti-pug" . "e686")
    ("nf-seti-puppet" . "e631")
    ("nf-seti-purescript" . "e630")
    ("nf-seti-python" . "e606")
    ("nf-seti-r" . "e68a")
    ("nf-seti-rails" . "e604")
    ("nf-seti-react" . "e625")
    ("nf-seti-reasonml" . "e687")
    ("nf-seti-rescript" . "e688")
    ("nf-seti-rollup" . "e689")
    ("nf-seti-ruby" . "e605")
    ("nf-seti-rust" . "e68b")
    ("nf-seti-salesforce" . "e68c")
    ("nf-seti-sass" . "e603")
    ("nf-seti-sbt" . "e68d")
    ("nf-seti-scala" . "e68e")
    ("nf-seti-search" . "e68f")
    ("nf-seti-settings" . "e690")
    ("nf-seti-shell" . "e691")
    ("nf-seti-slim" . "e692")
    ("nf-seti-smarty" . "e693")
    ("nf-seti-spring" . "e694")
    ("nf-seti-stylelint" . "e695")
    ("nf-seti-stylus" . "e600")
    ("nf-seti-sublime" . "e696")
    ("nf-seti-svelte" . "e697")
    ("nf-seti-svg" . "e698")
    ("nf-seti-swift" . "e699")
    ("nf-seti-terraform" . "e69a")
    ("nf-seti-tex" . "e69b")
    ("nf-seti-text" . "e612")
    ("nf-seti-time_cop" . "e641")
    ("nf-seti-todo" . "e69c")
    ("nf-seti-tsconfig" . "e69d")
    ("nf-seti-twig" . "e61c")
    ("nf-seti-typescript" . "e628")
    ("nf-seti-vala" . "e69e")
    ("nf-seti-video" . "e69f")
    ("nf-seti-vue" . "e6a0")
    ("nf-seti-wasm" . "e6a1")
    ("nf-seti-wat" . "e6a2")
    ("nf-seti-webpack" . "e6a3")
    ("nf-seti-wgt" . "e6a4")
    ("nf-seti-word" . "e6a5")
    ("nf-seti-xls" . "e6a6")
    ("nf-seti-xml" . "e619")
    ("nf-seti-yarn" . "e6a7")
    ("nf-seti-yml" . "e6a8")
    ("nf-seti-zig" . "e6a9")
    ("nf-seti-zip" . "e6aa")
    ))

(provide 'nerd-icons-data-sucicon)
;;; nerd-icons-data-sucicon.el ends here