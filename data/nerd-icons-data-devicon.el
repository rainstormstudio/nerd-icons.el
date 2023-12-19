;;; nerd-icons-data-devicon.el --- glyphset devicon -*- lexical-binding: t -*-

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

;; devicon
;; from Nerd Font Version: 3.1.1

;;; Code:

(defvar nerd-icons/devicon-alist
  '(
    ("nf-dev-android" . "e70e")
    ("nf-dev-angular" . "e753")
    ("nf-dev-appcelerator" . "e7ab")
    ("nf-dev-apple" . "e711")
    ("nf-dev-appstore" . "e713")
    ("nf-dev-aptana" . "e799")
    ("nf-dev-asterisk" . "e7ac")
    ("nf-dev-atlassian" . "e75b")
    ("nf-dev-atom" . "e764")
    ("nf-dev-aws" . "e7ad")
    ("nf-dev-backbone" . "e752")
    ("nf-dev-bing_small" . "e700")
    ("nf-dev-bintray" . "e794")
    ("nf-dev-bitbucket" . "e703")
    ("nf-dev-blackberry" . "e723")
    ("nf-dev-bootstrap" . "e747")
    ("nf-dev-bower" . "e74d")
    ("nf-dev-brackets" . "e79d")
    ("nf-dev-bugsense" . "e78d")
    ("nf-dev-celluloid" . "e76b")
    ("nf-dev-chart" . "e760")
    ("nf-dev-chrome" . "e743")
    ("nf-dev-cisco" . "e765")
    ("nf-dev-clojure" . "e768")
    ("nf-dev-clojure_alt" . "e76a")
    ("nf-dev-cloud9" . "e79f")
    ("nf-dev-coda" . "e793")
    ("nf-dev-code" . "e796")
    ("nf-dev-code_badge" . "e7a3")
    ("nf-dev-codeigniter" . "e780")
    ("nf-dev-codepen" . "e716")
    ("nf-dev-codrops" . "e72f")
    ("nf-dev-coffeescript" . "e751")
    ("nf-dev-compass" . "e761")
    ("nf-dev-composer" . "e783")
    ("nf-dev-creativecommons" . "e789")
    ("nf-dev-creativecommons_badge" . "e78a")
    ("nf-dev-css3" . "e749")
    ("nf-dev-css3_full" . "e74a")
    ("nf-dev-css_tricks" . "e701")
    ("nf-dev-cssdeck" . "e72a")
    ("nf-dev-dart" . "e798")
    ("nf-dev-database" . "e706")
    ("nf-dev-debian" . "e77d")
    ("nf-dev-digital_ocean" . "e7ae")
    ("nf-dev-django" . "e71d")
    ("nf-dev-dlang" . "e7af")
    ("nf-dev-docker" . "e7b0")
    ("nf-dev-doctrine" . "e774")
    ("nf-dev-dojo" . "e71c")
    ("nf-dev-dotnet" . "e77f")
    ("nf-dev-dreamweaver" . "e79c")
    ("nf-dev-dropbox" . "e707")
    ("nf-dev-drupal" . "e742")
    ("nf-dev-eclipse" . "e79e")
    ("nf-dev-ember" . "e71b")
    ("nf-dev-envato" . "e75d")
    ("nf-dev-erlang" . "e7b1")
    ("nf-dev-extjs" . "e78e")
    ("nf-dev-firebase" . "e787")
    ("nf-dev-firefox" . "e745")
    ("nf-dev-fsharp" . "e7a7")
    ("nf-dev-ghost" . "e71f")
    ("nf-dev-ghost_small" . "e714")
    ("nf-dev-git" . "e702")
    ("nf-dev-git_branch" . "e725")
    ("nf-dev-git_commit" . "e729")
    ("nf-dev-git_compare" . "e728")
    ("nf-dev-git_merge" . "e727")
    ("nf-dev-git_pull_request" . "e726")
    ("nf-dev-github" . "e70a")
    ("nf-dev-github_alt" . "e708")
    ("nf-dev-github_badge" . "e709")
    ("nf-dev-github_full" . "e717")
    ("nf-dev-gnu" . "e779")
    ("nf-dev-go" . "e724")
    ("nf-dev-google_cloud_platform" . "e7b2")
    ("nf-dev-google_drive" . "e731")
    ("nf-dev-grails" . "e7b3")
    ("nf-dev-groovy" . "e775")
    ("nf-dev-grunt" . "e74c")
    ("nf-dev-gulp" . "e763")
    ("nf-dev-hackernews" . "e71a")
    ("nf-dev-haskell" . "e777")
    ("nf-dev-heroku" . "e77b")
    ("nf-dev-html5" . "e736")
    ("nf-dev-html5_3d_effects" . "e735")
    ("nf-dev-html5_connectivity" . "e734")
    ("nf-dev-html5_device_access" . "e733")
    ("nf-dev-html5_multimedia" . "e732")
    ("nf-dev-ie" . "e744")
    ("nf-dev-illustrator" . "e7b4")
    ("nf-dev-intellij" . "e7b5")
    ("nf-dev-ionic" . "e7a9")
    ("nf-dev-java" . "e738")
    ("nf-dev-javascript" . "e74e")
    ("nf-dev-javascript_badge" . "e781")
    ("nf-dev-javascript_shield" . "e74f")
    ("nf-dev-jekyll_small" . "e70d")
    ("nf-dev-jenkins" . "e767")
    ("nf-dev-jira" . "e75c")
    ("nf-dev-joomla" . "e741")
    ("nf-dev-jquery" . "e750")
    ("nf-dev-jquery_ui" . "e754")
    ("nf-dev-komodo" . "e792")
    ("nf-dev-krakenjs" . "e785")
    ("nf-dev-krakenjs_badge" . "e784")
    ("nf-dev-laravel" . "e73f")
    ("nf-dev-less" . "e758")
    ("nf-dev-linux" . "e712")
    ("nf-dev-magento" . "e740")
    ("nf-dev-mailchimp" . "e79a")
    ("nf-dev-markdown" . "e73e")
    ("nf-dev-materializecss" . "e7b6")
    ("nf-dev-meteor" . "e7a5")
    ("nf-dev-meteorfull" . "e7a6")
    ("nf-dev-mitlicence" . "e78b")
    ("nf-dev-modernizr" . "e720")
    ("nf-dev-mongodb" . "e7a4")
    ("nf-dev-mootools" . "e790")
    ("nf-dev-mootools_badge" . "e78f")
    ("nf-dev-mozilla" . "e786")
    ("nf-dev-msql_server" . "e77c")
    ("nf-dev-mysql" . "e704")
    ("nf-dev-nancy" . "e766")
    ("nf-dev-netbeans" . "e79b")
    ("nf-dev-netmagazine" . "e72e")
    ("nf-dev-nginx" . "e776")
    ("nf-dev-nodejs" . "e719")
    ("nf-dev-nodejs_small" . "e718")
    ("nf-dev-npm" . "e71e")
    ("nf-dev-onedrive" . "e762")
    ("nf-dev-openshift" . "e7b7")
    ("nf-dev-opensource" . "e771")
    ("nf-dev-opera" . "e746")
    ("nf-dev-perl" . "e769")
    ("nf-dev-phonegap" . "e730")
    ("nf-dev-photoshop" . "e7b8")
    ("nf-dev-php" . "e73d")
    ("nf-dev-postgresql" . "e76e")
    ("nf-dev-prolog" . "e7a1")
    ("nf-dev-python" . "e73c")
    ("nf-dev-rackspace" . "e7b9")
    ("nf-dev-raphael" . "e75f")
    ("nf-dev-rasberry_pi" . "e722")
    ("nf-dev-react" . "e7ba")
    ("nf-dev-redhat" . "e7bb")
    ("nf-dev-redis" . "e76d")
    ("nf-dev-requirejs" . "e770")
    ("nf-dev-responsive" . "e797")
    ("nf-dev-ruby" . "e739")
    ("nf-dev-ruby_on_rails" . "e73b")
    ("nf-dev-ruby_rough" . "e791")
    ("nf-dev-rust" . "e7a8")
    ("nf-dev-safari" . "e748")
    ("nf-dev-sass" . "e74b")
    ("nf-dev-scala" . "e737")
    ("nf-dev-scriptcs" . "e7bc")
    ("nf-dev-scrum" . "e7a0")
    ("nf-dev-senchatouch" . "e78c")
    ("nf-dev-sizzlejs" . "e788")
    ("nf-dev-smashing_magazine" . "e72d")
    ("nf-dev-snap_svg" . "e75e")
    ("nf-dev-sqllite" . "e7c4")
    ("nf-dev-stackoverflow" . "e710")
    ("nf-dev-streamline" . "e705")
    ("nf-dev-stylus" . "e759")
    ("nf-dev-sublime" . "e7aa")
    ("nf-dev-swift" . "e755")
    ("nf-dev-symfony" . "e756")
    ("nf-dev-symfony_badge" . "e757")
    ("nf-dev-techcrunch" . "e72c")
    ("nf-dev-terminal" . "e795")
    ("nf-dev-terminal_badge" . "e7a2")
    ("nf-dev-travis" . "e77e")
    ("nf-dev-trello" . "e75a")
    ("nf-dev-typo3" . "e772")
    ("nf-dev-ubuntu" . "e73a")
    ("nf-dev-uikit" . "e773")
    ("nf-dev-unity_small" . "e721")
    ("nf-dev-vim" . "e7c5")
    ("nf-dev-visualstudio" . "e70c")
    ("nf-dev-w3c" . "e76c")
    ("nf-dev-webplatform" . "e76f")
    ("nf-dev-windows" . "e70f")
    ("nf-dev-wordpress" . "e70b")
    ("nf-dev-yahoo" . "e715")
    ("nf-dev-yahoo_small" . "e72b")
    ("nf-dev-yeoman" . "e77a")
    ("nf-dev-yii" . "e782")
    ("nf-dev-zend" . "e778")
    ))

(provide 'nerd-icons-data-devicon)
;;; nerd-icons-data-devicon.el ends here