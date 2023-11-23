"""
This script generates nerd-icon data elisp code files
under /data folder. It grabs contents from nerd-fonts
https://github.com/ryanoasis/nerd-fonts.

How to use:
cd script
python ./generate-data.py
"""

import requests
import os
import shutil
import re

# constants and variables
nerd_fonts_generated_css_file = "nerd-fonts-generated.css"
nerd_fonts_generated_css_url = "https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/css/" + nerd_fonts_generated_css_file
nerd_fonts_generated_css_download_folder = "../tmp"
nerd_fonts_data_folder = "../data"
nerd_fonts_version = ""

data_header = """;;; nerd-icons-data-{}.el --- glyphset {} -*- lexical-binding: t -*-

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

;; {}
;; from Nerd Font {}

;;; Code:

"""
data_footer = """;;; nerd-icons-data-{}.el ends here"""

def download_nerd_fonts_generated_css_file():
    """Downloads css files from Nerd Fonts repo."""
    if not os.path.exists(nerd_fonts_generated_css_download_folder):
        print("creating folder {}".format(nerd_fonts_generated_css_download_folder))
        os.makedirs(nerd_fonts_generated_css_download_folder)

    print("downloading {} to {}".format(nerd_fonts_generated_css_file, nerd_fonts_generated_css_download_folder))
    r = requests.get(nerd_fonts_generated_css_url, allow_redirects=True, verify=True)
    open(nerd_fonts_generated_css_download_folder + "/" +
         nerd_fonts_generated_css_file, 'wb').write(r.content)

def get_version():
    """change version to Nerd Fonts version."""
    global nerd_fonts_version
    input = open(nerd_fonts_generated_css_download_folder + "/" +
              nerd_fonts_generated_css_file, 'r')
    content = input.read()
    nerd_fonts_version = re.search('Version: .*', content).group(0)

def matches_from_prefix(prefix):
    """find matches from prefix."""
    with open(nerd_fonts_generated_css_download_folder + "/" +
              nerd_fonts_generated_css_file, 'r') as input:
        content = input.read()
        content_new = re.sub('\.' + prefix + '(.*):before {\n.*content: "\\\\(.*)";\n}', '("' + prefix + r'\1" . "\\x\2")', content, flags = re.M)
        matches = re.findall('\("' + prefix + '.*\)', content_new)
        return matches

def generate(folder, glyphset, prefixes):
    """generates elisp data for glyphset with prefixes in the folder."""
    output = open(folder + '/nerd-icons-data-' + glyphset + '.el', 'w')
    output.write(data_header.format(glyphset, glyphset, glyphset, nerd_fonts_version))
    output.write('(defvar nerd-icons/' + glyphset + '-alist\n  \'(\n')
    for prefix in prefixes:
        matches = matches_from_prefix(prefix)
        for match in matches:
            output.write('    ' + match + '\n')
    output.write('    ))\n\n')
    output.write('(provide \'nerd-icons-data-' + glyphset + ')\n')
    output.write(data_footer.format(glyphset))

download_nerd_fonts_generated_css_file()
get_version()

if os.path.exists(nerd_fonts_data_folder):
    shutil.rmtree(nerd_fonts_data_folder)
os.makedirs(nerd_fonts_data_folder)

generate(nerd_fonts_data_folder, 'codicon', ['nf-cod-'])
generate(nerd_fonts_data_folder, 'devicon', ['nf-dev-'])
generate(nerd_fonts_data_folder, 'faicon', ['nf-fa-', 'nf-fae-'])
generate(nerd_fonts_data_folder, 'flicon', ['nf-linux-'])
generate(nerd_fonts_data_folder, 'ipsicon', ['nf-iec-'])
generate(nerd_fonts_data_folder, 'mdicon', ['nf-md-'])
generate(nerd_fonts_data_folder, 'octicon', ['nf-oct-'])
generate(nerd_fonts_data_folder, 'pomicon', ['nf-pom-'])
generate(nerd_fonts_data_folder, 'powerline', ['nf-pl-', 'nf-ple-'])
generate(nerd_fonts_data_folder, 'sucicon', ['nf-custom-', 'nf-seti-'])
generate(nerd_fonts_data_folder, 'wicon', ['nf-weather-'])
