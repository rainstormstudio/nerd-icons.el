#!/usr/bin/bash

rm ../svg/*

# convert ttf to svg
fontforge -lang=ff -c "Open(\"../fonts/NFM.ttf\"); SelectWorthOutputting(); foreach Export('../svg/%u.svg'); endloop;"

# fix svg display issue
sed -i 's/viewBox/width="2048" height="2048" viewBox/g' ../svg/*.svg
