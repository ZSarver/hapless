#!/bin/bash
pulp browserify --to target/hapless.js
cp bower_components/rot.js/rot.js target/rot.js
cp resources/* target/
cp index.html target/index.html
