#! /bin/sh

pulp browserify -m Examples.Cats -t build/Cats.js -I examples -j 4 --optimise
