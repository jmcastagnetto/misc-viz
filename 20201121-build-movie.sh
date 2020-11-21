#! /bin/bash

ffmpeg -framerate 1 -pattern_type glob -i 'tmp/pm*.png' -i "assets/_ghost_-_Reverie_(small_theme).mp3" -c:v libx264 -preset slow -vf format=yuv420p movies/pm-cobertura-prov-weekly.mp4 -y

