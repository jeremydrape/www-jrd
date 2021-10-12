#!/usr/bin/python3

import subprocess

print("Content-type: text/html\n\n")

print("<p>Resizing images...</p>\n\n")

subprocess.call(["sh","../sh/resize.sh","../data/jpeg"])

print("<p>Resize complete</p>\n\n")
