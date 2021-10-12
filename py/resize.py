#!/usr/bin/python3

import subprocess

print("Content-type: text/html\n\n")

print("<p>Resizing images...</p>\n\n")

result = subprocess.call(["sh","../sh/resize.sh","../data/jpeg"])

print("<p>Resize complete, result = %(result)s (0 = success)</p>\n\n" % {'result': result})
