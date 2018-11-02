#!/usr/bin/env python
# # -*- coding: utf-8 -*-

import os
from pathlib import Path
import base64

output = "type file = { name: string; text: string }"
output += "\nlet files = ["

directory = os.fsencode("data/")
first = True


pathlist = Path("").glob('**/*.*')
for path in pathlist:
    # because path is object not string
    filename = str(path)
    if not first: output += "; "
    output += "{ name = \""
    output += filename
    output += "\"; text = \""
        
    with open(bytes(path), 'r') as content_file:
        content = os.fsdecode(content_file.read())
    output += (base64.b64encode(content.encode())).decode()
    output += "\" }"
    first = False

output += "]"
output += "\n let load () = List.iter (fun f -> Sys_js.create_file ~name:f.name ~content:(B64.decode f.text)) files"
print(output)
