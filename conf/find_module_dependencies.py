#!/usr/bin/env python
#
# Scan all .[fF]90 files and write out module dependencies
#

try:
    from string import lower
except ImportError:
    # For python3 cross-compatibility
    def lower(x):
        return x.lower()

import glob
import re
import sys
import os.path

# Command line arguments
src_path = sys.argv[1]
outfile  = sys.argv[2]

# Get list of source files
sources = glob.glob(src_path+'/'+'*.[fF]90')

# Regular expressions to find use/module statements
re_use    = re.compile(r"\A[\s]*use[\s]+([a-z_0-9]+)((\s*\Z)|(\s*(!|,).*\Z))",
                       flags=re.IGNORECASE)
re_mod    = re.compile(r"\A[\s]*module[\s]+([a-z_0-9]+)((\s*\Z)|(\s*!.*\Z))",
                       flags=re.IGNORECASE)

# Name of file containing each module
module_file = {}

# Required modules for each file. 
required_modules = {}

# Extract all "use" and "module" statements
for full_name in sources:
    fname = os.path.basename(full_name)
    modlist = []
    file = open(full_name)
    while 1:
        line = file.readline()
        if not line:
            break
        # Check for use statements
        m = re_use.match(line)
        if m:
            modname = lower(m.group(1))
            modlist.append(modname)
        # Check for module statements
        m = re_mod.match(line)
        if m:
            modname = lower(m.group(1))
            module_file[modname] = fname
    required_modules[fname] = modlist
    file.close()

# Regular expression to remove filename extension
re_root = re.compile(r"(.*)\.[fF]90\Z")

# Write out dependencies
f = open(outfile, 'w')
for full_name in sources:
    fname = os.path.basename(full_name)
    root = re_root.match(fname).group(1)
    deps = root + ".o : \t"+fname
    for mod in set(required_modules[fname]):
        if mod in module_file:
            modfile = module_file[mod]
            if modfile != fname:
                m = re_root.match(modfile)
                objfile = m.group(1)+".o"
                deps = deps + " " + objfile
    f.write(deps+"\n\n")

f.close()

