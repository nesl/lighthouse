#!/usr/bin/python

import os
import sys
import pexpect
import re
import time

# Poor man's debugging macro
debugOn = True
def DEBUG(out):
    if debugOn:
        print out


##
# Function to ignore bogus errors in the build logs.  Returns true if the line
# can be safely ignored.
##
def ignoreLine(line):

    # Error messages that we know are okay
    #whitelist = [ \
    #        'Warning: Return statement with a value in function returning void', \
    #        'Warning: Retrun value must reference memory in function ker_msg_take_data', \
    #        'Warning: Retrun value must reference memory in function ker_malloc', \
    #        'Warning: Foraml var ptr must be stored or released in function ker_free', \
    #        ]
    whitelist = []

    # Check if the line can be ignored
    for ignore in whitelist:
        if ignore in line:
            return True

    # Check for the empty line
    blank = re.compile("^\s$")
    if blank.match(line):
        return True
    
    # Note that the line can not be ignored
    return False



##
# Helper function to clean up the source tree
##
def check(file, options = "", check="/home/rshea/svn/lighthouse/trunk/src/memCheck/memory --config /home/rshea/svn/lighthouse/trunk/src/memCheck/config.txt"):
    
    cmdCheck = check + " " + options + " " + file
    
    if not os.path.exists(file):
        print("Unable to find file: " + file + " may need to rebuild")
        return
   
    checkTime = time.time()
    child = pexpect.spawn(cmdCheck, timeout=None)
    lines = child.readlines();
    
    # Write output into a file
    f = open(file + ".check", "w")
    for line in lines:
        if ignoreLine(line):
            continue
        
        print line,
        f.write(line)
    
    checkTime = time.time() - checkTime 
    print ("Check time: %f" % (checkTime))
    f.write("Check time: %f\n" % (checkTime))
    
    f.close()
    child.close()



if __name__ == '__main__':
    
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + " [file list]"
        print "Check each file is file list using the Lighthouse memory checker." 
        print "    file list: list of files to check"
        sys.exit()

    files = sys.argv[1:]

    count = 0
    total = len(files)
    for file in files:
        
        count = count + 1
        DEBUG("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        DEBUG("Checking %s (%d of %d)" % (file, count, total))
        DEBUG("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        
        saveFile = file + ".cil.c"
    
        check(file, "--out " + saveFile)
        
            


