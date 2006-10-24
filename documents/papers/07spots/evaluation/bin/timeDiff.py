#!/usr/bin/python

##
# For fun shell usage of this you can try something along the lines of:
# for i in `cat moduleNames.txt`; do python ../bin/timeDiff.py . $i; done
##

import re
import difflib
import sys
import os
import glob 

# Poor man's debugging macro
DEBUGON = True
def DEBUG(out):
    if DEBUGON:
        print out


################################################################
# Simple utility functions to save typing
################################################################

##
# Return a list of files that match a given string (pathname)
##
def getCheckFiles(path):
    
    checks = glob.glob(path)
    checks.sort(lambda x, y: cmp(y.lower(), x.lower()))
    return checks 


##
# Function to ignore bogus errors in the build logs.  Returns true if the line
# can be safely ignored.
##
def ignoreLine(line):

    # Error messages that we know are okay
    whitelist = [ \
            'Error: Function ker_msg_take_data fails to return allocated memory', \
            'Check time: ', \
            'Warning: Return statement with a value in function returning void', \
            'Unable to make address of non-lval expression', \
            'Function sys_msg_take_data fails to return allocated memory', \
            ]

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
# "Clean" files of data that is either knowen to be safe (ie. base level
# allocation functions) or to generalize numbers (ie. line number, temporary
# variable number).
##
def clean(lines):
    
    # Expressions we want to "clean"
    lineFileNum = re.compile('#line \d+ ".*"')
    lineNum = re.compile('#line \d+')
    cilTmpNum = re.compile('__cil_tmp\d+')
    tmpNum = re.compile('tmp___\d+')

    tmp = []

    for line in lines:
        clean = line
        clean = lineFileNum.sub('#line XXX', clean)
        clean = lineNum.sub('#line XXX', clean)
        clean = cilTmpNum.sub('__cil_tmpXXX', clean)
        clean = tmpNum.sub('tmp___XXX', clean)
        tmp.append(clean)

    return tmp


##
# Smart diff function
##
def smartDiff(fileA, fileB):

    a = open(fileA)
    b = open(fileB)

    aLines = a.readlines()
    bLines = b.readlines()

    a.close()
    b.close()

    # Filter out white listed statements
    aOrig = []
    for line in aLines:
        if ignoreLine(line):
            continue
        aOrig.append(line)
    bOrig = []
    for line in bLines:
        if ignoreLine(line):
            continue
        bOrig.append(line)


    cleanA = clean(aOrig)
    cleanB = clean(bOrig)
   
    # Hairy custom diff that uses the result of ndiff to output the NON-CLEAN
    # diff or cleanA and cleanB
    d = difflib.ndiff(cleanA, cleanB)

    indexA = 0
    indexB = 0
    diffList = []
    for line in d:
        if line[0:2] == '  ':
            indexA += 1
            indexB += 1
        elif line[0:2] == '- ': 
            # Slice indexes are pulling version from the file name
            diffList.append(fileA[-27:-8] + ": " + aOrig[indexA])
            indexA += 1
        elif line[0:2] == '+ ':
            # Slice indexes are pulling version from the file name
            diffList.append(fileB[-27:-8] + ": " + bOrig[indexB])
            indexB += 1
        
    return diffList        


if __name__ == '__main__':

    if len(sys.argv) != 3:
        print "Usage: " + sys.argv[0] + "<savePath> <moduleName>"
        print "Look for temporal diffs in a set of files"
        print "    savePath: directory containing files"
        print "    moduleName: base name of files to check"
        sys.exit()
        
    savePath = sys.argv[1]
    moduleName = sys.argv[2]
   
    os.chdir(savePath)

    files = getCheckFiles(moduleName + '_2*.check')

    # Generate a start file of time diff
    if len(files) > 0:
        f = open(files[0])
        cleanData = clean(f.readlines())
        f.close()
        outFile = files[0][-27:-8] + ".timeDiff"
        g = open(moduleName + "_" + "0000XXXXXXXXXXXXXXX__" + outFile, 'w')
        for line in cleanData:
            if ignoreLine(line):
                continue
            line = line.rstrip()
            g.write(line + "\n")
        g.close()

    
    # Generate time diffs for the module
    for index in range(len(files) - 1):
        
        diff = smartDiff(files[index], files[index+1])
        if len(diff) > 0:    
            DEBUG("Found point of interest between files: ")
            DEBUG("    " + files[index])
            DEBUG("    " + files[index + 1])
            outFile = moduleName + "_" + \
                    files[index][-27:-8] + "__" + files[index+1][-27:-8] + \
                    ".timeDiff"
            f = open(outFile, 'w')
            f.write("Found point of interest between files:\n")
            f.write("    " + files[index] + '\n') 
            f.write("    " + files[index + 1] + '\n')
            for line in diff:
                line = line.rstrip()
                f.write(line + "\n")
            #f.write("".join(diff))
            f.close()

