#!/usr/bin/python

import pexpect
import os
import sys
import re

# Poor man's debugging macro
debugOn = True
def DEBUG(out):
    if debugOn:
        print out


################################################################
# Simple utility functions to save typing
################################################################

##
# Helper function to log the output of a make
##
def build(dir, command="", stdout="/dev/null", stderr="/dev/null"):
    cmdMake = "make -C %s %s 1> %s 2> %s" % (dir, command, stdout, stderr)
    os.system(cmdMake)


##
# Helper function to clean up the source tree
##
def clean(dir, file):
    cmdClean = "make -C " + dir + " clean &> /dev/null"
    os.system(cmdClean)
    os.system("rm -f " + os.path.join(dir, file) + ".i")
    os.system("rm -f " + os.path.join(dir, file) + ".o")
    os.system("rm -f " + os.path.join(dir, file) + ".map")
    os.system("rm -f " + os.path.join(dir, file) + ".lst")


##
# Helper function to clean up the source tree
##
def copy(moduleDir, file, versionDate, saveDir="."):
    
    versionString = versionDate.replace("/", "_").replace(" ", "_").replace(":", "_")
   
    try:
        cin = open(os.path.join(moduleDir, file + ".c"), "rb")
    except IOError:
        print "Failed to find version " + versionString + " of " file + ".c"
        cin.close()
        return

    
    try:
        iin = open(os.path.join(moduleDir, file + ".i"), "rb")
    except IOError:
        print "Failed to find " + file + ".i"
        cin.close()
        return
    
    cout = open(os.path.join(saveDir, file + "_" + versionString + ".c"), "wb")
    iout = open(os.path.join(saveDir, file + "_" + versionString + ".i"), "wb")

    cout.write(cin.read())
    iout.write(iin.read())

    cin.close()
    iin.close()
    cout.close()
    iout.close()


################################################################
# Interesting functions to get work done
################################################################

##
# Function using pexpect to change a CVS repository to a specific dated
# version.
##
def changeCvsVersion(root, date):

    # Check out dated release (this assumes an anon checkout)
    os.environ['CVS_RSH'] = 'ssh'
    child = pexpect.spawn('cvs up -d -D"' + date + '"')
    child.expect('password:')
    child.sendline('anon')
    child.expect(pexpect.EOF)
    child.close()


##
# Build a specifc module
##
def buildModule(path, file, buildTarget="mica2"):
    clean(path, file)
    build(path, buildTarget)


##
# Get dates of all updates to a set module
##
def getVersionDates(path, file):

    dates = []
    
    os.environ['CVS_RSH'] = 'ssh'
    child = pexpect.spawn('cvs log ' + os.path.join(path, file) + ".c")
    child.expect('password:')
    child.sendline('anon')
    
    dateLine = re.compile("^date: ")
    branchLine = re.compile("^revision \d+\.\d+\.\d+\.\d+")
    
    lines = child.readlines();
    for line in lines:
        
        # Ignore dates of branches
        m = branchLine.match(line)
        if m:
            break
        
        # Grab date from date lines
        m = dateLine.match(line)
        if m:
            date = line.split(" ")[1:3]
            dates.append(date[0] + " " + date[1][:-1])
        
    child.close()
    
    return dates;
   

##
# Update module make files to include a .i in the mica2 bulid
##
def modifyMake():

    try:
        file = 'modules/Makerules'
        inFile = open(file, 'r')
        newFile = file + '.new'
        outFile = open(newFile, 'w')

        for line in inFile:
            if '$(PLATFORM):' in line:
                outFile.write('%.i : %.c\n')
                outFile.write('\t$(CC) -E $(CFLAGS) -DCHECK $(INCDIR) $< -o $@\n')
                outFile.write('\n')
                outFile.write(line.rstrip() + ' $(PROJ).i\n')
            else:
                outFile.write(line)

    except IOError:
        file = 'modules/Makemod'
        inFile = open(file, 'r')
        newFile = file + '.new'
        outFile = open(newFile, 'w')
        
        for line in inFile:
            if 'mica2:' in line:
                outFile.write('%.i : %.c\n')
                outFile.write('\t$(CC) -E $(CFLAGS) -DCHECK $(INCDIR) $< -o $@\n')
                outFile.write('\n')
                outFile.write(line.rstrip() + ' $(PROJ).i\n')
            else:
                outFile.write(line)

    os.rename(newFile, file)
    inFile.close()
    outFile.close()

    return file


################################################################
# Go go driver function.
################################################################

def buildAll(sosRoot, modulePath, moduleName, savePath, buildTarget):
    
    # Assume that this is run from the SOS directory so change into base directory
    origDir = os.getcwd()
    os.chdir(sosRoot)
        
    # Get all version dates that need to be checked out
    dates = getVersionDates(modulePath, moduleName)
    
    count = 0
    total = len(dates)
    for versionDate in dates:

        count = count + 1
        DEBUG("----------------------------------------")
        DEBUG("Building %s from %s (%d of %d)" % (moduleName, versionDate, count, total))
        DEBUG("----------------------------------------")
        
        changeCvsVersion(sosRoot, versionDate)
        mkFile = modifyMake()
        buildModule(modulePath, moduleName, buildTarget) 
        copy(modulePath, moduleName, versionDate, savePath)
        os.remove(mkFile)

    # Change back into the base directory
    os.chdir(origDir)

################################################################
# Stand alone program
################################################################
if __name__ == '__main__':
    
    if len(sys.argv) != 5:
        print "Usage: " + sys.argv[0] + " <sosRoot> <modulePath> <moduleName> <savePath>"
        print "Tool to generate .i files for each CVS version of an SOS module." 
        print "    sosRoot: Root of the SOS distrobution (assumes anonymous checkout"
        print "    modulePath: Path to module relative to sosRoot"
        print "    moduleName: Name of module (without .c)"
        print "    savePath: Location to save generated .c and .i files REALITIVE TO sosRoot"
        sys.exit()

    # Get vital stats
    sosRoot = sys.argv[1]
    modulePath = sys.argv[2]
    moduleName = sys.argv[3]
    savePath = sys.argv[4]
    buildTarget = "mica2"
    
    
