#!/usr/bin/python

import sys
import getCvs


# Poor man's debugging macro
debugOn = True
def DEBUG(out):
    if debugOn:
        print out


##
# Read configuration file
##
def getModules(file):
    config = open(file, "r")
    modules = []
    for line in config:
        (path, module) = line.strip().split(" ")
        modules.append((path, module))
    config.close()
    return modules



################################################################
# Stand alone program
################################################################
if __name__ == '__main__':
    
    if len(sys.argv) != 4:
        print "Usage: " + sys.argv[0] + " <configFile> <sosRoot> <savePath>"
        print "Tool to generate .i files for each CVS version of a collection of SOS modules." 
        print "    configFile: file containing the path to and name of modules to build"
        print "    sosRoot: Root of the SOS distrobution (assumes anonymous checkout"
        print "    savePath: Location to save generated .c and .i files REALITIVE TO sosRoot"
        sys.exit()

    # Get vital stats from command line
    configFile = sys.argv[1]
    sosRoot = sys.argv[2]
    savePath = sys.argv[3]

    # Use intelegent defaults
    buildTarget = "mica2"
    
    # Get modules
    modules = getModules(configFile)
    
    # Iterate over all modules
    count = 0
    total = len(modules)
    for (modulePath, moduleName) in modules:
        
        count = count + 1
        DEBUG("========================================") 
        DEBUG("Module %s (%d of %d)" % (moduleName, count, total))
        DEBUG("========================================") 
        
        getCvs.buildAll(sosRoot, modulePath, moduleName, savePath, buildTarget)

        
