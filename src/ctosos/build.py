import sys
import subprocess
import shlex
import re

def make_i(file):
    command = shlex.split("make check mica2")
    subprocess.call(command)


def ctosos(file):
    command = shlex.split("/home/rshea/svn/lighthouse/src/ctosos/ctosos " \
            + file + ".i --out " + file + ".i.c")
    subprocess.call(command)


def clean(file):

    # Remove the "#line ...." entries from file
    loc_line = re.compile("^#line.*$")
    f_in = open(file + ".i.c", "read")
    f_out = open(file + ".clean.c", "write")
    for line in f_in:
        if (loc_line.match(line) != None):
            continue
        else:
            f_out.write(line)
    f_in.close()
    f_out.close()

    # Remove code from header files sitting in file
    ctosos_start = re.compile("^/\* Start of CTOSOS Output \*/$")
    f_in = open(file + ".clean.c", "read")
    f_out = open(file + ".cleaner.c", "write")
    ctosos_found = False
    for line in f_in:
        if (ctosos_start.match(line) != None):
            ctosos_found = True
        if ctosos_found:
            f_out.write(line)
    f_in.close()
    f_out.close()

    # Make the code pretty
    command = shlex.split("indent -ts4 -bad -bap -bbb -sob " + file + ".cleaner.c")
    subprocess.call(command)


def add_headers(file):
    f_in = open(file + ".c", "read")
    f_out = open(file + ".done.c", "write")
    include = re.compile("^\s*#include.*$")
    for line in f_in:
        if(include.match(line) != None):
            f_out.write(line)
    f_in.close()

    #TODO: Ugly hack...
    f_out.write("\n")
    f_out.write("#define CTOSOS_ID 128")
    f_out.write("\n")

    f_in = open(file + ".cleaner.c", "read")
    for line in f_in:
        f_out.write(line)
    f_in.close()
    f_out.close()



if __name__ == "__main__":

    if len(sys.argv) != 2:
        print "Usage: %s <file>" % (sys.argv[0])
        print "    file: name of C file to convert into an SOS module" 
        sys.exit(-1)

    file_name = sys.argv[1]
    
    make_i(file_name)
    ctosos(file_name)
    clean(file_name)
    add_headers(file_name) 

