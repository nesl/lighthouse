import os
import sys
import subprocess
import shlex
import re

def make_i(file):
    command = shlex.split("make check mica2")
    subprocess.call(command)


def ctosos(file):
    command = shlex.split("/home/rshea/svn/lighthouse/src/ctosos/ctosos " \
            + file + ".i --out " + file + ".ctosos.c")
    subprocess.call(command)


def clean(file):

    # Remove the "#line ...." entries from file
    loc_line = re.compile("^#line.*$")
    f_in = open(file + ".ctosos.c", "read")
    f_out = open(file + ".ctosos.c.tmp", "write")
    for line in f_in:
        if (loc_line.match(line) != None):
            continue
        else:
            f_out.write(line)
    f_in.close()
    f_out.close()
    os.remove(file + ".i")
    os.rename(file + ".ctosos.c.tmp", file + ".ctosos.c")

    # Remove code from header files sitting in file
    ctosos_start = re.compile("^/\* Start of CTOSOS Output \*/$")
    f_in = open(file + ".ctosos.c", "read")
    f_out = open(file + ".ctosos.c.tmp", "write")
    ctosos_found = False
    for line in f_in:
        if (ctosos_start.match(line) != None):
            ctosos_found = True
        if ctosos_found:
            f_out.write(line)
    f_in.close()
    f_out.close()
    os.rename(file + ".ctosos.c.tmp", file + ".ctosos.c")

    # Remove type casts from SOS_CALL
    f_in = open(file + ".ctosos.c", "read")
    f_out = open(file + ".ctosos.c.tmp", "write")
    sos_call = re.compile("^.*=.*(.*).*SOS_CALL.*$")
    for line in f_in:
        if (sos_call.match(line) != None):
            # Strip potential type casts from SOS_CALL
            sos_call_cast = re.compile("=.*(.*).*SOS_CALL")
            m = sos_call_cast.search(line)
            head = line[:(m.start() + 1)]
            tail = line[(m.end()-len("SOS_CALL")):]
            f_out.write(head + " " + tail)
        else:
            f_out.write(line)
    f_in.close()
    f_out.close()
    os.rename(file + ".ctosos.c.tmp", file + ".ctosos.c")


    # Make the code pretty
    command = shlex.split("indent -i4 -lp -ts4 -nut -bad -bap -bbb -sob " + file + ".ctosos.c")
    subprocess.call(command)
    os.remove(file + ".ctosos.c~")


def add_headers(file):
    f_in = open(file + ".c", "read")
    f_out = open(file + ".ctosos.c.tmp", "write")
    include = re.compile("^\s*#include.*$")
    for line in f_in:
        if(include.match(line) != None):
            f_out.write(line)
    f_in.close()

    f_out.write("#include \"" + file + ".h\"\n")

    #TODO: Ugly hack...
    f_out.write("\n")
    f_out.write("#define CTOSOS_ID 128\n")
    f_out.write("\n")

    f_in = open(file + ".ctosos.c", "read")
    for line in f_in:
        f_out.write(line)
    f_in.close()
    f_out.close()
    os.rename(file + ".ctosos.c.tmp", file + ".ctosos.c")



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

