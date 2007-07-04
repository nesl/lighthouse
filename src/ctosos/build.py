#!/usr/bin/python

import os
import sys
import subprocess
import shlex
import re
import tempfile


def make_i(file):
    null = open("/dev/null", "w")
    command = shlex.split("make " + file + ".i mica2")
    subprocess.call(command, stderr=null, stdout=null)


def ctosos(file_i):
    command = shlex.split("/home/rshea/svn/lighthouse/src/ctosos/ctosos " + file_i)
    ctosos = subprocess.Popen(command, stdout=subprocess.PIPE)
    return ctosos.stdout


def clean(output):

    # Remove the "#line ...." entries from file
    loc_line = re.compile("^#line.*$")
    no_line_nums = []
    for line in output:
        if (loc_line.match(line) != None):
            continue
        else:
            no_line_nums.append(line)
    output = no_line_nums


    # Remove code from header files sitting in file
    ctosos_start = re.compile("^/\* Start of CTOSOS Output \*/$")
    ctosos_found = False
    cut_header = []
    for line in output:
        if (ctosos_start.match(line) != None):
            ctosos_found = True
        if ctosos_found:
            cut_header.append(line) 
    output = cut_header


    # Remove type casts from SOS_CALL
    sos_call = re.compile("^.*=.*(.*).*SOS_CALL.*$")
    no_call_casts = []
    for line in output:
        if (sos_call.match(line) != None):
            # Strip potential type casts from SOS_CALL
            sos_call_cast = re.compile("=.*(.*).*SOS_CALL")
            m = sos_call_cast.search(line)
            head = line[:(m.start() + 1)]
            tail = line[(m.end()-len("SOS_CALL")):]
            no_call_casts.append(head + " " + tail)
        else:
            no_call_casts.append(line)
    output = cut_header


    # Make the code pretty
    input = tempfile.TemporaryFile()
    for line in output:
        input.write(line)
    input.seek(0)   
    command = shlex.split("indent -i4 -lp -ts4 -nut -bad -bap -bbb -sob -st")
    pretty = subprocess.Popen(command, stdout=subprocess.PIPE, stdin=input)
    return pretty.stdout


def add_headers(file_c, output):
    f_in = open(file_c, "read")
    include = re.compile("^\s*#include.*$")
    with_headers = []
    for line in f_in:
        if(include.match(line) != None):
            with_headers.append(line)
    f_in.close()

    with_headers.append("#include \"" + file_c[:-2] + ".h\"\n")

    #TODO: Ugly hack...
    with_headers.append("\n")
    with_headers.append("#define CTOSOS_ID 128\n")
    with_headers.append("\n")

    for line in output:
        with_headers.append(line)

    return with_headers



if __name__ == "__main__":

    if len(sys.argv) != 3:
        print "Usage: %s <file.c> <file.i>" % (sys.argv[0])
        print "    file.c: name of C file to convert into an SOS module" 
        print "    file.i: name of preprocessed version of C file" 
        sys.exit(-1)

    file_name_c = sys.argv[1]
    file_name_i = sys.argv[2]
    
    # make_i(file_name)
    output = ctosos(file_name_i)
    output = clean(output)
    output = add_headers(file_name_c, output) 
    for line in output:
        print line,

