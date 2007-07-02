#!/usr/bin/python

import cgi
import cgitb; cgitb.enable()

import tempfile
import subprocess
import time


# Simple HTML form requesting file to verify.
def generate_form():
    form = """
<HTML>
<HEAD>
    <TITLE>Info Form</TITLE>
</HEAD>
    <FORM METHOD = post ENCTYPE = "multipart/form-data" ACTION = "dump.cgi">
    <H3>Please, enter the file you wish to be examined.</H3>
    <TABLE BORDER = 0>
        <TR><TH>File:</TH><TD><INPUT type = file name = "file"></TD></TR>
        <TR><TH>Enable Verbose Output:</TH><TD><INPUT type=checkbox name=verbose value=verbose></TD></TR>
    </TABLE>
    <INPUT TYPE = hidden NAME=action VALUE=display>
    <INPUT TYPE = submit VALUE=Enter>
    </FORM>
</BODY>
</HTML>"""
    
    print "Content-Type: text/html\n\n"
    print form


def checkFile(fname, verbose):
    args = ["./rocket", "--spec", "test.spec", fname]
    lighthouse = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    lh_error = lighthouse.stderr
    lh_out = lighthouse.stdout

    if verbose:
        print "Content-Type: text/html\n\n"
        print "<HTML>"
        print "<HEAD>"
        
        # Display command line 
        print "    <TITLE>Results</TITLE>"
        print "<H2> Command </H2>"
        print " ".join(args)
        
        # Display stderr 
        print "<H2> Error </H2>"
        for line in lh_error:
            print "<P>", line
        
        # Display stdout
        print "<H2> Out </H2>"
        for line in lh_out:
            print "<P>", line
        
        # Display file that was verified 
        print "<H2> File </H2>"
        print "<pre>"
        in_file = open(fname, "r")
        for line in in_file:
            print line,
        print "</pre>"


        print "</HEAD>"

    else:
        print "Content-Type: text/plain\n\n"
        print "Results:"
        for line in lh_error:
            print line,
        for line in lh_out:
            print line,

# Define main function.
def main():
    form = cgi.FieldStorage()
    if form.has_key("action") and \
            (form["action"].value == "display") and \
            form.has_key("file") and \
            form["file"].filename:
        
        verbose = False
        if form.has_key("verbose") and (form["verbose"].value == "verbose"):
            verbose = True

        fileitem = form["file"]
        
        date = time.ctime().split()
        date = "_".join(date)
        date = date.split(":")
        date = "_".join(date)

        file_name = "data/" + date + "_" + fileitem.filename
        file = open(file_name, "w")
        for line in fileitem.file:
            file.write(line)
        file.close()
        result = checkFile(file_name, verbose)
    else:
        generate_form()


# Call main function.
main()

