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
    <FORM METHOD = post ENCTYPE = "multipart/form-data" ACTION = "ctosos.cgi">
    <H3>Please, enter the file you wish to convert from C to SOS.</H3>
    <TABLE BORDER = 0>
        <TR><TH>C File:</TH><TD><INPUT type = file name = "file_c"></TD></TR>
        <TR><TH>I File:</TH><TD><INPUT type = file name = "file_i"></TD></TR>
        <TR><TH>Enable Verbose Output:</TH><TD><INPUT type=checkbox name=verbose value=verbose></TD></TR>
    </TABLE>
    <INPUT TYPE = hidden NAME=action VALUE=display>
    <INPUT TYPE = submit VALUE=Enter>
    </FORM>
</BODY>
</HTML>"""
    
    print "Content-Type: text/html\n\n"
    print form


def checkFile(fname_c, fname_i, verbose):
    args = ["./build.py", fname_c, fname_i]
    ctosos = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    ctosos_error = ctosos.stderr
    ctosos_out = ctosos.stdout

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
        for line in ctosos_error:
            print "<P>", line
        
        # Display stdout
        print "<H2> Out </H2>"
        for line in ctosos_out:
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
        for line in ctosos_error:
            print line,
        for line in ctosos_out:
            print line,

# Define main function.
def main():
    form = cgi.FieldStorage()
    if form.has_key("action") and \
            (form["action"].value == "display") and \
            form.has_key("file_c") and \
            form["file_c"].filename and \
            form.has_key("file_i") and \
            form["file_i"].filename:
        
        verbose = False
        if form.has_key("verbose") and (form["verbose"].value == "verbose"):
            verbose = True

        fileitem_c = form["file_c"]
        fileitem_i = form["file_i"]
        
        date = time.ctime().split()
        date = "_".join(date)
        date = date.split(":")
        date = "_".join(date)

        file_name_c = "data/" + date + "_" + fileitem_c.filename
        file_c = open(file_name_c, "w")
        for line in fileitem_c.file:
            file_c.write(line)
        file_c.close()
        
        file_name_i = "data/" + date + "_" + fileitem_i.filename
        file_i = open(file_name_i, "w")
        for line in fileitem_i.file:
            file_i.write(line)
        file_i.close()
       
        result = checkFile(file_name_c, file_name_i, verbose)
    else:
        generate_form()


# Call main function.
main()

