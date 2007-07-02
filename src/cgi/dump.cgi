#!/usr/bin/python

import cgi
import cgitb; cgitb.enable()

import tempfile
import subprocess

print "Content-Type: text/html\n\n"


# Define function to generate HTML form.
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
    </TABLE>
    <INPUT TYPE = hidden NAME = "action" VALUE = "display">
    <INPUT TYPE = submit VALUE = "Enter">
    </FORM>
</BODY>
</HTML>"""
    print form


# Define function display data.
def display_data(name, age):
    data = """
<HTML>
<HEAD>
    <TITLE>Results</TITLE>
</HEAD>
    <p>%s, you are %s years old.</p>
</BODY>
</HTML>"""
    print data % (name, age)


def checkFile(fname):
    args = ["./rocket", "--spec", "test.spec", fname]
    tmp = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    error = tmp.stderr
    out = tmp.stdout
    f = open(fname, "r")

    print "<HTML>"
    print "<HEAD>"
    print "    <TITLE>Results</TITLE>"
    print " ".join(args)
    print "<H2> Error </H2>"
    for line in error:
        print "<P>", line
    print "<H2> Out </H2>"
    for line in out:
        print "<P>", line
    print "<H2> File </H2>"
    print "<pre>"
    for line in f:
        print line,
    print "</pre>"
    print "</HEAD>"


# Define main function.
def main():
    form = cgi.FieldStorage()
    if (form.has_key("action") and form.has_key("file")):
        if (form["action"].value == "display") and form["file"].filename:
            fileitem = form["file"]
            tfile = tempfile.NamedTemporaryFile()
            for line in fileitem.file:
                tfile.write(line)
            tfile.flush()
            result = checkFile(tfile.name)
            #display_data(result, 0)
    else:
        generate_form()


# Call main function.
main()

