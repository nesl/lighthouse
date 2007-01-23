# Simple make :-)

all: spec_parse

spec_parse: spec_parse.ml
	ocamlc -pp camlp4o spec_parse.ml -o spec_parse

clean:
	rm -rf spec_parse *.cmi *.cmo

