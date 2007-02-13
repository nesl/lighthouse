CIL_PATH=/home/rshea/mote/cil-1.3.5/obj/x86_LINUX
echo "Assuming library is located in: $CIL_PATH"

echo "Building object files..."
ocamlc -c -I $CIL_PATH memUtil.mli
ocamlc -c -I $CIL_PATH memUtil.ml
ocamlc -c -I $CIL_PATH isEquivalent.mli
ocamlc -c -I $CIL_PATH isEquivalent.ml
ocamlc -c -I $CIL_PATH mayAliasWrapper.mli
ocamlc -c -I $CIL_PATH mayAliasWrapper.ml
ocamlc -c -I $CIL_PATH addAnnotations.mli
ocamlc -c -I $CIL_PATH addAnnotations.ml
ocamlc -c -I $CIL_PATH makeOneCFG.mli
ocamlc -c -I $CIL_PATH makeOneCFG.ml
ocamlc -c -I $CIL_PATH state.ml
ocamlc -c -pp "camlp4o " -I $CIL_PATH specParse.ml
ocamlc -c -I $CIL_PATH apollo.ml
ocamlc -c -I $CIL_PATH rocket.ml

echo "Linking object files..."
ocamlc \
    -I $CIL_PATH unix.cma str.cma cil.cma -o rocket \
    memUtil.cmo isEquivalent.cmo mayAliasWrapper.cmo addAnnotations.cmo \
    makeOneCFG.cmo state.cmo specParse.cmo apollo.cmo rocket.cmo

echo "Done. Now ready to run rocket."

