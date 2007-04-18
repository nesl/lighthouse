CIL_ROOT=/cil-1.3.5
ARCH=x86_LINUX

CIL_LIB=$CIL_ROOT/obj/$ARCH
echo "Assuming library is located in: $CIL_LIB"

echo "Building object files..."
ocamlc -c -I $CIL_LIB memUtil.mli
ocamlc -c -I $CIL_LIB memUtil.ml
ocamlc -c -I $CIL_LIB isEquivalent.mli
ocamlc -c -I $CIL_LIB isEquivalent.ml
ocamlc -c -I $CIL_LIB mayAliasWrapper.mli
ocamlc -c -I $CIL_LIB mayAliasWrapper.ml
ocamlc -c -I $CIL_LIB makeOneCFG.mli
ocamlc -c -I $CIL_LIB makeOneCFG.ml
ocamlc -c -I $CIL_LIB state.ml
ocamlc -c -pp "camlp4o " -I $CIL_LIB specParse.ml
ocamlc -c -I $CIL_LIB apollo.ml
ocamlc -c -I $CIL_LIB rocket.ml

echo "Linking object files..."
ocamlc \
    -I $CIL_LIB unix.cma str.cma cil.cma -o lighthouse \
    memUtil.cmo isEquivalent.cmo mayAliasWrapper.cmo \
    makeOneCFG.cmo state.cmo specParse.cmo apollo.cmo rocket.cmo

echo "Done. Now ready to run lighthouse."

