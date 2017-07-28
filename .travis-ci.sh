OPAM_DEPENDS="ocamlfind ounit core core_extended ppx_jane uri cohttp async \
              bencode sha"

case "$OCAML_VERSION,$OPAM_VERSION" in
   4.05,1.2.2)
        OCAML_VERSION=4.02; OPAM_SWITCH="4.05.0"
        ppa=avsm/ocaml42+opam12 ;;
        *) echo Unknown $OCAML_VERSION,$OPAM_VERSION
           exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time libssl-dev

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam install ${OPAM_DEPENDS}

eval `opam config env`
# make
# make unit
opam list
