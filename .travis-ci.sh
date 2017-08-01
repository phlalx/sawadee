OPAM_DEPENDS="ocamlfind oUnit core core_extended ppx_jane uri cohttp async 
              cohttp-async bencode sha hex"

# async requires ocaml >= 4.03. 
# we can only get 4.02 from avsm/ppa so we get it from opam

echo "\n" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get -y update
sudo apt-get -y install opam

export OPAMYES=true
opam init --comp=4.04.2
opam install ${OPAM_DEPENDS}

eval `opam config env`

make all
make unit
./scripts/run_test.sh
