
# apt repo to get a recent opam version 
echo "\n" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get -y update
sudo apt-get -y install opam

export OPAMYES=true
# get a recent version of ocaml via opam
opam init --comp=4.05.0
eval `opam config env`

# repo to get recent version of core and async
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
# this install sawadee via the pinning feature of opam
# it'll use the local opam description file to download all the needed packages
opam pin add sawadee .

make all
make unit
./scripts/bt_test.sh
./scripts/bt_test_dht.sh
