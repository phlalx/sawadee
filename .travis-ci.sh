
echo "\n" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get -y update
sudo apt-get -y install opam

export OPAMYES=true
opam init --comp=4.05.0
eval `opam config env`
opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
opam pin add ocaml-torrent .

make all
make unit
./scripts/bt_test.sh
./scripts/bt_test_dht.sh
