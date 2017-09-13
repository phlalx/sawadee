# "TO GO" test...
# these are legal *wild* torrent files. Torrent are usually short-lived, so
# they may be deprecated in the near future. A source of legal torrents 
# can be found 
 

URI1=tests/torrents/NuTyX_x86_64-20170625.torrent                          
URI2=tests/torrents/This_Week_@NASA,_July_7,_2017[V004848568].mp4.torrent  
URI3=tests/torrents/ubuntu-17.04-desktop-amd64.iso.torrent                 
URI4=tests/torrents/controller.torrent                                     
URI5="magnet:?xt=urn:btih:44600f089...."

URI=$URI1

OPTIONS="-l 7000 -v 2 -p download"
                                                                                 
EXEC_PATH=_build/install/default/bin                                             

# sawadee_repl                                            
CLIENT=$EXEC_PATH/sawadee                                                      
# CLIENT="rlwrap $EXEC_PATH/sawadee_repl"                                                      

set -x
$CLIENT $OPTIONS $URI