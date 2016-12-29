#!/bin/bash
bitcoind -server -listen -port=18444 -connect=localhost:18445 -regtest -pid=$HOME/regtest/A/.pid -daemon -debug # -datadir=$HOME/regtest/A/ -rpcuser=bitcoinrpc -rpcpassword=P$SECONDS -rpcport=16591 
bitcoind -server -listen -port=18445 -rpcuser=bitcoinrpc -rpcpassword=P$SECONDS -rpcport=16592 -datadir=$HOME/regtest/B/ -connect=localhost:18446 -regtest -pid=$HOME/regtest/B/.pid -daemon -debug
bitcoind -server -listen -port=18446 -rpcuser=bitcoinrpc -rpcpassword=P$SECONDS -rpcport=16593 -datadir=$HOME/regtest/C/ -connect=localhost:18447 -regtest -pid=$HOME/regtest/C/.pid -daemon -debug
bitcoind -server -listen -port=18447 -rpcuser=bitcoinrpc -rpcpassword=P$SECONDS -rpcport=16594 -datadir=$HOME/regtest/D/ -connect=localhost:18448 -regtest -pid=$HOME/regtest/D/.pid -daemon -debug
bitcoind -server -listen -port=18448 -rpcuser=bitcoinrpc -rpcpassword=P$SECONDS -rpcport=16595 -datadir=$HOME/regtest/E/ -connect=localhost:18444 -regtest -pid=$HOME/regtest/E/.pid -daemon -debug

sleep 2

bitcoin-cli -regtest generate 11
# bitcoin-cli -regtest sendtoaddress <ADDRESS>  0.1
