#!/bin/bash
sudo killall --regex bitcoind.*
sleep 5
rm -R ~/.bitcoin/regtest
