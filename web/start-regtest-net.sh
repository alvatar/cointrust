#!/bin/bash

bitcoind -regtest -daemon

sleep 1

bitcoin-cli -regtest generate 11
