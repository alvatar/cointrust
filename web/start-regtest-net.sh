#!/bin/bash

bitcoind -regtest -daemon

sleep 5

bitcoin-cli -regtest generate 101
