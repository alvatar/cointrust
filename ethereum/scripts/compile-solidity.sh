#!/usr/bin/env bash

SOURCE_DIR=./src/ethereum
SOLC=/data/Dropbox/projects/solidity/build/solc/solc
mkdir -p ./target/contracts
$SOLC --optimize --bin --abi --combined-json bin,abi $SOURCE_DIR/contract.sol -o ./target/contracts/ > ./target/contracts/contract.json
