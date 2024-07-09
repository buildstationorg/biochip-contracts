-include .env.local

build:
	forge build

test-all:
	forge test -vvvv

deploy-kairos:
	forge script script/DeployKaia.s.sol --rpc-url ${KAIROS_TESTNET_RPC_URL} --account dev --sender ${SENDER} --broadcast --gas-estimate-multiplier 200 -vvvv

deploy-kaia:
	forge script script/DeployKaia.s.sol --rpc-url ${KAIA_MAINNET_RPC_URL} --account dev --sender ${SENDER} --broadcast --gas-estimate-multiplier 200 -vvvv