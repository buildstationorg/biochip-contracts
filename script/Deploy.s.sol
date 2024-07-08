// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.19;

import { Script } from "lib/forge-std/src/Script.sol";
import { DeployConfig } from "script/DeployConfig.s.sol";
import { BioChipKaia } from "src/BioChipKaia.sol";


contract Deploy is Script {
    function run() external returns (DeployHelper, BioChipKaia) {
        /// @dev initialize the DeployHelper contract
        DeployConfig deplyConfig = new DeployConfig();

        /// @dev get the active network configuration using the DeployHelper contract 
        (uint16 startingNumber, address dataFeed, address coordinator, bytes32 keyHash, uint64 accountId) = deployHelper.activeNetworkConfig();

        /// @dev start the broadcast
        vm.startBroadcast();

        /// @dev create a new Counter contract with the active network configuration
        Counter counter = new Counter(startingNumber);
        LuckyDraw luckyDraw = new LuckyDraw(dataFeed, coordinator, keyHash, accountId);
        /// @dev create a new Monee contract with the active network configuration
        Monee monee = new Monee(address(luckyDraw));

        /// @dev stop the broadcast
        vm.stopBroadcast();

        /// @dev return the Counter and DeployHelper contracts
        return (counter, luckyDraw, monee, deployHelper);
    }
}