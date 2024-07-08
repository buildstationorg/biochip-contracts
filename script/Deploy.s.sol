// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.19;

import { Script } from "forge-std/Script.sol";
import { DeployConfig } from "script/DeployConfig.s.sol";
import { BioChipKaia } from "src/BioChipKaia.sol";


contract Deploy is Script {
    function run() external returns (DeployHelper, BioChipKaia) {
        /// @dev initialize the DeployConfig contract
        DeployConfig deplyConfig = new DeployConfig();

        /// @dev get the active network configuration using the DeployConfig contract
        (uint256 intialFee, address initialOwner) = deployHelper.activeNetworkConfig();

        /// @dev start the broadcast
        vm.startBroadcast();

        /// @dev create a new BioChipKaia contract
        BioChipKaia bioChipKaia = new BioChipKaia(intialFee, initialOwner);

        /// @dev stop the broadcast
        vm.stopBroadcast();

        /// @dev return the BioChipKaia and DeployHelper contracts
        return (deployConfig, bioChipKaia);
    }
}