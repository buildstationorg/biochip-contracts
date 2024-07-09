// SPDX-License-Identifier: MIT
pragma solidity 0.8.25;

import { Script } from "forge-std/Script.sol";

contract DeployConfig is Script {
    /// @dev declare a struct to store the network configuration
    struct NetworkConfig {
        uint256 initialFee;
        address initialOwner;
    }

    /// @dev declare a variable to store the network configuration
    NetworkConfig public activeNetworkConfig;

    constructor() {
        if (block.chainid == 1001) {
            activeNetworkConfig = getKaiaKairosConfig();
        } else if (block.chainid == 8217) {
            activeNetworkConfig = getKaiaMainnetConfig();
        } else {
            activeNetworkConfig = getAnvilConfig();
        }
    }

    function getKaiaKairosConfig()
        public
        pure
        returns (NetworkConfig memory kairosNetworkConfig)
    {
        kairosNetworkConfig = NetworkConfig({
            initialFee: 1 ether,
            initialOwner: 0xe3d25540BA6CED36a0ED5ce899b99B5963f43d3F
        });
    }

    function getKaiaMainnetConfig()
        public
        pure
        returns (NetworkConfig memory kaiaNetworkConfig)
    {
        kaiaNetworkConfig = NetworkConfig({
            initialFee: 1 ether,
            initialOwner: 0x439aa01146DEB050881a254c7490c7f466e4D88d
        });
    }

    function getAnvilConfig()
        public
        pure
        returns (NetworkConfig memory anvilNetworkConfig)
    {

      /// @dev return the network configuration with the mock data feed
        anvilNetworkConfig = NetworkConfig({
            initialFee: 1 ether,
            initialOwner: 0xe3d25540BA6CED36a0ED5ce899b99B5963f43d3F
        });
    }
}