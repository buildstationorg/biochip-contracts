// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.25;

import "forge-std/Test.sol";
import { BioChipKaia } from "src/BioChipKaia.sol";

contract BioChipKaiaTest is Test {
    BioChipKaia bioChipKaia;
    address constant OWNER = 0xe3d25540BA6CED36a0ED5ce899b99B5963f43d3F;
    uint256 constant SEND_VALUE = 5 ether;
    address bob = makeAddr("bob");

    function setUp() public {
        vm.deal(bob, 100 ether);
        vm.deal(OWNER, 1 ether);
        bioChipKaia = new BioChipKaia(5 ether, OWNER);
    }

    function test_Owner() public {
        assertEq(bioChipKaia.owner(), OWNER);
    }

    function test_StartingBalance() public {
        assertEq(bioChipKaia.getBalance(), 0 ether);
    }

    function test_Mint() public {
        vm.startPrank(bob);
        bioChipKaia.mint{value: SEND_VALUE}();
        vm.stopPrank();
        assertEq(bioChipKaia.getBalance(), SEND_VALUE);
        assertEq(bioChipKaia.ownerOf(1), bob);
    }

    function test_Withdraw() public {
        vm.prank(bob);
        bioChipKaia.mint{value: SEND_VALUE}();
        assertEq(bioChipKaia.getBalance(), SEND_VALUE);
        vm.prank(OWNER);
        bioChipKaia.withdrawFee();
        assertEq(bioChipKaia.getBalance(), 0 ether);
        assertEq(OWNER.balance, SEND_VALUE + 1 ether);
    }
}