// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

import "dn404/DN404.sol";
import "dn404/DN404Mirror.sol";
import { Ownable } from "solady/auth/Ownable.sol";
import { LibString } from "solady/utils/LibString.sol";
import { SafeTransferLib } from "solady/utils/SafeTransferLib.sol";

/**
 * @title StreetCredToken
 * @author zxstim
 * @notice Sample DN404 contract that demonstrates the owner selling fungible tokens.
 * When a user has at least one base unit (10^18) amount of tokens, they will automatically receive an NFT.
 * NFTs are minted as an address accumulates each base unit amount of tokens.
 */
contract StreetCredToken is DN404, Ownable {
    string private _name;
    string private _symbol;
    string private _baseURI;

    constructor(string memory name_, string memory symbol_, uint96 initialTokenSupply, address initialSupplyOwner) {
        _initializeOwner(msg.sender);

        _name = name_;
        _symbol = symbol_;

        address mirror = address(new DN404Mirror(msg.sender));
        _initializeDN404(initialTokenSupply, initialSupplyOwner, mirror);
    }

    function name() public view override returns (string memory) {
        return _name;
    }

    function symbol() public view override returns (string memory) {
        return _symbol;
    }

    function _tokenURI(uint256 tokenId) internal view override returns (string memory result) {
        if (bytes(_baseURI).length != 0) {
            result = string(abi.encodePacked(_baseURI, LibString.toString(tokenId)));
        }
    }

    // This allows the owner of the contract to mint more tokens.
    function mint(address to, uint256 amount) public onlyOwner {
        _mint(to, amount);
    }

    function setBaseURI(string calldata baseURI_) public onlyOwner {
        _baseURI = baseURI_;
    }

    function withdraw() public onlyOwner {
        SafeTransferLib.safeTransferAllETH(msg.sender);
    }
}
