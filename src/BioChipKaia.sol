// SPDX-License-Identifier: MIT
pragma solidity 0.8.25;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/utils/Base64.sol";
import "@openzeppelin/contracts/utils/Strings.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";

error InvalidTokenId(uint256 tokenId);
error InsufficientValueToMint();
error WithdrawalFailed();

/// @title buildstation BioChipKaia contract
/// @author zxstim
/// @notice You can use this contract for only the most basic simulation
/// @dev All function calls are currently implemented without side effects
contract BioChipKaia is ERC721, ERC721Enumerable, Ownable {
    uint256 private counter;
    uint256 private fee;

    /// @notice Constructor to create a BioChipKaia
    /// @param initialFee the initial fee to mint a token in native token
    /// @param initialOwner the initial owner of the contract
    constructor(
        uint256 initialFee,
        address initialOwner
    ) ERC721("BioChip", "BIO") Ownable(initialOwner) {
        fee = initialFee;
    }

    /// @notice Mint function to mint buildstation BioChipKaia NFT
    function mint() public payable {
        if (msg.value < fee) {
            revert InsufficientValueToMint();
        }
        counter++;
        _safeMint(msg.sender, counter);
    }

    /// @notice internal pure function set the base URI
    /// @return BaseURI which is a string
    function _baseURI() internal pure override returns (string memory) {
        return "data:application/json;base64,";
    }

    /// @notice function to set the token URI
    /// @param _tokenId the token id which is set by the counter
    /// @return TokenURI which is a base64 string with the SVG image encoded as a base64 string
    function tokenURI(
        uint _tokenId
    ) public view override returns (string memory) {
        if (_tokenId > counter) {
            revert InvalidTokenId(_tokenId);
        }

        string memory json = Base64.encode(
            bytes(
                string(
                    abi.encodePacked(
                        '{"name": "BioChip #',
                        Strings.toString(_tokenId),
                        '", "description": "BioChip is the entry point to buildstation community stored entirely onchain.", "image": "data:image/SVG+xml;base64,',
                        Base64.encode(
                            bytes(
                                string(
                                    abi.encodePacked(
                                        '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="500" zoomAndPan="magnify" viewBox="0 0 375 374.999991" height="500" preserveAspectRatio="xMidYMid meet" version="1.0"><defs><clipPath id="8e45a9c0ca"><path d="M 99 90 L 276 90 L 276 299 L 99 299 Z M 99 90 " clip-rule="nonzero"/></clipPath><clipPath id="20ca65ca72"><path d="M 97.839844 137.9375 L 199.253906 89.238281 L 277.246094 251.65625 L 175.835938 300.355469 Z M 97.839844 137.9375 " clip-rule="nonzero"/></clipPath><clipPath id="08a9e9cab3"><path d="M 101.894531 135.988281 L 195.195312 91.183594 C 196.269531 90.667969 197.507812 90.601562 198.632812 90.996094 C 199.761719 91.390625 200.683594 92.21875 201.199219 93.292969 L 275.214844 247.421875 C 275.730469 248.496094 275.796875 249.734375 275.402344 250.859375 C 275.007812 251.984375 274.179688 252.910156 273.105469 253.425781 L 179.804688 298.226562 C 178.730469 298.746094 177.492188 298.8125 176.367188 298.417969 C 175.238281 298.023438 174.316406 297.195312 173.800781 296.121094 L 99.785156 141.992188 C 98.710938 139.753906 99.65625 137.0625 101.894531 135.988281 Z M 101.894531 135.988281 " clip-rule="nonzero"/></clipPath><clipPath id="18c8ba5e8d"><path d="M 125 124 L 156 124 L 156 199 L 125 199 Z M 125 124 " clip-rule="nonzero"/></clipPath><clipPath id="9c1a81617e"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="1def99d45b"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="b9a41f6249"><path d="M 137 118 L 168 118 L 168 170 L 137 170 Z M 137 118 " clip-rule="nonzero"/></clipPath><clipPath id="ad95d07b4c"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="7591293936"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="2d3c272841"><path d="M 150 112 L 196 112 L 196 198 L 150 198 Z M 150 112 " clip-rule="nonzero"/></clipPath><clipPath id="ec625cf862"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="e4619d031b"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="54533ef356"><path d="M 163 106 L 210 106 L 210 168 L 163 168 Z M 163 106 " clip-rule="nonzero"/></clipPath><clipPath id="9cd436f0d9"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="c831b8a4bd"><path d="M 150.332031 216.34375 L 109.421875 132.828125 L 180.816406 97.855469 L 221.726562 181.375 Z M 150.332031 216.34375 " clip-rule="nonzero"/></clipPath><clipPath id="31cbd0d10b"><path d="M 241 220 L 269 220 L 269 255 L 241 255 Z M 241 220 " clip-rule="nonzero"/></clipPath><clipPath id="59b29926d3"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="bc0951b29e"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="0674db1a29"><path d="M 240.003906 226.792969 L 256.148438 219.070312 L 270.078125 248.203125 L 253.929688 255.921875 Z M 240.003906 226.792969 " clip-rule="nonzero"/></clipPath><clipPath id="0428bdbac2"><path d="M 240.0625 226.882812 L 256.15625 219.1875 L 270.015625 248.171875 L 253.917969 255.867188 Z M 240.0625 226.882812 " clip-rule="nonzero"/></clipPath><clipPath id="a57aa61717"><path d="M 240.003906 226.792969 L 256.148438 219.070312 L 270.078125 248.203125 L 253.929688 255.921875 Z M 240.003906 226.792969 " clip-rule="nonzero"/></clipPath><clipPath id="bb40fc4217"><path d="M 242.933594 225.511719 L 253.285156 220.5625 C 254.871094 219.804688 256.769531 220.472656 257.53125 222.058594 L 268.617188 245.25 C 269.375 246.835938 268.707031 248.738281 267.121094 249.496094 L 256.765625 254.445312 C 255.179688 255.203125 253.28125 254.53125 252.523438 252.945312 L 241.433594 229.753906 C 240.675781 228.167969 241.347656 226.269531 242.933594 225.511719 Z M 242.933594 225.511719 " clip-rule="nonzero"/></clipPath><clipPath id="6f8a303f49"><path d="M 233 233 L 269 233 L 269 262 L 233 262 Z M 233 233 " clip-rule="nonzero"/></clipPath><clipPath id="fa0b9627fb"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="29782cb6d9"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="191b40f858"><path d="M 232.230469 246.460938 L 262.355469 232.054688 L 270.078125 248.203125 L 239.949219 262.605469 Z M 232.230469 246.460938 " clip-rule="nonzero"/></clipPath><clipPath id="cdaea2ed47"><path d="M 232.304688 246.519531 L 262.375 232.140625 L 270.011719 248.113281 L 239.941406 262.488281 Z M 232.304688 246.519531 " clip-rule="nonzero"/></clipPath><clipPath id="61e0a4e9fa"><path d="M 232.230469 246.460938 L 262.355469 232.054688 L 270.078125 248.203125 L 239.949219 262.605469 Z M 232.230469 246.460938 " clip-rule="nonzero"/></clipPath><clipPath id="e3d944fbf7"><path d="M 235.175781 245.144531 L 259.484375 233.523438 C 261.070312 232.765625 262.96875 233.4375 263.726562 235.023438 L 268.617188 245.25 C 269.375 246.835938 268.707031 248.738281 267.121094 249.496094 L 242.8125 261.117188 C 241.226562 261.875 239.328125 261.203125 238.566406 259.617188 L 233.675781 249.390625 C 232.917969 247.804688 233.589844 245.902344 235.175781 245.144531 Z M 235.175781 245.144531 " clip-rule="nonzero"/></clipPath><clipPath id="1d69a8fa3e"><path d="M 237 230 L 259 230 L 259 252 L 237 252 Z M 237 230 " clip-rule="nonzero"/></clipPath><clipPath id="849554bb7b"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="38ec6b5fe4"><path d="M 201.917969 224.550781 L 264.167969 194.789062 L 293.929688 257.039062 L 231.679688 286.800781 Z M 201.917969 224.550781 " clip-rule="nonzero"/></clipPath><clipPath id="bcee0e2652"><path d="M 236.117188 236.625 L 252.261719 228.90625 L 259.980469 245.050781 L 243.835938 252.773438 Z M 236.117188 236.625 " clip-rule="nonzero"/></clipPath><clipPath id="8764c6dfaf"><path d="M 236.246094 236.671875 L 252.214844 229.035156 L 259.851562 245.007812 L 243.882812 252.640625 Z M 236.246094 236.671875 " clip-rule="nonzero"/></clipPath><clipPath id="64db4f9a83"><path d="M 236.117188 236.625 L 252.261719 228.90625 L 259.980469 245.050781 L 243.835938 252.773438 Z M 236.117188 236.625 " clip-rule="nonzero"/></clipPath><clipPath id="9f44411362"><path d="M 239.117188 235.300781 L 249.34375 230.410156 C 250.929688 229.652344 252.832031 230.320312 253.589844 231.90625 L 258.480469 242.136719 C 259.238281 243.722656 258.566406 245.621094 256.980469 246.378906 L 246.753906 251.269531 C 245.167969 252.027344 243.265625 251.355469 242.507812 249.769531 L 237.617188 239.542969 C 236.859375 237.957031 237.53125 236.058594 239.117188 235.300781 Z M 239.117188 235.300781 " clip-rule="nonzero"/></clipPath></defs><rect x="-37.5" width="450" fill="#ffffff" y="-37.499999" height="449.999989" fill-opacity="1"/><rect x="-37.5" width="450" fill="#7eda6c" y="-37.499999" height="449.999989" fill-opacity="1"/><g clip-path="url(#8e45a9c0ca)"><g clip-path="url(#20ca65ca72)"><g clip-path="url(#08a9e9cab3)"><path fill="#10391d" d="M 97.839844 137.9375 L 199.253906 89.238281 L 277.246094 251.65625 L 175.835938 300.355469 Z M 97.839844 137.9375 " fill-opacity="1" fill-rule="nonzero"/></g></g></g><g clip-path="url(#18c8ba5e8d)"><g clip-path="url(#9c1a81617e)"><g clip-path="url(#1def99d45b)"><path fill="#038137" d="M 154.792969 185.957031 C 152.871094 182.035156 148.371094 180.238281 144.324219 181.574219 L 137.835938 168.328125 L 143.253906 158.648438 C 143.453125 158.28125 143.433594 157.914062 143.261719 157.566406 L 127.53125 125.449219 C 127.253906 124.882812 126.539062 124.636719 125.972656 124.914062 C 125.40625 125.191406 125.160156 125.90625 125.441406 126.472656 L 140.914062 158.066406 L 135.496094 167.742188 C 135.300781 168.109375 135.316406 168.480469 135.488281 168.828125 L 142.253906 182.640625 C 138.71875 185.023438 137.378906 189.675781 139.300781 193.597656 C 141.390625 197.871094 146.585938 199.648438 150.855469 197.554688 C 155.128906 195.464844 156.882812 190.226562 154.792969 185.957031 Z M 141.414062 192.507812 C 139.898438 189.414062 141.1875 185.648438 144.28125 184.132812 C 147.375 182.617188 151.140625 183.910156 152.65625 187.003906 C 154.171875 190.097656 152.882812 193.863281 149.789062 195.378906 C 146.695312 196.894531 142.953125 195.648438 141.414062 192.507812 Z M 141.414062 192.507812 " fill-opacity="1" fill-rule="nonzero"/></g></g></g><g clip-path="url(#b9a41f6249)"><g clip-path="url(#ad95d07b4c)"><g clip-path="url(#7591293936)"><path fill="#038137" d="M 137.554688 120.539062 L 153.925781 153.960938 C 150.390625 156.34375 149.050781 160.996094 150.972656 164.917969 C 153.0625 169.1875 158.257812 170.96875 162.527344 168.875 C 166.796875 166.785156 168.574219 161.589844 166.484375 157.320312 C 164.5625 153.398438 160.0625 151.605469 156.015625 152.9375 L 139.644531 119.515625 C 139.367188 118.949219 138.652344 118.703125 138.085938 118.980469 C 137.519531 119.257812 137.277344 119.972656 137.554688 120.539062 Z M 164.394531 158.34375 C 165.90625 161.4375 164.617188 165.203125 161.523438 166.71875 C 158.429688 168.234375 154.621094 166.96875 153.105469 163.871094 C 151.589844 160.777344 152.878906 157.015625 155.972656 155.5 C 159.113281 153.960938 162.878906 155.25 164.394531 158.34375 Z M 164.394531 158.34375 " fill-opacity="1" fill-rule="nonzero"/></g></g></g><g clip-path="url(#2d3c272841)"><g clip-path="url(#ec625cf862)"><g clip-path="url(#e4619d031b)"><path fill="#038137" d="M 150.976562 113.964844 L 165.253906 143.117188 C 165.425781 143.464844 165.769531 143.730469 166.183594 143.796875 L 174.726562 144.691406 L 183.519531 162.644531 L 177.417969 171.144531 C 177.152344 171.492188 177.105469 171.945312 177.300781 172.339844 L 181.910156 181.75 C 178.375 184.132812 177.035156 188.785156 178.957031 192.707031 C 181.046875 196.976562 186.242188 198.757812 190.511719 196.664062 C 194.78125 194.574219 196.558594 189.378906 194.46875 185.109375 C 192.546875 181.1875 188.046875 179.390625 184 180.726562 L 179.710938 171.96875 L 185.8125 163.46875 C 186.074219 163.121094 186.125 162.667969 185.929688 162.273438 L 176.539062 143.101562 C 176.367188 142.753906 176.023438 142.488281 175.613281 142.421875 L 167.070312 141.527344 L 153.089844 112.984375 C 152.8125 112.417969 152.097656 112.171875 151.53125 112.449219 C 150.941406 112.683594 150.699219 113.398438 150.976562 113.964844 Z M 192.421875 186.113281 C 193.9375 189.207031 192.644531 192.972656 189.550781 194.488281 C 186.457031 196.003906 182.691406 194.710938 181.179688 191.617188 C 179.664062 188.523438 180.953125 184.757812 184.046875 183.242188 C 187.117188 181.683594 190.882812 182.972656 192.421875 186.113281 Z M 192.421875 186.113281 " fill-opacity="1" fill-rule="nonzero"/></g></g></g><g clip-path="url(#54533ef356)"><g clip-path="url(#9cd436f0d9)"><g clip-path="url(#c831b8a4bd)"><path fill="#038137" d="M 163.308594 107.925781 L 171.546875 124.746094 C 171.738281 125.136719 172.125 125.378906 172.515625 125.402344 L 183.40625 126.230469 L 195.828125 151.59375 C 192.292969 153.972656 190.953125 158.625 192.875 162.546875 C 194.964844 166.820312 200.160156 168.597656 204.429688 166.503906 C 208.699219 164.414062 210.476562 159.21875 208.386719 154.949219 C 206.464844 151.027344 201.964844 149.234375 197.917969 150.566406 L 185.195312 124.597656 C 185.003906 124.203125 184.617188 123.960938 184.226562 123.9375 L 173.339844 123.109375 L 165.398438 106.898438 C 165.121094 106.332031 164.40625 106.089844 163.839844 106.367188 C 163.273438 106.644531 163.03125 107.359375 163.308594 107.925781 Z M 206.292969 155.972656 C 207.808594 159.066406 206.519531 162.832031 203.425781 164.347656 C 200.332031 165.863281 196.566406 164.574219 195.050781 161.480469 C 193.535156 158.386719 194.824219 154.621094 197.917969 153.105469 C 200.992188 151.546875 204.757812 152.835938 206.292969 155.972656 Z M 206.292969 155.972656 " fill-opacity="1" fill-rule="nonzero"/></g></g></g><g clip-path="url(#31cbd0d10b)"><g clip-path="url(#59b29926d3)"><g clip-path="url(#bc0951b29e)"><g clip-path="url(#0674db1a29)"><g clip-path="url(#0428bdbac2)"><g clip-path="url(#a57aa61717)"><g clip-path="url(#bb40fc4217)"><path fill="#c1ff72" d="M 240.0625 226.882812 L 256.15625 219.1875 L 269.984375 248.109375 L 253.890625 255.804688 Z M 240.0625 226.882812 " fill-opacity="1" fill-rule="nonzero"/></g></g></g></g></g></g></g><g clip-path="url(#6f8a303f49)"><g clip-path="url(#fa0b9627fb)"><g clip-path="url(#29782cb6d9)"><g clip-path="url(#191b40f858)"><g clip-path="url(#cdaea2ed47)"><g clip-path="url(#61e0a4e9fa)"><g clip-path="url(#e3d944fbf7)"><path fill="#c1ff72" d="M 232.304688 246.519531 L 262.375 232.140625 L 270.011719 248.113281 L 239.941406 262.488281 Z M 232.304688 246.519531 " fill-opacity="1" fill-rule="nonzero"/></g></g></g></g></g></g></g><g clip-path="url(#1d69a8fa3e)"><g clip-path="url(#849554bb7b)"><g clip-path="url(#38ec6b5fe4)"><g clip-path="url(#bcee0e2652)"><g clip-path="url(#8764c6dfaf)"><g clip-path="url(#64db4f9a83)"><g clip-path="url(#9f44411362)"><path fill="#c1ff72" d="M 236.246094 236.671875 L 252.214844 229.035156 L 259.851562 245.007812 L 243.882812 252.640625 Z M 236.246094 236.671875 " fill-opacity="1" fill-rule="nonzero"/></g></g></g></g></g></g></g></svg>'
                                    )
                                )
                            )
                        ),
                        '", "attributes": [{"trait_type": "Category", "value": "Pass"}, {"trait_type": "Chain", "value": "Kaia"}]}'
                    )
                )
            )
        );
        return string(abi.encodePacked(_baseURI(), json));
    }

    // The following functions are overrides required by Solidity.

    function _update(address to, uint256 tokenId, address auth)
        internal
        override(ERC721, ERC721Enumerable)
        returns (address)
    {
        return super._update(to, tokenId, auth);
    }

    function _increaseBalance(address account, uint128 value)
        internal
        override(ERC721, ERC721Enumerable)
    {
        super._increaseBalance(account, value);
    }

    function supportsInterface(bytes4 interfaceId)
        public
        view
        override(ERC721, ERC721Enumerable)
        returns (bool)
    {
        return super.supportsInterface(interfaceId);
    }

    /// @notice function to get the balance of the contract
    /// @return current balance of the contract
    function getBalance() public view returns (uint256) {
        return address(this).balance;
    }

    /// @notice function to get the counter
    /// @return counter which can be set as tokenId
    function getCounter() public view returns (uint256) {
        return counter;
    }

    /// @notice function to get the fee
    /// @return fee which is current minting fee
    function getFee() public view returns (uint256) {
        return fee;
    }

    /// @notice function to set the mint fee
    /// @notice onlyOwner can set the mint fee
    /// @param newFee as the new fee to mint a token in native token
    function setFee(uint256 newFee) public onlyOwner {
        fee = newFee;
    }

    /// @notice function to withdraw the mint fee
    /// @notice onlyOwner can withdraw the mint fee
    function withdrawFee() public onlyOwner {
        (bool callSuccess, ) = payable(msg.sender).call{value: address(this).balance}("");
        if (callSuccess == false) {
            revert WithdrawalFailed();
        }
    }

}
