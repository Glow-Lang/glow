// This contract is used for tests only
// SPDX-License-Identifier: Apache2.0
// TODO: move it to a test directory? Under src/legilogic_ethereum ?
pragma solidity ^0.6.4;

contract HelloWorld {
    function hello() public pure returns (string memory) {
        return "Hello, World!";
    }

    function mul42(uint _num) public pure returns (uint) {
        uint product = _num*42;
        require (product / 42 == _num); // check for overflow
        return product;
    }

    event greetingsEvent(string result);

    function greetings(string memory _name) public returns (string memory) {
        string memory s = strConcat("Greetings, ", _name);
        emit greetingsEvent(s);
        return s;
    }

    function strConcat(string memory _a, string memory _b) internal pure returns (string memory) {
        bytes memory _ba = bytes(_a);
        bytes memory _bb = bytes(_b);
        string memory ab = new string(_ba.length + _bb.length);
        bytes memory bab = bytes(ab);
        uint k = 0;
        for (uint i = 0; i < _ba.length; i++) bab[k++] = _ba[i];
        for (uint i = 0; i < _bb.length; i++) bab[k++] = _bb[i];
        return string(bab);
    }
}
