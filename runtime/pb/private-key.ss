#lang :std/protobuf/proto ; -*- Protobuf -*-

/* TODO: pb should be pulled in a dependency, so it remains in sync
 * Pull it from https://github.com/libp2p/go-libp2p-core/blob/master/crypto/pb/crypto.proto
 */

syntax = "proto2";

enum KeyType {
	RSA = 0;
	Ed25519 = 1;
	Secp256k1 = 2;
	ECDSA = 3;
}

message PublicKey {
	required KeyType Type = 1;
	required bytes Data = 2;
}

message PrivateKey {
	required KeyType Type = 1;
	required bytes Data = 2;
}
