-- Contacts schema.

-- Drop tables in reverse order.
DROP TABLE IF EXISTS identity;
DROP TABLE IF EXISTS network;
DROP TABLE IF EXISTS contact;
DROP TABLE IF EXISTS txnlog;
DROP TABLE IF EXISTS editlog;

-- Missing tags.
CREATE TABLE contact (
  cid INTEGER PRIMARY KEY,
  name TEXT
);
CREATE UNIQUE INDEX contact_name ON contact(name);

CREATE TABLE network (
  name TEXT PRIMARY KEY NOT NULL,
  description TEXT,
  uri TEXT,
  native_token TEXT NOT NULL,
  exchange_token TEXT
);

INSERT INTO network (name, description, uri, native_token, exchange_token) VALUES
  ('ced', 'Cardano EVM Devnet', 'https://rpc-evm.portal.dev.cardano.org/', 'CED', 'ADA'),
  ('etc', 'Ethereum Classic Mainnet', 'https://ethereumclassic.network', 'ETC', NULL),
  ('eth', 'Ethereum Mainnet', 'https://mainnet.infura.io/v3/${INFURA_API_KEY}', 'ETH', NULL),
  ('kot', 'Ethereum Classic Testnet Kotti', 'https://www.ethercluster.com/kotti', 'KOT', 'ETC'),
  ('kov', 'Ethereum Testnet Kovan', 'https://kovan.poa.network', 'KOV', 'ETC'),
  ('ogor', 'Optimistic Ethereum Testnet Goerli', 'https://www.ethercluster.com/goerli', 'GOR', 'ETC'),
  ('pet', 'Private Ethereum Testnet', 'http://localhost:8545', 'PET', 'ETH'),
  ('rin', 'Ethereum Testnet Rinkeby', 'https://rinkeby.infura.io/v3/${INFURA_API_KEY}', 'ETH', NULL),
  ('rop', 'Ethereum Testnet Ropsten', 'https://ropsten.infura.io/v3/${INFURA_API_KEY}', 'ETH', NULL);

CREATE TABLE identity (
  cid INTEGER REFERENCES contact ON DELETE CASCADE,
  network TEXT NOT NULL REFERENCES network,
  address TEXT NOT NULL,
  nickname TEXT,
  public_key TEXT,
  secret_key_path TEXT,
  CHECK (length(address) > 0),
  UNIQUE (cid, network, address)
);
CREATE UNIQUE INDEX identity_nickname ON identity(nickname);

CREATE TABLE txnlog (
  txid INTEGER PRIMARY KEY,
  timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  command TEXT NOT NULL,
  output TEXT NOT NULL DEFAULT '',
  status INTEGER
);

CREATE TABLE editlog (
  edid INTEGER PRIMARY KEY,
  timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  command TEXT NOT NULL,
  arguments TEXT NOT NULL DEFAULT '[]'
);

-- Missing assets/resources.
