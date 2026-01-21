BEGIN;

CREATE TABLE IF NOT EXISTS urls (
  short_url TEXT PRIMARY KEY,
  original_url TEXT NOT NULL,
  creation_date TIMESTAMPTZ NOT NULL DEFAULT now(),
  expiration_date TIMESTAMPTZ NOT NULL,
  click_count INT NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS keys (
  key TEXT PRIMARY KEY,
  used BOOLEAN NOT NULL DEFAULT False,
  random_val FLOAT NOT NULL DEFAULT random()
);

CREATE INDEX idx_unused_keys ON keys(random_val) WHERE NOT used;

COMMIT
