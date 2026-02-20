# URL Shortener

A URL shortening service built with Haskell, PostgreSQL, and Redis, using Nix for reproducible development environments.

## Features

- **URL Shortening**: Generate short, unique aliases for long URLs
- **Custom Aliases**: Option to specify custom short URLs
- **Configurable Expiration**: Set custom expiration dates (default: 1 year)
- **Analytics**: Track click counts for shortened URLs
- **Dual Interface**: RESTful API and command-line interface
- **Caching**: Redis-based caching for improved performance
- **Key Generation System**: Pre-generated unique keys for fast URL creation

## Tech Stack

- **Language**: Haskell (GHC)
- **Web Framework**: Scotty
- **Database**: PostgreSQL (postgresql-simple)
- **Cache**: Redis
- **Build System**: Nix + Cabal
- **Testing**: HSpec + QuickCheck
- **CLI**: optparse-applicative

## Prerequisites

- **Nix Package Manager** (required)
- **PostgreSQL** (provided by Nix shell)
- **Redis** (provided by Nix shell)

## Quick Start

### 1. Clone and Enter Development Shell
```bash
git clone https://github.com/SVDerevyagin/url-shortener
cd url-shortener
nix develop  # or nix-shell for older Nix versions
```

### 2. Start Database Services
Start PostgreSQL and Redis
```bash
nix run .#startPostgres
nix run .#startRedis
```

### 3. Initialize Database
Create and populate databases:
```bash
nix run .#createDB
```

### 4. Build and Run the Web Server
```bash
nix run .#default  # Starts the Scotty web server
```

The server’s port is 2020. Visit `http://localhost:2020` to interact with the API.

### 5. Using the CLI (Alternative)
```bash
nix run .#cli -- --help
```

## Development

### Nix Development Shell
The Nix development shell provides all necessary dependencies:
```bash
nix develop
# Provides: ghc, haskell-language-server, cabal, postgresql, redis, ghcid
```

### Building with Cabal
```bash
hpack .                  # Generate .cabal file from package.yaml
cabal build              # Build the project
cabal run url-shortener  # Run web server
cabal run urlsh          # Run CLI tool
```

### Running Tests
```bash
nix run .#test
# or
hpack . ; cabal test
```

### Interactive Development with GHCi
```bash
cabal repl lib:url-shortener        # For library
cabal repl exe:url-shortener        # For web server
cabal repl exe:urlsh                # For CLI
cabal repl test:url-shortener-test  # For tests
```

## API Documentation

### 1. Create Short URL
```bash
POST /shorten
Content-Type: application/x-www-form-urlencoded

url=https://example.com/very/long/url&alias=myalias&edate=2025-12-31
```

**Form Parameters:**
- `url` (required): The original long URL to shorten
- `alias` (optional): Custom alias for the shortened URL
- `edate` (optional): Expiration date in `YYYY-MM-DD` format (default: 1 year from creation)

**Successful Response (200 OK):**
```json
{
  "short_link": "http://localhost:3000/myalias",
  "creation_date": "2024-01-15",
  "expiration_date": "2025-01-15"
}
```

**Error Response (200 OK with error object):**
```json
{
  "error": "provided custom short url is already in use"
}
```

**Example with curl:**
```bash
# Basic shortening
curl -X POST http://localhost:3000/shorten \
  -d "url=https://example.com/very/long/url"

# With custom alias
curl -X POST http://localhost:3000/shorten \
  -d "url=https://example.com/very/long/url" \
  -d "alias=myalias"

# With custom expiration date
curl -X POST http://localhost:3000/shorten \
  -d "url=https://example.com/very/long/url" \
  -d "alias=myalias" \
  -d "edate=2027-12-31"
```

### 2. Open Short URL
```bash
GET /{shortened_url}
```
Redirects to the original URL with HTTP 302.

**Example:**
```bash
curl -I http://localhost:3000/myalias
# Response: HTTP/1.1 302 Found
# Location: https://example.com/very/long/url
```

### 3. Get Analytics
```bash
GET /analytics/{shortened_url}

Response:
{
  "click_count": 42
}
```

**Successful Response (200 OK):**
```json
{
  "click_count": 42
}
```

**Error Response:**
```json
{
  "error": "404 Error: this link is not in the system"
}
```

## CLI Usage

The project includes a command-line interface for creating and managing URLs:
```bash
# Create a short URL
urlsh shorten https://example.com/very/long/url

# Create with custom alias
urlsh shorten https://example.com --alias mypage

# Create with custom expiration date
urlsh shorten https://example.com --expires 2025-12-31

# Get analytics
urlsh analytics myalias

# Show help
urlsh --help
urlsh shorten --help
urlsh analytics --help
```

## Environment Variables

The following environment variables are set in the Nix shell:

```bash
DB_MAIN_USER="urlsh"        # PostgreSQL main database user
DB_TEST_USER="urlsh_test"   # PostgreSQL test database user
R_PORT=2626                 # Redis port
```

## Database Schema

### PostgreSQL (Main Database)
```sql
-- URL mappings table
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
```

### Redis (Cache)
- Keys format: `short:{short_url}` (stores original URL and expiration)
- Unused keys set: `cashed_key` (set of available short URLs)

## Key Generation System

1. **Initial Population**: On first run, generates all possible keys with `used = False`
2. **Cache Loading**: On startup, loads 1000 random unused keys into Redis
3. **Key Reuse**: Expired URLs release their keys back to the pool

## Running Production Build

```bash
nix build .#default
./result/bin/url-shortener
```

## Troubleshooting

### PostgreSQL Connection Issues
```bash
# Check if PostgreSQL is running
psql -h ./data -l

# Reset databases (warning: deletes all data)
rm -rf data/db
nix run .#startPostgres
nix run .#createDB
```

### Redis Connection Issues
```bash
# Check Redis configuration
cat redis.conf

# Check Redis logs
tail -f data/redis.log
```

### Build Issues
```bash
# Clean and rebuild
rm -f url-shortener.cabal
hpack .
cabal clean
cabal build
```

## Testing

Run the complete test suite:
```bash
cabal test --test-show-details=direct
```

Run specific test modules:
```bash
cabal test url-shortener-test --test-option="-m DB"
```
