# Requirements

1. The service gets a URL and must generate a shorter and unique alias for it
2. When users access a short link, the service must redirect them to the
   original URL
3. Short links will expire after some time (let’s say one year)
4. (Optional) Short links should be customizable
5. (Optional) The expiration time should be customizable

# System Interface

1. Create Short URL

    - Endpoint: POST /shorten
    - Parameters:
        * (required) `original_url` (string): original long URL
        * (optional) `custom_alias` (string): custom alias for shortened URL
        * (optional) `expiration_date` (timestamp): the date and time when the
          short link should expire; default is one year
    - Response:
        * `shortened_link` (string)
        * `creation_date` (timestamp)
        * `expiration_date` (timestamp)

2. Open Short URL

    - Endpoint: GET /{shortened_url}
    - Parameters:
        * (required) `shortened_url` (string)
    - Response:
        * Redirects to the `original_url`

3. Analytics

    - Endpoint: GET /analytics/{shortened_url}
    - Parameters:
        * (required) `shortened_url` (string)
    - Response:
        * `click_count` (integer)

# Database design

## Main database (PostgreSQL)

1. Info about the URL mappings, fields:

    - `short_url`: string (primary key)
    - `original_url`: string
    - `creation_date`: datetime
    - `expiration_date`: datetime
    - `click_count`: integer

2. Pregenerated keys:

    - `key`: string
    - `used`: boolean

**Note**

The analytics in this project is only `click_count`, therefore it’s just stored
in the first table. For more enhanced analytics we should create special table
storing telemetry.

## Cache database (Redis)

1. Cached links:

    - key: shortened url (prefix `short:`)
    - value: `original_url` (string) and `expiration_date` (datetime)
    
2. Cached unused keys:

    - key: `cashed_key`
    - value: set of strings, those strings can be used as shortened url

# Basic System Design and Algorithms

## First system run

KGS generates table with all keys (`used = False`). 

Function `populateKeys :: P.Connection -> Bool -> IO ()`

## System start

Select 1000 random rows from key-database, with `used == False`.
Set `used = True`.
Populate Redis cache for unused keys.

Function `redisLoadKeys :: MainError ()`

## Create Short URL

When custom short URL is provided, check if it’s not used before.
If it’s used then return error.
Take from Redis a random unused key or use provided short URL.
If there is no unused keys, repeat the System start function.
Add row to the main database.
Add row to the Redis cached links, set time-to-live (7 days) in cache.
(see `setex` function in `hedis`; `maxmemory` and `maxmemory-policy` in `redis.conf`)

Function `createShortURL :: String -> Maybe String -> Maybe UTCTime -> MainError URL`

## Open Short URL

Check Redis cache if it has the short URL.
If yes then return the original URL (and redirect there),
else check if main database has the short URL.
If yes then update Redis cache and return the original URL (and redirect there),
else return 404 error.
Check if the link is not expired.
If expired then delete the row from both main and cache databases, 
set `used = False` in key-database, and return 404 error.
Update `click_count`.

Function `openShortURL :: String -> MainError String`

## Analytics

From main database take the short URL, return its `click_count`.

Function `getAnalytics :: String -> MainError Int`

## Cleanup (runs periodically)

From main database take expired rows.
Delete them from both main and cache databases.
Set `used = False` in key-database (so the short URL can be reused).

Function `cleanup :: MainError ()`
