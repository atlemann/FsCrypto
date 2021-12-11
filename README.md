# FsCrypto

Tool for browsing your crypto assets.

Was created for blog post for [F# Advent calendar 2021](https://sergeytihon.com/2021/10/18/f-advent-calendar-2021/)

To access your FIRI and Coinbase accounts, set the following environment variables or pass them in as CLI arguments in the specified order.

| Environment variable | Cli arg index |
|----------------------|:-------------:|
| FIRI_CLIENT_ID       |       0       |
| FIRI_SECRET          |       1       |
| COINBASE_API_KEY     |       2       |
| COINBASE_API_SECRET  |       3       |

To run:

```bash
$ dotnet run --project src/FsCrypto.Cli/ -- <FIRI_CLIENT_ID> <FIRI_SECRET> <COINBASE_API_KEY> <COINBASE_API_SECRET>
```
