module FsCrypto.Interpreter.Coinbase

open Coinbase
open Coinbase.Models
open FsCrypto.Instructions
open FSharp.Control
open FSharpPlus.Data

let [<Literal>] private ExchangeName = "Coinbase"

let private getPages (client:CoinbaseClient) (initialResponse:PagedResponse<'a>) : AsyncSeq<'a array> =
    initialResponse
    |> AsyncSeq.unfoldAsync (fun x -> async {
        if x.Pagination.NextUri |> isNull then
            return None
        else
            let! nextPage = client.GetNextPageAsync x |> Async.AwaitTask
            return Some (nextPage.Data, nextPage)
    })

let private getAccounts (client:CoinbaseClient) =
    asyncSeq {
        let! response = client.Accounts.ListAccountsAsync() |> Async.AwaitTask
        yield response.Data
        yield! getPages client response
    }
    |> AsyncSeq.concatSeq

let private getBalancesApi (client:CoinbaseClient) =
    client
    |> getAccounts
    |> AsyncSeq.filter (fun a -> a.Balance.Amount > 0.0M)
    |> AsyncSeq.map (fun a -> InstructionBalance.Create ExchangeName a.Balance.Currency a.Balance.Amount)
    |> AsyncSeq.toListAsync

let private getTransactionsForAccount (client:CoinbaseClient) (account:Account) =
    asyncSeq {
        let! response = client.Transactions.ListTransactionsAsync(account.Id) |> Async.AwaitTask
        yield response.Data
        yield! getPages client response
    }
    |> AsyncSeq.concatSeq

let private getTransactionsApi (client:CoinbaseClient) =
    getAccounts client
    |> AsyncSeq.mapAsyncParallel (getTransactionsForAccount client >> AsyncSeq.toListAsync)
    |> AsyncSeq.concatSeq
    |> AsyncSeq.groupBy (fun t -> t.Type)
    |> AsyncSeq.mapAsyncParallel (fun (key, group) ->
        match key with
        | "send" ->
            group
            |> AsyncSeq.map (fun t ->
                TransactionType.Deposit (InstructionAmount.Create t.Amount.Amount t.Amount.Currency)
                |> InstructionTransaction.Create ExchangeName t.UpdatedAt.Value)
            |> AsyncSeq.toListAsync

        | "trade" ->
            group
            |> AsyncSeq.groupBy (fun t -> t.ExtraJson.["trade"].Value<string>("id"))
            |> AsyncSeq.mapAsyncParallel (fun (_, xs) -> async {
                let! (timeStamp, bought) =
                    xs
                    |> AsyncSeq.pick (fun t ->
                        if t.Amount.Amount > 0.0m then
                            Some (t.CreatedAt.Value, InstructionAmount.Create t.Amount.Amount t.Amount.Currency)
                        else
                            None)

                let! sold =
                    xs
                    |> AsyncSeq.pick (fun t ->
                        if t.Amount.Amount < 0.0m then
                            Some (InstructionAmount.Create t.Amount.Amount t.Amount.Currency)
                        else
                            None)

                let fee = InstructionAmount.Create 0.0m ""

                return
                    TransactionType.Transaction (bought, sold, fee)
                    |> InstructionTransaction.Create ExchangeName timeStamp
            })
            |> AsyncSeq.toListAsync

        | "buy" ->
            group
            |> AsyncSeq.map (fun t ->
                TransactionType.Transaction
                    (
                        (InstructionAmount.Create t.Amount.Amount t.Amount.Currency),
                        (InstructionAmount.Create t.NativeAmount.Amount t.NativeAmount.Currency),
                        (InstructionAmount.Create 0.0m "")
                    )
                |> InstructionTransaction.Create ExchangeName t.UpdatedAt.Value)
            |> AsyncSeq.toListAsync

        | _ ->
            // TODO: Handle other types
            async.Return [])
    |> AsyncSeq.concatSeq
    |> AsyncSeq.toListAsync

let getMarketsApi (client:CoinbaseClient) =
    async {
        let! response = client.Data.GetExchangeRatesAsync("NOK") |> Async.AwaitTask
        return
            response.Data.Rates
            |> Seq.map (fun kvp ->
                let rate = 1.0m / kvp.Value
                InstructionMarket.Create kvp.Key rate rate rate)
            |> Seq.toList
    }

let interpret coinbaseClient instruction =
    match instruction with
    | GetBalances (_, next) ->
        async {
            let! balances = getBalancesApi coinbaseClient
            return balances |> next
        }
    | GetTransactions (_, next) ->
        async {
            let! transactions = getTransactionsApi coinbaseClient
            return transactions |> next
        }
    | GetMarkets (_, next) ->
        async {
            let! markets = getMarketsApi coinbaseClient
            return markets |> next
        }

let run coinbaseClient (instructions:Free<CryptoInstruction<'a>, 'a>)=
    instructions
    |> Free.fold (interpret coinbaseClient)