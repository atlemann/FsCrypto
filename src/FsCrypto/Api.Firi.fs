module FsCrypto.Api.Firi

open FsCrypto
open FsCrypto.Types.Firi
open FsToolkit.ErrorHandling
open System
open System.Net.Http
open Thoth.Json.Net

type FiriSecrets =
    { ClientId: string
      Secret: string }
    static member Create clientId secret =
        { ClientId = clientId
          Secret = secret }


[<RequireQualifiedAccess>]
type Endpoint =
    | ListMarkets
    | ListBalances
    | ListTransactions
    | ListTrades
    | ListDepositHistory
    | ListOrders
    | CreateOrder of OrderRequest
    | DeleteAllOrders
with
    member this.Uri =
        match this with
        | ListMarkets -> "markets"
        | ListBalances -> "balances"
        | ListTransactions -> "history/transactions"
        | ListTrades -> "history/trades"
        | ListDepositHistory -> "deposit/history"
        | ListOrders -> "history/orders"
        | CreateOrder _
        | DeleteAllOrders -> "orders"
    member this.HttpMethod =
        match this with
        | ListMarkets
        | ListBalances
        | ListTransactions _
        | ListTrades
        | ListDepositHistory
        | ListOrders ->
            HttpMethod.Get
        | CreateOrder _ ->
            HttpMethod.Post
        | DeleteAllOrders ->
            HttpMethod.Delete
    member this.Body =
        match this with
        | ListMarkets
        | ListBalances
        | ListTransactions _
        | ListTrades
        | ListDepositHistory
        | ListOrders
        | DeleteAllOrders ->
            None
        | CreateOrder order ->
            Encode.object
                [
                    "market", Encode.string order.Market
                    "type", Encode.string order.Type.AsString
                    "price", Encode.string (string order.Price)
                    "amount", Encode.string (string order.Amount)
                ]
            |> Encode.toString 0
            |> Some

let private getSignature secret =
    let timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds() |> string
    let validity = "2000"
    let body =
        Encode.object
            [
                "timestamp", Encode.string timestamp
                "validity", Encode.string validity
            ]
        |> Encode.toString 0

    let signature = Hashing.computeSignature secret body
    let queryParams = $"?timestamp={timestamp}&validity={validity}"
    (signature, queryParams)

let private createRequestMessage (secrets:FiriSecrets) (endpoint:Endpoint) =

    let (signature, queryParams) = getSignature secrets.Secret
    let requestUri = $"{endpoint.Uri}{queryParams}"

    let requestMessage = new HttpRequestMessage(endpoint.HttpMethod, requestUri)
    requestMessage.Headers.Add("miraiex-user-clientid", secrets.ClientId);
    requestMessage.Headers.Add("miraiex-user-signature", signature);

    endpoint.Body
    |> Option.iter (fun body ->
        let content = new StringContent(body, Text.Encoding.UTF8, "application/json")
        requestMessage.Content <- content)

    requestMessage

let private send (httpClient:HttpClient) secrets endpoint =
    task {
        use requestMessage = createRequestMessage secrets endpoint
        use! response = httpClient.SendAsync(requestMessage)
        let! responseString = response.Content.ReadAsStringAsync()
        return responseString
    }
    |> Async.AwaitTask

type FiriApi(secrets:FiriSecrets) =

    let httpClient = new HttpClient(BaseAddress = Uri "https://api.miraiex.com/v2/")
    let sendRequest = send httpClient secrets

    interface System.IDisposable with
        member this.Dispose() =
            httpClient.Dispose()

    member this.GetMarkets () =
        Endpoint.ListMarkets
        |> sendRequest
        |> Async.map (Decode.fromString (Decode.list Market.Decoder))

    member this.GetBalances () =
        Endpoint.ListBalances
        |> sendRequest
        |> Async.map (Decode.fromString (Decode.list Balance.Decoder))

    member this.GetTransactions () =
        Endpoint.ListTransactions
        |> sendRequest
        |> Async.map (Decode.fromString (Decode.list Transaction.Decoder))

    member this.GetAllTrades () =
        Endpoint.ListTrades
        |> sendRequest
        |> Async.map (Decode.fromString (Decode.list Trade.Decoder))

    member this.GetDepositHistory () =
        Endpoint.ListDepositHistory
        |> sendRequest
        |> Async.map (Decode.fromString DepositHistory.Decoder)

    member this.GetAllOrders () =
        Endpoint.ListOrders
        |> sendRequest
        |> Async.map (Decode.fromString (Decode.list Order.Decoder))

    member this.CreateOrder (order:OrderRequest) =
        order
        |> Endpoint.CreateOrder
        |> sendRequest
        |> Async.map (Decode.fromString CreateOrderResponse.Decoder)

    member this.DeleteAllOrders () =
        Endpoint.DeleteAllOrders
        |> sendRequest
        |> Async.Ignore
