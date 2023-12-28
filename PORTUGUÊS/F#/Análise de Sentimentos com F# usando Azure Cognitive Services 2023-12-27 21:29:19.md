Aqui está um código em F# que realiza uma análise de sentimentos em um texto usando a biblioteca FSharp.Data e a API de análise de sentimentos do Azure Cognitive Services:

```fsharp
open FSharp.Data
open System.Net
open System.IO

type SentimentResponse = {
    documents : DocumentResponse list
}

type DocumentResponse = {
    id : string
    sentiment : string
}

let apiKey = "SUA_CHAVE_DE_API_DO_AZURE"

let analyzeSentiment (text: string) =
    let endpoint = "https://<NOME_DO_SEU_SERVICO>.cognitiveservices.azure.com/text/analytics/v3.0/sentiment"

    let request = 
        {| documents = [| {| id = "1"; text = text |} |] |}

    let json = JsonValue.Serialize request

    let webRequest = WebRequest.Create(endpoint) :?> HttpWebRequest
    webRequest.Headers.Add("Ocp-Apim-Subscription-Key", apiKey)
    webRequest.Method <- "POST"
    webRequest.ContentType <- "application/json; charset=utf-8"

    use streamWriter = new StreamWriter(webRequest.GetRequestStream())
    streamWriter.Write(json)
    streamWriter.Flush()

    use response = webRequest.GetResponse() :?> HttpWebResponse
    use streamReader = new StreamReader(response.GetResponseStream())
    let responseJson = streamReader.ReadToEnd()

    let sentimentResponse = JsonValue.Deserialize<SentimentResponse>(responseJson)

    sentimentResponse.documents.[0].sentiment

[<EntryPoint>]
let main argv =
    printfn "Digite um texto para analisar o sentimento:"
    let text = System.Console.ReadLine()

    let sentiment = analyzeSentiment text

    printfn "Sentimento do texto: %s" sentiment

    0

```

Este código utiliza uma função `analyzeSentiment` que recebe um texto como entrada e retorna o sentimento do texto usando a API de análise de sentimentos do Azure Cognitive Services.

Para utilizá-lo, você precisará substituir `SUA_CHAVE_DE_API_DO_AZURE` pela sua chave de API do Azure e `<NOME_DO_SEU_SERVICO>` pelo nome do seu serviço no Azure.

O código solicitará que você digite um texto para analisar o sentimento e, em seguida, mostrará o resultado na saída.