Claro! Aqui está um código em Dart que realiza uma análise de sentimento em um texto utilizando a API do Azure Cognitive Services:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() {
  String texto = "Eu amo programar em Dart!"; // Texto a ser analisado

  analisarSentimento(texto);
}

void analisarSentimento(String texto) async {
  String apiKey = "SUA_API_KEY"; // Insira sua chave da API do Azure Cognitive Services aqui
  
  String endpoint = "https://southeastasia.api.cognitive.microsoft.com/text/analytics/v3.0/sentiment";

  Map<String, dynamic> requestBody = {
    "documents": [
      {
        "language": "pt",
        "id": "1",
        "text": texto,
      }
    ]
  };

  Map<String, String> headers = {
    "Content-Type": "application/json",
    "Ocp-Apim-Subscription-Key": apiKey
  };

  var response = await http.post(Uri.parse(endpoint),
      headers: headers, body: jsonEncode(requestBody));

  if (response.statusCode == 200) {
    var data = jsonDecode(response.body);

    double score = data['documents'][0]['sentiment']; // Pontuação de sentimento do texto

    if (score < 0.4) {
      print("O texto possui um sentimento negativo.");
    } else if (score >= 0.4 && score < 0.7) {
      print("O texto possui um sentimento neutro.");
    } else {
      print("O texto possui um sentimento positivo.");
    }
  } else {
    print("Ocorreu um erro na análise de sentimento.");
  }
}
```

Explicação do código:

1. Importamos as bibliotecas necessárias: `dart:convert` para lidar com a codificação e decodificação JSON e `http` para fazer a chamada HTTP para a API.

2. No método `main`, definimos o texto a ser analisado.

3. O método `analisarSentimento` é responsável por enviar o texto para a API do Azure Cognitive Services e realizar a análise de sentimento.

4. Definimos a `apiKey` com sua chave da API do Azure Cognitive Services e o `endpoint` para a URL da API de análise de sentimento.

5. Criamos um mapa `requestBody` com o texto a ser analisado, definindo o idioma como "pt" (português) e um ID para o documento.

6. Definimos os headers necessários para a chamada HTTP, incluindo o cabeçalho "Ocp-Apim-Subscription-Key" com a sua chave da API.

7. Fazemos a chamada HTTP utilizando `http.post` passando o endpoint, headers e o `requestBody` codificado em JSON.

8. Verificamos se a resposta tem um status code 200 (sucesso). Caso positivo, decodificamos o corpo da resposta em JSON.

9. Extraímos a pontuação de sentimento do texto a partir do JSON e a comparamos para determinar se o sentimento é negativo, neutro ou positivo.

10. Imprimimos o resultado da análise de sentimento.

Espero que este código atenda às suas expectativas!