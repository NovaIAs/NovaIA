**Assistant personnel intelligent**

```javascript
// Initialisation des modules
const nlp = require('natural');
const dialogflow = require('@google-cloud/dialogflow');
const speech = require('@google-cloud/speech');
const textToSpeech = require('@google-cloud/text-to-speech');

// Initialisation du client Dialogflow
const sessionClient = new dialogflow.SessionsClient();

// Initialisation du client Speech-to-Text
const speechClient = new speech.SpeechClient();

// Initialisation du client Text-to-Speech
const textToSpeechClient = new textToSpeech.TextToSpeechClient();

// Gestion des commandes vocales
const manageVoiceCommands = async (request) => {
    // Reconnaissance vocale
    const [transcript] = await speechClient.recognize(request);
    const queryText = transcript.results
        .map((result) => result.alternatives[0].transcript)
        .join(' ');

    // Traitement NLP
    const tokenizedQuery = nlp.tokenize(queryText);
    const keywords = nlp.PorterStemmer.stem(tokenizedQuery);

    // Envoi de la requête à Dialogflow
    const sessionPath = sessionClient.projectAgentSessionPath(
        process.env.GOOGLE_CLOUD_PROJECT,
        process.env.DIALOGFLOW_SESSION
    );
    const requestData = {
        session: sessionPath,
        queryInput: {
            text: {
                text: queryText,
                languageCode: 'fr',
            },
        },
    };
    const response = await sessionClient.detectIntent(requestData);

    // Synthèse vocale
    const textResponse = response[0].queryResult.fulfillmentText;
    const requestDataTts = {
        input: { text: textResponse },
        voice: { languageCode: 'fr', ssmlGender: 'FEMALE' },
        audioConfig: { audioEncoding: 'MP3' },
    };
    const [responseTts] = await textToSpeechClient.synthesizeSpeech(requestDataTts);

    // Renvoi de la réponse audio
    return responseTts.audioContent;
};

// Gestion des commandes textuelles
const manageTextCommands = (queryText) => {
    // Traitement NLP
    const tokenizedQuery = nlp.tokenize(queryText);
    const keywords = nlp.PorterStemmer.stem(tokenizedQuery);

    // Envoi de la requête à Dialogflow
    const sessionPath = sessionClient.projectAgentSessionPath(
        process.env.GOOGLE_CLOUD_PROJECT,
        process.env.DIALOGFLOW_SESSION
    );
    const requestData = {
        session: sessionPath,
        queryInput: {
            text: {
                text: queryText,
                languageCode: 'fr',
            },
        },
    };
    const response = sessionClient.detectIntent(requestData);

    // Renvoi de la réponse textuelle
    return response[0].queryResult.fulfillmentText;
};

// Lancement de l'assistant
const startAssistant = () => {
    // Initialisation du serveur Websocket
    const WebSocket = require('ws');
    const wss = new WebSocket.Server({ port: 8080 });

    // Gestion des connexions client
    wss.on('connection', (ws) => {
        ws.on('message', (message) => {
            const request = JSON.parse(message);

            if (request.type === 'voice') {
                manageVoiceCommands(request).then((response) => ws.send(response));
            } else if (request.type === 'text') {
                ws.send(manageTextCommands(request.query));
            }
        });
    });
};

// Lancement de l'assistant
startAssistant();
```

**Explication du code**

Ce code implémente un assistant personnel intelligent complet qui peut gérer les commandes vocales et textuelles. Il utilise les API Dialogflow, Speech-to-Text et Text-to-Speech de Google Cloud Platform.

**Voici une brève explication du fonctionnement du code:**

1. **Gestion des commandes vocales:**
   - Le code utilise l'API Speech-to-Text pour convertir la parole en texte.
   - Il utilise ensuite l'API Dialogflow pour traiter la requête textuelle et générer une réponse.
   - Enfin, il utilise l'API Text-to-Speech pour synthétiser la réponse vocale.

2. **Gestion des commandes textuelles:**
   - Le code utilise directement l'API Dialogflow pour traiter les requêtes textuelles.
   - Il renvoie la réponse textuelle générée par Dialogflow.

3. **Lancement de l'assistant:**
   - Le code lance un serveur Websocket qui attend les connexions des clients.
   - Lorsqu'un client se connecte, le serveur gère les commandes vocales ou textuelles envoyées par le client et renvoie les réponses appropriées.

Ce code fournit un exemple complexe et sophistiqué d'un assistant personnel intelligent utilisant diverses technologies d'intelligence artificielle et de traitement du langage naturel.