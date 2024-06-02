**Module de classification d'images utilisant des réseaux de neurones profonds**

```d
import dnn;
import image;
import math;

class ConvolutionalNeuralNetwork {
    // Paramètres du modèle
    private int hiddenLayers;
    private int hiddenNeurons;
    private float learningRate;

    // Couches du modèle
    private ConvolutionLayer[] convLayers;
    private FullyConnectedLayer[] fcLayers;

    public ConvolutionalNeuralNetwork(int hiddenLayers, int hiddenNeurons, float learningRate) {
        this.hiddenLayers = hiddenLayers;
        this.hiddenNeurons = hiddenNeurons;
        this.learningRate = learningRate;
    }

    public void initialize(InputImage image) {
        // Initialiser les couches convolutives
        convLayers = new ConvolutionLayer[hiddenLayers];
        for (int i = 0; i < hiddenLayers; i++) {
            convLayers[i] = new ConvolutionLayer(image.width, image.height, (i + 1) * 32);
        }

        // Initialiser les couches entièrement connectées
        fcLayers = new FullyConnectedLayer[2];
        fcLayers[0] = new FullyConnectedLayer(convLayers[hiddenLayers - 1].outputShape(), hiddenNeurons);
        fcLayers[1] = new FullyConnectedLayer(hiddenNeurons, 10);
    }

    public void train(Dataset dataset, int epochs) {
        // Itérer sur les époques
        for (int epoch = 0; epoch < epochs; epoch++) {
            // Mélanger les données
            dataset.shuffle();

            // Itérer sur les données
            foreach (Image image in dataset) {
                // Effectuer une passe avant
                float[] output = forwardPass(image);

                // Calculer la perte
                float loss = crossEntropyLoss(output, image.label);

                // Effectuer une passe arrière
                backwardPass(loss, image);

                // Mettre à jour les poids
                updateWeights();
            }
        }
    }

    public float[] forwardPass(Image image) {
        // Convertir l'image en tenseur
        Tensor imageTensor = image.toTensor();

        // Itérer sur les couches convolutives
        foreach (ConvolutionLayer layer in convLayers) {
            imageTensor = layer.forwardPass(imageTensor);
        }

        // Aplatir la sortie des couches convolutives
        float[] flattenedOutput = imageTensor.flatten();

        // Itérer sur les couches entièrement connectées
        foreach (FullyConnectedLayer layer in fcLayers) {
            flattenedOutput = layer.forwardPass(flattenedOutput);
        }

        // Renvoyer la sortie
        return flattenedOutput;
    }

    public void backwardPass(float loss, Image image) {
        // Convertir l'étiquette en tenseur
        Tensor labelTensor = image.label.toTensor();

        // Itérer sur les couches entièrement connectées
        foreach reverse (FullyConnectedLayer layer in fcLayers) {
            labelTensor = layer.backwardPass(labelTensor, loss);
        }

        // Reconstruire le tenseur d'image à partir de la sortie aplatie
        Tensor imageTensor = Tensor.fromArray(flattenedOutput);

        // Itérer sur les couches convolutives
        foreach reverse (ConvolutionLayer layer in convLayers) {
            imageTensor = layer.backwardPass(imageTensor);
        }
    }

    public void updateWeights() {
        foreach (ConvolutionLayer layer in convLayers) {
            layer.updateWeights(learningRate);
        }

        foreach (FullyConnectedLayer layer in fcLayers) {
            layer.updateWeights(learningRate);
        }
    }
}

class ConvolutionLayer {
    private int width;
    private int height;
    private int channels;

    private float[][][] filters;
    private float[][] biases;

    public ConvolutionLayer(int width, int height, int channels) {
        this.width = width;
        this.height = height;
        this.channels = channels;

        filters = new float[channels][3][3];
        biases = new float[channels];
    }

    public Tensor forwardPass(Tensor input) {
        int outputWidth = width - 2;
        int outputHeight = height - 2;

        Tensor output = new Tensor(outputWidth, outputHeight, channels);

        for (int i = 0; i < outputWidth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < channels; k++) {
                    float sum = 0;
                    for (int m = 0; m < 3; m++) {
                        for (int n = 0; n < 3; n++) {
                            sum += input[i + m][j + n][k] * filters[k][m][n];
                        }
                    }
                    output[i][j][k] = sum + biases[k];
                }
            }
        }

        return output;
    }

    public Tensor backwardPass(Tensor input) {
        Tensor output = new Tensor(width, height, channels);

        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                for (int k = 0; k < channels; k++) {
                    float sum = 0;

                    for (int m = 0; m < 3; m++) {
                        for (int n = 0; n < 3; n++) {
                            if (i + m >= 0 && i + m < width && j + n >= 0 && j + n < height) {
                                sum += input[i + m][j + n][k] * filters[k][m][n];
                            }
                        }
                    }

                    output[i][j][k] = sum;
                }
            }
        }

        return output;
    }

    public void updateWeights(float learningRate) {
        for (int i = 0; i < channels; i++) {
            for (int j = 0; j < 3; j++) {
                for (int k = 0; k < 3; k++) {
                    filters[i][j][k] -= learningRate * gradient[i][j][k];
                }
            }

            biases[i] -= learningRate * gradient[i];
        }
    }
}

class FullyConnectedLayer {
    private int inputSize;
    private int outputSize;

    private float[] weights;
    private float[] biases;

    public FullyConnectedLayer(int inputSize, int outputSize) {
        this.inputSize = inputSize;
        this.outputSize = outputSize;

        weights = new float[outputSize][inputSize];
        biases = new float[outputSize];
    }

    public float[] forwardPass(float[] input) {
        float[] output = new float[outputSize];

        for (int i = 0; i < outputSize; i++) {
            float sum = 0;
            for (int j = 0; j < inputSize; j++) {
                sum += input[j] * weights[i][j];
            }
            output[i] = sum + biases[i];
        }

        return output;
    }

    public float[] backwardPass(float[] input, float loss) {
        float[] gradient = new float[inputSize];

        for (int i = 0; i < inputSize; i++) {
            gradient[i] = 0;
            for (int j = 0; j < outputSize; j++) {
                gradient[i] += input[j] * weights[j][i];
            }
        }

        return gradient;
    }

    public void updateWeights(float learningRate) {
        for (int i = 0; i < outputSize; i++) {
            for (int j = 0; j < inputSize; j++) {
                weights[i][j] -= learningRate * gradient[i][j];
            }

            biases[i] -= learningRate * gradient[i];
        }
    }
}

class Dataset {
    private Image[] images;
    private int[] labels;

    public Dataset(Image[] images, int[] labels) {
        this.images = images;
        this.labels = labels;
    }

    public void shuffle() {
        for (int i = 0; i < images.length; i++) {
            int randomIndex = rand(0, images.length - 1);
            Image tempImage = images[i];
            int tempLabel = labels[i];

            images[i] = images[randomIndex];
            labels[i] = labels[randomIndex];

            images[randomIndex] = tempImage;
            labels[randomIndex] = tempLabel;