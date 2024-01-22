**Código en C++:**

```cpp
// Este código implementa un algoritmo de aprendizaje automático que puede identificar objetos en imágenes.

// Primero, incluimos las bibliotecas necesarias.
#include <iostream>
#include <vector>
#include <opencv2/opencv.hpp>

// Luego, definimos las clases y funciones que utilizaremos.

// La clase `Dataset` representa un conjunto de imágenes y sus etiquetas.
class Dataset {
public:
  // Constructor.
  Dataset(const std::string& path_to_images, const std::string& path_to_labels) {
    // Leemos las imágenes y las etiquetas de los archivos especificados.
    std::vector<cv::Mat> images = read_images(path_to_images);
    std::vector<int> labels = read_labels(path_to_labels);

    // Asignamos las imágenes y las etiquetas a las variables miembro de la clase.
    this->images = images;
    this->labels = labels;
  }

  // Devuelve las imágenes del conjunto de datos.
  const std::vector<cv::Mat>& get_images() const {
    return this->images;
  }

  // Devuelve las etiquetas del conjunto de datos.
  const std::vector<int>& get_labels() const {
    return this->labels;
  }

private:
  // Lee las imágenes de un directorio.
  static std::vector<cv::Mat> read_images(const std::string& path_to_images) {
    std::vector<cv::Mat> images;

    // Abrimos el directorio.
    DIR *dir = opendir(path_to_images.c_str());

    // Recorremos los archivos del directorio.
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
      // Ignoramos los archivos ocultos.
      if (entry->d_name[0] == '.') {
        continue;
      }

      // Obtenemos la ruta completa a la imagen.
      std::string path_to_image = path_to_images + "/" + entry->d_name;

      // Leemos la imagen.
      cv::Mat image = cv::imread(path_to_image);

      // Añadimos la imagen al vector.
      images.push_back(image);
    }

    // Cerramos el directorio.
    closedir(dir);

    return images;
  }

  // Lee las etiquetas de un archivo.
  static std::vector<int> read_labels(const std::string& path_to_labels) {
    std::vector<int> labels;

    // Abrimos el archivo.
    std::ifstream file(path_to_labels);

    // Leemos las etiquetas del archivo.
    int label;
    while (file >> label) {
      labels.push_back(label);
    }

    // Cerramos el archivo.
    file.close();

    return labels;
  }

  // Vector de imágenes.
  std::vector<cv::Mat> images;

  // Vector de etiquetas.
  std::vector<int> labels;
};

// La clase `Classifier` representa un clasificador de objetos.
class Classifier {
public:
  // Constructor.
  Classifier() {
    // Inicializamos el clasificador.
    this->init();
  }

  // Entrena el clasificador con un conjunto de datos.
  void train(const Dataset& dataset) {
    // Extraemos las características de las imágenes del conjunto de datos.
    std::vector<cv::Mat> features = extract_features(dataset.get_images());

    // Entrenamos el clasificador con las características y las etiquetas del conjunto de datos.
    this->train(features, dataset.get_labels());
  }

  // Clasifica una imagen.
  int classify(const cv::Mat& image) {
    // Extraemos las características de la imagen.
    cv::Mat features = extract_features(image);

    // Clasificamos la imagen utilizando el clasificador.
    return this->classify(features);
  }

private:
  // Inicializa el clasificador.