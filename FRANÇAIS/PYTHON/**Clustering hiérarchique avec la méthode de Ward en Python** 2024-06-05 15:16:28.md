```python
# **Implémenter un algorithme de clustering hiérarchique en Python**

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import dendrogram, linkage

# **Chargement et préparation des données**

# Chargement du dataset
data = pd.read_csv("donnees.csv")
# Conversion des données en matrice numérique
data_mat = data.to_numpy()

# **Calcul du dendrogramme**

# Calcul de la matrice de distances
distance_matrix = linkage(data_mat, method='ward')  # Méthode Ward pour le clustering hiérarchique

# **Affichage du dendrogramme**

# Création du dendrogramme
plt.figure(figsize=(15, 10))
plt.title("Dendrogramme")
plt.xlabel("Indice des observations")
plt.ylabel("Distance")

dendrogram(distance_matrix, labels=data.index, orientation='top')

# **Détermination du nombre optimal de clusters**

# Calcul du coefficient de cophénique
coph_coeff = cophenet(distance_matrix, linkage_matrix)
print("Coefficient de cophénique :", coph_coeff)

# Méthode du coude
distances = distance_matrix[:, 2]
plt.figure(figsize=(10, 6))
plt.plot(np.arange(len(distances)), distances, 'bx-')
plt.xlabel("Nombre de clusters")
plt.ylabel("Distance")
plt.title("Méthode du coude")

# **Attribution des clusters aux observations**

# Choix du nombre de clusters (par exemple, 3)
num_clusters = 3

# Coupe du dendrogramme au niveau choisi
cluster_ids = fcluster(distance_matrix, num_clusters, criterion='distance')

# Ajout des labels de clusters au dataframe
data['clusters'] = cluster_ids

# **Analyse des clusters**

# Affichage des données des clusters
print("Données des clusters :")
print(data.groupby('clusters').mean())

# Représentation graphique des clusters
plt.figure(figsize=(10, 6))
plt.scatter(data_mat[:, 0], data_mat[:, 1], c=cluster_ids)
plt.xlabel("Attribut 1")
plt.ylabel("Attribut 2")
plt.title("Représentation graphique des clusters")

# **Ce code implémente l'algorithme de clustering hiérarchique avec la méthode de Ward, et permet de déterminer le nombre optimal de clusters, d'attribuer les clusters aux observations et d'analyser les résultats.**
```