**Gestionnaire de ressources basé sur un pool**

Ce code illustre un gestionnaire de ressources basé sur un pool en TypeScript, qui fournit une interface unifiée pour gérer les ressources limitées de manière asynchrone.

```typescript
import { EventEmitter } from 'events';
import { PromiseResolver } from './promise-resolver';
import { Resource } from './resource';

// Interface pour les ressources gérées par le pool
interface IPoolResource {
  acquire(): Promise<Resource>;
  release(resource: Resource): void;
  close(): Promise<void>;
}

// Implémentation du pool de ressources
class Pool<T extends Resource> extends EventEmitter implements IPoolResource {
  // Nombre maximal de ressources dans le pool
  private readonly maxResources: number;

  // Ressources actuellement utilisées
  private usedResources: number = 0;

  // Ressources disponibles dans le pool
  private availableResources: Resource[] = [];

  // Résolveurs de promesses pour les nouvelles acquisitions de ressources
  private pendingAcquisitions: PromiseResolver<Resource>[] = [];

  // Indique si le pool est fermé
  private isClosed: boolean = false;

  constructor(maxResources: number) {
    super();
    this.maxResources = maxResources;
  }

  // Acquérir une ressource
  acquire(): Promise<Resource> {
    return new Promise((resolve, reject) => {
      if (this.availableResources.length > 0) {
        const resource = this.availableResources.pop()!;
        this.usedResources++;
        resolve(resource);
      } else if (this.usedResources < this.maxResources) {
        this.pendingAcquisitions.push({ resolve, reject });
      } else {
        reject(new Error("Le pool de ressources est plein."));
      }
    });
  }

  // Libérer une ressource
  release(resource: Resource): void {
    if (this.isClosed) {
      throw new Error("Le pool de ressources est fermé.");
    }

    this.availableResources.push(resource);
    this.usedResources--;

    // Résoudre la première promesse d'acquisition en attente
    if (this.pendingAcquisitions.length > 0) {
      const nextAcquisition = this.pendingAcquisitions.shift()!;
      nextAcquisition.resolve(this.availableResources.pop()!);
    }
  }

  // Fermer le pool
  async close(): Promise<void> {
    this.isClosed = true;

    // Rejeter toutes les promesses d'acquisition en attente
    while (this.pendingAcquisitions.length > 0) {
      const acquisition = this.pendingAcquisitions.pop()!;
      acquisition.reject(new Error("Le pool de ressources est fermé."));
    }

    // Fermer toutes les ressources disponibles
    await Promise.all(this.availableResources.map((resource) => resource.close()));
  }
}
```

**Explication du code :**

1. **Interface `IPoolResource` :** Définit les méthodes nécessaires pour les ressources gérées par le pool.
2. **Classe `Pool` :** Implémente l'interface `IPoolResource` et gère un pool de ressources de type `T` (en supposant qu'il s'agisse d'une classe `Resource`).
3. **`maxResources` :** Le nombre maximal de ressources autorisées dans le pool.
4. **`usedResources` :** Le nombre de ressources actuellement utilisées dans le pool.
5. **`availableResources` :** Un tableau contenant les ressources disponibles dans le pool.
6. **`pendingAcquisitions` :** Un tableau contenant les résolveurs de promesses pour les acquisitions de ressources en attente.
7. **`isClosed` :** Un booléen indiquant si le pool est fermé.
8. **Méthode `acquire` :** Essaie d'acquérir une ressource du pool, en attendant si nécessaire.
9. **Méthode `release` :** Libère une ressource dans le pool, rendant-la disponible pour d'autres acquisitions.
10. **Méthode `close` :** Ferme le pool, rejetant toutes les acquisitions en attente et fermant toutes les ressources disponibles.