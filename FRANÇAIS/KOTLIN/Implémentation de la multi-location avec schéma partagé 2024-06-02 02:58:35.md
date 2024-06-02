**Implémentation multi-tenant avec un schéma partagé**

**Objectif :** Permettre à plusieurs clients d'utiliser une seule base de données tout en maintenant l'isolement des données.

**Classes d'entités :**

```kotlin
abstract class MultiTenantEntity {
    @Column(name = "tenant_id")
    var tenantId: Long? = null
}

class Client(
    @Id
    var id: Long? = null,
    var name: String? = null,
    @OneToMany(mappedBy = "client")
    var projects: List<Project>? = null
) : MultiTenantEntity()

class Project(
    @Id
    var id: Long? = null,
    var name: String? = null,
    var description: String? = null,
    @ManyToOne
    var client: Client? = null
) : MultiTenantEntity()
```

**Gestion des filtres de locataires :**

```kotlin
@FilterProvider("tenantFilter")
class TenantFilter : FilterProvider {
    override fun createFilters(metadata: EntityMetadata): MutableList<QueryFilter> {
        val queryFilter = SimpleQueryFilter.eq("tenant_id", CurrentTenantIdentifier.getTenantId())
        return mutableListOf(queryFilter)
    }
}
```

**Intercepteur d'audit :**

```kotlin
@Intercepts(ENTITY_MANAGER_PRE_REMOVE, ENTITY_MANAGER_PRE_UPDATE)
class AuditInterceptor : AbstractEntityListener() {
    override fun preRemove(entity: Any, entityManager: EntityManager) {
        if (entity is Auditable) {
            entity.deletedAt = ZonedDateTime.now()
        }
    }

    override fun preUpdate(entity: Any, entityManager: EntityManager) {
        if (entity is Auditable) {
            entity.updatedAt = ZonedDateTime.now()
        }
    }
}
```

**Annotation d'entité auditable :**

```kotlin
@EntityListeners(AuditInterceptor::class)
interface Auditable {
    var createdAt: ZonedDateTime? = null
    var updatedAt: ZonedDateTime? = null
    var deletedAt: ZonedDateTime? = null
}
```

**Implémentation de la recherche élastique :**

```kotlin
// Classe de document Elasticsearch
data class ProjectDocument(
    var id: Long? = null,
    var name: String? = null,
    var description: String? = null,
    var tenantId: Long? = null
)

// Indexeur Elasticsearch
@Service
class ProjectIndexer {
    @Autowired
    lateinit var elasticsearchTemplate: ElasticsearchTemplate

    fun indexProject(project: Project) {
        elasticsearchTemplate.index(
            "projects",
            "project",
            project.id.toString(),
            ProjectDocument(project.id, project.name, project.description, project.tenantId)
        )
    }
}

// Écouteur d'événements Spring Data
@EventListener(ApplicationReadyEvent::class)
class ElasticsearchInitializer {
    @Autowired
    lateinit var projectIndexer: ProjectIndexer

    @Autowired
    lateinit var projectRepository: ProjectRepository

    fun initializeElasticsearch() {
        elasticsearchTemplate.createIndex(ProjectDocument::class.java)
        projectRepository.findAll().forEach { projectIndexer.indexProject(it) }
    }
}
```

**Exceptions personnalisées :**

```kotlin
class TenantNotFoundException(tenantId: Long) : RuntimeException("Tenant with id $tenantId not found.")
class ProjectNotFoundException(projectId: Long) : RuntimeException("Project with id $projectId not found.")
```

**Service de gestion des clients :**

```kotlin
@Service
class ClientService {
    @Autowired
    lateinit var clientRepository: ClientRepository

    fun createClient(client: Client): Client {
        client.tenantId = CurrentTenantIdentifier.getTenantId()
        return clientRepository.save(client)
    }

    fun findClientById(id: Long): Client {
        return clientRepository.findById(id).orElseThrow { TenantNotFoundException(id) }
    }

    fun findAllClients(): List<Client> {
        return clientRepository.findAll()
    }
}
```

**Contrôleur REST pour les clients :**

```kotlin
@RestController
@RequestMapping("/api/clients")
class ClientController {
    @Autowired
    lateinit var clientService: ClientService

    @PostMapping
    fun createClient(@RequestBody client: Client): Client {
        return clientService.createClient(client)
    }

    @GetMapping("/{id}")
    fun findClientById(@PathVariable id: Long): Client {
        return clientService.findClientById(id)
    }

    @GetMapping
    fun findAllClients(): List<Client> {
        return clientService.findAllClients()
    }
}
```

**Explication :**

Ce code implémente un système multi-tenant complexe avec :

* **Gestion des locataires :** Les entités sont annotées avec `MultiTenantEntity` et un filtre JPA est utilisé pour appliquer automatiquement les restrictions en fonction du locataire.
* **Audit des entités :** Les entités qui implémentent `Auditable` sont automatiquement mises à jour avec les horodatages `créé à`, `mis à jour à` et `supprimé à`.
* **Recherche Elasticsearch :** Les projets sont indexés dans Elasticsearch pour effectuer des recherches efficaces.
* **Exceptions personnalisées :** Des exceptions sont définies pour gérer les scénarios d'erreur courants.
* **Services et contrôleurs REST :** Les services et les contrôleurs implémentent la logique métier et fournissent une interface RESTful pour interagir avec le système.