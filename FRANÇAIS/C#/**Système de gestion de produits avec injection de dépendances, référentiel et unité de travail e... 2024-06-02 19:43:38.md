**Système de gestion de produits en C#**

Ce système gère la création, la mise à jour et la suppression de produits dans une base de données. Il utilise des techniques avancées telles que l'injection de dépendances, le modèle de référentiel et le modèle d'unité de travail.

**Modèle de domaine**

```csharp
public class Produit
{
    public int Id { get; set; }
    public string Nom { get; set; }
    public decimal Prix { get; set; }
}
```

**Interface du référentiel**

```csharp
public interface IProduitRepository
{
    Task<Produit> GetProduit(int id);
    Task<List<Produit>> GetProduits();
    Task<int> AjouterProduit(Produit produit);
    Task<int> ModifierProduit(Produit produit);
    Task<int> SupprimerProduit(int id);
}
```

**Implémentation du référentiel**

```csharp
public class ProduitRepository : IProduitRepository
{
    private readonly DbContext _dbContext;

    public ProduitRepository(DbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<Produit> GetProduit(int id)
    {
        return await _dbContext.Produits.FindAsync(id);
    }

    public async Task<List<Produit>> GetProduits()
    {
        return await _dbContext.Produits.ToListAsync();
    }

    public async Task<int> AjouterProduit(Produit produit)
    {
        await _dbContext.Produits.AddAsync(produit);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> ModifierProduit(Produit produit)
    {
        _dbContext.Produits.Update(produit);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> SupprimerProduit(int id)
    {
        var produit = await _dbContext.Produits.FindAsync(id);
        _dbContext.Produits.Remove(produit);
        return await _dbContext.SaveChangesAsync();
    }
}
```

**Service de gestion des produits**

```csharp
public class ProduitService
{
    private readonly IProduitRepository _produitRepository;

    public ProduitService(IProduitRepository produitRepository)
    {
        _produitRepository = produitRepository;
    }

    public async Task<Produit> ObtenirProduit(int id)
    {
        return await _produitRepository.GetProduit(id);
    }

    public async Task<List<Produit>> ObtenirProduits()
    {
        return await _produitRepository.GetProduits();
    }

    public async Task<int> AjouterProduit(Produit produit)
    {
        return await _produitRepository.AjouterProduit(produit);
    }

    public async Task<int> ModifierProduit(Produit produit)
    {
        return await _produitRepository.ModifierProduit(produit);
    }

    public async Task<int> SupprimerProduit(int id)
    {
        return await _produitRepository.SupprimerProduit(id);
    }
}
```

**Contrôleur API**

```csharp
[ApiController]
[Route("api/[controller]")]
public class ProduitController : ControllerBase
{
    private readonly ProduitService _produitService;

    public ProduitController(ProduitService produitService)
    {
        _produitService = produitService;
    }

    [HttpGet]
    public async Task<IActionResult> Get()
    {
        var produits = await _produitService.ObtenirProduits();
        return Ok(produits);
    }

    [HttpGet("{id}")]
    public async Task<IActionResult> Get(int id)
    {
        var produit = await _produitService.ObtenirProduit(id);
        return Ok(produit);
    }

    [HttpPost]
    public async Task<IActionResult> Post([FromBody] Produit produit)
    {
        var id = await _produitService.AjouterProduit(produit);
        return CreatedAtAction(nameof(Get), new { id = id }, produit);
    }

    [HttpPut("{id}")]
    public async Task<IActionResult> Put(int id, [FromBody] Produit produit)
    {
        await _produitService.ModifierProduit(produit);
        return NoContent();
    }

    [HttpDelete("{id}")]
    public async Task<IActionResult> Delete(int id)
    {
        await _produitService.SupprimerProduit(id);
        return NoContent();
    }
}
```

**Explications**

Ce code utilise les techniques suivantes :

* **Injection de dépendances** : Les services dépendent des interfaces, permettant de les échanger facilement.
* **Modèle de référentiel** : L'interface IProduitRepository définit les opérations CRUD de base sur les produits.
* **Modèle d'unité de travail** : Le DbContext gère les transactions de base de données, garantissant l'intégrité des données.
* **Contrôleur API REST** : Le contrôleur ProduitController expose les actions CRUD via des points de terminaison d'API REST.