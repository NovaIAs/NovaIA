```visual basic
' ** Projet : Calculatrice statistique avancée **

' ** Module : Fonctions mathématiques **

' Calcul de la moyenne d'une liste de nombres
Fonction Moyenne(nombres() As Double) As Double
    Dim somme As Double
    For i As Integer = LBound(nombres) To UBound(nombres)
        somme = somme + nombres(i)
    Next
    Moyenne = somme / (UBound(nombres) - LBound(nombres) + 1)
End Function

' Calcul de l'écart-type d'une liste de nombres
Fonction EcartType(nombres() As Double) As Double
    Dim moyenne As Double
    Dim variance As Double
    moyenne = Moyenne(nombres)
    For i As Integer = LBound(nombres) To UBound(nombres)
        variance = variance + (nombres(i) - moyenne) ^ 2
    Next
    variance = variance / (UBound(nombres) - LBound(nombres))
    EcartType = Sqr(variance)
End Function

' Calcul de la corrélation entre deux listes de nombres
Fonction Correlation(x() As Double, y() As Double) As Double
    Dim moyenneX As Double
    Dim moyenneY As Double
    Dim sommeXY As Double
    Dim sommeX2 As Double
    Dim sommeY2 As Double
    moyenneX = Moyenne(x)
    moyenneY = Moyenne(y)
    For i As Integer = LBound(x) To UBound(x)
        sommeXY = sommeXY + (x(i) - moyenneX) * (y(i) - moyenneY)
        sommeX2 = sommeX2 + (x(i) - moyenneX) ^ 2
        sommeY2 = sommeY2 + (y(i) - moyenneY) ^ 2
    Next
    Correlation = sommeXY / Sqr(sommeX2 * sommeY2)
End Function

' ** Module : Régression linéaire **

' Calcul des coefficients de régression linéaire simple
Fonction RegressionLineaireSimple(x() As Double, y() As Double) As Variant
    Dim moyenneX As Double
    Dim moyenneY As Double
    Dim beta0 As Double
    Dim beta1 As Double
    Dim r2 As Double
    moyenneX = Moyenne(x)
    moyenneY = Moyenne(y)
    beta1 = Correlation(x, y) * EcartType(y) / EcartType(x)
    beta0 = moyenneY - beta1 * moyenneX
    r2 = Correlation(x, y) ^ 2
    RegressionLineaireSimple = Array(beta0, beta1, r2)
End Function

' ** Module : Tests statistiques **

' Test t pour une moyenne
Fonction TestT(echantillon() As Double, moyenneNulle As Double, type As Integer) As Double
    Dim moyenne As Double
    Dim ecartType As Double
    Dim nbObservations As Integer
    moyenne = Moyenne(echantillon)
    ecartType = EcartType(echantillon)
    nbObservations = UBound(echantillon) - LBound(echantillon) + 1
    Select Case type
        Case 1 ' Test à deux queues
            TestT = Abs((moyenne - moyenneNulle) / (ecartType / Sqr(nbObservations)))
        Case 2 ' Test de l'hypothèse alternative : moyenne > moyenneNulle
            TestT = (moyenne - moyenneNulle) / (ecartType / Sqr(nbObservations))
        Case 3 ' Test de l'hypothèse alternative : moyenne < moyenneNulle
            TestT = (moyenne - moyenneNulle) / (ecartType / Sqr(nbObservations))
    End Select
End Function

' Test ANOVA pour comparer des moyennes de plusieurs groupes
Fonction ANOVA(groupes() As Variant) As Variant
    Dim sommeTotale As Double
    Dim sommeInterGroupes As Double
    Dim sommeIntraGroupes As Double
    Dim ddlt As Double
    Dim ddg As Double
    Dim F As Double
    sommeTotale = 0
    sommeInterGroupes = 0
    sommeIntraGroupes = 0
    For i As Integer = LBound(groupes) To UBound(groupes)
        sommeTotale = sommeTotale + Sommaire(groupes(i))
    Next
    Dim moyennes() As Double
    ReDim moyennes(LBound(groupes) To UBound(groupes))
    For i As Integer = LBound(groupes) To UBound(groupes)
        moyennes(i) = Moyenne(groupes(i))
    Next
    For i As Integer = LBound(groupes) To UBound(groupes)
        sommeInterGroupes = sommeInterGroupes + ((moyennes(i) - Moyenne(moyennes)) ^ 2) * (UBound(groupes(i)) - LBound(groupes(i)) + 1)
    Next
    For i As Integer = LBound(groupes) To UBound(groupes)
        For j As Integer = LBound(groupes(i)) To UBound(groupes(i))
            sommeIntraGroupes = sommeIntraGroupes + (groupes(i)(j) - moyennes(i)) ^ 2
        Next
    Next
    ddlt = UBound(moyennes) - LBound(moyennes)
    ddg = sommeTotale - sommeInterGroupes - sommeIntraGroupes
    F = sommeInterGroupes / sommeIntraGroupes
    ANOVA = Array(sommeTotale, sommeInterGroupes, sommeIntraGroupes, ddlt, ddg, F)
End Function

' ** Module : Graphiques **

' Créer un graphe à barres
Sub CreerGrapheBarres(x() As Variant, y() As Variant, titre As String, xLabel As String, yLabel As String)
    Dim graphique As Object
    Set graphique = CreateObject("Graph.Chart.8")
    graphique.ChartType = xlBarClustered
    graphique.SetSourceData Source:=Range(x, y), PlotBy:=xlRows
    graphique.ChartTitle.Text = titre
    graphique.Axes(xlCategory).Title.Text = xLabel
    graphique.Axes(xlValue).Title.Text = yLabel
    graphique.Show
End Sub

' Créer un graphe en nuage de points
Sub CreerGrapheNuagePoints(x() As Variant, y() As Variant, titre As String, xLabel As String, yLabel As String)
    Dim graphique As Object
    Set graphique = CreateObject("Graph.Chart.8")
    graphique.ChartType = xlXYScatterLines
    graphique.SetSourceData Source:=Range(x, y), PlotBy:=xlRows
    graphique.ChartTitle.Text = titre
    graphique.Axes(xlCategory).Title.Text = xLabel
    graphique.Axes(xlValue).Title.Text = yLabel
    graphique.Show
End Sub

```

**Explication du code :**

Ce code Visual Basic complexe couvre divers aspects des statistiques et des graphiques :

**Fonctions mathématiques :**

* `Moyenne` : calcule la moyenne d'une liste de nombres.
* `EcartType` : calcule l'écart-type d'une liste de nombres.
* `Correlation` : calcule la corrélation entre deux listes de nombres.

**Régression linéaire :**

* `RegressionLineaireSimple` : calcule les coefficients de régression linéaire simple entre deux variables.

**Tests statistiques :**

* `TestT` : effectue un test t pour une moyenne.
* `ANOVA` : effectue une analyse de variance pour comparer des moyennes de plusieurs groupes.

**Graphiques :**

* `CreerGrapheBarres` : crée un graphique à barres avec des étiquettes d'axe personnalisées.
* `CreerGrapheNuagePoints` : crée un graphe en nuage de points avec des étiquettes d'axe personnalisées.

Ce code offre une large gamme de fonctionnalités statistiques et graphiques, le rendant adapté à l'analyse de données avancées et à la visualisation.