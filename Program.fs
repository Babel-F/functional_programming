//Excercice 1A

    let five = 5
    let seven = 7
    let cube (x: int) = x * x * x

    printfn "x: %i" (cube(five))
    printfn "x: %i" (cube(seven))

//Excercice 1B

    /// 1.

    let interestRate (balance: decimal): single =
        match balance with
        |   balance when (balance < 0m) -> 0.03213f 
        |   balance when (1000m > balance && balance > 0m) -> 0.005f
        |   balance when (5000m > balance && balance > 1000m) -> 0.01621f
        |   balance when (balance >= 5000m) -> 0.02475f
        |   _-> 0f

    interestRate (200.75m) |> printfn "%f"   // f pour décimal, m pour entier

    /// 2.

    let interest (balance: decimal): decimal =
        System.Convert.ToDecimal (interestRate (200.75m)) * balance / 100m
    
    interest (200.75m) |> printfn "%f"

    /// 3.

    let annualBalanceUpdate(balance: decimal): decimal =
        balance + interest(200.75m)

    annualBalanceUpdate (200.75m)|> printfn "%f"

    /// 4.

    // pas réussi

// Exercice 2A

    /// 1.

    let message(msgLigneJournal: string): string = 
        let cut = msgLigneJournal.Split[|':'|]
        cut[1].Trim(' ')
    
    message "[WARNING] : Disque presque plein" |> printf "%s"

    /// 2.

    let logLevel(msgLigneJournal: string):string = 
        let cut = msgLigneJournal.Split[|':'|]
        cut[1].Trim('[',' ',']').ToLower()
    
    logLevel "[ERREUR] : Opération invalide" |> printf "%s"

    /// 3.    

    let reformat (msgLigneJournal: string): string =
        message(msgLigneJournal)+ " (" + logLevel(msgLigneJournal) + ")"

    reformat "[INFO] : Opération terminée" |> printf "%s"
    
// Exercice 2B

    let parlerBob(interaction: string): string =
        match interaction with
        | interaction when interaction = "Comment ça va ?" -> "Bien sûr"  // comment faire pour ajouter la contrainte du "?" sans qu'il y ait de conflit avec la question criée
        // | interaction when 1er char de interaction est une majuscule  et interaction contient "?"
        | interaction when interaction = interaction.ToUpper() -> "Wow, calme-toi !"
        | interaction when interaction = interaction.ToUpper() && interaction.Contains('?') -> "Calme toi je sais ce que je fais"
        | interaction when interaction = "" -> "Très bien. Sois comme ça"
        | _ -> "Peu importe"
    
    parlerBob "Comment ça va ?" |> printf "%s"

// Exercice 3A

    /// 1.

    type entraineurs = {
        nom: string
        ancienJoueur: bool
    };
    
    type stats = {
        victoires: int
        defaites: int
    };
    
    type equipes = {
        nom: string
        entraineurs: entraineurs
        stats: stats
    }

    /// 2.

    let creerEntraineur(nom: string, ancienJoueur: bool): entraineurs =
        {nom = nom; ancienJoueur = ancienJoueur}

    /// 3.

    let creerStats(victoire: int, defaite: int): stats =
        {victoires = victoire; defaites = defaite}

    /// 4.

    let creerEquipe(nom: string, entraineur: entraineurs, stat: stats): equipes =
        {nom = nom; entraineurs = entraineur; stats = stat}

    /// 5.

    let remplacerEntraineur(equipe: equipes, entraineur: entraineurs): equipes =
        let equipes = {equipe with entraineurs = entraineur}
        equipes

    /// 6.

    let memeEquipe(equipe1: equipes, equipe2: equipes): bool =
        equipe1 = equipe2

    let entraineurPacer = creerEntraineur "Larry Bird" true
    let statPacer = creerStats 58 24
    let equipePacer = creerEquipe "Indiana Pacers" entraineurPacer statPacer

    let entraineurLakers = creerEntraineur "Del Harris" false
    let statLakers = creerStats 61 21
    let equipeLakers = creerEquipe "LA Lakers" entraineurLakeres statLakers

    memeEquipe equipePacer equipeLakers |> printfn "%b"

    /// 7.

// Exercice 3B

    /// 1.
    
    type approbation =
        | non 
        | oui 
        | bof
    
    /// 2.
    
    type cuisines =
        | coreen 
        | turc 
    
    /// 3.
    
    type genre =
        | crime 
        | horreur 
        | romance 
        | thriller 
    
     /// 4.
    
    type activite = 
        | boardGame
        | chill
        | film of genre
        | restaurant of cuisines
        | walk  of int
     
     /// 5.
    
    let rateActivity(activity : activite)=
        match activity with
        | film f when f = genre.romance -> oui
        | restaurant r when r = cuisines.coreen -> oui
        | restaurant r when r = cuisines.turc -> bof
        | walk w when w < 3 -> oui
        | walk w when w < 5 -> bof
        |  _ -> non
    
    rateActivity restaurant turc) |> printfn "%O"