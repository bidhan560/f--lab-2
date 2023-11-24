type Coach = {
    Name : string
    former_player : bool
}

type Stats = {
    Wins : int
    Losses : int
}

type Team = {
    Name : string
    Coach : Coach
    Stats : Stats
}

let coaches = 
    Map.ofList [
        ("San Antonio Spurs", { Name = "Gregg Popovich"; former_player = true })
        ("Boston Celtics", { Name = "Joe Mazzulla"; former_player = false })
        ("Los Angeles Lakers", { Name = "Darvin Ham"; former_player = false })
        ("Utah Jazz", { Name = "Quin Snyder"; former_player = false })
        ("Phoenix Suns", { Name = "Igor Kokoskov"; former_player = false })
    ]

let stats = 
    Map.ofList [
        ("San Antonio Spurs", { Wins = 2283; Losses = 1502 })
        ("Boston Celtics", { Wins = 3570; Losses = 2462 })
        ("Los Angeles Lakers", { Wins = 3503; Losses = 2419 })
        ("Utah Jazz", { Wins = 2146; Losses = 1804 })
        ("Phoenix Suns", { Wins = 2380; Losses = 2063 })
    ]

let teams = 
    [
        { Name = "San Antonio Spurs"; Coach = coaches["San Antonio Spurs"]; Stats = stats["San Antonio Spurs"] }
        { Name = "Boston Celtics"; Coach = coaches["Boston Celtics"]; Stats = stats["Boston Celtics"] }
        { Name = "Los Angeles Lakers"; Coach = coaches["Los Angeles Lakers"]; Stats = stats["Los Angeles Lakers"] }
        { Name = "Utah Jazz"; Coach = coaches["Utah Jazz"]; Stats = stats["Utah Jazz"] }
        { Name = "Phoenix Suns"; Coach = coaches["Phoenix Suns"]; Stats = stats["Phoenix Suns"] }
    ]

let calculate_success_percentage team =
    let wins = float team.Stats.Wins
    let losses = float team.Stats.Losses
    (wins / (wins + losses)) * 100.0

let team_success_percentages = 
    teams 
    |> List.map (fun team -> team.Name, calculate_success_percentage team)

team_success_percentages 
|> List.iter (fun (name, successPercentage) -> 
    printfn "%s: %.2f%%" name successPercentage)

type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let rec calculate_movie_price movieType =
    match movieType with
    | Regular -> 12.0
    | IMAX -> 17.0
    | DBOX -> 20.0
    | RegularWithSnacks -> calculate_movie_price Regular + 5.0
    | IMAXWithSnacks -> calculate_movie_price IMAX + 5.0
    | DBOXWithSnacks -> calculate_movie_price DBOX + 5.0

let calculate_budget activity =
    match activity with
    | BoardGame | Chill -> 0.0
    | Movie movieType -> calculate_movie_price movieType
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (kilometers, fuelPerKm) -> float kilometers * fuelPerKm

let activity = Movie IMAXWithSnacks
let budget = calculate_budget activity
printfn "The budget for the IMAX with Snacks is: %f CAD" budget
