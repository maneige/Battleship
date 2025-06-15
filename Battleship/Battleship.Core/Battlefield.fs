namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    // n/a

    (* --- Fin nouvelles fonctions --- *)


    //Produit une grille vide
    let initClearGrid (dims: Dims) : Sector Grid =
        let rec initClearGrid' (i : int) =
            if i > 0 then 
                Row (initRow (snd dims) (fun x -> Clear), initClearGrid' (i - 1))
            else 
                Empty
        initClearGrid' (fst dims)

    //Retourne une grille dans laquelle le bateau a été ajouté
    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        let coords = ship.Coords
        //Parcours toute la grille et update seulement les secteurs correspondants
        mapGridCoord
            (fun i j sector -> 
                let c = Coord(i,j) in 
                if List.exists (isSameCoords c) coords then 
                    Active(ship.Name, (getIndexInShip ship c)) 
                else 
                    sector
            )
            grid

    //Retire le bateau de la grille (efface les anciennes positions) et l'ajoute à nouveau
    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        //let clearedGrid = mapGrid (removeShipFromSector ship.Name) grid

        let clearedGrid = 
            mapGrid 
                (fun sector ->
                    match sector with
                        | Clear -> Clear
                        | Active(n,i) -> if n=ship.Name then Clear else Active(n,i)) 
                grid

        addShip ship clearedGrid

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        match getCell coord grid with
        | Active (name, _) -> Some name
        | _ -> None

    let extractData (grid: Sector Grid) : Data =               
        let dims = getDims grid
        let shipData = getAllShips grid 
        { Dims = dims; Ships = shipData }

    let loadData (data: Data) : Sector Grid =
        let emptyGrid = initClearGrid data.Dims
        data.Ships
        |> List.fold (fun grid ship -> addShip ship grid) emptyGrid