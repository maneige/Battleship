namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)


    //Remet le secteur à Clear si un bateau de ce nom est enregistré dans ce secteur
    let removeShipFromSector (name : Name) (sector : Sector) : Sector =
        match sector with
        | Clear -> Clear
        | Active(n,i) -> if n=name then Clear else Active(n,i)


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
        let clearedGrid = mapGrid (removeShipFromSector ship.Name) grid
        addShip ship clearedGrid

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        None

    let extractData (grid: Sector Grid) : Data =               
        let dims = getDims grid
        
        let shipData = 
            let collectShipData = 
                collectGrid 
                    (fun row col sector acc -> 
                        match sector with
                        | Active (name, idx) -> ((row, col), name, idx) :: acc
                        | Clear -> acc)
                    []
                    grid
                    
            collectShipData
            |> List.groupBy (fun (_, name, _) -> name)
            |> List.map (fun (name, coordsWithIndices) ->
                let sortedCoords = 
                    coordsWithIndices 
                    |> List.sortBy (fun (_, _, idx) -> idx)
                    |> List.map (fun ((row, col), _, _) -> (row, col))
                    
                let center = 
                    let centerIdx = List.length sortedCoords / 2
                    List.item centerIdx sortedCoords
                    
                let facing =
                    if List.length sortedCoords >= 2 then
                        let (r1, c1) = List.head sortedCoords
                        let (r2, c2) = List.item 1 sortedCoords
                        if r1 = r2 then
                            if c2 > c1 then East else West
                        else
                            if r2 > r1 then South else North
                    else
                        South 
                        
                { Coords = sortedCoords; Center = center; Facing = facing; Name = name }
            )
            
        { Dims = dims; Ships = shipData }

    let loadData (data: Data) : Sector Grid =
        let emptyGrid = initClearGrid data.Dims
        data.Ships
        |> List.fold (fun grid ship -> addShip ship grid) emptyGrid