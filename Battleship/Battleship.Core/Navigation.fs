namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let rotateDirection current rotation =
        match rotation with
        | Clockwise ->
            match current with 
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | Counterclockwise ->
            match current with 
            | North -> West
            | West -> South
            | South -> East
            | East -> North


    //Retourne true si le secteur est vide (Clear) ou s'il contient une partie du bateau
    let isClearOrSame (sector : Sector) (name : Name) : bool =
        match sector with
            | Clear -> true
            | Active(cellName, _) -> cellName = name 

    //Dans le contexte du placement des bateaux, valide les limites de la grille, si la cellule est libre et les périmètres
    let validateDeploymentConditions (coords : Coord list) (name : Name) (grid: Sector Grid) : bool =
     
        //Toutes les coordonnées sont dans la grille
        let inBounds = checkCoords (fun coord -> isInGrid coord grid) coords
        
        //Les coordonnées sont libres ou occupées par ce bateau
        let isClear = 
            checkCoords
                (fun coord -> 
                    let cell = getCell coord grid
                    isClearOrSame cell name)
                coords

        //Compare les coordonnées avec la liste des périmètres de tous les autres bateaux
        let otherShipsCoords = filterGridCoordValues (fun x -> not (isClearOrSame x name)) grid
        //TODO - terminer le check de périmètre (ici ça ne regarde pas les périmètres mais les autres bateaux
        let isInAnyPerimeter = 
            checkCoords 
                (fun coord -> 
                    let rec checkPerimeters (cs : (Coord * Sector) list) : bool = 
                        match cs with
                        | [] -> false
                        | (cp,_)::crest -> if cp = coord then true else checkPerimeters crest
                    checkPerimeters otherShipsCoords)
                coords
        
        inBounds && isClear && not isInAnyPerimeter


    (* --- Fin nouvelles fonctions --- *)


    //Retourne true si toutes les coordonnées sont valides (secteur Clear, coord dans la grille et à l'extérieur de tous périmètres)
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let ship = createShip center direction name
        let coords = ship.Coords
        validateDeploymentConditions coords name grid
        
    //Retourne true si toutes les nouvelles coordonnées du bateau sont dans la grille et sur des secteurs Clear
    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let i,j = ship.Center
        let newCenter = getNextCoord (isYAxis direction) (getSign direction) i j
        let tempShip = createShip newCenter ship.Facing ship.Name
        validateDeploymentConditions tempShip.Coords ship.Name grid

    //Régénère les coordonnées du bateau
    let move (ship: Ship) (direction: Direction) : Ship =
        let i,j = ship.Center
        let yAxis = (isYAxis direction)
        let s = (getSign direction)
        let newCenter = getNextCoord yAxis s i j
        createShip newCenter ship.Facing ship.Name

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let rotation = 
            match direction with
            | North | West -> Counterclockwise
            | East | South -> Clockwise

        let (cx, cy) = ship.Center
        let rec checkCoords coords =
            match coords with
            | [] -> true
            | (x, y)::rest ->
                let dx = x - cx
                let dy = y - cy
                let (nx, ny) =
                    match rotation with
                    | Clockwise -> (cx - dy, cy + dx)
                    | Counterclockwise -> (cx + dy, cy - dx)
                isInGrid (nx, ny) grid &&
                match getCell (nx, ny) grid with
                | Clear -> checkCoords rest
                | Active (name, _) when name = ship.Name -> checkCoords rest
                | _ -> false
        checkCoords ship.Coords


    let rotate (ship: Ship) (direction: Direction) : Ship =
        let rotation = 
            match direction with
            | North | West -> Counterclockwise
            | East | South -> Clockwise

        let (cx, cy) = ship.Center
        let newFacing = rotateDirection ship.Facing rotation

        let rec rotateCoords coords =
            match coords with
            | [] -> []
            | (x, y)::rest ->
                let dx = x - cx
                let dy = y - cy
                let (nx, ny) =
                    match rotation with
                    | Clockwise -> (cx - dy, cy + dx)
                    | Counterclockwise -> (cx + dy, cy - dx)
                (nx, ny)::(rotateCoords rest)
                    
        let newCoords = rotateCoords ship.Coords
        { ship with Coords = newCoords; Facing = newFacing }


    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let newCoords = ship.Coords |> List.map (fun (row, col) ->
            match ship.Facing with
            | North -> (row - 1, col)
            | South -> (row + 1, col)
            | East -> (row, col + 1)
            | West -> (row, col - 1)
        )
        let isValidMove coord =
            isInGrid coord grid &&
            match getCell coord grid with
            | Clear -> true
            | Active (name, _) -> name = ship.Name
        newCoords |> List.forall isValidMove

    let moveForward (ship: Ship) : Ship =
        let newCoords = ship.Coords |> List.map (fun (row, col) ->
            match ship.Facing with
            | North -> (row - 1, col)
            | South -> (row + 1, col)
            | East -> (row, col + 1)
            | West -> (row, col - 1)
        )
        let (centerRow, centerCol) = ship.Center
        let newCenter =
            match ship.Facing with
            | North -> (centerRow - 1, centerCol)
            | South -> (centerRow + 1, centerCol)
            | East -> (centerRow, centerCol + 1)
            | West -> (centerRow, centerCol - 1)
        { ship with Coords = newCoords; Center = newCenter }

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        match rotation with
        | Clockwise ->
            match current with
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | Counterclockwise ->
            match current with
            | North -> West
            | West -> South
            | South -> East
            | East -> North

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let newDirection = rotateDirection ship.Facing rotation
        canRotate ship newDirection grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let newDirection = rotateDirection ship.Facing rotation
        rotate ship newDirection
        //{ Coords = []; Center = (0, 0); Facing = North; Name = Spy }
