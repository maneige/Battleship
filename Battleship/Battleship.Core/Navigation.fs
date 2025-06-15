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

    //Retourne une liste de tous les bateaux placés dans cette grille
    let getAllShips (grid : Sector Grid) : Ship list =
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
                        if c2 < c1 then East else West
                    else
                        if r2 < r1 then South else North
                else
                    South 
                        
            { Coords = sortedCoords; Center = center; Facing = facing; Name = name }
        )

    //Valide si toutes les coordonnées sont valides dans le contexte de jeu
    let isValidPlacement (coords : Coord list) (shipName : Name) (grid : Sector Grid) : bool =
        let isValidMove coord =
            isInGrid coord grid &&
            match getCell coord grid with
            | Clear -> true
            | Active (name, _) -> name = shipName
        coords |> List.forall isValidMove

    //Dans le contexte du placement des bateaux, valide les limites de la grille, si la cellule est libre et les périmètres
    let isValidDeployment (coords : Coord list) (name : Name) (grid: Sector Grid) : bool =
     
        //Valide d'abord si toutes les coordonnées sont dans la grille
        let inBounds = checkCoords (fun coord -> isInGrid coord grid) coords
        if not inBounds then false
        else
            //Si les coordonnées sont libres ou occupées par ce bateau
            let isClear = isValidPlacement coords name grid

            //Si les coordonnées sont à l'extérieur des autres périmètres
            let dims = getDims grid
            let allShips = getAllShips grid
            let otherShips = List.filter (fun x -> x.Name <> name) allShips //exclure le bateau à valider
            let perimeterCoords = List.collect (fun x -> getPerimeter x dims) otherShips
            let outsidePerimeters = checkCoords (fun coord -> List.forall (fun otherCoord -> otherCoord <> coord) perimeterCoords) coords

            isClear && outsidePerimeters


    (* --- Fin nouvelles fonctions --- *)


    //Retourne true si toutes les coordonnées sont valides (secteur Clear, coord dans la grille et à l'extérieur de tous périmètres)
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let ship = createShip center direction name
        let coords = ship.Coords
        isValidDeployment coords name grid
        
    //Retourne true si toutes les nouvelles coordonnées du bateau sont dans la grille et sur des secteurs Clear
    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let i,j = ship.Center
        let newCenter = getNextCoord (isYAxis direction) (getSign direction) i j
        let tempShip = createShip newCenter ship.Facing ship.Name
        isValidDeployment tempShip.Coords ship.Name grid

    //Régénère les coordonnées du bateau
    let move (ship: Ship) (direction: Direction) : Ship =
        let i,j = ship.Center
        let yAxis = (isYAxis direction)
        let s = (getSign direction)
        let newCenter = getNextCoord yAxis s i j
        createShip newCenter ship.Facing ship.Name

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let tempShip = createShip ship.Center direction ship.Name
        isValidDeployment tempShip.Coords ship.Name grid

    let rotate (ship: Ship) (direction: Direction) : Ship =
        createShip ship.Center direction ship.Name

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let newCoords = ship.Coords |> List.map (fun (row, col) ->
            match ship.Facing with
            | North -> (row - 1, col)
            | South -> (row + 1, col)
            | East -> (row, col + 1)
            | West -> (row, col - 1)
        )
        isValidPlacement newCoords ship.Name grid

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
        let i,j = ship.Center
        let newDirection = getNextDirection ship.Facing rotation
        let newCenter = getNextCoord (isYAxis newDirection) (getSign newDirection) i j
        let tempShip = createShip newCenter newDirection ship.Name
        isValidPlacement tempShip.Coords ship.Name grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let i,j = ship.Center
        let newDirection = getNextDirection ship.Facing rotation
        let newCenter = getNextCoord (isYAxis newDirection) (getSign newDirection) i j
        createShip newCenter newDirection ship.Name
