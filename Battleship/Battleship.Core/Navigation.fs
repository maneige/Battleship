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

    let rotateDirection current rotation =
        match rotation with
        | Clockwise ->
            match current with 
            | South -> West
            | West -> North
            | North -> East
            | East -> South
        | Counterclockwise ->
            match current with 
            | South -> East
            | West -> South
            | North -> West
            | East -> North

    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let ship = createShip center direction name
        let coords = ship.Coords
        let inBounds = checkCoords (fun coord -> isInGrid coord grid) coords
        let isClear = checkCoords (fun coord -> (getCell coord grid) = Clear) coords
        inBounds && isClear
        //TODO doit aussi valider le périmètre des bateaux! canMove sera identique mais sans périmètre

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let move (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let (cx, cy) = ship.Center

        let rec checkCoords coords =
        match coords with
        | [] -> true
        | (x, y)::rest ->
            let dx = x - cx
            let dy = y - cy
            let (nx, ny) =
                match direction with
                | Clockwise -> (cx - dy, cy + dx)
                | Counterclockwise -> (cx + dy, cy - dx)

            isInGrid (nx, ny) grid &&
            match Grid.getCell (ny, nx) grid with // inverser ny et nx
            | Grid.Clear -> true
            | Grid.Active (name, _) when name = ship.Name -> true 
            | _ -> false
        )

    let rotate (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let (cx, cy) = ship.Center
        let newFacing = rotateDirection ship.Facing rotation

        let rec rotateCoords coords =
        match coords with
        | [] -> []
        | (x, y)::rest ->
            let dx = x - cx
            let dy = y - cy
            let nx, ny =
                match rotation with
                | Clockwise -> (cx - dy, cy + dx)
                | Counterclockwise -> (cx + dy, cy - dx)
            let restCoords = rotateCoords rest
            (nx, ny)::restCoords

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
        canRotate ship rotation grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        rotate ship rotation
