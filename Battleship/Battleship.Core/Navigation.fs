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
        false

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

        let isWithinGrid (x, y) =
            x >= 0 && x < 10 && y >= 0 && y < 5 // largeur 10 colonnes, hauteur 5 lignes

        ship.Coords
        |> List.forall (fun (x, y) ->
            let dx = x - cx
            let dy = y - cy
            let (nx, ny) =
                match rotation with
                | Clockwise -> (cx - dy, cy + dx)
                | Counterclockwise -> (cx + dy, cy - dx)
            isWithinGrid (nx, ny) &&
            match Grid.getCell (nx, ny) grid with
            | Grid.Clear -> true
            | Grid.Active (name, _) when name = ship.Name -> true 
            | _ -> false
        )

    let rotate (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let (cx, cy) = ship.Center
        let newFacing = rotateDirection ship.Facing rotation

        let newCoords =
            ship.Coords
            |> List.map (fun (x, y) ->
                let dx = x - cx
                let dy = y - cy
                match rotation with
                | Clockwise -> (cx - dy, cy + dx)
                | Counterclockwise -> (cx + dy, cy - dx)
            )
        { ship with Coords = newCoords; Facing = newFacing }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let moveForward (ship: Ship) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        North

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let newDirection = rotateDirection ship.Facing rotation
        canRotate ship rotation grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        rotate ship rotation