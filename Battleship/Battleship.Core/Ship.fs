namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    //Retourne la taille d'un bateau de ce "Name". 
    //La fonction "func" est typiquement "id" (pour avoir la taille en int) ou "float"
    let getSize (func : int -> 'a) (name : Name) : 'a =
        let size = 
            match name with
            | Spy | PatrolBoat -> 2
            | Destroyer | Submarine -> 3
            | Cruiser -> 4
            | AircraftCarrier -> 5
        func size

    //Retourne la distance de la tête d'un Ship par rapport à son centre d'après K = (size / 2) - 1
    let getHeadDistance (name : Name) : int =
        let size = getSize float (name)
        let half = size / 2.0 
        let rounded_half = ceil (half)
        (int rounded_half) - 1

    //Retourne (-) si "facing" North ou East et (+) si South ou West
    let getSign (facing : Direction) : (int -> int -> int) =
        if facing=North || facing=East then (-) else (+)

    //Retourne la direction opposée
    let getOpposite (facing : Direction) : Direction =
        match facing with
        | North -> South
        | South -> North
        | East -> West
        | West -> East

    //Retourne True si "facing" North ou South
    let isYAxis (facing: Direction) : bool =
        facing=North || facing=South

    //Retourne True si "facing" East ou West
    let isXAxis (facing: Direction) : bool =
        not (isYAxis facing)

    //Retourne les coordonnées de la tête
    //Les coordonnées ne sont pas nécessairement valides par rapport à une grille d'origine 0,0
    let getHeadCoord (center: Coord) (facing: Direction) (name: Name) : Coord = 
        if isYAxis facing then 
            ((getSign facing) (fst center) (getHeadDistance name), 
            snd center)
        else 
            (fst center, 
            (getSign facing) (snd center) (getHeadDistance name))

    let makeCoordsList (isYAxis : bool) (size : int) (x : int) (y : int) (sign : int -> int -> int) : Coord list =
        let rec make_coords (yAxis : bool) (n : int) (i : int) (j : int) : Coord list =
            match yAxis, n with
            | _, 0 -> []
            | true,_ -> [(i,j)]@(make_coords (yAxis) (n-1) (sign i 1) j)
            | false,_ -> [(i,j)]@((make_coords yAxis (n-1) i (sign j 1)))
        make_coords isYAxis size x y
        //NOTE: une version apparemment plus efficiente serait d'accumuler dans une liste avec :: (O(1)) et de faire List.rev à la fin (O(n)). @ est en O(n)

       

    (* --- Fin nouvelles fonctions --- *)


    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let size = getSize id name
        let headi, headj = getHeadCoord center facing name
        let yAxis = isYAxis facing
        let s = getSign (getOpposite facing)
        let coords = makeCoordsList yAxis size headi headj s
        { Coords = coords; Center = center; Facing = facing ; Name = name }


    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []
