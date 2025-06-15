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

    //Retourne (-) si "facing" North ou West et (+) si South ou East
    let getSign (facing : Direction) : (int -> int -> int) =
        if facing=North || facing=West then (-) else (+)

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

    //Retourne les coordonnées de la tête par rapport au centre. 
    //Les coordonnées ne sont pas nécessairement valides par rapport à une grille d'origine 0,0
    let getHeadCoord (center: Coord) (facing: Direction) (name: Name) : Coord = 
        if isYAxis facing then 
            ((getSign facing) (fst center) (getHeadDistance name), 
            snd center)
        else 
            (fst center, 
            (getSign facing) (snd center) (getHeadDistance name))

    //Retourne une liste de coordonnées de taille size avec x y comme première valeur
    let makeCoordsList (isYAxis : bool) (size : int) (x : int) (y : int) (sign : int -> int -> int) : Coord list =
        let rec make_coords (yAxis : bool) (n : int) (i : int) (j : int) : Coord list =
            match yAxis, n with
            | _, 0 -> []
            | true,_ -> [(i,j)]@(make_coords (yAxis) (n-1) (sign i 1) j)
            | false,_ -> [(i,j)]@((make_coords yAxis (n-1) i (sign j 1)))
        make_coords isYAxis size x y

    //Étant donnée une coordonnée, donne sa position dans le bateau (0 étant la tête)
    let getIndexInShip (ship : Ship) (coord : Coord) : int =
        List.findIndex (fun x -> coord = x ) ship.Coords

    //Retourne une coordonnée adjacente à i,j en fonction de l'axe et de la direction indiquée par le signe
    let getNextCoord (yAxis : bool) (signFunction : int -> int -> int) (i : int) (j : int) : Coord =
        if yAxis then Coord(signFunction i 1, j) 
        else Coord(i, signFunction j 1)


    (* --- Fin nouvelles fonctions --- *)


    //Crée un bateau
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let size = getSize id name
        let headi, headj = getHeadCoord center facing name
        let yAxis = isYAxis facing
        let s = getSign (getOpposite facing)
        let coords = makeCoordsList yAxis size headi headj s
        { Coords = coords; Center = center; Facing = facing ; Name = name }

    //Retourne une liste de coordonnées qui constituent le périmètre du bateau
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =

        //Produit une liste de coordonnées adjacentes selon l'axe représenté par le signe
        let getAdjacentCoords (yAxis : bool) (sign : int -> int -> int) (cList : Coord List) : Coord list =
            let adjList = List.map (fun c -> let i,j = c in if yAxis then (i, sign j 1) else (sign i 1, j )) cList
            let filteredList,_ = List.partition (fun x -> isInDims x dims) adjList
            filteredList

        let dir = ship.Facing
        let posSign = getSign dir
        let negSign = getSign (getOpposite dir)
        let isY = isYAxis dir
        let i1, j1 = List.head ship.Coords
        let i2, j2 = List.last ship.Coords

        //Garde les coordonnées de périmètre au bout de la tête et de la queue si elles sont dans la grille
        let perimeter1 = let nh = getNextCoord isY (posSign) i1 j1 in if isInDims nh dims then [nh] else []
        let perimeter2 = let nl = getNextCoord isY (negSign) i2 j2 in if isInDims nl dims then [nl] else []
        let extraCoords = perimeter1@perimeter2@ship.Coords

        //Crée des listes de coordonnées adjacentes et conserve celles qui sont valides
        let adj1 = getAdjacentCoords isY (+) extraCoords
        let adj2 = getAdjacentCoords isY (-) extraCoords
        perimeter1@perimeter2@adj1@adj2
        
