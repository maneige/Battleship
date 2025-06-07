namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    //Rempli une Row vide
    let initClearRow (length : int) : Sector list =
        let rec initClearRow' (n : int) (row : Sector list) : Sector list =
            if n > 0 then 
                initClearRow' (n - 1) (Clear::row)
            else 
                row
        initClearRow' length []


    (* --- Fin nouvelles fonctions --- *)


    let initClearGrid (dims: Dims) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let rec initClearGrid' (i : int) =
            if i > 0 then 
                Row (initClearRow (snd dims), initClearGrid' (i - 1))
            else 
                Empty
        initClearGrid' (fst dims)

    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let coords = ship.Coords
        let firstx, firsty = List.head coords
        let size = List.length coords
        Empty

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        None

    let extractData (grid: Sector Grid) : Data =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Dims = (0, 0); Ships = [] }

    let loadData (data: Data) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty