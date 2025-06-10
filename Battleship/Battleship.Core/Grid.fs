namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    //Compte le nombre de Row d'une Grid (= hauteur)
    let numRows (grid : 'a Grid) : int =
        let rec nR (i : int) (g : 'a Grid) =
            match g with
            | Empty -> i
            | Row (_, rest) -> nR (i+1) rest
        nR 0 grid

    //Compte le nombre de colonnes (longueur d'une Row) d'une Grid (= largeur)
    let numColumns (grid : 'a Grid) : int =
        match grid with 
            | Empty -> 0
            | Row(first, _) -> List.length first

    //Calcule les dimensions d'une Grid
    let getDims (grid : 'a Grid) : Dims =
        let rows = numRows grid
        let cols = numColumns grid
        (rows, cols)

    //Retourne la liste ('a list) d'une énième Row dans une Grid
    let getRowList (n : int) (grid : 'a Grid) : 'a list = 
        let rec gR (i : int) (g : 'a Grid) = 
            match i, g with
            | 0, Row(r, _) -> r
            | _, Row(_, rest) -> gR (i-1) rest
            | _, Empty -> failwith "Index is out of bounds"
        gR n grid

    //Retourne la cellule correspondante à la coordonnée
    let getCell (coord : Coord) (grid : 'a Grid) : 'a =
        let i, j = coord
        let row = getRowList i grid
        let cell = List.item j row
        cell

    //Retourne true si la coordonnée est positive et contenue dans ces dimensions
    let isInDims (coords : Coord) (dims : Dims) : bool =
        let i, j = coords
        let rows, columns = dims
        (i >= 0 && i < rows) && (j >= 0 && j < columns)

    //Retourne true si la coordonnée est dans la grille. 
    //Utile pour valider le placement/rotation/movement des bateaux près des bordures
    let isInGrid (coords : Coord) (grid : 'a Grid) : bool =
        isInDims coords (getDims grid)

    //Retourne true si les deux coordonnées sont identiques
    let isSameCoords (coord1: Coord) (coord2 : Coord) : bool = 
        coord1 = coord2


    (* --- Abstraction --- *)


    //Retourne true si toutes les coordonnées de la liste respectent la fonction "condition"
    //Exemple: checkCoords (fun coord -> isInGrid coord grid) ship.Coords
    let checkCoords (condition : Coord -> bool) (coords : Coord list) : bool =
        let rec cC c =
            match c with
            | [] -> true
            | coord::rest -> if condition coord then cC rest else false
        cC coords

    //Retourne une liste remplie selon une valeur donnée par la fonction
    let initRow (length : int) (valueFunction : int -> 'a) : 'a list =
        let rec iR (n : int) (row : 'a list) : 'a list =
            if n > 0 then 
                iR (n - 1) ((valueFunction n)::row)
            else 
                row
        iR length []

    //Applique f sur tous les éléments de la rangée (Row) avec information sur les coordonnées (i,j)
    let mapRowCoord (f : int -> int -> 'a -> 'a) (row : 'a list * 'a Grid) (i : int) : 'a list * 'a Grid =
        let rowInit,grid = row
        let rowResult =
            let rec updateRow' (r : 'a list) (j : int) (acc : 'a list) : 'a list =
                match r with
                | [] -> List.rev acc
                | e::rest -> updateRow' rest (j+1) (f i j e :: acc)
            updateRow' rowInit 0 []
        rowResult,grid

    //Applique f sur tous les éléments de la rangée (Row)
    let mapRow (f : 'a -> 'a) (row : 'a list * 'a Grid) : 'a list * 'a Grid =
        mapRowCoord (fun _ _ -> fun x -> f x) row 0

    //Applique f sur tous les éléments de la grille (Grid) avec information sur les coordonnées
    let mapGridCoord (f : int -> int -> 'a -> 'a) (grid : 'a Grid) : ('a Grid) =
        let rec mapGrid' (g : 'a Grid) (i: int) (j : int) =
            match g with
            | Empty -> Empty
            | Row(rowList, nextGrid)-> 
                let newValues, _ = mapRowCoord f (rowList, nextGrid) i
                Row (newValues, mapGrid' nextGrid (i+1) j)
        mapGrid' grid 0 0

    //Applique f sur tous les éléments de la grille (Grid), sans tenir compte des coordonnées
    let mapGrid (f : 'a -> 'a) (grid : 'a Grid) : ('a Grid) =
        mapGridCoord (fun _ _ -> fun x -> f x) grid

    //Retourne une liste de paires des coordonnées et des valeurs ('a) qui remplissaient la condition
    let filterGridCoordValues (condition : 'a -> bool) (grid : 'a Grid) : (Coord * 'a) list =
        let rec aux (g : 'a Grid) (i : int) (acc : (Coord * 'a) list) =
            match g with
            | Empty -> acc
            | Row(list, nextGrid) -> 
                let rowCoords =
                    //Parcourt les éléments de cette rangée (soit j, les colonnes)
                    let rec processRow (r : 'a list) (j : int) (accRow : (Coord * 'a) list) =
                        match r with
                        | [] -> accRow
                        | c::rest -> 
                            let curr = Coord(i,j)
                            let acc = if condition c then (curr, c)::accRow else accRow
                            processRow rest (j+1) acc
                    in processRow list 0 ([] : (Coord * 'a) list)
                aux nextGrid (i+1) rowCoords @ acc
        in aux grid 0 []