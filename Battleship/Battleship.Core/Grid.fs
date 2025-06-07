namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    //Compte le nombre de Row d'une Grid
    let numRows (grid : 'a Grid) : int =
        let rec nR (i : int) (g : 'a Grid) =
            match g with
            | Empty -> i
            | Row (_, rest) -> nR (i+1) rest
        nR 0 grid

    //Compte le nombre de colonnes (longueur d'une Row) d'une Grid
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

    //Retourne true si la coordonnée est dans la grille. 
    //Utile pour valider le placement/rotation/movement des bateaux près des bordures
    let isInGrid (coords : Coord) (grid : 'a Grid) : bool =
        let i, j = coords
        let rows, columns = getDims grid
        (i >= 0 && i < rows) && (j >= 0 && j < columns)

    //Retourne true si toutes les coordonnées de la liste respectent la fonction "condition"
    //Exemple: checkCoords (fun coord -> isInGrid coord grid) ship.Coords
    let checkCoords (condition : Coord -> bool) (coords : Coord list) : bool =
        let rec cC c =
            match c with
            | [] -> true
            | coord::rest -> if condition coord then cC rest else false
        cC coords


    (* --- Fin nouvelles fonctions --- *)