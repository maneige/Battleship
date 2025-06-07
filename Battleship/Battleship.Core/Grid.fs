namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)
    let rec getCell (x: int, y: int) (grid: 'a Grid) : 'a =
        match grid, y with
        | Empty, _ -> failwithf "Coordonnée (%d,%d) hors de la grille (Empty)" x y
        | Row (cols, restRows), 0 ->
            if x >= 0 && x < List.length cols then
                List.item x cols
            else
                failwithf "Coordonnée (%d,%d) hors de la ligne" x y
        | Row (_, restRows), _ ->
            getCell x (y - 1) restRows