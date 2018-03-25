open Core
open Stdio

module IntInt = struct
    module T = struct 
        type t = int * int [@@deriving sexp, hash]
        let compare (x0,_) (y0,_) = compare x0 y0
    end
    include T
    include Comparable.Make(T)
    include Hashable.Make(T)
end

(* graph is an array of list of tuples (distance, vertex) *)
let dijkstra g s e = 
    let n = Array.length g in 
    let dist = Array.create ~len:n Int.max_value in
    let q = ref (IntInt.Set.of_list [(0,s)]) in 
    let parent = Array.init n ~f: (fun x-> x) in
    dist.(s) <- 0;
    parent.(s) <- s;
    while not (Set.is_empty !q) do
        let (d,u) = Option.value_exn (Set.min_elt !q) in 
        q := Set.remove (!q) (d,u);

        List.iter 
            ~f: (fun (v,w) -> 
                    let newdist = dist.(u) + w in 
                    if newdist < dist.(v) then 
                    begin
                        dist.(v) <- newdist;
                        q := Set.add !q (newdist, v);
                        parent.(v) <- u;
                    end
                )
            g.(u)
    done;
    parent , dist.(e)

let () = 
    let g = Array.create ~len:6 [] in 
    g.(0) <- [(1,7); (2,9); (5, 14)];
    g.(1) <- [(0,7); (2,10); (3,15)];
    g.(2) <- [(0,9); (1,10); (3,11); (5,2)];
    g.(3) <- [(1,15); (2,11); (4,6)];
    g.(4) <- [(3,6); (5,9)];
    g.(5) <- [(0,14); (2,2); (4,9)];

    let (parent, min_distance) = dijkstra g 0 4 in
    printf "max distance from 0 to 4 is: %d\n\n" min_distance; 
    printf "The shortest path from 0 to 4 backward is: \n4 \n";
    let vertex = ref 4 in
    while (!vertex != 0) do 
        vertex := parent.(!vertex);
        printf "%d\n"  !vertex;
    done;
