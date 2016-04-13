open List

(* Definición del tipo para representar gráficas dirigidas a través de su lista de adyacencias *)
type 'a grafica = ('a*'a list) list

(* insert_vtx, función que inserta un vértice en una gráfica y regresa la nueva gráfica*)
let rec insert_vtx v g = match g with
  | [] -> [(v,[])]
  | ((n,l) as h)::t -> if v=n then g else h::(insert_vtx v t)

  
(* insert_edge, que agrega una arista a una gráfica de la cual asumiremos tiene el vértice v2*)
let rec insert_edge ((v1,v2) as e) g = let aux v la = if (mem v la) then la else v::la 
  in match g with 
    |[] -> []
    |(n,l)::t when v1=n -> (n,(aux v2 l))::t
    |((n,l) as h)::t -> h::(insert_edge e t)

(* successors, función que regresa una lista con los sucesores directos de un vértice*)
let rec successors v g = match g with
  |[] -> []
  |(n,l)::t when n=v -> l
  |(n,l)::t -> successors v t
  
  (* predecessores, función que devuelve una lista con los predecesores directos de un vértice *)
(* let predecesores v g = *)



let (g1:('a grafica)) = [(1,[3]);(2,[1]);(3,[4;5]);(4,[5]);(5,[])]

let g2 = insert_vtx 7 g1
let g3 = insert_edge (7,5) g2
let ls = successors 3 g3
