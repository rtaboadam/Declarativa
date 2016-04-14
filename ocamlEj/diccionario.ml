open Char
open String
type node = Nodo of char*bool*lex_tree
 and lex_tree = node list
type palabra = string


(* exists funcion que verifica si una palabra pertenece al diccionario*)
let rec exists p d = 
  let aux pal i n = match d with
    |[] -> false
    |(Nodo (c,b,l))::t when c=pal.[i] -> if n=1 then b else exists (sub pal (i+1) (n-1)) l
    |(Nodo (c,b,l))::t -> exists pal t
  in aux p 0 (length p)
         
(*insert función que toma una palabra y un diccionario y regresa un nuevo diccionario que contiene la palabra
   Si la palabra ya están en el diccionario no es necesario insertarla*)
let rec insert p d = 
  let aux pal i n = 
    if n=0 then d
    else match d with 
	 |[] -> [Nodo(pal.[i],n=1,insert (sub pal (i+1) (n-1)) [])]
	 |(Nodo (c,b,l))::t when c=pal.[i] -> if n=1 then (Nodo (c,true,l))::t
					      else (Nodo (c,b,insert (sub pal (i+1) (n-1)) l))::t
	 |((Nodo (c,b,l)) as e1)::t -> e1::(insert pal t)
  in aux p 0 (length p)
         
         
(*construct  función que toma una lista de palabras y construye el diccionario correspondiente *)
let construct  l = let rec construct' list = match list  with
                     | [] -> []
                     | (x::t) -> insert x (construct' t)
                   in
                   construct' l
                             
(*verify función que toma una lista de palabras y un diccionario y regresa una lista de las palabras que no pertenecen al diccionario. *)
let rec verify l d = match l with
  | [] -> []
  | (k::t) -> if exists k d then verify t d else k::(verify t d)
                                                      
(* select función que toma un diccionario y una longitud y regresa el conjunto de palabras con esta longitud *)
(*let rec select d n =*)  
  
let rec d1 = [Nodo('d',false,[Nodo('a',false,ta);Nodo('e',true,te);Nodo('i',false,ti)])]
and ta = [Nodo('d',false,[Nodo('o',true,[])])]
and te = [Nodo('d',false,[Nodo('o',true,[])])]
and ti = [Nodo('e',false,[Nodo('z',true,[]);Nodo('t',false,[Nodo('a',true,[])])])]
           
           
 (*let d2 = construct ["alto";"ala";"b";"a";"banco";"bar"]*)
             
