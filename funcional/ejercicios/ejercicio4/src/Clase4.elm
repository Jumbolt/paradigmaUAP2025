module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo


arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of
        Empty ->
            True

        Node _ _ _ ->
            False


-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of
        Node _ Empty Empty ->
            True
        _ -> False



-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamaño : Tree a -> Int
tamaño arbol =
    case arbol of
        Empty ->
            0

        Node _ izq der ->
            (tamaño izq) + (tamaño der) + 1


-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty ->
            0

        Node _ izq der ->
            (max (altura izq) (altura der)) + 1


-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of
        Empty ->
            0
        Node valor izq der ->
            valor + (sumarArbol izq) + (sumarArbol der)


-- 7. Contiene Valor


contiene : a -> Tree a -> Bool
contiene valor arbol =
    case arbol of
        Empty ->
            False

        Node v izq der ->
            v == valor || (contiene valor izq) || (contiene valor der)


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of
        Empty ->
            0
        Node _ Empty Empty ->
            1
        Node _ izq der ->
            (contarHojas izq) + (contarHojas der)


-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty -> 0
        Node v Empty _ -> v
        Node v izq der -> (min v (min (minimo izq) (minimo der)))


-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
     case arbol of
        Empty -> 0
        Node v Empty _ -> v
        Node v izq der -> (max v (max (maximo izq) (maximo der)))


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor

buscarEnLista : a -> List a -> Maybe a
buscarEnLista valor lista =
    case lista of
        [] -> Nothing
        head :: tail ->
            if head == valor then
                Just head
            else
                buscarEnLista valor tail

buscar : a -> Tree a -> Maybe a
buscar valor arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if v == valor then 
                Just v
            else
                case buscar valor izq of
                    Just encontrado -> Just encontrado
                    Nothing -> buscar valor der


-- 12. Encontrar Mínimo (con Maybe)


encontrarMinimo : Tree comparable -> Maybe comparable
encontrarMinimo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der ->
            case (encontrarMinimo izq, encontrarMinimo der) of
                (Nothing, Nothing) -> Just v
                (Just minIzq, Nothing) -> Just (min v minIzq)
                (Nothing, Just minDer) -> Just (min v minDer)
                (Just minIzq, Just minDer) -> Just (min v (min minIzq minDer))


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo arbol =
    case arbol of 
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der ->
            case (encontrarMaximo izq, encontrarMaximo der) of
                (Nothing, Nothing) -> Just v
                (Just maxIzq, Nothing) -> Just (max v maxIzq)
                (Nothing, Just maxDer) -> Just (max v maxDer)
                (Just maxIzq, Just maxDer) -> Just (max v (max maxIzq maxDer))


-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if predicado v then
                Just v
            else
                case buscarPor predicado izq of
                    Just valor -> Just valor
                    Nothing -> buscarPor predicado der


-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of
        Empty -> Nothing
        Node v _ _ -> Just v


-- 16. Obtener Hijo Izquierdo


hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _ -> Nothing
        Node _ izq _ -> Just izq


hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty -> Nothing
        Node _ _ Empty -> Nothing
        Node _ _ der -> Just der


-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    Maybe.andThen (hijoIzquierdo) (hijoIzquierdo arbol)

-- 18. Buscar en Profundidad


obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol valor arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if v == valor then
                Just arbol
            else
                case obtenerSubarbol valor izq of
                    Just subarbol -> Just subarbol
                    Nothing -> obtenerSubarbol valor der



buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if v == valor1 then
                buscar valor2 arbol
            else
                case buscarEnSubarbol valor1 valor2 izq of
                    Just encontrado -> Just encontrado
                    Nothing -> buscarEnSubarbol valor1 valor2 der


-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of
        Empty -> Err "El árbol está vacío"
        Node _ _ _ -> Ok arbol


-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case arbol of
        Empty -> Err "No se puede obtener la raíz de un árbol vacío"
        Node v _ _ -> Ok v


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of
        Empty -> Err "No se puede dividir un árbol vacío"
        Node v izq der -> Ok (v, izq, der)


-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case arbol of 
        Empty -> Err "No hay mínimo en un árbol vacío"
        Node v Empty Empty -> Ok v
        Node v izq der ->
            case (obtenerMinimo izq, obtenerMinimo der) of
                (Err e1, Err e2) -> Ok v
                (Ok minIzq, Err _) -> Ok (min v minIzq)
                (Err _, Ok minDer) -> Ok (min v minDer)
                (Ok minIzq, Ok minDer) -> Ok (min v (min minIzq minDer))


-- 23. Verificar si es BST


esBST : Tree comparable -> Bool
esBST arbol =
    let
        esBSTHelper : Maybe comparable -> Maybe comparable -> Tree comparable -> Bool
        esBSTHelper minVal maxVal tree =
            case tree of
                Empty -> True
                Node v izq der ->
                    let
                        validoMin = case minVal of
                            Nothing -> True
                            Just min -> v > min
                        validoMax = case maxVal of
                            Nothing -> True
                            Just max -> v < max
                    in
                    validoMin && validoMax &&
                    esBSTHelper minVal (Just v) izq &&
                    esBSTHelper (Just v) maxVal der
    in
    esBSTHelper Nothing Nothing arbol


-- 24. Insertar en BST


insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    case arbol of
        Empty -> Ok (Node valor Empty Empty)
        Node v izq der ->
            if valor == v then
                Err "El valor ya existe en el árbol"
            else if valor < v then
                case insertarBST valor izq of
                    Ok nuevoIzq -> Ok (Node v nuevoIzq der)
                    Err e -> Err e
            else
                case insertarBST valor der of
                    Ok nuevoDer -> Ok (Node v izq nuevoDer)
                    Err e -> Err e


-- 25. Buscar en BST


buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    case arbol of
        Empty -> Err "El valor no se encuentra en el árbol"
        Node v izq der ->
            if valor == v then 
                Ok v
            else if valor < v then
                buscarEnBST valor izq
            else
                buscarEnBST valor der


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    if esBST arbol then
        Ok arbol
    else
        Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Nothing -> Err mensajeError
        Just valor -> Ok valor


-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Ok valor -> Just valor
        Err _ -> Nothing


-- 29. Buscar y Validar


buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    case buscar valor arbol of
        Nothing -> Err "El valor no se encuentra en el árbol"
        Just v ->
            if v > 0 then
                Ok v
            else
                Err "El valor no es positivo"


-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    validarNoVacio arbol
        |> Result.andThen (\a -> 
            if tamaño a > 0 then 
                Ok a 
            else 
                Err "El árbol está vacío")
        |> Result.andThen validarBST


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    case buscar valor arbol1 of
        Just v -> Ok v
        Nothing ->
            case buscar valor arbol2 of
                Just v -> Ok v
                Nothing -> Err "Búsqueda fallida"


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> inorder izq ++ (v :: inorder der)


-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> v :: (preorder izq ++ preorder der)


-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> (postorder izq) ++ (postorder der) ++ [v]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty -> Empty
        Node v izq der -> Node (funcion v) (mapArbol funcion izq) (mapArbol funcion der)


-- 36. Filter sobre Árbol


filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    case arbol of
        Empty -> Empty
        Node v izq der ->
            let
                izqFiltrado = filterArbol predicado izq
                derFiltrado = filterArbol predicado der
            in
            if predicado v then
                Node v izqFiltrado derFiltrado
            else
                case (izqFiltrado, derFiltrado) of
                    (Empty, Empty) -> Empty
                    (Empty, _) -> derFiltrado
                    (_, Empty) -> izqFiltrado
                    (_, _) -> Node v izqFiltrado derFiltrado


-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty -> acumulador
        Node v izq der ->
            let
                acumIzq = foldArbol funcion acumulador izq
                acumActual = funcion v acumIzq
            in
            foldArbol funcion acumActual der


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    case arbol of
        Empty -> Err "El valor no existe en el árbol"
        Node v izq der ->
            if valor < v then
                case eliminarBST valor izq of
                    Ok nuevoIzq -> Ok (Node v nuevoIzq der)
                    Err msg -> Err msg
            else if valor > v then
                case eliminarBST valor der of
                    Ok nuevoDer -> Ok (Node v izq nuevoDer)
                    Err msg -> Err msg
            else
                case (izq, der) of
                    (Empty, Empty) -> Ok Empty
                    (Empty, _) -> Ok der
                    (_, Empty) -> Ok izq
                    (_, _) ->
                        case encontrarMinimo der of
                            Nothing -> Err "Error al encontrar mínimo"
                            Just minDer ->
                                case eliminarBST minDer der of
                                    Ok nuevoDer -> Ok (Node minDer izq nuevoDer)
                                    Err msg -> Err msg


-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    let
        insertarEnArbol : comparable -> Result String (Tree comparable) -> Result String (Tree comparable)
        insertarEnArbol valor resultArbol =
            case resultArbol of
                Err msg -> Err msg
                Ok arbol -> insertarBST valor arbol
    in
    List.foldl insertarEnArbol (Ok Empty) lista


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    let
        alturaBalanceada : Tree a -> Maybe Int
        alturaBalanceada tree =
            case tree of
                Empty -> Just 0
                Node _ izq der ->
                    case (alturaBalanceada izq, alturaBalanceada der) of
                        (Just hIzq, Just hDer) ->
                            if abs (hIzq - hDer) <= 1 then
                                Just (max hIzq hDer + 1)
                            else
                                Nothing
                        _ -> Nothing
    in
    case alturaBalanceada arbol of
        Just _ -> True
        Nothing -> False


-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    let
        listaOrdenada = inorder arbol
        construirBalanceadoHelper : List comparable -> Tree comparable
        construirBalanceadoHelper lista =
            let
                len = List.length lista
            in
            if len == 0 then
                Empty
            else
                let
                    medio = len // 2
                    izqLista = List.take medio lista
                    derLista = List.drop (medio + 1) lista
                in
                case lista |> List.drop medio |> List.head of
                    Nothing -> Empty
                    Just valorMedio -> Node valorMedio (construirBalanceadoHelper izqLista) (construirBalanceadoHelper derLista)
    in
    construirBalanceadoHelper listaOrdenada


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha


encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino valor arbol =
    case arbol of
        Empty -> Err "El valor no existe en el árbol"
        Node v izq der ->
            if v == valor then
                Ok []
            else
                case encontrarCamino valor izq of
                    Ok camino -> Ok (Izquierda :: camino)
                    Err _ ->
                        case encontrarCamino valor der of
                            Ok camino -> Ok (Derecha :: camino)
                            Err _ -> Err "El valor no existe en el árbol"


-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    case (camino, arbol) of
        ([], Empty) -> Err "Camino inválido"
        ([], Node v _ _) -> Ok v
        (_, Empty) -> Err "Camino inválido"
        (Izquierda :: resto, Node _ izq _) -> seguirCamino resto izq
        (Derecha :: resto, Node _ _ der) -> seguirCamino resto der


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun valor1 valor2 arbol =
    let
        encuentra : comparable -> Tree comparable -> Bool
        encuentra val tree =
            contiene val tree
    in
    case arbol of
        Empty -> Err "Uno o ambos valores no existen en el árbol"
        Node v izq der ->
            if not (encuentra valor1 arbol) || not (encuentra valor2 arbol) then
                Err "Uno o ambos valores no existen en el árbol"
            else if encuentra valor1 izq && encuentra valor2 izq then
                ancestroComun valor1 valor2 izq
            else if encuentra valor1 der && encuentra valor2 der then
                ancestroComun valor1 valor2 der
            else
                Ok v


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido arbol =
    esBST arbol


estaBalanceadoCompleto : Tree comparable -> Bool
estaBalanceadoCompleto arbol =
    estaBalanceado arbol


contieneValor : comparable -> Tree comparable -> Bool
contieneValor valor arbol =
    contiene valor arbol


-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe valor arbol =
    buscar valor arbol


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe arbol =
    encontrarMinimo arbol


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe arbol =
    encontrarMaximo arbol


-- Operaciones que retornan Result
insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
insertarResult valor arbol =
    insertarBST valor arbol


eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarResult valor arbol =
    eliminarBST valor arbol


validarResult : Tree comparable -> Result String (Tree comparable)
validarResult arbol =
    validarBST arbol


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion posicion arbol =
    let
        lista = inorder arbol
        elemento = lista
            |> List.drop posicion
            |> List.head
    in
    case elemento of
        Nothing -> Err "Posición inválida"
        Just valor -> Ok valor


-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map funcion arbol =
    mapArbol funcion arbol


filter : (a -> Bool) -> Tree a -> Tree a
filter predicado arbol =
    filterArbol predicado arbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold funcion acumulador arbol =
    foldArbol funcion acumulador arbol


-- Conversiones
aLista : Tree a -> List a
aLista arbol =
    inorder arbol


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada lista =
    let
        listaOrdenada = List.sort lista
        construir : List comparable -> Tree comparable
        construir xs =
            let
                len = List.length xs
            in
            if len == 0 then
                Empty
            else
                let
                    medio = len // 2
                    izqLista = List.take medio xs
                    derLista = List.drop (medio + 1) xs
                in
                case xs |> List.drop medio |> List.head of
                    Nothing -> Empty
                    Just v -> Node v (construir izqLista) (construir derLista)
    in
    construir listaOrdenada
