
module Operativas where

import System.IO



nombre = "Alquiler Bici Laita"
web = "laita.com"
telefono = "27691234"
tarifaPedal = 500
tarifaElectrico = 1000



archParqueos = "datos/parqueos.txt"
archBicicletas = "datos/bicicletas.txt"
archUsuarios = "datos/usuarios.txt"
archAlquileres = "datos/alquileres.txt"
archFacturas = "datos/facturas.txt"


data Bicicleta =  Bicicleta String String

data Parqueo = Parqueo String String Float Float [Bicicleta]

data Provincia = Provincia String [Parqueo]


-- Entrada: 
-- Salida: 
-- Objetivo
pedirRutas :: IO [Provincia]
pedirRutas = do
    putStr "Ingrese la ruta al archivo de parqueos: "
    rutaP <- getLine
    putStr "Ingrese la ruta al archivo de bicicletas: "
    rutaB <- getLine
    putStr "Ingrese la ruta al archivo de usuarios: "
    rutaU <- getLine
    formarProvincias archParqueos rutaP archBicicletas rutaB


-- leer archivos de base
-- leer archivos ingresados

-- funcion recursiva con 7 parámetros acumulativos, para cada parqueo...
-- * pero por cada parqueo, recorrer y adjuntar todas las bicicletas correspondientes

-- Entrada: 
-- Salida: 
-- Objetivo
formarProvincias :: FilePath -> FilePath -> FilePath -> FilePath -> IO [Provincia]
formarProvincias parqPers parqIngr biciPers biciIngr =
    do
        ppFile <- openFile parqPers ReadMode
        ppContents <- hGetContents ppFile
        let parqueosP = map splitEnvolt (lines ppContents)

        piFile <- openFile parqIngr ReadMode
        piContents <- hGetContents piFile
        let parqueosI = map splitEnvolt (lines piContents)

        bpFile <- openFile biciPers ReadMode
        bpContents <- hGetContents bpFile
        let bicicletasP = map splitEnvolt (lines bpContents)

        biFile <- openFile biciIngr ReadMode
        biContents <- hGetContents biFile
        let bicicletasI = map splitEnvolt (lines biContents)
        
        return (go parqueosP parqueosI bicicletasP bicicletasI) where
            go pP pI bP bI =
                aux [] [] [] [] [] [] [] pP where
                aux :: [Parqueo] -> [Parqueo] -> [Parqueo] -> [Parqueo] -> [Parqueo] -> [Parqueo] -> [Parqueo] -> [[String]] -> [Provincia]
                aux li pu gu sj al ca he [] =
                    [Provincia "LI" li, Provincia "PU" pu, Provincia "GU" gu, Provincia "SJ" sj, Provincia "AL" al, Provincia "CA" ca, Provincia "HE" he]
                aux li pu gu sj al ca he (x:xs) =
                    if length x /= 5 then
                        aux li pu gu sj al ca he xs
                    else
                        case x!!2 of
                            "LI" -> aux ((formarParqueo x bP bI):li) pu gu sj al ca he xs
                            "PU" -> aux li ((formarParqueo x bP bI):pu) gu sj al ca he xs
                            "GU" -> aux li pu ((formarParqueo x bP bI):gu) sj al ca he xs
                            "SJ" -> aux li pu gu ((formarParqueo x bP bI):sj) al ca he xs
                            "AL" -> aux li pu gu sj ((formarParqueo x bP bI):al) ca he xs
                            "CA" -> aux li pu gu sj al ((formarParqueo x bP bI):ca) he xs
                            "HE" -> aux li pu gu sj al ca ((formarParqueo x bP bI):he) xs






-- Entrada: 
-- Salida: 
-- Objetivo
formarParqueos :: FilePath -> FilePath -> FilePath -> FilePath -> IO [Parqueo]
formarParqueos parqPers parqIngr biciPers biciIngr =
    do
        ppFile <- openFile parqPers ReadMode
        ppContents <- hGetContents ppFile
        let parqueosP = map splitEnvolt (lines ppContents)

        piFile <- openFile parqIngr ReadMode
        piContents <- hGetContents piFile
        let parqueosI = map splitEnvolt (lines piContents)

        bpFile <- openFile biciPers ReadMode
        bpContents <- hGetContents bpFile
        let bicicletasP = map splitEnvolt (lines bpContents)

        biFile <- openFile biciIngr ReadMode
        biContents <- hGetContents biFile
        let bicicletasI = map splitEnvolt (lines biContents)
        
        return (go parqueosP parqueosI bicicletasP bicicletasI) where
            go pP pI bP bI =
                aux pP [] where
                aux :: [[String]] -> [Parqueo] -> [Parqueo]
                aux [] p = aux2 pI p where
                    aux2 :: [[String]] -> [Parqueo] -> [Parqueo]
                    aux2 [] p = p
                    aux2 (x:xs) p
                        | length x /= 5 || existeParqueo p (x!!0)  = aux xs p
                        | otherwise = aux2 xs ((formarParqueo x bP bI):p)
                aux (x:xs) p
                    | length x /= 5 = aux xs p
                    | otherwise = aux xs ((formarParqueo x bP bI):p)


-- Entrada: 
-- Salida: 
-- Objetivo
existeParqueo :: [Parqueo] -> String -> Bool
existeParqueo [] _ = True
existeParqueo ((Parqueo nomE _ _ _ _):xs) nomN
    | nomE == nomN = False
    | otherwise = existeParqueo xs nomN


-- Entrada: 
-- Salida: 
-- Objetivo
formarParqueo :: [String] -> [[String]] -> [[String]] -> Parqueo
formarParqueo (nombre:direccion:x:y:empty) bicicletasP bicicletasI =
    Parqueo nombre direccion (read x :: Float) (read y :: Float) (aux bicicletasP []) where
        aux :: [[String]] -> [Bicicleta] -> [Bicicleta]
        aux [] p = aux2 bicicletasI p where
            aux2 :: [[String]] -> [Bicicleta] -> [Bicicleta]
            aux2 [] p = p
            aux2 (x:xs) p
                | length x /= 3 || nombre /= (x!!2) || existeBicicleta p (x!!0) = aux xs p
                | otherwise = aux2 xs ((Bicicleta (x!!0) (x!!1)):p)
        aux (x:xs) p
            | length x /= 3 || nombre /= (x!!2) = aux xs p
            | otherwise = aux xs ((Bicicleta (x!!0) (x!!1)):p)


-- Entrada: 
-- Salida: 
-- Objetivo
existeBicicleta :: [Bicicleta] -> String -> Bool
existeBicicleta [] _ = True
existeBicicleta ((Bicicleta idE _):xs) idN
    | idE == idN = False
    | otherwise = existeBicicleta xs idN

-- Entrada: 
-- Salida: 
-- Objetivo
splitEnvolt :: String -> [String]
splitEnvolt str = funcionSplit(str, "")


-- Entrada: 
-- Salida: 
-- Objetivo
funcionSplit :: ([Char],[Char]) -> [[Char]]
funcionSplit (str,dato) = 
    if str == "" then [dato]
    else
        if(head str) == (head ",") then
            [dato] ++ funcionSplit((tail str),"")
        else
            funcionSplit((tail str),dato++[(head str)])


-- Entrada: 
-- Salida: 
-- Objetivo
mostP :: IO ()
mostP = do
    putStr "Ingrese una provincia: "
    prov <- getLine
    return ()

-- Entrada: 
-- Salida: 
-- Objetivo
mostB :: IO ()
mostB = do
    putStr "Ingrese un parqueo: "
    parq <- getLine
    return ()

-- Entrada: 
-- Salida: 
-- Objetivo
mostU :: IO ()
mostU = do
    putStr "Ingrese una cedula: "
    usua <- getLine
    return ()

-- Entrada: 
-- Salida: 
-- Objetivo
stat :: IO ()
stat = do
    putStr "\n-- Estadisticas --\n\
    \1. Top 5 usuarios con mas viajes\n\
    \2. Top 5 parqueos con mas viajes\n\
    \3. Top 3 bicicletas con mas km recorridos\n\
    \4. Resumen\n\
    \5. Volver\n\n\
	\-> "
    opt <- getLine
    case opt of
        "1" -> stat--mostrarParqueos
        "2" -> stat--mostrarBicicletas
        "3" -> stat--mostrarUsuarios
        "4" -> stat--estadisticas
        "5" -> return ()--volver
        _ -> stat--error "Entrada Invalida"

-- Entrada: 
-- Salida: 
-- Objetivo
menuOp :: IO ()
menuOp = do
    putStr "\n-- Menu Operativo --\n\
    \1. Mostrar parqueos\n\
    \2. Mostrar bicicletas\n\
    \3. Mostrar usuarios\n\
    \4. Estadisticas\n\
    \5. Volver\n\n\
	\-> "
    opt <- getLine
    case opt of
        "1" -> do mostP--mostrarParqueos
                  menuOp
        "2" -> do mostB--mostrarBicicletas
                  menuOp
        "3" -> do mostU--mostrarUsuarios
                  menuOp
        "4" -> do stat--estadisticas
                  menuOp
        "5" -> return ()--volver
        _ -> menuOp--error "Entrada Invalida"
