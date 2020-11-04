
import System.IO


-- Información Comercial
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



existeParqueo :: [Parqueo] -> String -> Bool
existeParqueo [] _ = True
existeParqueo ((Parqueo nomE _ _ _ _):xs) nomN
    | nomE == nomN = False
    | otherwise = existeParqueo xs nomN



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



existeBicicleta :: [Bicicleta] -> String -> Bool
existeBicicleta [] _ = True
existeBicicleta ((Bicicleta idE _):xs) idN
    | idE == idN = False
    | otherwise = existeBicicleta xs idN


splitEnvolt :: String -> [String]
splitEnvolt str = funcionSplit(str, "")


funcionSplit :: ([Char],[Char]) -> [[Char]]
funcionSplit (str,dato) = 
    if str == "" then [dato]
    else
        if(head str) == (head ",") then
            [dato] ++ funcionSplit((tail str),"")
        else
            funcionSplit((tail str),dato++[(head str)])


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
        "1" -> menuOp--mostrarParqueos
        "2" -> menuOp--mostrarBicicletas
        "3" -> menuOp--mostrarUsuarios
        "4" -> menuOp--estadisticas
        "5" -> return ()--volver
        _ -> menuOp--error "Entrada Invalida"

main = menuOp
