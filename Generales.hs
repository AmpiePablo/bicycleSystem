import System.IO
import Data.List
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
-- Datos generales de la empresa
nombre = "Alquiler Bici Laita"
web = "laita.com"
telefono = "27691234"
tarifaPedal = 500
tarifaElectrico = 1000


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
-- *****************Data para el manejo de alquileres****************************
-- ******************************************************************************
type IdAlquiler = Integer
type Estado = String
type IdUsuario = Integer
type IdBicicleta = String
type TipoBici = String
type Origen = String -- **Esto tiene que ser un objeto(para consultar sobre todo lo que tenga el objeto)
type Destino = String -- **Objeto parqueo (para calcular los km recorrido y los precios en factura)
data Alquiler = Alquiler IdAlquiler Estado IdUsuario IdBicicleta TipoBici Origen Destino
crearAlquiler(dato) = Alquiler (read (dato!!0)::Integer) (dato!!1) (read(dato!!2)::Integer) (dato!!3) (dato!!4) (dato!!5) (dato!!6) 
-- funciones gets de los "atributos" de la data Alquiler
-- *********************getIdAlquiler**********
-- ********************************************
getIdAlquiler :: Alquiler -> Integer
getIdAlquiler (Alquiler idAlquiler _ _ _ _ _ _) = idAlquiler;
getIdUsuario :: Alquiler -> Integer
getIdUsuario (Alquiler _ _ idUsuario _ _ _ _ ) = idUsuario;
getTipoBici :: Alquiler -> String
getTipoBici (Alquiler _ _ _ _ tipoBici _ _) = tipoBici;
getEstado :: Alquiler -> String
getEstado (Alquiler _ estado _ _ _ _ _) = estado;
getIdBicicleta :: Alquiler -> String
getIdBicicleta (Alquiler _ _ _ idBicicleta _ _ _) = idBicicleta; 
getOrigen :: Alquiler -> String
getOrigen (Alquiler _ _ _ _ origen _ _) = origen;
getDestino :: Alquiler -> String
getDestino (Alquiler _ _ _ _ _ _ destino) = destino;
-- ******************************************************************************
-- ****estructura lista 


-- Entrada: lista de alquileres y un string vacio
-- Salida: retorna un string con todos los alquileres
-- objetivo: Tener un string todos los alquileres para agregarlos al txt
cambiarAlquiler :: [Alquiler] -> String -> String
cambiarAlquiler [] s = s
cambiarAlquiler alquileres string = do
    let id = getIdAlquiler (head alquileres)
    let estado = getEstado (head alquileres)
    let cedUsuario = getIdUsuario(head alquileres)
    let origen = getOrigen(head alquileres)
    let destino = getDestino(head alquileres)
    let codigoBici = getIdBicicleta(head alquileres)
    let tipoBici = getTipoBici(head alquileres)
    let nuevo = show id ++ "," ++ estado ++ "," ++ show cedUsuario ++ "," ++ origen ++ "," ++ destino ++ "," ++ codigoBici ++ "," ++ tipoBici ++ "\n"
    cambiarAlquiler (tail alquileres) (string ++ nuevo)

--Escribe en un archivo, para cuando se necesite actualizar un valor
--E: Recibe un string, el que se va a escribir en el archivo, y la ruta del archivo
--Salida:El archivo escrito
reescribirAlquileres :: String ->FilePath-> IO ()
reescribirAlquileres datos fileName= do
    removeFile fileName
    writeFile fileName datos
    return ()


-- Entrada: una data alquiler
-- Salida: Muestra en pantalla los datos asociados a una data alquiler
-- Objetivo: Mostrar los datos de diferentes objetos de tipo Alquiler
muestraAlquiler :: Alquiler -> IO ()
muestraAlquiler alquiler =
    let
        id = getIdAlquiler(alquiler)
        estado = getEstado(alquiler)
        cedUsuario = getIdUsuario(alquiler)
        parqueoSalida = getOrigen(alquiler)
        parqueoLlegada = getDestino(alquiler)
        codigoBici = getIdBicicleta(alquiler)
        tipoBici = getTipoBici(alquiler)
    in
        if estado == "activo" then
            putStr("ID: " ++ show id ++ ", estado: " ++ estado ++ ", cedula: " ++ show cedUsuario ++ ", parqueo llegada: " ++ parqueoLlegada ++ ", parqueo salida: " ++ parqueoSalida ++ ", Codigo bici: " ++ codigoBici ++ ", Tipo: " ++ tipoBici ++ "\n")
        else
            putStr ("")

-- **muestra los alquileres
-- Entrada: Una lista de alquileres
-- Salida: la información detallada de todos los alquileres 
-- Objetivo: Tomar la lista de alquileres y recorrer la lista, para así poder mostrar los datos relacionada a cada uno de los alquileres
muestrarAlquileres :: [Alquiler] -> IO()
muestrarAlquileres [] = return ()
muestrarAlquileres xs = do
    muestraAlquiler (head xs)
    muestrarAlquileres (tail xs)

-- Separa las lineas de alquileres, y crea los alquileres
-- Entrada: Una lista de strings
-- Salida: los alquileres separados y por comas, y convertidos en objetos alquileres
-- Objetivo: Recorrer toda la lista de caracteres que hacen los objetos alquileres
separaAlquileres :: [[Char]] -> [Alquiler]
separaAlquileres xs = 
    if null(xs) then []
    else
        [crearAlquiler(funcionSplit ((head xs),""))] ++ separaAlquileres(tail xs)

-- **Leer los alquileres que hay 
-- Entrada: una ruta de un archivo
-- Salida: una lista de alquileres
-- Objetivo: leer un archivo de texto plano y derivar a funciones para crear objetos de tipo alquiler
leerAlquileres :: FilePath -> IO[Alquiler]
leerAlquileres archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let alquileres =  separaAlquileres(lines contenido)
    --hClose file
    return alquileres

-- Entrada: toma una lista de alquileres y un identificador
-- Salida: la lista de alquileres con el estado del alquiler asociado a la identificador cambiado
-- Objetivo: recorrer la lista de alquileres y encontrar un alquiler y cambiar su estado
cambiarEstado ::[Alquiler] -> Integer->[Alquiler]
cambiarEstado alquileres id = do
    let codigoActual = getIdAlquiler(head alquileres)
    if id == codigoActual then do
        let id2 = getIdAlquiler(head alquileres)
        let cedUsuario = getIdUsuario(head alquileres)
        let parqueoSalida = getOrigen(head alquileres)
        let parqueoLlegada = getDestino(head alquileres)
        let codigoBici = getIdBicicleta(head alquileres)
        let tipoBici = getTipoBici(head alquileres)
        let nuevo = crearAlquiler ([show id2,"facturado",show cedUsuario,parqueoSalida,parqueoLlegada,codigoBici,tipoBici])
        [nuevo] ++ (tail alquileres)
    else
        [head alquileres] ++ cambiarEstado (tail alquileres) id



-- *****************Data para el manejo de Factura*******************************
-- ******************************************************************************
type IdFactura = Integer
type KmTotal = Double -- Calculado de Origen-Destino de alquiler, se busca sobre el binary tree
type TarifaKm = Double -- Se busca el tipo de bici 
type Total = Double
type AlquilerF = Integer
data Factura = Factura IdFactura KmTotal TarifaKm Total AlquilerF

crearFactura(elemento) = Factura (read (elemento!!0) :: Integer) (read (elemento!!1)::Double) (read (elemento!!2)::Double) (read (elemento!!3)::Double) (read (elemento!!4) :: Integer)
-- *************Funciones get de los "atributos" de la data Factura
-- *****************getIdFactura***************
-- ********************************************
getIdFactura :: Factura -> Integer
getIdFactura (Factura idFactura _ _ _ _) = idFactura;

-- *****************getKmTotal*****************
-- ********************************************
getKmTotal :: Factura -> Double
getKmTotal (Factura _ kmTotal _ _ _) = kmTotal;

-- *****************getTarifaKm****************
-- ********************************************
getTarifaKm :: Factura -> Double
getTarifaKm (Factura _ _ tarifaKm _ _) = tarifaKm;

-- *****************getTotal*******************
-- ********************************************
getTotal :: Factura -> Double
getTotal (Factura _ _ _ total _) = total;

-- *****************getAlquilerF***************
-- ********************************************
getAlquilerF :: Factura -> Integer
getAlquilerF (Factura _ _ _ _ alquilerF) = alquilerF;
-- ******************************************************************************
-- ************
-- Entrada: una lista de facturas y un res
-- Salida: toma la lista de facturas y las convierte en string
-- Objetivo: Tomar las facturas y aplanarlas
cambiarFactura :: [Factura] -> String -> String
cambiarFactura [] s = s
cambiarFactura facturas string = do
    let id = getIdFactura(head facturas)
    let kmTotal = getKmTotal(head facturas)
    let tarifaTotal = getTarifaKm(head facturas)
    let total = getTotal(head facturas)
    let alquilerF = getAlquilerF(head facturas)
    let nuevo = show id ++ "," ++ show kmTotal ++ "," ++  show tarifaTotal ++ "," ++ show total ++ "," ++ show alquilerF ++ "\n"
    cambiarFactura (tail facturas) (string ++ nuevo)


-- Entrada: Una factura
-- Salida: muestra los datos de una factura
-- Objetivo: toma una factura y con diferentes metodos get, muestra su información
--Muestra una factura
muestraFactura :: Factura -> IO ()
muestraFactura factura =
    let
        id = getIdFactura(factura)
        kmTotal = getKmTotal(factura)
        tarifaTotal = getTarifaKm(factura)
        total = getTotal(factura)
        alquilerF = getAlquilerF(factura)
    in
        putStr("\tID: " ++ show id ++ "\n\tTotal de kilometros: " ++ show kmTotal ++ "\n\tTarifa por bicicleta: " ++ show tarifaTotal ++ "\n\tTotal: " ++ show total ++ "\n\tCódigo de alquiler: " ++ show alquilerF ++ "\n")

-- Entrada: una lista de facturas
-- Salida: deriva a otra funcion para mostrar información
-- Objetivo: recorrer la lista de facturas, para mostrar su información
-- Muestra todos los alquilres
mostrarFacturas :: [Factura] -> IO()
mostrarFacturas [] = return ()
mostrarFacturas xs = do
    muestraFactura (head xs)
    mostrarFacturas (tail xs)

-- Entrada: Una lista de char
-- Salida: una lista de facturas
-- Objetivo: Crear tipos de datos facturas y hacer un spli
---Separa las facturas del txt y crea nuevas facturas
separaFacturas :: [[Char]] -> [Factura]
separaFacturas xs = 
    if null(xs) then []
    else
        [crearFactura(funcionSplit ((head xs),""))] ++ separaFacturas(tail xs)

-- Entrada: un fichero
-- Salida: el fichero leido y una lista de facturas
-- Objetivo: leer un fichero de txt y crear objetos de facturas
leerFacturas :: FilePath -> IO[Factura]
leerFacturas archivo = do
    file <- openFile archivo ReadWriteMode
    contenido <- hGetContents file
    let facturas =  separaFacturas(lines contenido)
    return facturas

-- Entrada: 
-- Salida: 
-- Objetivo
reescribirFacturas :: String ->FilePath-> IO ()
reescribirFacturas datos fileName= do
    removeFile fileName
    writeFile fileName datos
    return ()
------Funcionalidades implementadas
------------------------------------

-- Entrada: 
-- Salida: 
-- Objetivo
consultarBicicletas () = do
    putStr "\nDigite el valor de X en el plano cartesino: "
    valorX <- getLine
    let valorXInt = (read valorX::Integer)

    putStr "\nDigite el valor de X en el plano cartesino: "
    valorY <- getLine
    let valorYInt = (read valorY::Integer)

    print valorXInt
    print valorYInt

-- Entrada: 
-- Salida: 
-- Objetivo
alquilarServicio () = do
    putStr "\nDigite la cedula del usuario: "
    cedula <- getLine
    let cedulaInt = (read cedula::Integer)

    putStr "\ndigite el punto de salida\n"
    putStr "Valor de X: "
    valorX <- getLine

    putStr "\n digite el punto de salida\n"
    putStr "Valor de Y: "
    valorY <- getLine
    let valorIntX = (read valorX::Integer)
    let valorIntY = (read valorY::Integer)


    putStr "\nDigite el parqueo de llegada: "
    parqueo <- getLine
    let parqueoStr = (read $ show parqueo::String)
    
    print cedulaInt
    print valorIntX
    print valorIntY
    putStr parqueoStr


-- Entrada: 
-- Salida: 
-- Objetivo
agregarFactura :: Factura -> IO ()
agregarFactura factura = do
    let id = getIdFactura(factura)
    let kmTotal = getKmTotal(factura)
    let tarifaTotal = getTarifaKm(factura)
    let total = getTotal(factura)
    let alquilerF = getAlquilerF(factura)
    let string = show id ++ "," ++ show kmTotal ++ "," ++  show tarifaTotal ++ "," ++ show total ++ "," ++ show alquilerF ++ "\n"
    return ()

-- E:Una lista de alquileres
-- S:Un alquiler facturada
-- O:Buscar un alquiler en la lista de alquileres y facturarlo
facturarAlquiler :: [Alquiler] ->Integer->Integer -> IO[Factura]
facturarAlquiler [] i l= do
    putStr("No hay alquileres por facturar")
    return []
facturarAlquiler alquiTotal id largo= do
    let alquilerActual = getIdAlquiler(head alquiTotal)
    if id == alquilerActual then do
        let estado = getEstado(head alquiTotal)
        if estado == "facturado" then do
            putStr "\nEl alquiler ya fue facturado\n"
            return []
        else do
            let tipoBici = getTipoBici(head alquiTotal)
            if tipoBici == "TR" then 
                return [facturarAlquilerAux(head alquiTotal) 500 largo]
            else 
                return [facturarAlquilerAux(head alquiTotal) 700 largo]
    else
        facturarAlquiler (tail alquiTotal) id largo

-- Entrada: 
-- Salida: 
-- Objetivo
facturarAlquilerAux :: Alquiler -> Double -> Integer ->Factura
facturarAlquilerAux alquilerActual tarifa idFactura = do
    let idAlquiler = getIdAlquiler(alquilerActual)
    let origen = getOrigen(alquilerActual)
    let destino = getDestino(alquilerActual)
    let kmTotal = 15
    let total = kmTotal * tarifa
    crearFactura [show idFactura, show kmTotal, show total, show idAlquiler]

-- Entrada: una lista de alquileres, una lista de facturas, un enteroo
-- Salida: Retorna diferentes funciones para completar el proceso de facturación
-- Objetivo: Facturar un alquiler
facturar :: [Alquiler] ->[Factura] ->Integer -> IO()
facturar alquileres facturas idFactura = do
    putStr "\nDigite el código de alquiler que desea facturar: "
    idAlquiler <- getLine
    let idAlquilerInt = (read idAlquiler::Integer)
    nueva <- facturarAlquiler alquileres idAlquilerInt idFactura
    if null(nueva) then
        putStr "No se facturó"
    else do
        let nuevasFacturas = facturas ++ nueva
        let nuevosAlquileres = cambiarEstado alquileres idAlquilerInt
        let alquileresEscribir = cambiarAlquiler nuevosAlquileres ""
        let facturasEscribir = cambiarFactura nuevasFacturas ""

        reescribirAlquileres alquileresEscribir "alquilerFacturado.txt"
        agregarFactura (head nueva) 
        putStr "Facturado"
        putStr "\n--------------------------------------"
        putStr ("\nEmpresa: " ++ nombre)
        putStr ("\nSitio Web: " ++ web)
        putStr ("\nContacto: " ++ telefono)
       -- muestraFactura (head nueva)


    
-- E:Una lista de facturas,  y una factura a buscar
-- S:Los datos de una factura
-- O: Recorrer la lista de facturas hasta encontrar la factura deseada y retornar su información
consultarFactura :: [Factura] ->Integer ->IO()
consultarFactura [] i = do
    putStr "No hay facturas por consultar"
    return ()
consultarFactura lista idFacturaInt = do
    let codigoActual = getIdFactura(head lista)
    if idFacturaInt == codigoActual then
        muestraFactura (head lista)
    else
        consultarFactura (tail lista) idFacturaInt

-- Entrada: Una lista de facturas
-- Salida: El largo de esa lista de facturas 
-- Objetivo: Tomar el largo sde la factura y convertirlo en el identificador de una factura nueva
largoFactura :: [Factura] -> Integer
largoFactura facturas=
    1+largoFactura(tail facturas)

-- Entrada: Una opcion que el usuario deseee completar de las generales
-- Salida: retorna diferente funcionalidades para completar el uso
-- Objetivo: Solicitar informacioón al usuario para poder llevar a cabo los funcionalidades generales de sistema
menuGeneral (alqui,facturas) = do
    putStr "\n|--------OPCIONES GENERALES DEL SISTEMA--------|\n"
    putStr "\n\tMENÚ PRINCIPAL\n"
    putStr "1. CONSULTAR BICICLETAS\n"
    putStr "2. ALQUILER SERVICIO\n"
    putStr "3. FACTURAR ALQUILER\n"
    putStr "4. CONSULTAR FACTURA\n"
    putStr "5. VOLVER\n"
    putStr "Digite la opción que desea realizar: "
    digito <- getLine
    let digitoInt = (read digito::Integer)
    case digitoInt of 
        1 -> do
            putStr("\t|=====CONSULTA DE BICICLETAS=====|\n")
            consultarBicicletas()
            menuGeneral(alqui,facturas)
        2-> do
            putStr("\t|=====ALQUILER DE BICICLETAS=====|\n")
            alquilarServicio()
            menuGeneral(alqui,facturas)
        3 -> do
            putStr("\t|=====FACTURACIÓN DE ALQUILERES=====|\n")
            let largo = largoFactura facturas
            facturar alqui facturas largo
            menuGeneral(alqui,facturas)
        4 -> do
            putStr("\t|=====CONSULTAR FACTURA=====|")
            putStr "\nDigite el código de factura que desea consultar: "
            codigo <- getLine
            let facturaInt = (read codigo::Integer)
            consultarFactura facturas facturaInt
            menuGeneral(alqui,facturas)
        5 -> do
            putStr("¡Buena Suerte!")
            return()
            

-- Entrada: nada
-- Salida: menu de opciones
-- Objetivo: Leer archivos, formar estructuras
-- *****Menu principal 
menuAux () = do 
    alqui <- leerAlquileres "alquiler.txt"
    facturas <- leerFacturas "factura.txt"
    print("Este es el menu general")
    algo <- menuGeneral (alqui,facturas)
    return algo
