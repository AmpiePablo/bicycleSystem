import System.IO
-- *****************Data para el manejo de alquileres****************************
-- ******************************************************************************
type IdAlquiler = Integer
type Estado = String
type IdBicicleta = String
type Origen = String -- **Esto tiene que ser un objeto(para consultar sobre todo lo que tenga el objeto)
type Destino = String -- **Objeto parqueo (para calcular los km recorrido y los precios en factura)
data Alquiler = Alquiler IdAlquiler Estado IdBicicleta Origen Destino

-- funciones gets de los "atributos" de la data Alquiler
-- *********************getIdAlquiler**********
-- ********************************************
getIdAlquiler :: Alquiler -> Integer
getIdAlquiler (Alquiler idAlquiler _ _ _ _) = idAlquiler;
-- *********************getEstado**************
-- ********************************************
getEstado :: Alquiler -> String
getEstado (Alquiler _ estado _ _ _) = estado;
-- *********************getIdBicicleta*********
-- ********************************************
getIdBicicleta :: Alquiler -> String
getIdBicicleta (Alquiler _ _ idBicicleta _ _) = idBicicleta; 
-- *********************getOrigen**************
-- ********************************************
getOrigen :: Alquiler -> String
getOrigen (Alquiler _ _ _ origen _) = origen;
-- *********************getDestino*************
-- ********************************************
getDestino :: Alquiler -> String
getDestino (Alquiler _ _ _ _ destino) = destino;
-- ******************************************************************************
-- ****estructura lista 


-- *****************Data para el manejo de Factura*******************************
-- ******************************************************************************
type IdFactura = Integer
type KmTotal = Double -- Calculado de Origen-Destino de alquiler, se busca sobre el binary tree
type TarifaKm = Double -- Se busca el tipo de bici 
type Total = Double
type AlquilerF = Integer
data Factura = Factura IdFactura KmTotal TarifaKm Total AlquilerF

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
-- ************Estructura lista

consultarBicicletas () = do
    putStr "\nDigite el valor de X en el plano cartesino: "
    valorX <- getLine
    let valorXInt = (read valorX::Integer)

    putStr "\nDigite el valor de X en el plano cartesino: "
    valorY <- getLine
    let valorYInt = (read valorY::Integer)

    print valorXInt
    print valorYInt

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

facturarAlquiler () = do
    putStr "\nDigite el código de alquiler que desea facturar: "
    idAlquiler <- getLine
    let idAlquilerInt = (read idAlquiler::Integer)
    print idAlquilerInt

consultarFactura () = do
    putStr "\nDigite el código de factura que desea consultar: "
    idFactura <- getLine
    let idFacturaInt = (read idFactura::Integer)
    print idFacturaInt

menuGeneral () = do
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
            menuGeneral()
        2-> do
            putStr("\t|=====ALQUILER DE BICICLETAS=====|\n")
            alquilarServicio()
            menuGeneral()
        3 -> do
            putStr("\t|=====FACTURACIÓN DE ALQUILERES=====|")
            facturarAlquiler()
            menuGeneral()
        4 -> do
            putStr("\t|=====CONSULTAR FACTURA=====|")
            consultarFactura()
            menuGeneral()
        5 -> do
            putStr("¡Buena Suerte!")
            menuAux()
            

-- *****Menu principal 
menuAux () = do 
    putStr "\n|--------Mantenimiento de alquileres de bicicletas--------|\n"
    putStr "\nMenú principal\n"
    putStr "1. Opciones operativas\n"
    putStr "2. Opciones Generales\n"
    putStr "3. Salir\n"
    putStr "Digite la opción que desea realizar: "
    opcion <- getLine
    let opcionInt = (read opcion::Integer)
    case opcionInt of 
        1 -> do
            print("Este es el menu operativo")
            menuAux()
        2 -> do
            print("Este es el menu general")
            menuGeneral()
        3 -> do
            print("chao")
            return ()