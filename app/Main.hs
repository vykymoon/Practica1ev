import System.IO (hFlush, stdout, appendFile)
import Data.List (group, sort)
-- declaramos las varibles
type Nombre = String
type Categoria = String
type Producto = (Nombre, Categoria)
type Catalogo = [Producto]

-- Creamos el Menu
main :: IO ()
main = do
    let catalogo = []
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"
    menu catalogo

menu :: Catalogo -> IO ()
menu catalogo = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir"
    putStr "Opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> registrarArticulo catalogo
        "2" -> buscarPorCategoria catalogo
        "3" -> listarArticulos catalogo                     
        "4" -> mostrarCantidadPorCategoria catalogo
        "5" -> putStrLn "¡Hasta luego!"
        _   -> do
            putStrLn "Opción no válida, intente de nuevo."
            menu catalogo

-- Registrar un item en el catologo 
registrarArticulo :: Catalogo -> IO ()
registrarArticulo catalogo = do
    putStr "Ingrese el nombre del artículo: "
    hFlush stdout
    nombre <- getLine
    putStr "Ingrese la categoría del artículo: "
    hFlush stdout
    categoria <- getLine
    let nuevoCatalogo = (nombre, categoria) : catalogo
    appendFile "inventario.txt" ("Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}\n")
    putStrLn $ "Artículo " ++ nombre ++ " registrado en el inventario."
    putStrLn "Inventario guardado en el archivo inventario.txt."
    menu nuevoCatalogo

-- Buscar artículos pot su categorias
buscarPorCategoria :: Catalogo -> IO ()
buscarPorCategoria catalogo = do
    putStr "Ingrese la categoría a buscar: "
    hFlush stdout
    categoria <- getLine
    let articulos = filter (\(_, c) -> c == categoria) catalogo
    if null articulos
    then putStrLn "No se encontraron artículos en esta categoría."
    else do
        putStrLn "Artículos en el inventario:"
        mapM_ (putStrLn . mostrarArticulo) articulos
    menu catalogo

-- Listar los items
listarArticulos :: Catalogo -> IO ()
listarArticulos catalogo = do
    if null catalogo
    then putStrLn "No hay artículos en el catálogo."
    else do
        putStrLn "Artículos en el inventario:"
        mapM_ (putStrLn . mostrarArticulo) catalogo
    menu catalogo

-- para que printee de la manera que se pide
mostrarArticulo :: Producto -> String
mostrarArticulo (nombre, categoria) = "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\"}"

-- Mostrar cantidad items de derterminada categoria
mostrarCantidadPorCategoria :: Catalogo -> IO ()
mostrarCantidadPorCategoria catalogo = do
    putStr "Ingrese la categoría para contar artículos: "
    hFlush stdout
    categoria <- getLine
    let cantidad = length (filter (\(_, c) -> c == categoria) catalogo)
    putStrLn $ "Hay " ++ show cantidad ++ " artículos en la categoría " ++ categoria
    menu catalogo
