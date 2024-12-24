import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

-- Type declarations
type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- (PT) cities: Recebe como argumento o mapa (roadMap) e passa pelas todas as possibilidades de caminhos (edge) para retirar as cidades repetidas e colocá-la em uma lista.
-- (EN) cities: Takes as an argument a city map graph (roadMap) and goes through all the edges to collect each of the cities in a list with no duplicates.
cities :: RoadMap -> [City]
cities [] = []
cities roadMap = Data.List.nub (concat [[city1, city2] | (city1, city2, _) <- roadMap])

-- (PT) areAdjacent: Recebe como argumento o mapa (roadMap) e duas cidades dadas pelo utilizador (cidade1, cidade2) para conferir se existe um caminho (edge) entre eles e, se sim,
-- (PT) analizar se eles são vizinhos, devolvendo um booleano.
-- (EN) areAdjacent: Takes as arguments a city map graph (roadMap) and two cities provided by the user (cidade1, cidade2) to check if there exists a direct path (edge) between those two
-- (EN) if found, the function will return True, else it will return False
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent _ [] _ = False
areAdjacent _ _ [] = False
areAdjacent roadMap cidade1 cidade2 = any (\(city1, city2, _) -> (city1 == cidade1 && city2 == cidade2) || (city1 == cidade2 && city2 == cidade1)) roadMap

-- (PT) distance: Recebe como argumento o mapa (roadMap) e duas cidades (cidade1, cidade2) para, por meio de recursividade, devolver a distãncia entre elas caso exista um caminho direto (areAdjacent).
-- (EN) distance (Recursive): Takes as arguments a city map graph (roadMap) and two cities provided by the user (cidade1, cidade2) to find the distance between them.
-- (EN) if found, the function will return Just distance, else it will return Nothing
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance _ [] _ = Nothing
distance _ _ [] = Nothing
distance ((city1,city2,dist):roadMap) cidade1 cidade2
    | areAdjacent [(city1, city2, dist)] cidade1 cidade2 = Just dist
    | otherwise = distance roadMap cidade1 cidade2

-- (PT) adjacent: Recebe o mapa (roadMap) e uma cidade para analisar quais são as cidades adjacentes, por meio de compreensão de listas, e devolve dentro de uma lista.
-- (EN) adjacent: Takes as argument a city map graph (roadMap) and a city (cidade) to collect the list of cities that are directly adjacent to that one, making use of list comprehensions
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent _ [] = []
adjacent roadMap cidade = [(city1, dist) | (city1, city2, dist) <- roadMap, city2 == cidade] ++ [(city2, dist) | (city1, city2, dist) <- roadMap, city1 == cidade]

-- (PT) pairCities: Função auxiliar utilizado em pathDistance
-- (PT) Cria pares consecutivamente por base do caminho recebido (xs) por zip e retorna uma lista de touples.
-- (EN) pairCities: Auxiliar function used in pathDistance
-- (EN) Creates consecutive pairs by taking a path as argument (xs) and returning a list of tuples.
pairCities :: [City] -> [(City,City)]
pairCities xs = zip xs (tail xs)

-- (PT) pairDistanceReturn: Função auxiliar utilizado em pathDistance
-- (PT) Núcleo principal da função pathDistance. Recebe o mapa (roadMap) e o resultante de pairCities (indicado acima) e calcula a distância total por meio de recusrividade e somatória,
-- (PT) ao devolver Just dist, caso exista o caminho, ou Nothing caso contrário.
-- (EN) pairDistanceReturn: Auxiliar function used in pathDistance
-- (EN) Main code for pathDistance. Takes as argument a city map graph (roadMap) and the result from pairCities (check above), calculating the total distance in that path using recursion.
-- (EN) if the path is fully traversed, it returns a Just distance, else it returns Nothing
pathDistanceReturn :: RoadMap -> [(City, City)] -> Maybe Distance
pathDistanceReturn _ [] = Just 0
pathDistanceReturn roadMap ((cidade1, cidade2):xs) =
    case distance roadMap cidade1 cidade2 of
        Nothing -> Nothing
        Just dist -> case pathDistanceReturn roadMap xs of
                          Nothing -> Nothing
                          Just dist_ -> Just (dist + dist_)

-- (PT) pathDistance: Recebe o mapa (roadMap) e o caminho (path) para calcular a sua distância total. Utiliza funções auxiliares pairCities e pathDistanceReturn.
-- (EN) pathDistance: Takes as argument a city graph map (roadMap) and a path (path) to calculate the total distance in that path. it makes use of auxiliar functions: pairCities and pathDistanceReturn
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing
pathDistance _ [] = Nothing
pathDistance roadMap path = pathDistanceReturn roadMap (pairCities path)

-- (PT) compareByDegree: Função de comparação para ordenar pelo segundo elemento de uma tuple
-- (EN) compareByDegree: Custom comparison function for sorting by the second element of a tuple
compareByDegree :: (Ord b) => (a, b) -> (a, b) -> Ordering
compareByDegree (_, degree1) (_, degree2) = compare degree1 degree2

-- (PT) highestDegree: Função auxiliar que recebe uma lista de tuples ([(city, (length (adjacent roadMap city)))]) e coleta o degree mais alto da cidade com mais adjacentes.
-- (EN) highestDegree: Auxiliar function that takes as argument a list of tuples ([(city, (length (adjacent roadMap city)))]) and collects the highest degree from all cities.
highestDegree :: (Ord a) => [(b, a)] -> a
highestDegree xs = snd (Data.List.maximumBy compareByDegree xs)

-- (PT) rome: Recebe o mapa (roadMap) para selecionar as cidades que têm o highest degree, usando uma compreensão de listas.
-- (EN) rome: Takes as argument a city graph map (roadMap) and collects the cities that have the highest degree making use of a list comprehension.
rome :: RoadMap -> [City]
rome [] = []
rome roadMap = [city | city <- cities roadMap, (length (adjacent roadMap city)) == (highestDegree ([(city, (length (adjacent roadMap city))) | city <- cities roadMap]))]

-- (PT) dfs: Depth-first search precebe o mapa (roadMap) para marcar as cidades como visitadas por método recusrivo, utilizado como função auxiliar em traverseAll.
-- (EN) dfs: Depth-first search takes a city graph map (roadMap) as an argument and marks all the visited cities using recursion, function to be used in traverseAll.
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap current visited
    | current `elem` visited = visited
    | otherwise = foldl (flip (dfs roadMap)) visitedList visitedCities
        where 
            visitedCities = map fst (adjacent roadMap current);
            visitedList   = current : visited;

-- (PT) traverseAll: Função auxiliar que recebe como argumento o mapa (roadMap), a ciade de início do caminho e a lista de todas as cidades (declarada quando invocada)
-- para checar se todas as cidades são alcançáveis a partir de um determinado ponto do mapa (start).
-- (EN) traverseAll: An auxiliary function that connects isStronglyConnected to the dfs function to check if all cities are reachable from a given starting city.
-- It takes the city map (roadMap), the starting city of the path, and all the cities as arguments.
traverseAll :: RoadMap -> City -> [City] -> Bool
traverseAll roadMap start cidades = length (dfs roadMap start []) == length cidades

-- (PT) isStronglyConnected: Função principal que verifica se o mapa fornecido (roadMap) é fortemente conectado, utilizando as funções auxiliares traverseAll e cities, e retorna um valor booleano.
-- (EN) isStronglyConnected: Main function to check if the cities are all strongly conected returning a boolean, using auxiliar functions as traverseAll and cities.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected roadMap = all (\city -> traverseAll roadMap city (cities roadMap)) (cities roadMap)

-- (PT) compareByDistance: Função de comparação para ordenar pelo primeiro elemento de uma tuple
-- (EN) compareByDistance: Custom comparison function for sorting by the first element of a tuple
compareByDistance :: (Ord a) => (a, b) -> (a, b) -> Ordering
compareByDistance (dist1, _) (dist2, _) = compare dist1 dist2

-- (PT) dijkstra: Recebe o mapa (roadMap), uma lista de tuples (Distance,Path), uma lista de visitados, lista de caminhos mais curtos e a cidade final para retornar
-- uma lista de caminhos da cidade inicial à final.
-- (EN) dijkstra: Takes as arguments a city graph map (roadMap), a list of tuples, a list of visited cities, a list of shortest paths, and the target city to return
-- a list of paths from source to target.
dijkstra :: RoadMap -> [(Distance, Path)] -> [City] -> [Path] -> City -> [Path]
dijkstra _ [] _ shortestPaths _ = shortestPaths
dijkstra roadMap ((dist, path):queue) visited shortestPaths target
    | (head path) == target      = dijkstra roadMap queue visited (path : shortestPaths) target
    | (head path) `elem` visited = dijkstra roadMap queue visited shortestPaths target
    | otherwise                  = dijkstra roadMap updatedQueue ((head path) : visited) shortestPaths target
        where
            adjacentCities = filter (\(city, _) -> city `notElem` visited) (adjacent roadMap (head path))
            newPaths       = [(dist + d, city : path) | (city, d) <- adjacentCities]
            updatedQueue   = Data.List.sortBy compareByDistance (queue ++ newPaths)

-- (PT) getShortestPaths: Levando como argumento o mapa (mroadMap) e duas cidades, é uma função auxiliar utilizado em shortestPath para retornar uma lista de caminhos mais curtos
-- filtrados por smallestDistance durante a varredura da lista de caminhos obtidos na execução de dijkstra. 
-- (EN) getShortestPaths: Auxiliar function that, taking as argument the city map (roadMap) and two cities, runs through the list collected by djikstra,
-- and returns those paths that have the same path from smallestDistance. 
getShortestPaths :: RoadMap -> City -> City -> [Path]
getShortestPaths roadMap source target = [path | path <- paths, pathDistance roadMap path == smallestDistance]
    where
        paths            = dijkstra roadMap [(0, [source])] [] [] target
        smallestDistance = minimum [pathDistance roadMap path | path <- paths]

-- (PT) shortestPath: Função principal para encontrar o caminho mais curto entre duas cidades fornecidas de um mapa (roadMap), utiliza getShortestPath e, para caso de
-- haver mais de um caminho com mesmas distâncias, restorna uma lista de caminhos.
-- (EN) shortestPath: Main function that returns a list of the shortest paths existing between two given cities in a map (roadMap) using auxiliary functions like getShortestPaths.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath [] _ _ = []
shortestPath _ [] _ = []
shortestPath _ _ [] = []
shortestPath roadMap source target = getShortestPaths roadMap target source

-- (PT) minimumByDistance: Recebe o mapa (roadMap) e uma lista de caminhos (paths) para selecionar aquele com distância menor (1. os dois têm distância | 2. pelo menos um não tem).
-- (EN) minimumByDistance: Takes as argument a city map graph (roadMap) and a list of paths (paths) to find the one with smallest distance
minimumByDistance :: RoadMap -> [Path] -> Path
minimumByDistance roadMap paths = foldr1 (\path1 path2 -> case (pathDistance roadMap path1, pathDistance roadMap path2) of
                           (Just distance1, Just distance2) -> if distance1 < distance2 then path1 else path2
                           _ -> path1
                           ) paths

-- (PT) isValidPath: Recebe o mapa (roadMap) e um caminho para verificar se este é válido, fazendo uso da função auxiliar pairCities (cria pares de cidades consecutivas).
-- (EN) isValidPath: Takes as argument a city map graph (roadMap) and a path to check if its valid through the use of pairCities.
isValidPath :: RoadMap -> Path -> Bool
isValidPath roadMap path = all (\(cidade1, cidade2) -> areAdjacent roadMap cidade1 cidade2) (pairCities path)

-- travelSales: Implements a solution to the Traveling Salesman Problem
-- (PT) travelSales: Recebe o mapa (roadMap) e calcula um caminho que alcance todas as cidades. A função começa por verificar se o grafo é fortemente conectado, e, 
-- se sim, gera todos os caminhos válidos a iniciarem-se a terminarem na mesma cidade e retorna aquela com menor distância.
-- (EN) travelSales: Takes as argument a city map graph (roadMap) and looks for an optimal path. If the graph is strongly connected, the function generates all valid
-- paths that start and end in the same city. It will return the one with the smallest distance.
travelSales :: RoadMap -> Path
travelSales [] = []
travelSales roadMap
    | not (isStronglyConnected roadMap) = []
    | otherwise =
        if null validPaths then [] else minimumByDistance roadMap validPaths
        where
            validPaths = filter (isValidPath roadMap) allPaths
            allCities  = cities roadMap
            allPaths   = [startCity : path ++ [startCity] | startCity <- allCities, path <- Data.List.permutations (filter (/= startCity) allCities)]

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- shortestPath
gTest4 = [("0","1",4),("1","2",6),("2","3",1),("0","4",6),("4","2",4)]

gTest5 :: RoadMap -- TSP
gTest5 = [("0","1",2),("1","2",2),("2","3",2),("0","3",6)]

gTest6 :: RoadMap -- complete
gTest6 = [("A", "B", 2), ("A", "C", 3), ("A", "D", 4), ("B", "C", 5), ("B", "D", 6), ("C", "D", 1)]

gTest7 :: RoadMap -- linear
gTest7 = [("1", "2", 3), ("2", "3", 2), ("3", "4", 4), ("4", "5", 1)]

gTest8 :: RoadMap -- cycle
gTest8 = [("X", "Y", 5), ("Y", "Z", 10), ("Z", "X", 3), ("X", "W", 6), ("W", "Y", 4)]

gTest9 :: RoadMap
gTest9 = [("City1", "City2", 7), ("City1", "City3", 10), ("City2", "City4", 5), ("City2", "City3", 3), ("City3", "City4", 1), ("City4", "City1", 8)]

gTest10 :: RoadMap -- sparse
gTest10 = [("Alpha", "Beta", 2), ("Alpha", "Gamma", 4), ("Beta", "Delta", 6), ("Gamma", "Delta", 1), ("Delta", "Epsilon", 3)]

gTest11 :: RoadMap -- multiple paths
gTest11 = [("Start", "A", 4), ("Start", "B", 2), ("A", "C", 5), ("B", "C", 1), ("C", "End", 3), ("A", "End", 7)]

gTest12 :: RoadMap -- unconnected
gTest12 = [("0","1",1),("1","2",2),("3","4",3),("5","6",4)]

gTest13 :: RoadMap -- same distance 
gTest13 = [("0", "1", 1), ("0", "2", 1), ("0", "3", 1), ("0", "4", 1),("1", "5", 1), ("2", "5", 1), ("3", "5", 1), ("4", "5", 1)]