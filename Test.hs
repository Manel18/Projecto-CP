--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: 75369
-- Nome:  Diogo Figueiredo Pimenta
-- Curso: MIEI
--
-- Aluno 2
-- Número: 71646
-- Nome: Manuel Castro Freitas
-- Curso: MIEI
--

module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Control.Monad(replicateM)
import Data.Maybe

--
-- Teste unitário
--

g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

-- Um exemplo de um teste unitário.
test_adj :: Test
test_adj = adj g1 1 ~?= fromList [Edge 1 1]

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

grafoSimples :: Graph Int
grafoSimples = Graph (fromList[1,2]) (fromList[Edge 1 2])

testaSwap :: Test
testaSwap = TestList [ swap (Edge 1 2) ~?= Edge 2 1 ]

testaEmpty :: Test
testaEmpty = let g = Graph.empty :: Graph Int
                 f = Graph Set.empty Set.empty :: Graph Int
             in TestList [ g ~=? f]

testaIsEmpty :: Test
testaIsEmpty = TestList [ isEmpty Graph.empty ~?= True  ,
                          isEmpty grafoSimples ~?= False ]

testaIsValid :: Test
testaIsValid = let inv = Graph (fromList [1,2]) (fromList [Edge 3 2])
               in TestList [ isValid inv ~?= False ,
                             isValid grafoSimples ~?= True  ]

testaIsDAG :: Test
testaIsDAG = let inv = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3,Edge 3 1])
                 passa = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3])
             in TestList [ isDAG inv ~?= False ,
                           isDAG passa ~?= True  ]

testaIsForest :: Test
testaIsForest = let passa = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4])
                    inv = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 1 3,Edge 3 4,Edge 4 2])
                in TestList [ isForest passa ~?= True  ,
                              isForest inv ~?= False ]

testaIsSubgraphOf :: Test
testaIsSubgraphOf = let passaM = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2, Edge 2 3,Edge 3 4])
                        passam = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3])
                        invm = Graph (fromList [1,2,3,4,5]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4,Edge 4 5])
                        invmd = Graph (fromList [1,2,3]) (fromList [Edge 1 3,Edge 3 2])
                    in TestList [ isSubgraphOf passam passaM ~?= True   ,
                                  isSubgraphOf invm passaM ~?= False  ,
                                  isSubgraphOf invmd passaM ~?= False ]

testaAdj :: Test
testaAdj = let grafo = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3])
           in TestList [ adj grafo 1 ~?= fromList[Edge 1 2] ,
                         adj grafo 3 ~?= Set.empty]

testaTranspose :: Test
testaTranspose = let grafo = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3])
                     tgrafo = Graph (fromList [1,2,3]) (fromList [Edge 3 2,Edge 2 1])
                 in transpose grafo ~?= tgrafo

testaUnion :: Test
testaUnion = let grafoum = Graph (fromList [1,2,3]) (fromList [Edge 1 2,Edge 2 3])
                 grafodois = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4])
                 grafov = Graph.empty
                 graforesult = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4])
             in TestList [ Graph.union grafoum grafodois ~?= graforesult ,
                           Graph.union grafoum grafov ~?= grafoum        ]

testaReachable :: Test
testaReachable = let grafo = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4])
                 in TestList [ reachable grafo 1 ~?= fromList [1,2,3,4] ,
                               reachable grafo 3 ~?= fromList [3,4]     ]

testaIsPathOf :: Test
testaIsPathOf = let grafo = Graph (fromList [1,2,3,4]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4])
                in TestList [ isPathOf [] grafo ~?= True                   ,
                              isPathOf [Edge 1 2,Edge 2 3] grafo ~?= True  ,
                              isPathOf [Edge 1 2,Edge 3 4] grafo ~?= False ]

testaPath :: Test
testaPath = let grafo = Graph (fromList [1,2,3,4,5,6]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4,Edge 2 5,Edge 5 6, Edge 6 4])
            in TestList [ path grafo 1 4 ~?= Just [Edge 1 2,Edge 2 3,Edge 3 4] ,
                          path grafo 3 2 ~?= Nothing                           ,
                          path grafo 1 1 ~?= Just []                                ]

testaTopo :: Test
testaTopo = let grafo = Graph (fromList [1,2,3,4,5,6]) (fromList [Edge 1 2,Edge 2 3,Edge 3 4,Edge 2 5,Edge 5 6,Edge 6 4])
            in topo grafo ~?= [fromList [1],fromList [2],fromList [3,5],fromList [6],fromList [4]]

testabft :: Test
testabft = let grafo = Graph (fromList [1,2,3,4,5]) (fromList [Edge 1 3,Edge 1 2,Edge 4 2,Edge 5 3])
           in TestList [bft grafo (fromList [4,5]) ~?= Graph (fromList [2,3,4,5]) (fromList [Edge 2 4,Edge 3 5])  ,
                        bft Graph.empty (fromList []) ~?= (Graph.empty:: Graph Int)                               ,
                        bft grafo (fromList []) ~?= (Graph.empty :: Graph Int)                                    ]

main :: IO Counts
main = runTestTT $ TestList [test_adj,testaSwap,testaIsEmpty,testaIsValid,
                             testaEmpty,testaIsDAG,testaIsForest,testaIsSubgraphOf,
                             testaAdj,testaTranspose,testaUnion,testaReachable,
                             testaIsPathOf,testaPath,testaTopo,testabft]

{---
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--
---}
-- Instância de Arbitrary para arestas
instance (Arbitrary v, Eq v) => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary `suchThat` (/= s)
                   return Edge {source = s, target = t}

-- Funções auxiliares do gerador de grafos

removeDups :: Ord a => [a] -> Set a -> [a]
removeDups [] _ = []
removeDups (h:t) acc | member h acc = removeDups t acc
                     | otherwise = h:removeDups t (insert h acc)

arbtNodes :: Arbitrary a => Gen [a]
arbtNodes = listOf arbitrary

arbtGrauSaida :: Int -> Gen [Int]
arbtGrauSaida x = replicateM x (choose(0, x))

arbtEdge :: (Arbitrary v) => v -> [v] -> Gen (Edge v)
arbtEdge src l = do dst <- elements l
                    return (Edge src dst)

arbtEdges :: (Arbitrary v) => [v] -> [Int] -> Gen [Edge v]
arbtEdges ns gs = do m <- mapM (aux ns) (zip ns gs)
                     return (concat m)

aux :: (Arbitrary v) => [v] -> (v, Int) -> Gen [Edge v]
aux _ (_, 0) = return []
aux l (n, x) = do e <- arbtEdge n l
                  t <- aux l (n, x-1)
                  return (e:t)


juntaTudo :: [Gen [Edge v]] -> Gen [Edge v]
juntaTudo [] = return []
juntaTudo (x:xs) = do k <- x
                      t <- juntaTudo xs
                      return (k++t)

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = aux `suchThat` isValid
        where aux = do ns <- listOf arbitrary
                       gs <- arbtGrauSaida (length (removeDups ns Set.empty))
                       es <- arbtEdges (removeDups ns Set.empty) gs
                       return Graph {nodes = fromList ns, edges = fromList es}

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

{-- Gerador de DAGs

APAGAMOS ISTO QUANDO ENTRAGARMOS, PODE SERVIR DE INSPIRAÇÃO, OU NÃO

dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = aux `suchThat` isDAG
    where aux = do ns <- listOf (arbitrary)
                   gs <- arbtGrauSaida (length ns)
                   es <- arbtEdgeDAG ns gs []
                   return $ Graph {nodes = fromList ns, edges = fromList es}

arbtEdgeDAG :: [v] -> [Int] -> [Edge v] -> Gen [(Edge v)]
arbtEdgeDAG ns gs es = auxDAG ns (zip ns gs) es

-- es -> acumulador
-- Ao criar o Edge verificar se já alguma edge com o destino deste edge criado como source
auxDAG :: [v] -> [(v, int)] -> [Edge v] -> Gen [(Edge v)]
auxDAG ns [] es = return []
auxDAG ns (z:zs) es = do lst <- (aux ns z) `suchThat` (not (fazCiclo ns))
                         t <- auxDAG ns zs es
                         return lst++t

--          es Graph    es Inserir
fazCiclo :: (Eq v) => [Edge v] -> [Edge v] -> Bool
fazCiclo [] _ = True
fazCiclo esG esI = let srcG = Prelude.map source esG
                       destI = Prelude.map target esI
                   in auxFazCiclo srcG destI

auxFazCiclo :: (Eq v) => [v] -> [v] -> Bool
auxFazCiclo src	[] = True
auxFazCiclo src (h:t) = (elem h src) && auxFazCiclo src t

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g



dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do arbtGraph <- arbitrary
         let nodos = toList $ nodes arbtGraph
         in return $ Graph {nodes = nodes arbtGraph, edges = fromList $ Prelude.filter (filtraEdges nodos) $ toList $ edges arbtGraph }

arbtEdgeDAG :: (Ord v, Arbitrary v) => v -> [v] -> Gen (Edge v)
arbtEdgeDAG src l = do dest <- elements l `suchThat` (aux' src l)
                       return (Edge src dest)

-- o objectivo é retirar todos os vertices em que o src == dest, que
--                      nodos       edge
filtraEdges :: Ord v => [Edge v] -> Edge v -> Bool
filtraEdges (h:t) (Edge x y) | x == y = False
                             |

--}
{-
-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = aux `suchThat` isDAG
    where aux = do ns <- listOf $ resize 15 arbitrary
                   gs <- arbtGrauSaida (length (removeDups ns Set.empty))
                   es <- arbtEdgesDAG (removeDups ns Set.empty) gs
                   return  Graph {nodes = fromList ns, edges = filtraLacos (fromList es)}
-}

-- um DAG pode ser visto como o triangulo inferior de uma matriz de adjacencia, ou seja, as Edges tem de ter  source > target
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do arbtG <- arbitrary
         return  Graph {nodes = nodes arbtG, edges = filtraLacos $ Set.filter (\(Edge s t) -> s > t) (edges arbtG)}

notLaco :: Eq v => Edge v -> Bool
notLaco e = if source e == target e then False else True

filtraLacos :: Eq v => Set (Edge v) -> Set (Edge v)
filtraLacos = Set.filter (notLaco)

arbtEdgesDAG :: (Ord v, Arbitrary v) => [v] -> [Int] -> Gen [Edge v]
arbtEdgesDAG ns gs = do m <- mapM (auxDAG ns) (zip ns gs)
                        return (concat m)

auxDAG :: (Ord v, Arbitrary v) => [v] -> (v, Int) -> Gen [Edge v]
auxDAG _ (n, 0) = return []
auxDAG l (n, x) = do e <- arbtEdgeDAG n l
                     t <- auxDAG l (n, x-1)
                     return (e:t)

arbtEdgeDAG :: (Ord v, Arbitrary v) => v -> [v] -> Gen (Edge v)
arbtEdgeDAG src l = do dest <- elements l `suchThat` aux' src l
                       return (Edge src dest)

aux' :: Ord v => v -> [v] -> v -> Bool
aux' src l e = if minimum l == src
               then e == src
               else e < src

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g


-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do arbtDAG <- dag
            es <- mapM (filtraAdjF arbtDAG) (toList $ nodes arbtDAG)
            esF <- retiraMaybes es
            return Graph {nodes = nodes arbtDAG, edges = fromList esF}

-- Basicamente cada nodo só pode ter um nodo adjacente ou nenhum

retiraMaybes :: Eq v => [Maybe (Edge v)] -> Gen [Edge v]
retiraMaybes x = let semNothings = Prelude.filter (/= Nothing) x
                     soEdges = Prelude.map (\(Just n) -> n) semNothings
                 in  return soEdges

filtraAdjF :: (Ord v) =>DAG v -> v -> Gen (Maybe (Edge v))
filtraAdjF g n = let adjs = adj g n
                 in if Set.null adjs
                    then return Nothing
                    else fAdjFAux (toList adjs)

fAdjFAux :: [Edge v] -> Gen (Maybe (Edge v))
fAdjFAux s = do e <- elements s
                frequency [(6, return (Just e)), (2, return Nothing)]

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g


prop_empty :: Property
prop_empty = isEmpty Graph.empty  ==> Set.null $ nodes Graph.empty

prop_swap :: Edge Int -> Property
prop_swap g = forAll (pure g)  (\g -> swap (swap g) == g)

prop_isValid :: Graph Int -> Property
prop_isValid g = let aux = Set.map source (edges g) `Set.union` Set.map target (edges g)
                 in if aux == Set.empty 
                    then property $ True 
                    else forAll (elements $ elems aux) $ \a -> a `member` nodes g

prop_transpose :: Graph Int -> Property
prop_transpose g = property $ g == (transpose $ transpose g)

prop_isValidT :: Graph Int -> Property
prop_isValidT g = not(isEmpty g) ==> (property . isValid) $ transpose g

-- Verifica se os nodos a que podemos chegar a partir de um nodo fazem parte do set de nodos do grafo... é fraca, mas é uma propriedade
prop_reachable1 :: Graph Int -> Property
prop_reachable1 g | isEmpty g = label "empty" True
                  | otherwise = property $  all (\v -> isSubsetOf (reachable g v) (nodes g)) (elems $ nodes g)

-- O set de nodos adjacentes do nodo em questão será claramente um subset dos nodos a que podemos chegar a partir do mesmo
prop_reachable2 :: Graph Int -> Property
prop_reachable2 g | isEmpty g = label "empty" True
                  | otherwise = forAll (elements $ elems $ nodes g) (\v -> isSubsetOf (setAdj v) (setReach v))
                  where setAdj n = Set.map target (adj g n)
                        setReach n = reachable g n

-- Uma travessia breath first a partir de um nodo cria uma árvore, um árvore não tem ciclos
-- Uma travessia breath first a partir de vários nodos cria várias árvores
-- Logo a bft cria necessariamente uma floresta
-- assim temos esta propriedade
--
-- Esta prop também se aplica á bft, porque uma bft está correcta se o resultado for uma floresta
prop_isForest :: Graph Int -> Property
prop_isForest g | isEmpty g = label "empty" True
                | otherwise = forAll (setNodos $ nodes g) (\ns -> isForest $ bft g (fromList ns))

setNodos :: Set v -> Gen [v]
setNodos ns = do x <- choose(1, size ns)
                 sequence $ replicate x (elements $ elems $ ns)

-- o resultado de uma bft será uma dag
prop_isDAG :: Graph Int -> Property
prop_isDAG g | isEmpty g = label "empty" True
             | otherwise = forAll (setNodos $ nodes g) (\ns -> isDAG $ bft g (fromList ns))

prop_union :: Graph Int -> Graph Int -> Property
prop_union g g' | isEmpty g && isEmpty g' = label "2 empty" True
                | isEmpty g || isEmpty g' = label "1 empty" True
                | otherwise = ((isSubsetOf nsG nsU) && (isSubsetOf nsG' nsU)) .&&. ((isSubsetOf esG esU) && (isSubsetOf esG' esU))
                where uni = Graph.union g g'
                      nsU = nodes uni
                      esU = edges uni
                      nsG = nodes g
                      nsG'= nodes g'
                      esG = edges g
                      esG'= edges g'

-- se x = union g f, então g e f são subgrafos de x
prop_union2 :: Graph Int -> Graph Int -> Property
prop_union2 g g' | isEmpty g && isEmpty g' = label "2 empty" True
                 | isEmpty g || isEmpty g' = label "1 empty" True
                 | otherwise = (isSubgraphOf g uni) .&&. (isSubgraphOf g' uni)
                 where uni = Graph.union g g'

-- o desespero é tão grande
prop_union3 :: Graph Int -> Property
prop_union3 g = property $ Graph.union g g == g

prop_isSubgraphOf :: Graph Int -> Property
prop_isSubgraphOf g | isEmpty g = label "empty" True
                    | otherwise = forAll (elements $ elems $ nodes g) (\n -> isSubgraphOf (transpose $ bft g (singleton n)) g)


aux'' :: Ord v => Graph v -> Gen (v, v)
aux'' g = do i <- elements $ elems $ nodes g
             f <- elements $ elems $ reach i
             return $ (i, f)
          where reach i = reachable g i

-- escolher um nodo, ver os nodos a que ele consegue chegar (reachable)
-- escolher um desses, chamar a path com esses dois nodos e verificar se
-- é um path do grafo
prop_isPathOf :: Graph Int -> Property
prop_isPathOf g | isEmpty g = label "empty" True
                | otherwise = forAll (aux'' g) (\(i, f) -> case path g i f of Nothing -> label "No path" True
                                                                              Just x -> property $ isPathOf x g)
                where reach n = reachable g n

-- Props para a path (não verificam a parte de ser o caminho mais curto though)

-- verifica se os Edges de um path fazem parte do grafo
-- trivialidades quando são 3h e não há ideias brilhantes
prop_path1 :: Graph Int -> Property
prop_path1 g | isEmpty g = label "empty" True
             | otherwise = forAll (aux'' g) (\(i, f) -> case path g i f of Nothing -> label "no path" True
                                                                           Just x -> property $ isSubsetOf (fromList x) (edges g))

-- verifica se o set dos nodos do path é um subset de (reachable g n)
prop_path2 :: Graph Int -> Property
prop_path2 g | isEmpty g = label "empty" True
             | otherwise = forAll (aux'' g) (\(i, f) -> case path g i f of Nothing -> label "no path" True
                                                                           Just x -> property $ isSubsetOf (setU x) (reachable g i))
           where setU y = Set.union (Set.map source (st y)) (Set.map target (st y))
                 st x = fromList x

prop_path3 :: Graph Int -> Property
prop_path3 g | isEmpty g = label "empty" True
             | otherwise = forAll (aux'' g) 
                                  (\(i,f) -> if isEmpty g
                                             then label "empty" True
                                             else if i == f
                                                  then label "i == f" True
                                                  else case path g i f of Nothing -> label "no path" True
                                                                          Just x -> property $ tail x == fromJust (path g (target $ head x) f))

inDegree :: Graph Int -> Int -> Int
inDegree g v = size $ Set.filter (\e -> (target e) == v) (edges g)

prop_topo :: DAG Int -> Property
prop_topo g = (not $ isEmpty g) ==> property $ all (0==) $ Prelude.map (inDegree g) (elems $ head $ topo g)

-- PORQUE FALHA!???? NÃO FAZ SENTIDO!!! LAÇOS???  DESTS > SRC!??? O GERADOR RETIRA ISTO TUDO... como é que é possivel!???
--prop_isDagTrans :: DAG Int -> Property
--prop_isDagTrans g = not (isEmpty g) ==> (property . isDAG . transpose) g

prop_isValidU :: Graph Int -> Graph Int -> Property
prop_isValidU g g' = not(isEmpty g) && not(isEmpty g') ==> (property . isValid) $ Graph.union g g'
