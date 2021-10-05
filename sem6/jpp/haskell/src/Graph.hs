module Graph where

import Set(Set)
import qualified Set as Set
import Data.List (intercalate)



class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a


data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
  deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)



instance Graph Relation where
  empty     = Relation { domain = emptyDomain, relation = emptyRelation }
    where
      emptyDomain         = Set.empty
      emptyRelation       = Set.empty

  vertex a  = Relation { domain = vertexDomain, relation = emptyRelation }
    where
      vertexDomain        = Set.singleton a
      emptyRelation       = Set.empty

  union (Relation domain1 relation1) (Relation domain2 relation2)   = Relation { domain = unionDomain, relation = unionRelation }
    where
      unionDomain         = Set.union domain1 domain2
      unionRelation       = Set.union relation1 relation2

  connect (Relation domain1 relation1) (Relation domain2 relation2) = Relation { domain = connectDomain, relation = connectRelation }
    where
      connectDomain       = Set.union domain1 domain2

      domain1xDomain2     = cartesianProduct (Set.toList domain1) (Set.toList domain2)
      domainsRelation     = Set.fromList domain1xDomain2 

      relationsAfterUnion = Set.union relation1 relation2
      connectRelation     = Set.union relationsAfterUnion domainsRelation


instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id



instance Graph Basic where
  empty                 = Empty

  vertex                = Vertex

  union   Empty  graph2 = graph2
  union   graph1 Empty  = graph1
  union   graph1 graph2 = Union graph1 graph2

  connect Empty  graph2 = graph2
  connect graph1 Empty  = graph1
  connect graph1 graph2 = Connect graph1 graph2


instance Ord a => Eq (Basic a) where
  graph1 == graph2 = eqResult
    where
      (graph1Edges, graph1SingleVertices) = getCanonicalReprezentation graph1
      (graph2Edges, graph2SingleVertices) = getCanonicalReprezentation graph2

      edgesEqResult                       = graph1Edges          == graph2Edges
      singleVerticesEqResult              = graph1SingleVertices == graph2SingleVertices

      eqResult                            = edgesEqResult && singleVerticesEqResult


instance (Ord a, Num a) => Num (Basic a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id


instance Semigroup (Basic a) where
  (<>) = union


instance Monoid (Basic a) where
  mempty = Empty


fromBasic :: Graph g => Basic a -> g a
fromBasic Empty                   = empty
fromBasic (Vertex v)              = vertex v
fromBasic (Union graph1 graph2)   = fromBasic graph1 `union` fromBasic graph2
fromBasic (Connect graph1 graph2) = fromBasic graph1 `connect` fromBasic graph2


instance (Ord a, Show a) => Show (Basic a) where
  show graph = concat["edges ", show edgesList, " + vertices ", show singleVerticesList]
    where
      (edges, singleVertices) = getCanonicalReprezentation graph
      edgesList               = Set.toAscList edges
      singleVerticesList      = Set.toAscList singleVertices


-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17


todot :: (Ord a, Show a) => Basic a -> String
todot graph = concat ["digraph {", mergedDotEdgesAndVertices, "}"]
  where
    (edges, singleVertices)   = getCanonicalReprezentation graph

    dotEdges                  = edgeToDotEdge <$> Set.toAscList edges
    dotVertices               = show <$> Set.toAscList singleVertices

    mergedDotEdgesAndVertices = intercalate "; " $ dotEdges ++ dotVertices

    edgeToDotEdge (v1, v2)    = concat [show v1, " -> ", show v2]


instance Functor Basic where
  fmap _ Empty                   = Empty
  fmap f (Vertex v)              = Vertex (f v)
  fmap f (Connect graph1 graph2) = Connect (f <$> graph1) (f <$> graph2)
  fmap f (Union graph1 graph2)   = Union (f <$> graph1) (f <$> graph2)


-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV oldVertex1 oldVertex2 newVertex graph = merge <$> graph
  where
    merge vertex
      | vertex == oldVertex1 = newVertex
      | vertex == oldVertex2 = newVertex
      | otherwise            = vertex


instance Applicative Basic where
  pure = Vertex

  Empty           <*> _     = Empty
  (Vertex f)      <*> x     = f <$> x
  (Connect f1 f2) <*> graph = Connect (f1 <*> graph) (f2 <*> graph)
  (Union f1 f2)   <*> graph = Union (f1 <*> graph) (f2 <*> graph)


instance Monad Basic where
  return = Vertex

  Empty                   >>= _ = Empty
  (Vertex v)              >>= f = f v
  (Connect graph1 graph2) >>= f = Connect (graph1 >>= f) (graph2 >>= f)
  (Union graph1 graph2)   >>= f = Union (graph1 >>= f) (graph2 >>= f)


-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV oldVertex newVertex1 newVertex2 graph = graph >>= split
  where
    split vertex
      | vertex == oldVertex = Union (return newVertex1) (return newVertex2)
      | otherwise           = return vertex



getCanonicalReprezentation :: Ord a => Basic a -> (Set (a, a), Set a)
getCanonicalReprezentation graph = (edges, singleVertices)
  where
    (edges, _)     = getEdgesAndVertices graph
    edgesVertices  = getEdgeVertices edges
    singleVertices = getSingleVertices graph edgesVertices


    getEdgeVertices :: Ord a => Set (a, a) -> Set a
    getEdgeVertices edges = Set.union (Set.fromList edgeVertices') (Set.fromList edgeVertices'')
      where
        (edgeVertices', edgeVertices'') = unzip $ Set.toAscList edges


    getSingleVertices :: Ord a => Basic a -> Set a -> Set a
    getSingleVertices (Vertex v) edgeVertices
      | Set.member v edgeVertices                        = Set.empty
      | otherwise                                        = Set.singleton v

    getSingleVertices (Union graph1 graph2) edgeVertices = Set.union graph1SingleVertices graph2SingleVertices
      where
        graph1SingleVertices = getSingleVertices graph1 edgeVertices
        graph2SingleVertices = getSingleVertices graph2 edgeVertices

    getSingleVertices  _ _                               = Set.empty


    getEdgesAndVertices :: Basic a -> (Set (a, a), Set a)
    getEdgesAndVertices Empty                   = (Set.empty, Set.empty)
    getEdgesAndVertices (Vertex v)              = (Set.empty, Set.singleton v)
    getEdgesAndVertices (Connect graph1 graph2) = (allEdges,  allVertices)
      where
        (graph1Edges, graph1Vertices) = getEdgesAndVertices graph1
        (graph2Edges, graph2Vertices) = getEdgesAndVertices graph2

        newEdges                      = Set.fromList $ cartesianProduct (Set.toList graph1Vertices) (Set.toList graph2Vertices)

        allEdges                      = Set.union (Set.union graph1Edges graph2Edges) newEdges
        allVertices                   = Set.union graph1Vertices graph2Vertices

    getEdgesAndVertices (Union graph1 graph2)    = (allEdges, allVertices)
      where
        (graph1Edges, graph1Vertices) = getEdgesAndVertices graph1
        (graph2Edges, graph2Vertices) = getEdgesAndVertices graph2

        allEdges                      = Set.union graph1Edges graph2Edges
        allVertices                   = Set.union graph1Vertices graph2Vertices


cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct list1 list2 = [(v1, v2) | v1 <- list1, v2 <- list2]

