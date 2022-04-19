import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
data VLabel = VLStart
            | VLInter
            | VLFinal
            | VLSandF
            deriving (Show)
type V = (String, VLabel)
data ELabel c = ELTrans c
              | ELStart c
            deriving (Show)
type E = (String, Char, String, ELabel Char)
--fsm2svg :: (Show q,Eq q) => FSM q -> IO FilePath
fsm2svg m = G.runGraphviz dotGraph G.Svg "file.svg" where
    (k,a,s,f,t) = m
    vs = map (\e->(show e,fv e)) k
    es = map (\e->(show e,show e,ELStart 'e')) s ++ map (\(a,b,c)->(show a,show c,ELTrans b)) t
    dotGraph = G.graphElemsToDot fsmGraphParams vs es :: G.DotGraph String
    fv e | e `elem` s && e `elem` f = VLSandF
         | e `elem` s               = VLStart
         | e `elem` f               = VLFinal
         | otherwise                = VLInter
    fsmGraphParams :: G.GraphvizParams String VLabel (ELabel Char) () VLabel
    fsmGraphParams = G.blankParams {
      G.isDirected       = True,
      G.globalAttributes = [G.GraphAttrs [G.RankDir G.FromLeft],G.NodeAttrs [G.Shape G.Circle]],
      G.clusterBy        = G.N,
      G.isDotCluster     = const True,
      G.clusterID        = const (G.Num $ G.Int 0),
      G.fmtCluster       = const [],
      G.fmtNode = \(v, vl) -> case vl of
          VLStart -> colorAttribute (G.RGB 0 255 0)
          VLInter -> colorAttribute (G.RGB 0 0 255)
          VLFinal -> G.shape G.DoubleCircle:colorAttribute (G.RGB 255 0 0)
          VLSandF -> G.shape G.DoubleCircle:colorAttribute (G.RGB 200 200 0),
      G.fmtEdge = \(from, to, el) -> case el of
          ELTrans c -> G.toLabel c:colorAttribute (G.RGB 0 0 0)
          ELStart c -> G.Dir G.Back:G.PenWidth 0:colorAttribute (G.RGB 0 255 0)
      } where
        colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
-- Usage:

--dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [[0]],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])
-- fsm2svg dm1