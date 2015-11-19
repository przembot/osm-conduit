# osm-conduit

### OSM conduit parsing
Parse OSM maps using conduits.

Use any of these to get your desired data.
```haskell
sourceFile
conduitNWR
conduitNodes
conduitWays
conduitRelations
```
For types see Data.Conduit.OSM.Types

### Example usage
See example.hs, program gets all nodes which have `shop=alcohol` tag.

### TODO
- purify `fromStr`
- benchmark
- profiling
