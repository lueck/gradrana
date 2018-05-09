-- | Construct a graph on the basis of whether a pair of characters is
-- present on the same time slice. A new time slice can be "deemed to
-- begin whenever a character is stated or can be inferred to have
-- left the stage". (J. Stiller, D. Nettle, R.I.M. Dunbar: The Small
-- World of Shakespeare's Plays, in: Human Nature, Vol. 14, No. 4,
-- 2003, p.399) Following the approach of J. Stiller et al. the two
-- nodes which represent two characters are conneted by an edge, if
-- these characters are both present in at least one the play's time
-- slices. The edges neither have a direction nor labels.

-- | This approach is more accurate than the construction of the graph
-- on the basis of co-presence in a scene, because a scene may consist
-- of multiple time slices. Although being present in the same scene,
-- two persons may not be present simultaneously on stage.

-- | J. Stiller and his co-authors seem to have constructed their
-- graphs by hand. It turns out to be complicated to implement a time
-- slice splitter as a computer program, doesn't it?

-- | A simplistic time slice splitter would split at each stage
-- direction. Yes, this would result in many more time slices than
-- there really are. But provided that the graph's edges are not
-- labeled and the graph is not a multigraph, the resulting graph can
-- be assumed to be very close to a graph constructed on the basis of
-- a human time slice splitter.


module GraDrAna.TimeSlice where

