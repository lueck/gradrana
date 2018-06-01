import Data.Default.Class
import Options.Applicative
import Data.Semigroup

import GraDrAna.App
import GraDrAna.Config
import GraDrAna.Opts


run :: ConfiguredAppOpts -> IO ()
run opts = runGraDrAnaApp configuredApp (passOpts def opts)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info
           (configuredAppOpts_ <**> helper)
           (fullDesc
            <> progDesc "tei2graph reads a play encoded in TEI format and generates a graph representing the social network of the play's persons. tei2graph does not visualize the network, but writes a file in a common format, e.g. GraphML for external visualization tools like Gephi.\n\ntei2graph is highly configurable and knows several approaches to graph construction from dramatic texts."
            <> header "tei2graph - generate a social network graph from a play encoded in TEI format")
