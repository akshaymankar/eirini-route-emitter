module KubernetesExtras.TLSUtils
  (updateClientParams)
where

import Data.ByteString
import Data.Function     ((&))
import Kubernetes.Client
import Network.TLS

updateClientParams :: ClientParams -> ByteString -> Either String ClientParams
updateClientParams cp certText = parsePEMCerts certText
                                 & (fmap (flip setCAStore cp))
