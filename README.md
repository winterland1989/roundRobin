# A simple round-robin data type

This package provide a simple data type wrap a round-robin table. so you can select resources(host, connection...) using round-robin fashion.

## Example

```haskell
import qualified Date.RoundRobin as RR
import qualified Network.HTTP.Client as HTTP

main :: IO ()
main = do
    reqs <- mapM HTTP.parseUrl ["http://foo.com", "http://bar.com", "http://qux.com"]
    proxyTable <- RR.newRoundRobin HTTP.parseUrl reqs
    manager <- newManager defaultManagerSettings

    ...
    -- maybe now you're inside a server service(a forked thread)
    -- use select to choose a request in round-robin fashion
        req <- RR.select proxyTable
        res <- httpLbs req manager
        ...
```
