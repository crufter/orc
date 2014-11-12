```
ghci main.hs
*Main> main
Starting ORC
```

```
curl http://127.0.0.1:8081/connect -d '{
	"address": "127.0.0.1:6060",
	"serviceName": "wrapper",
	"instanceName": "wrapper 1234",
	"endpoints": {
		"wrap": {
			"alias": "wrap",
			"path": "wrap"
		}
	}
}'
```