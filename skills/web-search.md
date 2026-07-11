---
name: web-search
description: Search the web from bash using a search API via curl
---
# Web search skill

When you need current information from the web, use the `bash` tool:

```sh
curl -s "https://api.example-search.com/q?query=<url-encoded terms>"
```

Parse the JSON results and cite the source URLs in your answer.
