#!/usr/bin/env python3
# Compare raw CoT style across deepseek models on the SAME inline question (no tools).
import json, urllib.request, pathlib, sys
BASE = pathlib.Path(__file__).parent
KEY = (BASE / ".dskey").read_text().strip()
EP = "https://api.deepseek.com/anthropic/v1/messages"
Q = (pathlib.Path(BASE/"lab1"/"cache.py").read_text() +
     "\n\nUser: dedup() sometimes returns items from a previous unrelated call. Root cause + one-line fix.")
MODELS = sys.argv[1:] or ["deepseek-v4-pro", "deepseek-v4-flash", "deepseek-reasoner", "deepseek-chat"]
for m in MODELS:
    body = {"model": m, "max_tokens": 3000, "messages": [{"role": "user", "content": Q}],
            "thinking": {"type": "enabled", "budget_tokens": 2000}}
    try:
        req = urllib.request.Request(EP, data=json.dumps(body).encode(),
            headers={"x-api-key": KEY, "anthropic-version": "2023-06-01", "content-type": "application/json"})
        with urllib.request.urlopen(req, timeout=180) as r:
            d = json.load(r)
    except Exception as e:
        # some models may not accept the thinking param — retry without it
        try:
            body.pop("thinking")
            req = urllib.request.Request(EP, data=json.dumps(body).encode(),
                headers={"x-api-key": KEY, "anthropic-version": "2023-06-01", "content-type": "application/json"})
            with urllib.request.urlopen(req, timeout=180) as r:
                d = json.load(r)
            note = " (no-thinking-param)"
        except Exception as e2:
            print(f"\n########## {m} ##########\nERROR: {e} / {e2}"); continue
    else:
        note = ""
    if d.get("type") == "error":
        print(f"\n########## {m} ##########\nAPI ERROR: {json.dumps(d)[:200]}"); continue
    th = "".join(b.get("thinking","") for b in d.get("content",[]) if b.get("type")=="thinking")
    tx = "".join(b.get("text","") for b in d.get("content",[]) if b.get("type")=="text")
    u = d.get("usage",{})
    print(f"\n########## {m}{note}  [out={u.get('output_tokens')} stop={d.get('stop_reason')}] ##########")
    print(f"--- COT ({len(th)} chars) ---\n{th if th else '(no thinking block)'}")
    print(f"--- ANSWER ({len(tx)} chars) ---\n{tx if tx else '(empty)'}")
