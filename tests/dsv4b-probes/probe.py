#!/usr/bin/env python3
# Single-turn probe: given (system, tools, task, thinking budget), call deepseek-v4-pro
# and report the model's step-0 behavior: thinking, which tool(s) it reaches for, final text.
import sys, json, urllib.request, argparse, pathlib

BASE = pathlib.Path(__file__).parent
KEY = (BASE / ".dskey").read_text().strip()
ENDPOINT = "https://api.deepseek.com/anthropic/v1/messages"

def call(system, tools, task, think_budget, max_tokens):
    body = {
        "model": "deepseek-v4-pro",
        "max_tokens": max_tokens,
        "system": system,
        "messages": [{"role": "user", "content": task}],
    }
    if tools:
        body["tools"] = tools
        body["tool_choice"] = {"type": "auto"}
    if think_budget and think_budget > 0:
        body["thinking"] = {"type": "enabled", "budget_tokens": think_budget}
    req = urllib.request.Request(
        ENDPOINT, data=json.dumps(body).encode(),
        headers={"x-api-key": KEY, "anthropic-version": "2023-06-01", "content-type": "application/json"})
    with urllib.request.urlopen(req, timeout=300) as r:
        return json.load(r)

def summarize(d):
    think, tools, text = "", [], ""
    for b in d.get("content", []):
        t = b.get("type")
        if t == "thinking": think += b.get("thinking", "")
        elif t == "tool_use":
            inp = b.get("input", {})
            # compact one-line preview of the tool input
            prev = json.dumps(inp, ensure_ascii=False)
            if len(prev) > 240: prev = prev[:240] + "…"
            tools.append((b.get("name"), prev))
        elif t == "text": text += b.get("text", "")
    usage = d.get("usage", {})
    return think, tools, text, usage

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--system-file", required=True)
    ap.add_argument("--tools-file", required=True)
    ap.add_argument("--task-file", required=True)
    ap.add_argument("--think", type=int, default=1500)
    ap.add_argument("--max-tokens", type=int, default=3000)
    ap.add_argument("--label", default="")
    a = ap.parse_args()
    system = pathlib.Path(a.system_file).read_text()
    tools = json.loads(pathlib.Path(a.tools_file).read_text()) if a.tools_file != "NONE" else None
    task = pathlib.Path(a.task_file).read_text()
    try:
        d = call(system, tools, task, a.think, a.max_tokens)
    except Exception as e:
        print(f"[{a.label}] ERROR: {e}"); return
    if d.get("type") == "error":
        print(f"[{a.label}] API ERROR: {json.dumps(d)[:300]}"); return
    think, tuse, text, usage = summarize(d)
    print(f"===== {a.label} =====")
    print(f"[usage] in={usage.get('input_tokens')} out={usage.get('output_tokens')} tools_offered={0 if not tools else len(tools)} think_budget={a.think}")
    print(f"[THINKING {len(think)} chars]\n{think}\n")
    print(f"[TOOL CALLS: {len(tuse)}]")
    for n, p in tuse: print(f"  → {n}  {p}")
    print(f"[FINAL TEXT {len(text)} chars]\n{text}\n")

if __name__ == "__main__":
    main()
