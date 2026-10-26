#!/usr/bin/env python3
# Minimal multi-turn agent loop for controlled study of the schema-anchoring effect.
# Executes the model's tool calls in an isolated fixture dir, loops until the model
# answers (no tool_use) or max-turns. Records the trajectory: tool sequence, whether
# it EXECUTED code (ran python/the program) vs only read, and the final diagnosis.
import sys, json, urllib.request, argparse, pathlib, subprocess, glob as globlib

BASE = pathlib.Path(__file__).parent
KEY = (BASE / ".dskey").read_text().strip()
ENDPOINT = "https://api.deepseek.com/anthropic/v1/messages"

def api(body):
    req = urllib.request.Request(ENDPOINT, data=json.dumps(body).encode(),
        headers={"x-api-key": KEY, "anthropic-version": "2023-06-01", "content-type": "application/json"})
    with urllib.request.urlopen(req, timeout=300) as r:
        return json.load(r)

def run_tool(name, inp, workdir):
    try:
        if name == "bash":
            cmd = inp.get("command", "")
            import os
            env = dict(os.environ); env["PATH"] = str(pathlib.Path(workdir).resolve()) + ":" + env.get("PATH", "")
            p = subprocess.run(cmd, shell=True, cwd=workdir, capture_output=True, text=True, timeout=30, env=env)
            out = (p.stdout + p.stderr)[:4000]
            return out or "(no output)", ("python" in cmd or "./" in cmd or "pytest" in cmd or "-c " in cmd)
        if name == "read_file":
            fp = pathlib.Path(workdir) / inp.get("path", "")
            return fp.read_text()[:4000], False
        if name == "grep":
            pat = inp.get("pattern", ""); path = inp.get("path", ".")
            p = subprocess.run(["grep", "-rn", pat, path], cwd=workdir, capture_output=True, text=True, timeout=20)
            return (p.stdout + p.stderr)[:4000] or "(no match)", False
        if name == "glob":
            hits = globlib.glob(str(pathlib.Path(workdir) / inp.get("pattern", "*")), recursive=True)
            return "\n".join(hits)[:2000] or "(no match)", False
        if name == "git":
            p = subprocess.run("git " + inp.get("args", "status"), shell=True, cwd=workdir, capture_output=True, text=True, timeout=20)
            return (p.stdout + p.stderr)[:3000] or "(no output)", False
        if name in ("str_replace_editor", "write_file", "edit_file"):
            return "(edit acknowledged; ignored in read-only study)", False
        return f"(unknown tool {name})", False
    except Exception as e:
        return f"(tool error: {e})", False

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--system-file", required=True)
    ap.add_argument("--tools-file", required=True)
    ap.add_argument("--task-file", required=True)
    ap.add_argument("--workdir", required=True)
    ap.add_argument("--max-turns", type=int, default=8)
    ap.add_argument("--think", type=int, default=1500)
    ap.add_argument("--label", default="")
    a = ap.parse_args()
    system = pathlib.Path(a.system_file).read_text()
    tools = json.loads(pathlib.Path(a.tools_file).read_text())
    task = pathlib.Path(a.task_file).read_text()
    messages = [{"role": "user", "content": task}]
    traj, executed_code, tin, tout = [], False, 0, 0
    final = ""
    for turn in range(a.max_turns):
        body = {"model": "deepseek-v4-pro", "max_tokens": 2600, "system": system,
                "messages": messages, "tools": tools, "tool_choice": {"type": "auto"}}
        if a.think > 0: body["thinking"] = {"type": "enabled", "budget_tokens": a.think}
        d = api(body)
        if d.get("type") == "error":
            traj.append(("API_ERROR", json.dumps(d)[:150])); break
        u = d.get("usage", {}); tin += u.get("input_tokens", 0); tout += u.get("output_tokens", 0)
        content = d.get("content", [])
        assistant_blocks, tool_uses, text = [], [], ""
        for b in content:
            if b.get("type") == "text": text += b.get("text", "")
            if b.get("type") == "tool_use": tool_uses.append(b)
            # keep thinking+text+tool_use blocks for the assistant turn (thinking needs signature)
            if b.get("type") in ("thinking", "text", "tool_use", "redacted_thinking"):
                assistant_blocks.append(b)
        messages.append({"role": "assistant", "content": assistant_blocks})
        _think = "".join(b.get("thinking","") for b in content if b.get("type")=="thinking")
        _tnames = ", ".join(b.get("name") for b in tool_uses) or "(none)"
        print(f"\n----- turn {turn+1} | tools_called: {_tnames} -----")
        print(f"[COT]\n{_think}")
        if text.strip(): print(f"[SAID]\n{text.strip()}")
        if not tool_uses:
            final = text; break
        results = []
        for tu in tool_uses:
            out, ran = run_tool(tu.get("name"), tu.get("input", {}), a.workdir)
            if ran: executed_code = True
            cmdprev = json.dumps(tu.get("input", {}), ensure_ascii=False)[:80]
            traj.append((tu.get("name"), cmdprev))
            results.append({"type": "tool_result", "tool_use_id": tu.get("id"), "content": out})
        messages.append({"role": "user", "content": results})
    forced = False
    if not final:
        # Force termination: strip tools, demand an answer now. Distinguishes
        # "has the answer but won't stop" from "never converged".
        forced = True
        messages.append({"role": "user", "content": "Tools are now disabled. Based on what you have already found, give your final diagnosis NOW: the exact failing scenario and the one-line fix. Do not ask for more investigation."})
        body = {"model": "deepseek-v4-pro", "max_tokens": 1500, "system": system, "messages": messages}
        if a.think > 0: body["thinking"] = {"type": "enabled", "budget_tokens": 800}
        d = api(body)
        u = d.get("usage", {}); tin += u.get("input_tokens", 0); tout += u.get("output_tokens", 0)
        for b in d.get("content", []):
            if b.get("type") == "text": final += b.get("text", "")
    print(f"===== {a.label} =====")
    print(f"[forced_final={forced}]")
    print(f"[usage] in={tin} out={tout} turns={len(set(range(len(traj))))} tool_calls={len(traj)} executed_code={executed_code}")
    print("[TRAJECTORY]")
    for n, p in traj: print(f"  → {n}  {p}")
    print(f"[FINAL DIAGNOSIS {len(final)} chars]\n{final}\n")

if __name__ == "__main__":
    main()
