def collect(item, seen=[]):
    """Append item to a fresh list of seen items and return the list."""
    if item not in seen:
        seen.append(item)
    return seen

def dedup(items):
    out = []
    for it in items:
        out = collect(it, out)
    return out
