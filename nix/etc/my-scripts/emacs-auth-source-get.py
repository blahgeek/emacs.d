import sys
import subprocess
import json

queries = []

# each argv is HOST or HOST:USER

for v in sys.argv[1:]:
    fields = v.split(':')
    assert 1 <= len(fields) <= 2
    if len(fields) == 1:
        queries.append([fields[0], "apikey"])
    else:
        queries.append(fields)

result = subprocess.run(
    ["emacs-safeclient"],
    input=json.dumps(['auth-source-get', queries]),
    stdout=subprocess.PIPE,
    text=True,
    check=True,
)
result = json.loads(result.stdout)

if not isinstance(result, list) or len(result) != len(sys.argv[1:]):
    raise ValueError(f"Failed to get auth-source: {result}")

for v in result:
    if not v:
        print('__invalid__')
    else:
        print(v.replace('\n', ''))
