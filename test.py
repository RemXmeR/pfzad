import os
import subprocess
import re

output_folder = os.path.abspath('./examples')

SEPARATOR = '------------------------------------------------------------'

print("🔧 Running 'cabal build'...")
build_result = subprocess.run(['cabal', 'build'], capture_output=True, text=True)


for filename in os.listdir(output_folder):
    file_path = os.path.join(output_folder, filename)

    if not os.path.isfile(file_path):
        continue

    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Extract the part after dashed line
    if SEPARATOR not in content:
        print(f"[{filename}] ⚠️ No dashed line.")
        continue

    post_dash = content.split(SEPARATOR, 1)[1]

    # Extract content inside `{- ... -}`
    match = re.search(r'\{\-(.*?)\-\}', post_dash, re.DOTALL)
    if not match:
        print(f"[{filename}] ⚠️ No expected result block in {{- -}} found.")
        continue

    expected_block = match.group(1)
    expected_lines = [line.strip() for line in expected_block.strip().splitlines() if line.strip()]

    # Run cabal
    command = ['cabal', 'run', 'zadanie3', '--', file_path]
    result = subprocess.run(command, capture_output=True, text=True)

    if result.returncode != 0:
        print(f"[{filename}] ❌ Cabal run failed:\n{result.stderr}")
        continue

    output = result.stdout
    actual_part = output.split(SEPARATOR, 1)[1] if SEPARATOR in output else output
    actual_lines = [line.strip() for line in actual_part.strip().splitlines() if line.strip()]

    # Compare
    if actual_lines == expected_lines:
        print(f"[{filename}] ✅ Match.")
    else:
        print(f"[{filename}] ❌ Mismatch.")
        print("  Expected:")
        for line in expected_lines:
            print(f"    {line}")
        print("  Actual:")
        for line in actual_lines:
            print(f"    {line}")
