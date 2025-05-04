import os
import subprocess
import re
import pandas as pd

def parse_output(output):
    """Parses the output of 'vine run' to extract metrics."""
    metrics = {}
    patterns = {
        'Interactions_Total': r'Total\s+(\d+)',
        'Interactions_Annihilate': r'Annihilate\s+(\d+)',
        'Interactions_Commute': r'Commute\s+(\d+)',
        'Interactions_Copy': r'Copy\s+(\d+)',
        'Interactions_Erase': r'Erase\s+(\d+)',
        'Interactions_Expand': r'Expand\s+(\d+)',
        'Interactions_Call': r'Call\s+(\d+)',
        'Interactions_Branch': r'Branch\s+(\d+)',
        'Memory_Heap': r'Memory\s+Heap\s+(\d+)\s+B',
        'Memory_Allocated': r'Allocated\s+(\d+)\s+B',
        'Memory_Freed': r'Freed\s+(\d+)\s+B',
        'Performance_Time': r'Performance\s+Time\s+(\d+)\s+ms',
        'Performance_Speed': r'Speed\s+([\d,_]+)\s+IPS'
    }

    for key, pattern in patterns.items():
        match = re.search(pattern, output)
        if match:
            # Clean up numbers with underscores
            value_str = match.group(1).replace('_', '')
            try:
                # Try converting to int first, then float if needed (though all current patterns capture ints)
                metrics[key] = int(value_str)
            except ValueError:
                 # Fallback if somehow a non-integer gets captured (e.g., future changes)
                 try:
                    metrics[key] = float(value_str)
                 except ValueError:
                    metrics[key] = match.group(1) # Store as string if not numeric
        else:
            metrics[key] = None # Indicate missing data

    return metrics

def run_tests(test_root_dir='.'):
    """Scans for tests, runs them, and collects results."""
    results = []
    test_dirs = [d.name for d in os.scandir(test_root_dir) if d.is_dir() and not d.name.startswith('.')] # Ignore hidden dirs

    for test_dir in test_dirs:
        test_file_path = os.path.join(test_root_dir, test_dir, 'src', 'test.vi')
        if os.path.exists(test_file_path):
            command = f'vine run {test_file_path}'
            print(f"Running test: {command}")
            try:
                # Use powershell explicitly if needed, otherwise rely on default shell
                # For Windows, shell=True might be needed depending on PATH setup for 'vine'
                result = subprocess.run(command, capture_output=True, text=True, check=True, shell=True)
                print(f"Output for {test_dir}:\n{result.stdout}")
                parsed_metrics = parse_output(result.stdout)
                parsed_metrics['TestName'] = test_dir
                results.append(parsed_metrics)
            except subprocess.CalledProcessError as e:
                print(f"Error running test {test_dir}: {e}")
                print(f"Stderr:\n{e.stderr}")
                results.append({'TestName': test_dir, 'Error': str(e)})
            except FileNotFoundError:
                 print(f"Error: 'vine' command not found. Make sure it's in your PATH.")
                 # Optionally break or return early if vine is essential
                 return pd.DataFrame() # Return empty dataframe
        else:
            print(f"Skipping directory {test_dir}: No src/test.vi found.")

    df = pd.DataFrame(results)
    # Reorder columns to have TestName first
    if not df.empty and 'TestName' in df.columns:
        cols = ['TestName'] + [col for col in df.columns if col != 'TestName']
        df = df[cols]

    return df

if __name__ == "__main__":
    # Assuming the script is run from the parallax/tests directory
    results_df = run_tests()
    print("\\n--- Test Results ---")
    print(results_df)

    # Optional: Save to CSV
    # results_df.to_csv("test_results.csv", index=False)
    # print("\\nResults saved to test_results.csv")
