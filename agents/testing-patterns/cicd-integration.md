# CI/CD Integration Patterns

Integrating advanced testing strategies into CI/CD pipelines requires careful consideration of performance, reliability, and maintainability. This guide covers best practices for running property-based tests, snapshot tests, and enhanced assertions in continuous integration environments.

## Table of Contents

- [CI/CD Testing Strategy](#cicd-testing-strategy)
- [GitHub Actions Integration](#github-actions-integration)
- [Test Categorization](#test-categorization)
- [Performance Optimization](#performance-optimization)
- [Failure Handling](#failure-handling)
- [Artifact Management](#artifact-management)
- [Cross-Platform Testing](#cross-platform-testing)
- [Security Considerations](#security-considerations)

## CI/CD Testing Strategy

### Test Pyramid for CI/CD

```yaml
# .github/workflows/test.yml
name: Comprehensive Testing

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  # Fast feedback loop - runs on every commit
  quick-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      # Unit tests (fast)
      - name: Run unit tests
        run: cargo test --lib --bins
        
      # Basic integration tests
      - name: Run basic integration tests
        run: cargo test --test '*' -- --test-threads=1
        
      # Quick property tests (reduced cases)
        env:
          PROPTEST_CASES: 100
        run: cargo test proptest

  # Comprehensive tests - runs on PR and main
  comprehensive-tests:
    runs-on: ubuntu-latest
    if: github.event_name == 'pull_request' || github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      # Full property tests
      - name: Run comprehensive property tests
        env:
          PROPTEST_CASES: 1000
          PROPTEST_MAX_SHRINK_ITERS: 10000
        run: cargo test proptest
        
      # Snapshot tests with update check
      - name: Run snapshot tests
        run: |
          cargo test expect
          # Verify snapshots are up to date
          if ! git diff --quiet; then
            echo "Snapshots are out of date. Run 'UPDATE_EXPECT=1 cargo test' locally."
            git diff
            exit 1
          fi

  # Nightly tests - extensive testing
  nightly-tests:
    runs-on: ubuntu-latest
    if: github.event_name == 'schedule'
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      # Stress tests
      - name: Run stress tests
        env:
          PROPTEST_CASES: 10000
          RUST_LOG: debug
        run: cargo test --features stress-tests
        
      # Fuzzing
      - name: Run fuzzing tests
        run: cargo test --features fuzzing
```

### Test Configuration Matrix

```yaml
# .github/workflows/matrix-test.yml
name: Matrix Testing

on:
  pull_request:
    branches: [ main ]

jobs:
  test-matrix:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        rust: [stable, beta, nightly]
        features: [default, all-features, no-default-features]
        exclude:
          # Exclude some combinations to reduce CI time
          - rust: beta
            features: no-default-features
          - rust: nightly
            features: no-default-features
            
    runs-on: ${{ matrix.os }}
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@${{ matrix.rust }}
      
      - name: Configure features
        run: |
          if [ "${{ matrix.features }}" = "all-features" ]; then
            echo "CARGO_FEATURES=--all-features" >> $GITHUB_ENV
          elif [ "${{ matrix.features }}" = "no-default-features" ]; then
            echo "CARGO_FEATURES=--no-default-features" >> $GITHUB_ENV
          else
            echo "CARGO_FEATURES=" >> $GITHUB_ENV
          fi
        shell: bash
        
      - name: Run tests
        run: cargo test ${{ env.CARGO_FEATURES }}
        env:
          # Adjust test configuration for CI
          PROPTEST_CASES: 256
          RUST_BACKTRACE: 1
```

## GitHub Actions Integration

### Complete Workflow Example

```yaml
# .github/workflows/comprehensive.yml
name: Comprehensive Testing Pipeline

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  schedule:
    # Run nightly tests at 2 AM UTC
    - cron: '0 2 * * *'

env:
  CARGO_TERM_COLOR: always
  RUST_BACKTRACE: 1

jobs:
  # Check code formatting and linting
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: rustfmt, clippy
          
      - name: Check formatting
        run: cargo fmt --all -- --check
        
      - name: Run clippy
        run: cargo clippy --all-targets --all-features -- -D warnings

  # Unit and integration tests
  test:
    runs-on: ubuntu-latest
    needs: check
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      # Cache dependencies
      - uses: Swatinem/rust-cache@v2
        
      # Install additional tools
      - name: Install testing tools
        run: |
          cargo install cargo-nextest
          cargo install cargo-llvm-cov
          
      # Run tests with coverage
      - name: Run tests with coverage
        run: |
          cargo llvm-cov nextest --all-features --workspace --lcov --output-path lcov.info
        env:
          PROPTEST_CASES: 500
          FORCE_COLOR: 1
          
      # Upload coverage
      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          files: lcov.info
          fail_ci_if_error: true

  # Property-based testing with different configurations
  property-tests:
    runs-on: ubuntu-latest
    needs: check
    
    strategy:
      matrix:
        proptest_cases: [100, 500, 1000]
        
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run property tests
        run: cargo test proptest
        env:
          PROPTEST_CASES: ${{ matrix.proptest_cases }}
          PROPTEST_MAX_SHRINK_ITERS: 10000
          RUST_LOG: proptest=debug
          
      # Save regression files
      - name: Upload proptest regressions
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: proptest-regressions-${{ matrix.proptest_cases }}
          path: proptest-regressions/

  # Snapshot testing
  snapshot-tests:
    runs-on: ubuntu-latest
    needs: check
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run snapshot tests
        run: cargo test expect
        
      - name: Check for snapshot changes
        run: |
          if ! git diff --quiet; then
            echo "::error::Snapshots are out of date"
            echo "Run 'UPDATE_EXPECT=1 cargo test' locally and commit the changes"
            git diff
            exit 1
          fi
          
      # Generate snapshot report
      - name: Generate snapshot report
        if: always()
        run: |
          echo "## Snapshot Test Report" >> $GITHUB_STEP_SUMMARY
          echo "- Total snapshot tests: $(grep -r "expect\!" tests/ src/ | wc -l)" >> $GITHUB_STEP_SUMMARY
          echo "- Files with snapshots: $(grep -r "expect\!" tests/ src/ | cut -d: -f1 | sort -u | wc -l)" >> $GITHUB_STEP_SUMMARY

  # Performance and benchmarks
  performance:
    runs-on: ubuntu-latest
    needs: check
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run benchmarks
        run: cargo bench --bench '*' -- --output-format json | tee benchmark-results.json
        
      - name: Store benchmark results
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: benchmark-results.json
          github-token: ${{ secrets.GITHUB_TOKEN }}
          auto-push: true
          
      - name: Performance regression check
        run: |
          # Custom script to check for performance regressions
          python scripts/check_performance_regression.py benchmark-results.json

  # Cross-platform testing
  cross-platform:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        
    runs-on: ${{ matrix.os }}
    needs: check
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - uses: Swatinem/rust-cache@v2
      
      - name: Run cross-platform tests
        run: cargo test --all-features
        env:
          PROPTEST_CASES: 200
          # Platform-specific environment variables
          FORCE_COLOR: ${{ matrix.os != 'windows-latest' && '1' || '0' }}
          
      - name: Test platform-specific features
        run: cargo test --features platform-specific
        if: matrix.os == 'ubuntu-latest'

  # Security and dependency checks
  security:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      
      - name: Security audit
        run: |
          cargo install cargo-audit
          cargo audit
          
      - name: Check for outdated dependencies
        run: |
          cargo install cargo-outdated
          cargo outdated --exit-code 1
```

## Test Categorization

### Feature-Based Test Categories

```rust
// Cargo.toml
[features]
default = []
slow-tests = []
network-tests = []
stress-tests = []
fuzzing = []
integration-tests = []

# tests/lib.rs
#[cfg(test)]
mod tests {
    // Fast tests - always run
    mod unit_tests {
        use super::*;
        
        #[test]
        fn test_basic_functionality() {
            // Quick unit tests
        }
    }
    
    // Slow tests - run on PR and main
    #[cfg(feature = "slow-tests")]
    mod slow_tests {
        use super::*;
        use proptest::prelude::*;
        
        proptest! {
            #![proptest_config(ProptestConfig::with_cases(1000))]
            
            #[test]
            fn comprehensive_property_test(data in any::<Vec<i32>>()) {
                // Comprehensive testing
            }
        }
    }
    
    // Network tests - require network access
    #[cfg(feature = "network-tests")]
    mod network_tests {
        use super::*;
        
        #[test]
        fn test_api_integration() {
            // Tests that require network access
        }
    }
    
    // Stress tests - run nightly
    #[cfg(feature = "stress-tests")]
    mod stress_tests {
        use super::*;
        
        #[test]
        fn stress_test_large_inputs() {
            // Memory and performance stress tests
        }
    }
}
```

### Environment-Based Configuration

```rust
// tests/common/config.rs
pub struct TestConfig {
    pub is_ci: bool,
    pub proptest_cases: u32,
    pub timeout: std::time::Duration,
    pub verbose: bool,
}

impl TestConfig {
    pub fn from_env() -> Self {
        Self {
            is_ci: std::env::var("CI").is_ok(),
            proptest_cases: std::env::var("PROPTEST_CASES")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(if std::env::var("CI").is_ok() { 256 } else { 100 }),
            timeout: std::time::Duration::from_secs(
                std::env::var("TEST_TIMEOUT")
                    .ok()
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(30)
            ),
            verbose: std::env::var("TEST_VERBOSE").is_ok(),
        }
    }
}

// Usage in tests
#[test]
fn configurable_test() {
    let config = TestConfig::from_env();
    
    if config.verbose {
        println!("Running with config: {:?}", config);
    }
    
    // Adjust test behavior based on environment
    let test_data_size = if config.is_ci { 1000 } else { 100 };
    let test_data = generate_test_data(test_data_size);
    
    // Run test with timeout
    let result = std::panic::catch_unwind(|| {
        test_function(&test_data)
    });
    
    assert!(result.is_ok());
}
```

## Performance Optimization

### Parallel Test Execution

```yaml
# .github/workflows/parallel-tests.yml
- name: Run tests in parallel
  run: |
    # Use nextest for better parallelization
    cargo nextest run --all-features --workspace
  env:
    # Optimize for CI environment
    CARGO_BUILD_JOBS: 2
    NEXTEST_TEST_THREADS: 4
    
- name: Run property tests in parallel
  run: |
    # Split property tests across multiple jobs
    cargo test proptest -- --test-threads=4
  env:
    PROPTEST_CASES: 1000
    RAYON_NUM_THREADS: 4
```

### Caching Strategies

```yaml
# Advanced caching configuration
- name: Cache Rust dependencies
  uses: Swatinem/rust-cache@v2
  with:
    # Cache key includes test configuration
    key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}-${{ env.PROPTEST_CASES }}
    
- name: Cache test fixtures
  uses: actions/cache@v3
  with:
    path: tests/fixtures/generated
    key: test-fixtures-${{ hashFiles('tests/fixtures/**/*.toml') }}
    
- name: Cache proptest regressions
  uses: actions/cache@v3
  with:
    path: proptest-regressions
    key: proptest-regressions-${{ github.sha }}
    restore-keys: |
      proptest-regressions-
```

### Resource Management

```rust
// tests/common/resources.rs
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

pub struct ResourceManager {
    resources: Arc<Mutex<HashMap<String, Box<dyn std::any::Any + Send>>>>,
}

impl ResourceManager {
    pub fn new() -> Self {
        Self {
            resources: Arc::new(Mutex::new(HashMap::new())),
        }
    }
    
    pub fn get_or_create<T, F>(&self, key: &str, factory: F) -> Arc<T>
    where
        T: 'static + Send + Sync,
        F: FnOnce() -> T,
    {
        let mut resources = self.resources.lock().unwrap();
        
        if let Some(resource) = resources.get(key) {
            if let Some(typed_resource) = resource.downcast_ref::<Arc<T>>() {
                return typed_resource.clone();
            }
        }
        
        let new_resource = Arc::new(factory());
        resources.insert(key.to_string(), Box::new(new_resource.clone()));
        new_resource
    }
}

// Usage in tests
lazy_static::lazy_static! {
    static ref RESOURCE_MANAGER: ResourceManager = ResourceManager::new();
}

#[test]
fn test_with_shared_resource() {
    let expensive_resource = RESOURCE_MANAGER.get_or_create("test_db", || {
        create_test_database() // Only created once across all tests
    });
    
    // Use the shared resource
    let result = test_function_with_db(&*expensive_resource);
    assert!(result.is_ok());
}
```

## Failure Handling

### Retry Mechanisms

```yaml
# .github/workflows/retry.yml
- name: Run flaky tests with retry
  uses: nick-fields/retry@v2
  with:
    timeout_minutes: 10
    max_attempts: 3
    retry_on: error
    command: cargo test flaky_tests
    
- name: Run network tests with retry
  uses: nick-fields/retry@v2
  with:
    timeout_minutes: 5
    max_attempts: 3
    retry_wait_seconds: 30
    command: cargo test --features network-tests
```

### Failure Analysis

```rust
// tests/common/failure_analysis.rs
use std::panic;

pub fn analyze_test_failure<F, R>(test_name: &str, test_fn: F) -> R
where
    F: FnOnce() -> R + panic::UnwindSafe,
{
    let result = panic::catch_unwind(test_fn);
    
    match result {
        Ok(value) => value,
        Err(panic_info) => {
            eprintln!("Test '{}' failed with panic:", test_name);
            
            if let Some(message) = panic_info.downcast_ref::<String>() {
                eprintln!("  Message: {}", message);
            } else if let Some(message) = panic_info.downcast_ref::<&str>() {
                eprintln!("  Message: {}", message);
            }
            
            // Collect additional debugging information
            eprintln!("  Environment:");
            eprintln!("    CI: {}", std::env::var("CI").unwrap_or_default());
            eprintln!("    OS: {}", std::env::consts::OS);
            eprintln!("    ARCH: {}", std::env::consts::ARCH);
            
            // Re-panic to fail the test
            panic::resume_unwind(panic_info);
        }
    }
}

// Usage
#[test]
fn analyzed_test() {
    analyze_test_failure("analyzed_test", || {
        // Your test code here
        assert_eq!(2 + 2, 4);
    });
}
```

### Failure Reporting

```yaml
# .github/workflows/reporting.yml
- name: Generate test report
  if: always()
  run: |
    cargo nextest run --all-features --workspace --reporter json > test-results.json
    
- name: Upload test results
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: test-results
    path: test-results.json
    
- name: Comment on PR with test results
  if: failure() && github.event_name == 'pull_request'
  uses: actions/github-script@v6
  with:
    script: |
      const fs = require('fs');
      const testResults = JSON.parse(fs.readFileSync('test-results.json', 'utf8'));
      
      const failedTests = testResults.tests.filter(test => test.status === 'failed');
      
      if (failedTests.length > 0) {
        const comment = `## Test Failures
        
        ${failedTests.length} test(s) failed:
        
        ${failedTests.map(test => `- \`${test.name}\`: ${test.failure_message}`).join('\n')}
        `;
        
        github.rest.issues.createComment({
          issue_number: context.issue.number,
          owner: context.repo.owner,
          repo: context.repo.repo,
          body: comment
        });
      }
```

## Artifact Management

### Test Artifacts

```yaml
# .github/workflows/artifacts.yml
- name: Generate test artifacts
  run: |
    # Generate test coverage report
    cargo llvm-cov --html --output-dir coverage-report
    
    # Generate benchmark report
    cargo bench -- --output-format json > benchmark-results.json
    
    # Collect proptest regression files
    find . -name "proptest-regressions" -type d -exec cp -r {} artifacts/ \;
    
- name: Upload coverage report
  uses: actions/upload-artifact@v3
  with:
    name: coverage-report
    path: coverage-report/
    
- name: Upload benchmark results
  uses: actions/upload-artifact@v3
  with:
    name: benchmark-results
    path: benchmark-results.json
    
- name: Upload proptest regressions
  if: failure()
  uses: actions/upload-artifact@v3
  with:
    name: proptest-regressions
    path: proptest-regressions/
    retention-days: 30
```

### Snapshot Management

```yaml
# .github/workflows/snapshots.yml
- name: Check snapshot consistency
  run: |
    # Run tests to generate any missing snapshots
    UPDATE_EXPECT=1 cargo test expect
    
    # Check if any snapshots were updated
    if ! git diff --quiet; then
      echo "::warning::Snapshots were updated during test run"
      git diff --name-only
      
      # Create a patch file
      git diff > snapshot-updates.patch
    fi
    
- name: Upload snapshot updates
  if: failure()
  uses: actions/upload-artifact@v3
  with:
    name: snapshot-updates
    path: snapshot-updates.patch
    
- name: Auto-update snapshots on main
  if: github.ref == 'refs/heads/main' && github.event_name == 'push'
  run: |
    UPDATE_EXPECT=1 cargo test expect
    
    if ! git diff --quiet; then
      git config --local user.email "action@github.com"
      git config --local user.name "GitHub Action"
      git add .
      git commit -m "Auto-update test snapshots [skip ci]"
      git push
    fi
```

## Cross-Platform Testing

### Platform-Specific Configuration

```yaml
# .github/workflows/cross-platform.yml
strategy:
  matrix:
    include:
      - os: ubuntu-latest
        target: x86_64-unknown-linux-gnu
        features: default
      - os: windows-latest
        target: x86_64-pc-windows-msvc
        features: windows-specific
      - os: macos-latest
        target: x86_64-apple-darwin
        features: unix-specific
        
steps:
  - name: Configure platform-specific settings
    shell: bash
    run: |
      case "${{ matrix.os }}" in
        ubuntu-latest)
          echo "RUST_LOG=debug" >> $GITHUB_ENV
          echo "PROPTEST_CASES=500" >> $GITHUB_ENV
          ;;
        windows-latest)
          echo "RUST_LOG=info" >> $GITHUB_ENV
          echo "PROPTEST_CASES=200" >> $GITHUB_ENV
          ;;
        macos-latest)
          echo "RUST_LOG=debug" >> $GITHUB_ENV
          echo "PROPTEST_CASES=300" >> $GITHUB_ENV
          ;;
      esac
      
  - name: Run platform-specific tests
    run: cargo test --features ${{ matrix.features }}
```

### Cross-Compilation Testing

```yaml
# .github/workflows/cross-compile.yml
- name: Install cross-compilation targets
  run: |
    rustup target add aarch64-unknown-linux-gnu
    rustup target add x86_64-pc-windows-gnu
    
- name: Cross-compile tests
  run: |
    cargo test --target aarch64-unknown-linux-gnu --no-run
    cargo test --target x86_64-pc-windows-gnu --no-run
```

## Security Considerations

### Secure Test Execution

```yaml
# .github/workflows/security.yml
- name: Run tests in secure environment
  run: |
    # Limit network access for tests
    cargo test --offline
    
    # Run with minimal permissions
    cargo test --features no-network
    
  env:
    # Prevent information leakage
    RUST_BACKTRACE: 0
    
- name: Audit dependencies
  run: |
    cargo install cargo-audit
    cargo audit --deny warnings
    
- name: Check for secrets in test files
  run: |
    # Use tools like truffleHog or git-secrets
    git secrets --scan tests/
```

### Isolation and Sandboxing

```rust
// tests/common/sandbox.rs
use std::process::Command;

pub fn run_test_in_sandbox<F>(test_fn: F) -> Result<(), Box<dyn std::error::Error>>
where
    F: FnOnce() -> Result<(), Box<dyn std::error::Error>>,
{
    // Create isolated test environment
    let temp_dir = tempfile::tempdir()?;
    std::env::set_current_dir(&temp_dir)?;
    
    // Set up minimal environment
    std::env::remove_var("HOME");
    std::env::set_var("TMPDIR", temp_dir.path());
    
    // Run the test
    let result = test_fn();
    
    // Cleanup
    std::env::set_current_dir("/")?;
    temp_dir.close()?;
    
    result
}

#[test]
fn sandboxed_test() {
    run_test_in_sandbox(|| {
        // Test code that needs isolation
        Ok(())
    }).unwrap();
}
```

---

Effective CI/CD integration ensures that your advanced testing strategies provide reliable feedback while maintaining reasonable build times and resource usage. By following these patterns, you can create robust testing pipelines that catch issues early and provide confidence in your code quality.