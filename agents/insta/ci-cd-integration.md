# CI/CD Integration

Integrating Insta snapshot tests into your CI/CD pipeline ensures that snapshot changes are properly reviewed and that tests remain reliable across different environments.

## Core Principles

1. **Never auto-update snapshots in CI** - Always require manual review
2. **Fail fast on snapshot mismatches** - Catch issues early
3. **Provide clear feedback** - Help developers understand what changed
4. **Support multiple environments** - Handle platform differences

## Environment Variables

### Essential CI Configuration

```bash
# Prevent automatic snapshot updates
export INSTA_UPDATE=no

# Force tests to pass but report snapshot mismatches
export INSTA_FORCE_PASS=1

# Use consistent snapshot paths
export INSTA_SNAPSHOT_PATH=tests/snapshots
```

### Advanced Configuration

```bash
# Disable colored output in CI
export INSTA_FORCE_NO_COLOR=1

# Set workspace root explicitly
export INSTA_WORKSPACE_ROOT=/workspace

# Control glob behavior
export INSTA_GLOB_FILTER="*.rs"
```

## GitHub Actions

### Basic Setup

```yaml
name: Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          components: rustfmt, clippy
          
      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
          
      - name: Run tests
        run: |
          INSTA_UPDATE=no cargo test
        env:
          RUST_BACKTRACE: 1
```

### Advanced GitHub Actions Workflow

```yaml
name: Comprehensive Tests
on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        rust: [stable, beta]
        
    runs-on: ${{ matrix.os }}
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: ${{ matrix.rust }}
          override: true
          
      - name: Install cargo-insta
        run: cargo install cargo-insta
        
      - name: Run tests
        run: |
          INSTA_UPDATE=no cargo test --all-features
        env:
          RUST_BACKTRACE: 1
          
      - name: Check for pending snapshots
        run: |
          if cargo insta pending-snapshots --check; then
            echo "✅ All snapshots are up to date"
          else
            echo "❌ Snapshots need review!"
            echo "Run 'cargo insta review' locally to update snapshots"
            exit 1
          fi
          
      - name: Upload snapshot artifacts (on failure)
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: snapshot-diffs-${{ matrix.os }}-${{ matrix.rust }}
          path: |
            tests/snapshots/**/*.snap.new
            **/*.snap.new
```

### Snapshot Review Workflow

```yaml
name: Review Snapshots
on:
  workflow_dispatch:
    inputs:
      auto_approve:
        description: 'Auto-approve all snapshots'
        required: false
        default: 'false'

jobs:
  review:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
          
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          
      - name: Install cargo-insta
        run: cargo install cargo-insta
        
      - name: Generate snapshots
        run: |
          INSTA_UPDATE=always cargo test
          
      - name: Review snapshots
        run: |
          if [ "${{ github.event.inputs.auto_approve }}" = "true" ]; then
            cargo insta accept
          else
            cargo insta pending-snapshots
          fi
          
      - name: Commit updated snapshots
        if: github.event.inputs.auto_approve == 'true'
        run: |
          git config --local user.email "action@github.com"
          git config --local user.name "GitHub Action"
          git add tests/snapshots/
          git diff --staged --quiet || git commit -m "Update snapshots [skip ci]"
          git push
```

## GitLab CI

### Basic Configuration

```yaml
# .gitlab-ci.yml
stages:
  - test
  - review

variables:
  CARGO_HOME: $CI_PROJECT_DIR/.cargo
  INSTA_UPDATE: "no"
  RUST_BACKTRACE: "1"

cache:
  paths:
    - .cargo/
    - target/

test:
  stage: test
  image: rust:latest
  script:
    - cargo test --all-features
  artifacts:
    when: on_failure
    paths:
      - "**/*.snap.new"
    expire_in: 1 week

snapshot-check:
  stage: test
  image: rust:latest
  before_script:
    - cargo install cargo-insta
  script:
    - cargo insta pending-snapshots --check
  allow_failure: false
```

### Advanced GitLab Pipeline

```yaml
.rust-template: &rust-template
  image: rust:latest
  before_script:
    - apt-get update && apt-get install -y git
    - cargo --version
    - rustc --version

test:matrix:
  <<: *rust-template
  stage: test
  parallel:
    matrix:
      - RUST_VERSION: ["stable", "beta", "nightly"]
  image: rust:${RUST_VERSION}
  script:
    - INSTA_UPDATE=no cargo test --all-features
  artifacts:
    when: on_failure
    paths:
      - "tests/snapshots/**/*.snap.new"
    reports:
      junit: test-results.xml

snapshot-review:
  <<: *rust-template
  stage: review
  when: manual
  before_script:
    - cargo install cargo-insta
  script:
    - INSTA_UPDATE=always cargo test
    - cargo insta pending-snapshots
  artifacts:
    paths:
      - "tests/snapshots/"
```

## Jenkins

### Jenkinsfile Example

```groovy
pipeline {
    agent any
    
    environment {
        CARGO_HOME = "${WORKSPACE}/.cargo"
        INSTA_UPDATE = "no"
        RUST_BACKTRACE = "1"
    }
    
    stages {
        stage('Setup') {
            steps {
                sh 'curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y'
                sh 'source ~/.cargo/env && cargo --version'
            }
        }
        
        stage('Test') {
            steps {
                sh 'source ~/.cargo/env && cargo test --all-features'
            }
            post {
                failure {
                    archiveArtifacts artifacts: '**/*.snap.new', allowEmptyArchive: true
                }
            }
        }
        
        stage('Snapshot Check') {
            steps {
                sh '''
                    source ~/.cargo/env
                    cargo install cargo-insta
                    if ! cargo insta pending-snapshots --check; then
                        echo "Snapshots need review!"
                        exit 1
                    fi
                '''
            }
        }
    }
    
    post {
        always {
            cleanWs()
        }
    }
}
```

## Azure DevOps

### azure-pipelines.yml

```yaml
trigger:
  - main
  - develop

pool:
  vmImage: 'ubuntu-latest'

variables:
  CARGO_HOME: $(Pipeline.Workspace)/.cargo
  INSTA_UPDATE: 'no'

steps:
- task: Cache@2
  inputs:
    key: 'cargo | "$(Agent.OS)" | Cargo.lock'
    restoreKeys: |
      cargo | "$(Agent.OS)"
    path: $(CARGO_HOME)
  displayName: Cache cargo

- script: |
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    echo "##vso[task.setvariable variable=PATH;]$PATH:$HOME/.cargo/bin"
  displayName: Install Rust

- script: cargo test --all-features
  displayName: Run tests
  env:
    INSTA_UPDATE: no
    RUST_BACKTRACE: 1

- script: |
    cargo install cargo-insta
    cargo insta pending-snapshots --check
  displayName: Check snapshots
  condition: always()

- task: PublishTestResults@2
  condition: always()
  inputs:
    testResultsFormat: 'JUnit'
    testResultsFiles: 'test-results.xml'

- task: PublishBuildArtifacts@1
  condition: failed()
  inputs:
    pathToPublish: '.'
    artifactName: 'snapshot-diffs'
    includeRootFolder: false
```

## Docker Integration

### Multi-stage Dockerfile for Testing

```dockerfile
# Dockerfile.test
FROM rust:1.70 as builder

WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src/ src/
COPY tests/ tests/

# Install cargo-insta
RUN cargo install cargo-insta

# Run tests
ENV INSTA_UPDATE=no
ENV RUST_BACKTRACE=1
RUN cargo test --all-features

# Check snapshots
RUN cargo insta pending-snapshots --check

FROM scratch as artifacts
COPY --from=builder /app/tests/snapshots/ /snapshots/
```

### Docker Compose for CI

```yaml
# docker-compose.ci.yml
version: '3.8'

services:
  test:
    build:
      context: .
      dockerfile: Dockerfile.test
    environment:
      - INSTA_UPDATE=no
      - RUST_BACKTRACE=1
    volumes:
      - ./tests/snapshots:/app/tests/snapshots
      - snapshot-cache:/app/target
    command: cargo test --all-features

  snapshot-check:
    build:
      context: .
      dockerfile: Dockerfile.test
    depends_on:
      - test
    command: cargo insta pending-snapshots --check

volumes:
  snapshot-cache:
```

## Platform-Specific Considerations

### Windows Line Endings

```yaml
# .github/workflows/test.yml
- name: Configure git line endings
  run: |
    git config --global core.autocrlf false
    git config --global core.eol lf
```

### macOS Specific Setup

```yaml
- name: Setup Rust (macOS)
  if: runner.os == 'macOS'
  run: |
    brew install rust
    echo "$HOME/.cargo/bin" >> $GITHUB_PATH
```

### Cross-platform Testing

```rust
// tests/platform_tests.rs
#[cfg(target_os = "windows")]
#[test]
fn test_windows_specific() {
    let result = windows_specific_function();
    insta::assert_snapshot!(result);
}

#[cfg(target_os = "linux")]
#[test]
fn test_linux_specific() {
    let result = linux_specific_function();
    insta::assert_snapshot!(result);
}
```

## Monitoring and Alerts

### Slack Integration

```bash
#!/bin/bash
# scripts/notify-snapshot-changes.sh

if ! cargo insta pending-snapshots --check; then
    curl -X POST -H 'Content-type: application/json' \
        --data '{"text":"🔍 Snapshot tests need review in '"$CI_PROJECT_NAME"'"}' \
        $SLACK_WEBHOOK_URL
fi
```

### Email Notifications

```yaml
# .github/workflows/notify.yml
- name: Send email on snapshot changes
  if: failure()
  uses: dawidd6/action-send-mail@v3
  with:
    server_address: smtp.gmail.com
    server_port: 465
    username: ${{ secrets.MAIL_USERNAME }}
    password: ${{ secrets.MAIL_PASSWORD }}
    subject: Snapshot tests need review
    body: |
      Snapshot tests in ${{ github.repository }} need review.
      
      Please run: cargo insta review
```

## Best Practices for CI/CD

### 1. Never Auto-Update in CI

```bash
# ❌ Never do this in CI
INSTA_UPDATE=always cargo test

# ✅ Always use this in CI
INSTA_UPDATE=no cargo test
```

### 2. Provide Clear Failure Messages

```bash
#!/bin/bash
if ! cargo test; then
    echo "❌ Tests failed!"
    echo "If snapshot tests failed, run 'cargo insta review' locally"
    echo "Then commit the updated snapshots"
    exit 1
fi
```

### 3. Cache Dependencies

```yaml
- name: Cache cargo dependencies
  uses: actions/cache@v3
  with:
    path: |
      ~/.cargo/registry
      ~/.cargo/git
      target
    key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
```

### 4. Parallel Testing

```yaml
strategy:
  matrix:
    test-group: [unit, integration, snapshots]
    
steps:
  - name: Run ${{ matrix.test-group }} tests
    run: |
      case "${{ matrix.test-group }}" in
        "snapshots")
          INSTA_UPDATE=no cargo test --test snapshot_tests
          ;;
        "unit")
          cargo test --lib
          ;;
        "integration")
          cargo test --test integration_tests
          ;;
      esac
```

### 5. Artifact Collection

```yaml
- name: Collect snapshot artifacts
  if: failure()
  uses: actions/upload-artifact@v3
  with:
    name: snapshot-diffs
    path: |
      **/*.snap.new
      tests/snapshots/
    retention-days: 7
```

## Troubleshooting CI Issues

### Permission Problems

```bash
# Ensure proper permissions
chmod -R u+w tests/snapshots/
```

### Path Issues

```bash
# Use absolute paths in CI
export INSTA_SNAPSHOT_PATH="$PWD/tests/snapshots"
```

### Environment Differences

```rust
// Use settings to normalize environment differences
use insta::Settings;

#[test]
fn test_cross_platform() {
    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.bind(|| {
        assert_yaml_snapshot!(data);
    });
}
```