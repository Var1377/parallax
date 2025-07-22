// Web Server Response Cache Example
// 
// This example demonstrates how to use DashMap as a high-performance
// cache for web server responses, including TTL support and cache statistics.

use dashmap::DashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::thread;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone)]
struct CacheEntry {
    content: String,
    content_type: String,
    expires_at: Instant,
    access_count: Arc<AtomicU64>,
}

impl CacheEntry {
    fn new(content: String, content_type: String, ttl: Duration) -> Self {
        Self {
            content,
            content_type,
            expires_at: Instant::now() + ttl,
            access_count: Arc::new(AtomicU64::new(0)),
        }
    }
    
    fn is_expired(&self) -> bool {
        Instant::now() > self.expires_at
    }
    
    fn access(&self) {
        self.access_count.fetch_add(1, Ordering::Relaxed);
    }
}

struct WebCache {
    cache: Arc<DashMap<String, CacheEntry>>,
    default_ttl: Duration,
    max_size: usize,
    
    // Statistics
    hits: Arc<AtomicU64>,
    misses: Arc<AtomicU64>,
    evictions: Arc<AtomicU64>,
}

impl WebCache {
    fn new(max_size: usize, default_ttl: Duration) -> Self {
        Self {
            cache: Arc::new(DashMap::new()),
            default_ttl,
            max_size,
            hits: Arc::new(AtomicU64::new(0)),
            misses: Arc::new(AtomicU64::new(0)),
            evictions: Arc::new(AtomicU64::new(0)),
        }
    }
    
    fn get(&self, key: &str) -> Option<CacheEntry> {
        if let Some(entry) = self.cache.get(key) {
            if entry.is_expired() {
                // Remove expired entry
                self.cache.remove(key);
                self.misses.fetch_add(1, Ordering::Relaxed);
                None
            } else {
                // Update access statistics
                entry.access();
                self.hits.fetch_add(1, Ordering::Relaxed);
                Some(entry.clone())
            }
        } else {
            self.misses.fetch_add(1, Ordering::Relaxed);
            None
        }
    }
    
    fn put(&self, key: String, content: String, content_type: String) {
        self.put_with_ttl(key, content, content_type, self.default_ttl);
    }
    
    fn put_with_ttl(&self, key: String, content: String, content_type: String, ttl: Duration) {
        let entry = CacheEntry::new(content, content_type, ttl);
        
        // Check if we need to evict entries
        if self.cache.len() >= self.max_size {
            self.evict_lru();
        }
        
        self.cache.insert(key, entry);
    }
    
    fn evict_lru(&self) {
        // Simple LRU: find entry with oldest access time
        // In a real implementation, you'd want a more efficient LRU algorithm
        let mut oldest_key = None;
        let mut oldest_time = Instant::now();
        
        for entry in self.cache.iter() {
            if entry.expires_at < oldest_time {
                oldest_time = entry.expires_at;
                oldest_key = Some(entry.key().clone());
            }
        }
        
        if let Some(key) = oldest_key {
            self.cache.remove(&key);
            self.evictions.fetch_add(1, Ordering::Relaxed);
        }
    }
    
    fn cleanup_expired(&self) {
        let now = Instant::now();
        let mut expired_keys = Vec::new();
        
        // Collect expired keys
        for entry in self.cache.iter() {
            if now > entry.expires_at {
                expired_keys.push(entry.key().clone());
            }
        }
        
        // Remove expired entries
        for key in expired_keys {
            self.cache.remove(&key);
        }
    }
    
    fn stats(&self) -> CacheStats {
        let hits = self.hits.load(Ordering::Relaxed);
        let misses = self.misses.load(Ordering::Relaxed);
        let total_requests = hits + misses;
        let hit_rate = if total_requests > 0 {
            hits as f64 / total_requests as f64
        } else {
            0.0
        };
        
        CacheStats {
            hits,
            misses,
            hit_rate,
            entries: self.cache.len(),
            evictions: self.evictions.load(Ordering::Relaxed),
        }
    }
    
    fn clear(&self) {
        self.cache.clear();
    }
}

#[derive(Debug)]
struct CacheStats {
    hits: u64,
    misses: u64,
    hit_rate: f64,
    entries: usize,
    evictions: u64,
}

// Simulated web server request handler
fn handle_request(cache: &WebCache, path: &str) -> (String, String) {
    // Try to get from cache first
    if let Some(entry) = cache.get(path) {
        println!("Cache HIT for {}", path);
        return (entry.content, entry.content_type);
    }
    
    println!("Cache MISS for {}", path);
    
    // Simulate expensive operation (database query, file I/O, etc.)
    thread::sleep(Duration::from_millis(100));
    
    let (content, content_type, ttl) = match path {
        "/api/users" => {
            (r#"{"users": ["alice", "bob", "charlie"]}"#.to_string(), 
             "application/json".to_string(),
             Duration::from_secs(60)) // Cache API responses for 1 minute
        },
        "/static/style.css" => {
            ("body { font-family: Arial; }".to_string(),
             "text/css".to_string(),
             Duration::from_secs(3600)) // Cache static files for 1 hour
        },
        "/dynamic/news" => {
            ("Breaking: DashMap is awesome!".to_string(),
             "text/html".to_string(),
             Duration::from_secs(300)) // Cache news for 5 minutes
        },
        _ => {
            ("404 Not Found".to_string(),
             "text/plain".to_string(),
             Duration::from_secs(60))
        }
    };
    
    // Store in cache
    cache.put_with_ttl(path.to_string(), content.clone(), content_type.clone(), ttl);
    
    (content, content_type)
}

fn main() {
    println!("DashMap Web Server Cache Example");
    println!("================================");
    
    // Create cache with 1000 entry limit and 5-minute default TTL
    let cache = Arc::new(WebCache::new(1000, Duration::from_secs(300)));
    
    // Simulate concurrent web requests
    let mut handles = Vec::new();
    
    for thread_id in 0..4 {
        let cache = cache.clone();
        let handle = thread::spawn(move || {
            let requests = [
                "/api/users",
                "/static/style.css", 
                "/dynamic/news",
                "/api/users", // Repeat to test cache hits
                "/static/style.css",
                "/api/products",
                "/dynamic/news",
            ];
            
            for (i, &path) in requests.iter().enumerate() {
                let (content, content_type) = handle_request(&cache, path);
                println!("Thread {}, Request {}: {} ({}) - {} bytes", 
                        thread_id, i, path, content_type, content.len());
                
                // Small delay between requests
                thread::sleep(Duration::from_millis(50));
            }
        });
        handles.push(handle);
    }
    
    // Wait for all request threads to complete
    for handle in handles {
        handle.join().unwrap();
    }
    
    // Print cache statistics
    println!("\nCache Statistics:");
    println!("================");
    let stats = cache.stats();
    println!("Hits: {}", stats.hits);
    println!("Misses: {}", stats.misses);
    println!("Hit Rate: {:.2}%", stats.hit_rate * 100.0);
    println!("Current Entries: {}", stats.entries);
    println!("Evictions: {}", stats.evictions);
    
    // Demonstrate cache expiration
    println!("\nTesting Cache Expiration:");
    println!("========================");
    
    // Add a short-lived entry
    cache.put_with_ttl(
        "/temp/data".to_string(),
        "Temporary data".to_string(),
        "text/plain".to_string(),
        Duration::from_millis(500)
    );
    
    // Immediate access should hit
    if cache.get("/temp/data").is_some() {
        println!("Short-lived entry found immediately");
    }
    
    // Wait for expiration
    thread::sleep(Duration::from_millis(600));
    
    // Access after expiration should miss
    if cache.get("/temp/data").is_none() {
        println!("Short-lived entry expired as expected");
    }
    
    // Demonstrate cleanup
    println!("\nBefore cleanup: {} entries", cache.cache.len());
    cache.cleanup_expired();
    println!("After cleanup: {} entries", cache.cache.len());
    
    // Final statistics
    let final_stats = cache.stats();
    println!("\nFinal Statistics:");
    println!("================");
    println!("Total Hits: {}", final_stats.hits);
    println!("Total Misses: {}", final_stats.misses);
    println!("Final Hit Rate: {:.2}%", final_stats.hit_rate * 100.0);
    
    // Demonstrate cache warming
    println!("\nCache Warming Example:");
    println!("=====================");
    
    let warm_cache = WebCache::new(100, Duration::from_secs(3600));
    
    // Pre-populate cache with common requests
    let common_paths = [
        ("/api/users", "application/json"),
        ("/api/products", "application/json"),
        ("/static/main.css", "text/css"),
        ("/static/app.js", "application/javascript"),
    ];
    
    for (path, content_type) in &common_paths {
        let content = format!("Warmed content for {}", path);
        warm_cache.put(path.to_string(), content, content_type.to_string());
    }
    
    println!("Warmed cache with {} entries", warm_cache.cache.len());
    
    // Test warmed cache performance
    let start = Instant::now();
    for _ in 0..1000 {
        for (path, _) in &common_paths {
            warm_cache.get(path);
        }
    }
    let duration = start.elapsed();
    
    println!("1000 requests to warmed cache took: {:?}", duration);
    println!("Average request time: {:?}", duration / 4000);
    
    let warm_stats = warm_cache.stats();
    println!("Warmed cache hit rate: {:.2}%", warm_stats.hit_rate * 100.0);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_cache_basic_operations() {
        let cache = WebCache::new(10, Duration::from_secs(60));
        
        // Test miss
        assert!(cache.get("missing").is_none());
        
        // Test put and hit
        cache.put("key".to_string(), "value".to_string(), "text/plain".to_string());
        assert!(cache.get("key").is_some());
        
        let stats = cache.stats();
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }
    
    #[test]
    fn test_cache_expiration() {
        let cache = WebCache::new(10, Duration::from_millis(100));
        
        cache.put_with_ttl(
            "temp".to_string(),
            "data".to_string(),
            "text/plain".to_string(),
            Duration::from_millis(50)
        );
        
        // Should be available immediately
        assert!(cache.get("temp").is_some());
        
        // Wait for expiration
        thread::sleep(Duration::from_millis(60));
        
        // Should be expired
        assert!(cache.get("temp").is_none());
    }
    
    #[test]
    fn test_concurrent_access() {
        let cache = Arc::new(WebCache::new(100, Duration::from_secs(60)));
        let mut handles = Vec::new();
        
        for i in 0..10 {
            let cache = cache.clone();
            let handle = thread::spawn(move || {
                for j in 0..10 {
                    let key = format!("key_{}_{}", i, j);
                    cache.put(key.clone(), "value".to_string(), "text/plain".to_string());
                    assert!(cache.get(&key).is_some());
                }
            });
            handles.push(handle);
        }
        
        for handle in handles {
            handle.join().unwrap();
        }
        
        assert_eq!(cache.cache.len(), 100);
    }
}