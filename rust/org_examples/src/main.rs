use std::collections::HashMap;
use std::fs;
fn std
fn main() {
    // Create a HashMap with some data
    let mut scores = HashMap::new();
    scores.insert("Alice", 50);
    scores.insert("Bob", 30);

    // Print the scores
    for (name, score) in &scores {
        println!("{}: {}", name, score);
    }

    // Use std::fs to demonstrate library completion
    let path = "/tmp/test.txt";
    
    match fs::read_to_string(path) {
        Ok(contents) => println!("File contents: {}", contents),
        Err(e) => println!("Error reading file: {}", e),
    }
}
