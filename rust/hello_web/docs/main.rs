fn main(){
    println!("hello world");
}

let name = "John";
let age = 30;
println!("{} is {} years old.", name, age);

let mut x = 5;
println!("Before: {}", x);
x = 10;
println!("After: {}", x);

fn main() {
    let z = 5;  // Immutable variable
    let z = z + 1;  // Shadowing the previous variable
    println!("The value of z after shadowing is: {}", z);
    let z = "six";  // Shadowing with a different type
    println!("The value of z after shadowing with a different type is: {}", z);
}

fn main() {
    let int_num: i32 = -42;         // 32-bit signed integer
    let uint_num: u64 = 123456;     // 64-bit unsigned integer
    println!("Signed Integer: {}", int_num);
    println!("Unsigned Integer: {}", uint_num);
}

let age: i32 = 25;
println!("Age is: {}", age);

let price: f64 = 19.99;
println!("Price is: ${}", price);

fn main() {
    let float_num: f64 = 3.14;      // 64-bit floating-point number
    println!("Floating-point: {}", float_num);
}

let my_grade: char = 'B';
println!("{}", my_grade);

fn main() {
    let char_symbol: char = 'A';    // Unicode character
    println!("Character: {}", char_symbol);
}

let name: &str = "John";
println!("Hello, {}!", name);

let is_logged_in: bool = true;
println!("User logged in? {}", is_logged_in);

fn main() {
    let is_true: bool = true;       // Boolean (true/false)
    println!("Boolean: {}", is_true);
}

fn main() {
    const BIRTHYEAR: i32 = 1980;
    const MINUTES_PER_HOUR: i32 = 60;
    const MAX_POINTS: u32 = 100_000;
    println!("The maximum points are: {}", MAX_POINTS);
}

fn main() {
  let add = 5 + 3;
  let sub = 10 - 4;
  let mul = 6 * 2;
  let div = 12 / 3;
  let rem = 10 % 3;

  println!("Add: {}", add);
  println!("Sub: {}", sub);
  println!("Mul: {}", mul);
  println!("Div: {}", div);
  println!("Rem: {}", rem);
}

fn main() {
  let mut x = 10;
  println!("Start: {}", x);

  x += 5;
  println!("After += 5: {}", x);

  x -= 2;
  println!("After -= 2: {}", x);

  x *= 2;
  println!("After *= 2: {}", x);

  x /= 3;
  println!("After /= 3: {}", x);

  x %= 4;
  println!("After %= 4: {}", x);
}

fn main() {
  let a = 5;
  let b = 10;

  println!("5 == 10: {}", a == b);
  println!("5 != 10: {}", a != b);
  println!("5 < 10: {}", a < b);
  println!("5 >= 10: {}", a >= b);
}

fn main() {
  let logged_in = true;
  let is_admin = false;

  println!("Is regular user: {}", logged_in && !is_admin);
  println!("Has any access: {}", logged_in || is_admin);
  println!("Not logged in: {}", !logged_in);
}

let is_programming_fun: bool = true;
let is_fish_tasty: bool = false;

println!("Is Programming Fun? {}", is_programming_fun);
println!("Is Fish Tasty? {}", is_fish_tasty);

let is_programming_fun = true;
let is_fish_tasty = false;

println!("Is Programming Fun? {}", is_programming_fun);
println!("Is Fish Tasty? {}", is_fish_tasty);

let age = 20;
let can_vote = age >= 18;

println!("Can vote? {}", can_vote);

let is_logged_in = true;

if is_logged_in {
  println!("Welcome back!");
} else {
  println!("Please log in.");
}

let x = 7;
let y = 5;

if x > y {
  println!("x is greater than y.");
}

let age = 16;

if age >= 18 {
  println!("You can vote.");
} else {
  println!("You are too young to vote.");
}

let score = 85;

if score >= 90 {
  println!("Grade: A");
} else if score >= 80 {
  println!("Grade: B");
} else if score >= 70 {
  println!("Grade: C");
} else {
  println!("Grade: F");
}

let time = 20;
let greeting = if time < 18 {
  "Good day."
} else {
  "Good evening."
};
println!("{}", greeting);

let number = 5;
let result = if number < 10 { "Too small" } else { 100 };
println!("{}", result);

fn main() {
    let day = 4;

    match day {
	1 => println!("Monday"),
	2 => println!("Tuesday"),
	3 => println!("Wednesday"),
	4 => println!("Thursday"),
	5 => println!("Friday"),
	6 => println!("Saturday"),
	7 => println!("Sunday"),
	_ => println!("Invalid day."),
    }
}

fn main() {
  let day = 6;

  match day {
    1 | 2 | 3 | 4 | 5 => println!("Weekday"),
    6 | 7 => println!("Weekend"),
    _ => println!("Invalid day"),
  }
}

fn main() {
  let day = 4;

  let result = match day {
    1 => "Monday",
    2 => "Tuesday",
    3 => "Wednesday",
    4 => "Thursday",
    5 => "Friday",
    6 => "Saturday",
    7 => "Sunday",
    _ => "Invalid day.",
  };

  println!("{}", result);
}

let mut count = 1;

loop {
  println!("Hello World!");

  if count == 3 {
    break;
  }

  count += 1;
}

let mut count = 1;

let result = loop {
  println!("Hello!");

  if count == 3 {
    break count; // Stop the loop and return the number 3
  }

  count += 1;
};

println!("The loop stopped at: {}", result);

let mut count = 1;

while count <= 5 {
  println!("Count: {}", count);
  count += 1;
}

let mut num = 1;

while num <= 10 {
  if num == 6 {
    break;
  }
  println!("Number: {}", num);
  num += 1;
}

let mut num = 1;

while num <= 10 {
  if num == 6 {
    num += 1;
    continue;
  }

  println!("Number: {}", num);
  num += 1;
}

for i in 1..6 {
  println!("i is: {}", i);
}

for i in 1..=6 {
  println!("i is: {}", i);
}

for i in 1..=10 {
  if i == 3 {
    continue; // skip 3
  }
  if i == 5 {
    break; // stop before printing 5
  }
  println!("i is: {}", i);
}

// Create a function
fn say_hello() {
  println!("Hello from a function!");
}

say_hello(); // Call the function

fn greet(name: &str) {
  println!("Hello, {}!", name);
}

greet("John");

fn add(a: i32, b: i32) -> i32 {
  return a + b;
}

let sum = add(3, 4);
println!("Sum is: {}", sum);

fn add(a: i32, b: i32) -> i32 {
  a + b  // omit return
}

let sum = add(3, 4);
println!("Sum is: {}", sum);

let x = 5;

{
  let x = 10;
  println!("Inside block: {}", x);
}

println!("Outside block: {}", x);

let greeting: &str = "Hello";
println!("{}", greeting);

let text1 = "Hello World".to_string();
println!("{}", text1)

let text2 = String::from("Hello World");
println!("{}", text2)

let mut greeting = String::from("Hello");
greeting.push_str(" World");
println!("{}", greeting); // Hello World

let mut word = String::from("Hi");
word.push('!');
println!("{}", word); // Hi!

let s1 = String::from("Hello");
let s2 = String::from("World!");
let s3 = String::from("What a beautiful day!");
let result = format!("{} {} {}", s1, s2, s3);
println!("{}", result);

let s1 = String::from("Hello");
let s2 = String::from("World!");
let s3 = String::from("What a beautiful day!");
let result = s1 + " " + &s2 + " " + &s3;
println!("{}", result);

let name = String::from("John");
println!("Length: {}", name.len()); // 4

let a = String::from("Hello");
let b = a;

// println!("{}", a); Error: a no longer owns the value
println!("{}", b); // Ok: b now owns the value

let a = 5;
let b = a;
println!("a = {}", a);  // Works
println!("b = {}", b);  // Works

let a = String::from("Hello");
let b = a.clone(); // Now both have the same value

println!("a = {}", a);  // Works
println!("b = {}", b);  // Works

let a = String::from("Hello");
let b = &a;

println!("a = {}", a);
println!("b = {}", b);

let mut name = String::from("John");
let name_ref = &mut name;
name_ref.push_str(" Doe");

println!("{}", name_ref); // John Doe

let fruits = ["apple", "banana", "orange"];
println!("Last fruit: {}", fruits[2]);

let mut fruits = vec!["apple", "banana"];
fruits.push("cherry");

println!("Last fruit: {}", fruits[2]);

let person = ("John", 30, true);
println!("Name: {}", person.0);
println!("Age: {}", person.1);
println!("Is active: {}", person.2);

// Import HashMap
use std::collections::HashMap;

fn main() {
  let mut capital_cities = HashMap::new();
  capital_cities.insert("France", "Paris");
  capital_cities.insert("Japan", "Tokyo");

  println!("Capital of Japan is {}", capital_cities["Japan"]);
}

let numbers = [1, 2, 3, 4, 5];
println!("The first number is: {}", numbers[0]);

let mut numbers = [1, 2, 3, 4, 5];
numbers[0] = 10;
println!("The new first number is: {}", numbers[0]);

let numbers = [1, 2, 3, 4, 5];
println!("This array has {} elements.", numbers.len());

let fruits = ["apple", "banana", "orange"];
for fruit in fruits {
  println!("I like {}.", fruit);
}

let numbers = [1, 2, 3, 4, 5];
println!("{:?}", numbers);

let numbers = [1, 2, 3, 4, 5];
println!("{}", numbers[0]);

// A vector with 3 elements
let mut cars = vec!["Volvo", "BMW", "Ford"];

// Add another element
cars.push("Mazda");

println!("{:?}", cars); // ["Volvo", "BMW", "Ford", "Mazda"]

let fruits = vec!["apple", "banana", "orange"];
println!("First fruit: {}", fruits[0]);

let mut fruits = vec!["apple", "banana", "orange"];
fruits[0] = "grape";
println!("New first fruit: {}", fruits[0]);

let mut fruits = vec!["apple", "banana"];
fruits.push("cherry");
println!("{:?}", fruits); // ["apple", "banana", "cherry"]

let mut fruits = vec!["apple", "banana", "cherry"];
fruits.pop();
println!("{:?}", fruits); // ["apple", "banana"]

let mut fruits = vec!["banana", "orange"];
fruits.insert(0, "apple");
println!("{:?}", fruits); // ["apple", "banana", "orange"]

let mut fruits = vec!["banana", "orange"];
fruits.insert(1, "apple");
println!("{:?}", fruits); // ["banana", "apple", "orange"]

let mut fruits = vec!["apple", "banana", "orange"];
fruits.remove(0);
println!("{:?}", fruits); // ["banana", "orange"]

let fruits = vec!["apple", "banana", "cherry"];
println!("There are {} fruits.", fruits.len());

let fruits = vec!["apple", "banana", "orange"];
for fruit in &fruits {
  println!("I like {}.", fruit);
}

let person = ("John", 30, true);
println!("Name: {}", person.0);
println!("Age: {}", person.1);
println!("Is active: {}", person.2);

let person = ("Jenny", 45, false);
let (name, age, active) = person;

println!("Name: {}", name);
println!("Age: {}", age);
println!("Active: {}", active);

fn get_user() -> (String, i32) {
  (String::from("Liam"), 25)
}

fn main() {
  let user = get_user();
  println!("User: {} ({} years old)", user.0, user.1);
}

// Import HashMap
use std::collections::HashMap;

fn main() {
  // Create a HashMap called capitalCities
  let mut capitalCities = HashMap::new();

  // Add keys and values (Country, City)
  capitalCities.insert("England", "London");
  capitalCities.insert("Germany", "Berlin");
  capitalCities.insert("Norway", "Oslo");

  println!("{:?}", capitalCities);
}

// Import HashMap
use std::collections::HashMap;

let mut capitalCities = HashMap::new();

capitalCities.insert("England", "London");
capitalCities.insert("Germany", "Berlin");
capitalCities.insert("Norway", "Oslo");

if let Some(city) = capitalCities.get("England") {
  println!("The capital of England is {}.", city);
} else {
  println!("England is not in the map.");
}

// Import HashMap
use std::collections::HashMap;

let mut capitalCities = HashMap::new();

capitalCities.insert("England", "London");
capitalCities.insert("England", "Berlin");

println!("{:?}", capitalCities);

// Import HashMap
use std::collections::HashMap;

let mut capitalCities = HashMap::new();

// Add keys and values (Country, City)
capitalCities.insert("England", "London");
capitalCities.insert("Germany", "Berlin");
capitalCities.insert("Norway", "Oslo");

// Remove the key "England"
capitalCities.remove("England");

println!("{:?}", capitalCities);

// Import HashMap
use std::collections::HashMap;

let mut capitalCities = HashMap::new();

// Add keys and values (Country, City)
capitalCities.insert("England", "London");
capitalCities.insert("Germany", "Berlin");
capitalCities.insert("Norway", "Oslo");

// Loop through the HashMap
for (country, city) in &capitalCities {
  println!("The capital of {} is {}.", country, city);
}

// Create a Struct called Person
struct Person {
  name: String,
  age: u32,
  can_vote: bool,
}

// Create a Person object
let user = Person {
  name: String::from("John"),
  age: 35,
  can_vote: true,
};

// Access and print the values
println!("Name: {}", user.name);
println!("Age: {}", user.age);
println!("Can vote? {}", user.can_vote);

struct Person {
  name: String,
  age: u32,
}

let mut user = Person {
  name: String::from("John"),
  age: 35,
};

user.age = 36; // Change value of age
println!("Name: {}", user.name);
println!("Updated age: {}", user.age);

enum Direction {
  Up,
  Down,
  Left,
  Right,
}

fn main() {
  let my_direction = Direction::Up;
  println!("We are going up!");
}

enum Direction {
  Up,
  Down,
  Left,
  Right,
}

fn main() {
  let my_direction = Direction::Left;

  match my_direction {
    Direction::Up => println!("Going up"),
    Direction::Down => println!("Going down"),
    Direction::Left => println!("Going left"),
    Direction::Right => println!("Going right"),
  }
}

enum LoginStatus {
  Success(String),
  Error(String),
}

fn main() {
  let result1 = LoginStatus::Success(String::from("Welcome, John!"));
  let result2 = LoginStatus::Error(String::from("Incorrect password"));

  match result1 {
    LoginStatus::Success(message) => println!("Success: {}", message),
    LoginStatus::Error(message) => println!("Error: {}", message),
  }
}

fn main() {
    // Function definition
    fn greet(name: &str) {
        println!("Hello, {}!", name);
    }
    // Function invocation
    greet("Alice");
    greet("Bob");
    // Another function example
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }
    let sum = add(5, 7);
    println!("The sum is: {}", sum);
}

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

fn main() {
    let user1 = User {
        username: String::from("someusername123"),
        email: String::from("someone@example.com"),
        sign_in_count: 1,
        active: true,
    };
    println!("Username: {}", user1.username);
    println!("Email: {}", user1.email);
    println!("Sign in count: {}", user1.sign_in_count);
    println!("Active: {}", user1.active);
}

struct Color(i32, i32, i32);
fn main() {
    let black = Color(0, 0, 0);
    println!("Black: ({}, {}, {})", black.0, black.1, black.2);
}

struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}
fn main() {
    let user1 = User {
        username: String::from("user1"),
        email: String::from("user1@example.com"),
        sign_in_count: 1,
        active: true,
    };
    let user2 = User {
        email: String::from("user2@example.com"),
        ..user1 // Copies the remaining fields from user1
    };
    println!("Username: {}", user2.username);
    println!("Email: {}", user2.email);
    println!("Sign in count: {}", user2.sign_in_count);
    println!("Active: {}", user2.active);
}

struct Rectangle {
    width: u32,
    height: u32,
}
impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}
fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };
    let rect2 = Rectangle {
        width: 10,
        height: 40,
    };
    let rect3 = Rectangle {
        width: 60,
        height: 45,
    };
    println!("The area of rect1 is {} square pixels.", rect1.area());
    println!("Can rect1 hold rect2? {}", rect1.can_hold(&rect2));
    println!("Can rect1 hold rect3? {}", rect1.can_hold(&rect3));
}

struct Rectangle {
    width: u32,
    height: u32,
}
impl Rectangle {
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }
    // A method (takes &self)
    fn area(&self) -> u32 {
        self.width * self.height
    }
}
fn main() {
    let sq = Rectangle::square(3);
    println!("The area of the square is {} square pixels.", sq.area());
}


