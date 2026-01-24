use dioxus::prelude::*;
use image::{DynamicImage, GrayImage, ImageBuffer, Luma};
use std::fs;
use std::process::Command;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
struct FileEntry {
    path: PathBuf,
    name: String,
    size: u64,
    is_dir: bool,
    is_image: bool,
}

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    let mut original_image = use_signal(|| None::<String>);
    let mut edge_image = use_signal(|| None::<String>);
    let mut status = use_signal(|| String::from("No image loaded"));
    let mut file_path = use_signal(|| String::new());
    let mut lightbox_image = use_signal(|| None::<String>);
    let mut lightbox_title = use_signal(|| String::new());
    let mut current_directory = use_signal(|| {
        std::env::var("HOME")
            .unwrap_or_else(|_| "/".to_string())
    });
    let mut file_entries = use_signal(|| Vec::<FileEntry>::new());
    let mut processing_progress = use_signal(|| 0);
    let mut is_processing = use_signal(|| false);

    // Load files from current directory
    let mut load_directory = move || {
        let dir = current_directory.read().clone();
        if let Ok(entries) = fs::read_dir(&dir) {
            let mut files: Vec<FileEntry> = entries
                .filter_map(|entry| entry.ok())
                .filter_map(|entry| {
                    let path = entry.path();
                    let metadata = fs::metadata(&path).ok()?;
                    let name = path.file_name()?.to_string_lossy().to_string();
                    let is_dir = metadata.is_dir();
                    let is_image = !is_dir && path.extension()
                        .and_then(|s| s.to_str())
                        .map_or(false, |ext| {
                            matches!(ext.to_lowercase().as_str(), "png" | "jpg" | "jpeg" | "bmp" | "gif" | "webp")
                        });
                    
                    Some(FileEntry {
                        path: path.clone(),
                        name,
                        size: metadata.len(),
                        is_dir,
                        is_image,
                    })
                })
                .collect();
            
            // Sort: directories first, then by name
            files.sort_by(|a, b| {
                match (a.is_dir, b.is_dir) {
                    (true, false) => std::cmp::Ordering::Less,
                    (false, true) => std::cmp::Ordering::Greater,
                    _ => a.name.to_lowercase().cmp(&b.name.to_lowercase()),
                }
            });
            
            file_entries.set(files);
        }
    };

    // Load directory on mount
    use_effect(move || {
        load_directory();
    });

    let open_file_dialog = move |_evt: Event<MouseData>| {
        spawn(async move {
            status.set("Opening file chooser...".to_string());
            
            // Try different file dialog tools in order of preference
            let result = tokio::task::spawn_blocking(|| {
                // Try zenity first (common on GNOME/GTK systems)
                if let Ok(output) = Command::new("zenity")
                    .args(&[
                        "--file-selection",
                        "--title=Select an Image",
                        "--file-filter=Images | *.png *.jpg *.jpeg *.bmp *.gif *.webp",
                    ])
                    .output()
                {
                    if output.status.success() {
                        return Some(String::from_utf8_lossy(&output.stdout).trim().to_string());
                    }
                }
                
                // Try kdialog (common on KDE/Plasma systems)
                if let Ok(output) = Command::new("kdialog")
                    .args(&[
                        "--getopenfilename",
                        "~",
                        "*.png *.jpg *.jpeg *.bmp *.gif *.webp | Image files",
                    ])
                    .output()
                {
                    if output.status.success() {
                        return Some(String::from_utf8_lossy(&output.stdout).trim().to_string());
                    }
                }
                
                None
            }).await;
            
            match result {
                Ok(Some(path)) if !path.is_empty() => {
                    println!("Selected file: {}", path);
                    
                    // Update current directory
                    if let Some(parent) = Path::new(&path).parent() {
                        current_directory.set(parent.to_string_lossy().to_string());
                        load_directory();
                    }
                    
                    file_path.set(path.clone());
                    status.set("Processing...".to_string());
                    
                    match fs::read(&path) {
                        Ok(bytes) => {
                            println!("File read successfully, size: {} bytes", bytes.len());
                            match process_image_from_bytes(&bytes) {
                                Ok((orig_base64, edge_base64)) => {
                                    println!("Image processed successfully!");
                                    original_image.set(Some(orig_base64));
                                    edge_image.set(Some(edge_base64));
                                    status.set(format!("Processed: {}", path));
                                }
                                Err(e) => {
                                    println!("Error processing image: {}", e);
                                    status.set(format!("Error: {}", e));
                                }
                            }
                        }
                        Err(e) => {
                            println!("Error reading file: {}", e);
                            status.set(format!("Error reading file: {}", e));
                        }
                    }
                }
                Ok(Some(_)) => {
                    status.set("No file selected".to_string());
                }
                Ok(None) => {
                    status.set("File chooser not available. Please install zenity or kdialog, or enter path manually.".to_string());
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                    status.set("Error opening file chooser".to_string());
                }
            }
        });
    };

    let mut open_lightbox = move |image: String, title: String| {
        lightbox_image.set(Some(image));
        lightbox_title.set(title);
    };

    let close_lightbox = move |_evt: Event<MouseData>| {
        lightbox_image.set(None);
    };

    let mut select_file_from_browser = move |entry: FileEntry| {
        if entry.is_dir {
            // Navigate into directory
            current_directory.set(entry.path.to_string_lossy().to_string());
            load_directory();
            status.set(format!("Opened directory: {}", entry.name));
            return;
        }
        
        if !entry.is_image {
            status.set("Not an image file".to_string());
            return;
        }
        
        let path_str = entry.path.to_string_lossy().to_string();
        file_path.set(path_str.clone());
        
        spawn(async move {
            is_processing.set(true);
            processing_progress.set(0);
            status.set("Starting...".to_string());
            
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            processing_progress.set(15);
            status.set("Reading file...".to_string());
            
            match fs::read(&path_str) {
                Ok(bytes) => {
                    tokio::time::sleep(std::time::Duration::from_millis(150)).await;
                    processing_progress.set(40);
                    println!("File read successfully, size: {} bytes", bytes.len());
                    status.set("Decoding image...".to_string());
                    
                    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                    processing_progress.set(60);
                    status.set("Applying edge detection...".to_string());
                    
                    match process_image_from_bytes(&bytes) {
                        Ok((orig_base64, edge_base64)) => {
                            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                            processing_progress.set(90);
                            println!("Image processed successfully!");
                            status.set("Finalizing...".to_string());
                            
                            original_image.set(Some(orig_base64));
                            edge_image.set(Some(edge_base64));
                            
                            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                            processing_progress.set(100);
                            status.set(format!("✓ Processed: {}", entry.name));
                            
                            // Reset progress after showing complete
                            tokio::time::sleep(std::time::Duration::from_millis(800)).await;
                            is_processing.set(false);
                            processing_progress.set(0);
                        }
                        Err(e) => {
                            println!("Error processing image: {}", e);
                            status.set(format!("Error: {}", e));
                            is_processing.set(false);
                            processing_progress.set(0);
                        }
                    }
                }
                Err(e) => {
                    println!("Error reading file: {}", e);
                    status.set(format!("Error reading file: {}", e));
                    is_processing.set(false);
                    processing_progress.set(0);
                }
            }
        });
    };

    let mut change_directory = move |new_path: String| {
        if !new_path.is_empty() {
            let path = Path::new(&new_path);
            if path.is_dir() {
                current_directory.set(new_path);
                load_directory();
                status.set("Directory changed".to_string());
            } else if path.is_file() {
                if let Some(parent) = path.parent() {
                    current_directory.set(parent.to_string_lossy().to_string());
                    load_directory();
                }
            } else {
                status.set("Invalid path".to_string());
            }
        }
    };

    let mut path_input = use_signal(|| String::new());

    let on_path_input = move |evt: Event<FormData>| {
        path_input.set(evt.value().clone());
    };

    let on_path_keypress = move |evt: Event<KeyboardData>| {
        if evt.key() == Key::Enter {
            let path = path_input.read().clone();
            change_directory(path);
            path_input.set(String::new());
        }
    };

    // Sync path_input with current_directory
    use_effect(move || {
        path_input.set(current_directory.read().clone());
    });

    let go_to_parent = move |_evt: Event<MouseData>| {
        let current = current_directory.read().clone();
        if let Some(parent) = Path::new(&current).parent() {
            change_directory(parent.to_string_lossy().to_string());
        }
    };

    let go_to_home = move |_evt: Event<MouseData>| {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/".to_string());
        change_directory(home);
    };

    let go_to_pictures = move |_evt: Event<MouseData>| {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/".to_string());
        let pictures = format!("{}/Pictures", home);
        if Path::new(&pictures).exists() {
            change_directory(pictures);
        } else {
            change_directory(home);
        }
    };

    let go_to_downloads = move |_evt: Event<MouseData>| {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/".to_string());
        let downloads = format!("{}/Downloads", home);
        if Path::new(&downloads).exists() {
            change_directory(downloads);
        } else {
            change_directory(home);
        }
    };

    let quit_app = move |_evt: Event<MouseData>| -> () {
        std::process::exit(0);
    };

    rsx! {
        style { {include_str!("style.css")} }
        div { class: "app-container",
            // Sidebar file browser
            div { class: "sidebar",
                h3 { class: "sidebar-title", "📁 Image Browser" }
                
                // Quick navigation buttons
                div { class: "quick-nav",
                    button {
                        class: "nav-button",
                        onclick: go_to_home,
                        title: "Home",
                        "🏠 Home"
                    }
                    button {
                        class: "nav-button",
                        onclick: go_to_pictures,
                        title: "Pictures",
                        "🖼️ Pictures"
                    }
                    button {
                        class: "nav-button",
                        onclick: go_to_downloads,
                        title: "Downloads",
                        "📥 Downloads"
                    }
                    button {
                        class: "nav-button",
                        onclick: go_to_parent,
                        title: "Parent Directory",
                        "⬆️ Up"
                    }
                }
                
                // Current directory display
                input {
                    class: "current-path",
                    r#type: "text",
                    value: "{path_input}",
                    oninput: on_path_input,
                    onkeydown: on_path_keypress,
                    placeholder: "Enter directory path...",
                }
                
                div { class: "file-list",
                    if file_entries.read().is_empty() {
                        p { class: "no-files", "Empty directory" }
                    }
                    for entry in file_entries.read().iter() {
                        {
                            let file_size_str = format_file_size(entry.size);
                            let entry_clone = entry.clone();
                            let item_class = if entry.is_image { 
                                "file-item image-file" 
                            } else if entry.is_dir { 
                                "file-item dir-item" 
                            } else { 
                                "file-item" 
                            };
                            let icon = if entry.is_dir { 
                                "📁" 
                            } else if entry.is_image { 
                                "🖼️" 
                            } else { 
                                "📄" 
                            };
                            let name = entry.name.clone();
                            
                            rsx! {
                                div { 
                                    key: "{entry.path.to_string_lossy()}",
                                    class: "{item_class}",
                                    onclick: move |_| select_file_from_browser(entry_clone.clone()),
                                    span { class: "file-icon", "{icon}" }
                                    span { class: "file-name", "{name}" }
                                    span { class: "file-size", "{file_size_str}" }
                                }
                            }
                        }
                    }
                }
            }

            // Main content area
            div { class: "main-content",
                div { class: "container",
                    div { class: "header-bar",
                        h1 { "Edge Detector" }
                        button {
                            class: "quit-button",
                            onclick: quit_app,
                            title: "Quit Application",
                            "❌ Quit"
                        }
                    }
            
                    div { class: "controls",
                        h2 { class: "subtitle", "Load Image" }
                
                        button { 
                            class: "file-button",
                            onclick: open_file_dialog,
                            "📁 Choose File"
                        }
                        
                        // Progress bar
                        if is_processing() {
                            div { class: "progress-container",
                                div { class: "progress-bar",
                                    div { 
                                        class: "progress-fill",
                                        style: "width: {processing_progress()}%",
                                    }
                                }
                                p { class: "progress-text", "{processing_progress()}%" }
                            }
                        }
                        
                        p { class: "status", "{status}" }
                        p { class: "hint", "💡 Click images in sidebar or use Choose File button" }
                    }

                    div { class: "images",
                        div { class: "image-box",
                            h3 { "Original" }
                            if let Some(img) = original_image() {
                                img { 
                                    class: "clickable-image",
                                    src: "data:image/png;base64,{img}",
                                    alt: "Original image",
                                    onclick: move |_| open_lightbox(img.clone(), "Original Image".to_string()),
                                }
                            } else {
                                div { class: "placeholder", "No image" }
                            }
                        }

                        div { class: "image-box",
                            h3 { "Edge Detection" }
                            if let Some(img) = edge_image() {
                                img { 
                                    class: "clickable-image",
                                    src: "data:image/png;base64,{img}",
                                    alt: "Edge detected image",
                                    onclick: move |_| open_lightbox(img.clone(), "Edge Detection".to_string()),
                                }
                            } else {
                                div { class: "placeholder", "No image" }
                            }
                        }
                    }

                    // Lightbox overlay
                    if let Some(img) = lightbox_image() {
                        div { 
                            class: "lightbox-overlay",
                            onclick: close_lightbox,
                            div { 
                                class: "lightbox-content",
                                onclick: move |evt: Event<MouseData>| evt.stop_propagation(),
                                h2 { class: "lightbox-title", "{lightbox_title}" }
                                img { 
                                    class: "lightbox-image",
                                    src: "data:image/png;base64,{img}",
                                    alt: "Full resolution view",
                                }
                                p { class: "lightbox-hint", "Click outside to close" }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn process_image_from_bytes(bytes: &[u8]) -> Result<(String, String), String> {
    // Load the image from bytes
    let img = image::load_from_memory(bytes)
        .map_err(|e| format!("Failed to load image: {}", e))?;

    // Convert original to base64
    let original_base64 = image_to_base64(&img)?;

    // Convert to grayscale
    let gray = img.to_luma8();

    // Apply Sobel edge detection
    let edges = sobel_edge_detection(&gray);

    // Convert edge image to DynamicImage
    let edge_img = DynamicImage::ImageLuma8(edges);

    // Convert edges to base64
    let edge_base64 = image_to_base64(&edge_img)?;

    Ok((original_base64, edge_base64))
}

fn image_to_base64(img: &DynamicImage) -> Result<String, String> {
    let mut buffer = Vec::new();
    img.write_to(&mut std::io::Cursor::new(&mut buffer), image::ImageFormat::Png)
        .map_err(|e| format!("Failed to encode image: {}", e))?;
    Ok(base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &buffer))
}

fn sobel_edge_detection(img: &GrayImage) -> GrayImage {
    let (width, height) = img.dimensions();
    let mut output = ImageBuffer::new(width, height);

    // Sobel kernels
    let gx = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
    let gy = [[-1, -2, -1], [0, 0, 0], [1, 2, 1]];

    for y in 1..height - 1 {
        for x in 1..width - 1 {
            let mut sum_x = 0i32;
            let mut sum_y = 0i32;

            for ky in 0..3 {
                for kx in 0..3 {
                    let pixel = img.get_pixel(x + kx - 1, y + ky - 1)[0] as i32;
                    sum_x += pixel * gx[ky as usize][kx as usize];
                    sum_y += pixel * gy[ky as usize][kx as usize];
                }
            }

            let magnitude = ((sum_x * sum_x + sum_y * sum_y) as f64).sqrt();
            let pixel_value = magnitude.min(255.0) as u8;
            output.put_pixel(x, y, Luma([pixel_value]));
        }
    }

    output
}

fn format_file_size(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;
    
    if bytes >= GB {
        format!("{:.1} GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.1} MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.1} KB", bytes as f64 / KB as f64)
    } else {
        format!("{} B", bytes)
    }
}
