use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum LineType {
    Paragraph,
    Heading1,
    Heading2,
    Heading3,
    Heading4,
    UList,
    OList,
    TodoList,
    CodeBlock,
    Link,
    Quote,
    Table,
    Row,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TextType {
    Link,
    Bold,
    Italic,
    Strike,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetterSpacing {
    Xs,      // 0.025em
    Md,      // 0.05em
    Lg,      // 0.1em
    Xl,      // 0.15em
    TwoXl,   // 0.2em
    ThreeXl, // 0.25em
    FourXl,  // 0.3em
    Custom(f32),
}

impl LetterSpacing {
    pub fn to_em(&self) -> f32 {
        match self {
            LetterSpacing::Xs => 0.025,
            LetterSpacing::Md => 0.05,
            LetterSpacing::Lg => 0.1,
            LetterSpacing::Xl => 0.15,
            LetterSpacing::TwoXl => 0.2,
            LetterSpacing::ThreeXl => 0.25,
            LetterSpacing::FourXl => 0.3,
            LetterSpacing::Custom(value) => *value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextStyle {
    pub letter_spacing: LetterSpacing,
}

impl Default for TextStyle {
    fn default() -> Self {
        Self {
            letter_spacing: LetterSpacing::Md,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextDetail {
    pub text_type: TextType,
    pub start: usize,
    pub end: usize,
    pub some_value: Option<String>, // For link URLs or other values
    pub style: TextStyle,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutStyle {
    Start,
    Center,
    End,
}

impl Default for LayoutStyle {
    fn default() -> Self {
        LayoutStyle::Start
    }
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub parent_id: Option<String>,
    pub text: String,
    pub line_number: usize,
    pub line_type: LineType,
    pub layout_style: LayoutStyle,
    pub text_details: Vec<TextDetail>,
    pub children: bool,
}

pub type FlatTree = HashMap<String, AstNode>;

pub struct HmdDocument {
    pub flat_tree: FlatTree,
    pub generation_counter: usize,
    pub line_to_id_map: HashMap<usize, String>, // line_number -> node_id mapping
}

impl Default for HmdDocument {
    fn default() -> Self {
        Self::new()
    }
}

impl HmdDocument {
    pub fn new() -> Self {
        Self {
            flat_tree: HashMap::new(),
            generation_counter: 0,
            line_to_id_map: HashMap::new(),
        }
    }

    pub fn next_generation(&mut self) -> String {
        self.generation_counter += 1;
        self.generation_counter.to_string()
    }

    pub fn add_node(&mut self, mut node: AstNode) -> String {
        let id = self.next_generation();
        
        // Set parent_id based on previous line number
        if node.line_number > 1 {
            let previous_line = node.line_number - 1;
            if let Some(parent_id) = self.line_to_id_map.get(&previous_line) {
                node.parent_id = Some(parent_id.clone());
            }
        }
        
        // Update line to ID mapping
        self.line_to_id_map.insert(node.line_number, id.clone());
        
        self.flat_tree.insert(id.clone(), node);
        id
    }
    
    pub fn get_node_by_line(&self, line_number: usize) -> Option<&AstNode> {
        if let Some(id) = self.line_to_id_map.get(&line_number) {
            self.flat_tree.get(id)
        } else {
            None
        }
    }
    
    pub fn get_parent_node(&self, node_id: &str) -> Option<&AstNode> {
        if let Some(node) = self.flat_tree.get(node_id) {
            if let Some(parent_id) = &node.parent_id {
                return self.flat_tree.get(parent_id);
            }
        }
        None
    }
}

pub struct HmdParser;

impl HmdParser {
    pub fn parse_line_type(input: &str) -> Option<LineType> {
        let trimmed = input.trim();
        
        match trimmed {
            // Basic forms
            "Paragraph" => Some(LineType::Paragraph),
            "Heading1" => Some(LineType::Heading1),
            "Heading2" => Some(LineType::Heading2),
            "Heading3" => Some(LineType::Heading3),
            "Heading4" => Some(LineType::Heading4),
            "UList" => Some(LineType::UList),
            "OList" => Some(LineType::OList),
            "TodoList" => Some(LineType::TodoList),
            "CodeBlock" => Some(LineType::CodeBlock),
            "Link" => Some(LineType::Link),
            "Quote" => Some(LineType::Quote),
            "Table" => Some(LineType::Table),
            "Row" => Some(LineType::Row),
            
            // Abbreviated forms
            "P" => Some(LineType::Paragraph),
            "H1" => Some(LineType::Heading1),
            "H2" => Some(LineType::Heading2),
            "H3" => Some(LineType::Heading3),
            "H4" => Some(LineType::Heading4),
            "UL" => Some(LineType::UList),
            "OL" => Some(LineType::OList),
            "TL" => Some(LineType::TodoList),
            "CB" => Some(LineType::CodeBlock),
            "Li" => Some(LineType::Link),
            "Q" => Some(LineType::Quote),
            "T" => Some(LineType::Table),
            "R" => Some(LineType::Row),
            
            _ => None,
        }
    }
    
    pub fn parse_hmd_line(line: &str, line_number: usize) -> Option<AstNode> {
        if let Some(colon_pos) = line.find(':') {
            let (line_type_str, content) = line.split_at(colon_pos);
            let line_type_str = line_type_str.trim();
            let content = content.trim_start_matches(':').trim();
            
            if let Some(line_type) = Self::parse_line_type(line_type_str) {
                return Some(AstNode {
                    parent_id: None,
                    text: content.to_string(),
                    line_number,
                    line_type,
                    layout_style: LayoutStyle::Start,
                    text_details: Vec::new(),
                    children: false,
                });
            }
        }
        None
    }
    
    pub fn parse_text_details(_text: &str, details_str: &str) -> Vec<TextDetail> {
        let mut details = Vec::new();
        
        // Simple parser for text details like "Link[0:2] Bold[6:10]"
        let parts: Vec<&str> = details_str.split_whitespace().collect();
        
        for part in parts {
            if let Some(bracket_start) = part.find('[') {
                if let Some(bracket_end) = part.find(']') {
                    let text_type_str = &part[..bracket_start];
                    let range_str = &part[bracket_start + 1..bracket_end];
                    
                    if let Some(text_type) = Self::parse_text_type(text_type_str) {
                        if let Some((start, end)) = Self::parse_range(range_str) {
                            details.push(TextDetail {
                                text_type,
                                start,
                                end,
                                some_value: None,
                                style: TextStyle::default(),
                            });
                        }
                    }
                }
            }
        }
        
        details
    }
    
    fn parse_text_type(input: &str) -> Option<TextType> {
        match input {
            "Link" => Some(TextType::Link),
            "Bold" => Some(TextType::Bold),
            "Italic" => Some(TextType::Italic),
            "Strike" => Some(TextType::Strike),
            _ => None,
        }
    }
    
    fn parse_range(range_str: &str) -> Option<(usize, usize)> {
        if let Some(colon_pos) = range_str.find(':') {
            let (start_str, end_str) = range_str.split_at(colon_pos);
            let end_str = end_str.trim_start_matches(':');
            
            if let (Ok(start), Ok(end)) = (start_str.parse::<usize>(), end_str.parse::<usize>()) {
                return Some((start, end));
            }
        }
        None
    }
}

pub struct HmdFormatter;

impl HmdFormatter {
    pub fn format_line_type(line_type: &LineType, abbreviated: bool) -> &'static str {
        if abbreviated {
            match line_type {
                LineType::Paragraph => "P",
                LineType::Heading1 => "H1",
                LineType::Heading2 => "H2",
                LineType::Heading3 => "H3",
                LineType::Heading4 => "H4",
                LineType::UList => "UL",
                LineType::OList => "OL",
                LineType::TodoList => "TL",
                LineType::CodeBlock => "CB",
                LineType::Link => "Li",
                LineType::Quote => "Q",
                LineType::Table => "T",
                LineType::Row => "R",
            }
        } else {
            match line_type {
                LineType::Paragraph => "Paragraph",
                LineType::Heading1 => "Heading1",
                LineType::Heading2 => "Heading2",
                LineType::Heading3 => "Heading3",
                LineType::Heading4 => "Heading4",
                LineType::UList => "UList",
                LineType::OList => "OList",
                LineType::TodoList => "TodoList",
                LineType::CodeBlock => "CodeBlock",
                LineType::Link => "Link",
                LineType::Quote => "Quote",
                LineType::Table => "Table",
                LineType::Row => "Row",
            }
        }
    }
    
    pub fn format_node(node: &AstNode, abbreviated: bool) -> String {
        let type_name = Self::format_line_type(&node.line_type, abbreviated);
        let padding = if abbreviated { 3 } else { 11 };
        
        let mut result = format!("{:width$} : {}", type_name, node.text, width = padding);
        
        if !node.text_details.is_empty() {
            result.push('\n');
            for detail in &node.text_details {
                result.push_str(&format!(
                    "  |- {}[{}:{}]",
                    Self::format_text_type(&detail.text_type),
                    detail.start,
                    detail.end
                ));
                if let Some(ref value) = detail.some_value {
                    result.push_str(&format!(" Value[{}]", value));
                }
                result.push('\n');
            }
        }
        
        result
    }
    
    fn format_text_type(text_type: &TextType) -> &'static str {
        match text_type {
            TextType::Link => "Link",
            TextType::Bold => "Bold",
            TextType::Italic => "Italic",
            TextType::Strike => "Strike",
        }
    }
}

pub struct AutoComplete;

impl AutoComplete {
    pub fn get_suggestions(input: &str) -> Vec<&'static str> {
        let input_lower = input.to_lowercase();
        let mut suggestions = Vec::new();
        
        match input_lower.as_str() {
            "p" => suggestions.push("Paragraph"),
            "h" => {
                suggestions.extend_from_slice(&["Heading1", "Heading2", "Heading3", "Heading4"]);
            }
            "u" => suggestions.push("UList"),
            "o" => suggestions.push("OList"),
            "l" => suggestions.extend_from_slice(&["Link", "UList", "OList", "TodoList"]),
            "r" => suggestions.push("Row"),
            "c" => suggestions.push("CodeBlock"),
            "q" => suggestions.push("Quote"),
            "t" => suggestions.extend_from_slice(&["Table", "TodoList"]),
            _ => {}
        }
        
        suggestions
    }
}

fn main() {
    println!("HMD - Hierarchical Markdown Language");
    
    let mut document = HmdDocument::new();
    
    // Test parser with parent_id functionality
    println!("\n=== Parser Test with Parent ID ===");
    let test_lines = [
        "Paragraph : Hello World",
        "H1 : This is a heading", 
        "P : Short paragraph",
        "UL : List item",
        "P : Another paragraph",
    ];
    
    let mut node_ids = Vec::new();
    for (i, line) in test_lines.iter().enumerate() {
        if let Some(node) = HmdParser::parse_hmd_line(line, i + 1) {
            let node_id = document.add_node(node);
            node_ids.push(node_id.clone());
            
            let stored_node = document.flat_tree.get(&node_id).unwrap();
            println!("Line {}: {} -> Node ID: {}", i + 1, line, node_id);
            println!("  Parent ID: {:?}", stored_node.parent_id);
            
            if let Some(parent) = document.get_parent_node(&node_id) {
                println!("  Parent text: '{}'", parent.text);
            }
            println!();
        }
    }
    
    // Test parent-child relationship queries
    println!("=== Parent-Child Relationship Test ===");
    for node_id in &node_ids {
        let node = document.flat_tree.get(node_id).unwrap();
        println!("Node {} (line {}): '{}'", node_id, node.line_number, node.text);
        
        if let Some(parent_node) = document.get_parent_node(node_id) {
            println!("  Has parent: '{}'", parent_node.text);
        } else {
            println!("  No parent (root node)");
        }
    }
    
    // Test auto-completion
    println!("\n=== Auto-completion Test ===");
    let test_inputs = ["p", "h", "l"];
    for input in test_inputs {
        let suggestions = AutoComplete::get_suggestions(input);
        println!("{} -> {:?}", input, suggestions);
    }
    
    // Test text details parsing
    println!("\n=== Text Details Test ===");
    let details = HmdParser::parse_text_details("Hello World", "Link[0:4] Bold[6:10]");
    for detail in details {
        println!("{:?}", detail);
    }
}
