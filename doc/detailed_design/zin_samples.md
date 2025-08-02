# Zin Language Sample Code

This document contains comprehensive examples demonstrating all features of the zin markup language.

## 1. Basic Text Elements

### Paragraphs

```zin
p : This is a simple paragraph in zin.
p : This is another paragraph with multiple sentences. It can contain any text content.
```

or

```zin
p : This is a simple paragraph in zin.
  : This is another paragraph with multiple sentences. It can contain any text content.
```

### Headers

```zin
h1 : Main Title
h2 : Section Title
h3 : Subsection Title
h4 : Minor Heading
```

## 2. Text Styling

### Basic Styling

```zin
p bold[0:4] : Bold text example
p italic[0:6] : Italic text example
p strike[0:6] : Strike through text
```

### Links

```zin
p link[0:5] url[https://example.com] : Click here to visit example
p link[0:10] url[https://github.com] : Visit GitHub for more projects
```

### Multiple Styles Combined

```zin
p bold[0:5] italic[6:11] strike[12:18] : Hello world sample text
p link[0:4] url[https://zin-lang.org] bold[0:4] italic[5:9] : zin docs here
```

### Multi-line Style Definitions

```zin
p
  bold[0:5]
  italic[6:11]
  link[12:16] url[https://example.com]
   : Hello world test link
```

## 3. Lists

### Unordered Lists

```zin
ul : First item
   : Second item
   : Third item with more content
```

### Ordered Lists

```zin
ol : Step one of the process
   : Step two with details
   : Final step
```

### Todo Lists

```zin
tl : Complete the documentation
   : Test all features
   : Deploy the compiler
```

## 4. Code Blocks

### Simple Code Block

```zin
cb : print("Hello, World!")
python : def greet(name):
       :     return f"Hello, {name}!"
       :
       : greet("zin")
```

### JavaScript Example

```zin
cb : const factorial = (n) => n <= 1 ? 1 : n * factorial(n - 1);
javascript : console.log(factorial(5));
```

### JSON Data Example

```zin
cb : ["a", "b", "c"]
json : {
     :   "name": "zin",
     :   "version": "1.0.0",
     :   "type": "compiler"
     : }
```

### Zin Language Example

```zin
cb : p bold[0:5] : Hello World
zin : h1 : Documentation
    : ul : Item one
    :    : Item two
```

## 5. Quotes

### Simple Quote

```zin
q : The best way to predict the future is to invent it.
```

### Styled Quote

```zin
q italic[0:4] bold[5:11] : Life is what happens when you're busy making other plans.
```

### Multi-line Quote

```zin
q : Programming is not about typing,
  : it's about thinking.
```

## 6. Tables

### Simple Table

```zin
t : | Name | Age | City |
r : | Alice | 25 | Tokyo |
r : | Bob | 30 | Osaka |
r : | Charlie | 28 | Kyoto |
```

### Product Catalog Table

```zin
t : | Product | Price | Stock | Description |
r : | Laptop | $999 | 15 | High-performance laptop |
r : | Mouse | $25 | 50 | Wireless optical mouse |
r : | Keyboard | $75 | 30 | Mechanical keyboard |
```

## 7. Mixed Content Examples

### Article with Various Elements

```zin
h1 : Getting Started with Zin

p : Welcome to the zin markup language documentation.
  : This guide will help you understand all the features.

h2 : Basic Syntax

p link[0:8] url[https://zin-docs.com] bold[0:8] : Zin docs provide comprehensive examples.

ul : Simple and intuitive syntax
   : Powerful styling capabilities
   : Multiple output formats supported

h3 : Code Example

cb : p bold[0:5] : Hello World
zin : h1 : My Document
    : ul : Feature one
    :    : Feature two

q italic[0:8] : Remember: simplicity is the ultimate sophistication.
```

### Technical Documentation Sample

```zin
h1 : API Reference

h2 : Authentication

p : All API requests require authentication using an API key.

cb : curl -H "Authorization: Bearer YOUR_API_KEY"
bash :      -X GET https://api.example.com/users

h3 : Response Format

t : | Field | Type | Description |
r : | id | integer | Unique identifier |
r : | name | string | User's full name |
r : | email | string | User's email address |

h2 : Error Codes

ol : 400 - Bad Request
   : 401 - Unauthorized
   : 404 - Not Found
   : 500 - Internal Server Error
```

### Blog Post Example

```zin
h1 bold[0:15] : The Future of Web Development

p italic[0:6] : Posted on March 15, 2024

p : Web development continues to evolve at a rapid pace.
  : New frameworks and tools emerge constantly.

h2 : Key Trends

ul : Server-side rendering revival
   : Static site generation
   : Progressive Web Apps
   : WebAssembly adoption

h3 : Framework Comparison

t : | Framework | Performance | Learning Curve | Community |
r : | React | High | Medium | Large |
r : | Vue | High | Low | Medium |
r : | Svelte | Very High | Low | Growing |

q : The best framework is the one that solves your specific problem effectively.

h2 : Code Example

cb : import { createApp } from 'vue'
javascript : import App from './App.vue'
           :
           : createApp(App).mount('#app')

p link[0:9] url[https://github.com/example] : Check out our sample project for more examples.
```

## 8. Advanced Styling Combinations

### Complex Text Formatting

```zin
p bold[0:4] italic[5:9] strike[10:16] link[17:21] url[https://example.com] : Bold text sample here
```

### Nested Style Example

```zin
h2 link[0:15] url[https://tutorials.com] bold[0:15] : Complete Tutorial Guide

p : This paragraph demonstrates complex styling with
bold[0:4] italic[17:29] link[41:45] url[https://docs.com]
  : This text multiple styles docs.
```

## 9. Configuration Examples

### Setting File Sample (setting.zin)

```zin
cb : // Basic configuration
zin : MODE = default
    : OUTPUT = html
    :
    : // Custom tag definitions
    : CUSTOM_TAG = warning -> div class="alert warning"
    : CUSTOM_TAG = info -> div class="alert info"
```

## 10. Comments and Documentation

```zin
// This is a comment - it won't appear in output
h1 : Document Title

// Multiple comment lines
// to explain complex sections
p : Regular paragraph content.

// TODO: Add more examples
ul : First item
   : Second item
```

This comprehensive sample demonstrates all major features of the zin markup language including basic text elements, styling, lists, code blocks, quotes, tables, and advanced combinations.
