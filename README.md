# Haskell Design Patterns Guide

A comprehensive guide to Haskell design patterns, from basic to advanced. This guide is designed to be viewed as a GitHub Pages site.

## 📚 Contents

### Basic Patterns
- **Functor** - Mapping over computational contexts
- **Applicative** - Applying functions in contexts
- **Monad** - Sequencing computations with context
- **Arrow** - Composing computations as arrows
- **Monad Transformer** - Stacking monadic effects
- **Typeclasses** - Ad-hoc polymorphism and abstraction
- **Comonad** - Dual of monads for context-dependent computations

### Advanced Patterns
- **Tagless Final** - Type-safe embedded DSLs
- **Free Monad** - Separating structure from interpretation
- **Lenses / Prisms** - Functional references and optics
- **Algebraic Effects** - Modular effect handling
- **GADTs** - Generalized Algebraic Data Types
- **Arrowized FRP** - Functional Reactive Programming with Arrows
- **HKD** - Higher-Kinded Data patterns
- **Type-Level Programming** - Computing with types
- **Streaming Abstractions** - Iteratee, Conduit, and Pipes

## 🚀 Viewing the Guide

### Option 1: GitHub Pages (Recommended)

1. Fork this repository to your GitHub account
2. Go to repository Settings → Pages
3. Under "Source", select the `main` branch
4. Click Save
5. Your site will be published at `https://yalamaddisasiKumar.github.io/hs-tour/`

### Option 2: Local Jekyll Server

1. Install Ruby and Bundler:
   ```bash
   # On Ubuntu/Debian
   sudo apt-get install ruby-full build-essential zlib1g-dev
   
   # On macOS
   brew install ruby
   ```

2. Install Jekyll and dependencies:
   ```bash
   gem install jekyll bundler
   ```

3. Create a Gemfile in the project root:
   ```ruby
   source 'https://rubygems.org'
   gem 'jekyll', '~> 4.3'
   gem 'jekyll-theme-cayman'
   ```

4. Install dependencies:
   ```bash
   bundle install
   ```

5. Run the local server:
   ```bash
   bundle exec jekyll serve
   ```

6. Open your browser to `http://localhost:4000/hs-tour/`

### Option 3: Simple File Viewing

You can also view the Markdown files directly in your editor or on GitHub, though you'll miss the styling and navigation.

## 🛠️ Project Structure

```
hs-tour/
├── _config.yml              # Jekyll configuration
├── index.md                 # Home page
├── _layouts/
│   ├── default.html         # Default layout template
│   └── pattern.html         # Pattern page layout
├── _basic/
│   ├── functor.md
│   ├── applicative.md
│   ├── monad.md
│   ├── arrow.md
│   ├── monad-transformer.md
│   ├── typeclasses.md
│   └── comonad.md
├── _advanced/
│   ├── tagless-final.md
│   ├── free-monad.md
│   ├── lenses-prisms.md
│   ├── algebraic-effects.md
│   ├── gadts.md
│   ├── arrowized-frp.md
│   ├── hkd.md
│   ├── type-level-programming.md
│   └── streaming-abstractions.md
├── assets/
│   └── css/
│       └── style.css        # Custom styles
└── README.md                # This file
```

## 🎨 Customization

### Updating the Configuration

Edit `_config.yml` to customize:
- Site title and description
- Base URL (change from `/hs-tour` to your repo name)
- Your GitHub username

```yaml
title: Your Custom Title
description: Your custom description
baseurl: "/your-repo-name"
url: "https://yalamaddisasiKumar.github.io"
```

### Modifying Styles

Edit `assets/css/style.css` to customize colors, fonts, and layout.

### Adding New Patterns

1. Create a new Markdown file in `_basic/` or `_advanced/`
2. Add the front matter:
   ```yaml
   ---
   title: Pattern Name
   ---
   ```
3. Write your content using Markdown
4. Add a link to the pattern in `index.md`

## 📖 Pattern Page Structure

Each pattern page includes:
- **Overview** - What the pattern is and why it matters
- **Core Concepts** - Key ideas and terminology
- **Type Signatures** - Essential type definitions
- **Examples** - Practical code examples
- **Common Use Cases** - When and where to apply the pattern
- **Related Patterns** - Connections to other patterns
- **Best Practices** - Guidelines for effective use
- **Further Reading** - External resources

## 🤝 Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-pattern`)
3. Make your changes
4. Commit your changes (`git commit -am 'Add new pattern'`)
5. Push to the branch (`git push origin feature/new-pattern`)
6. Create a Pull Request

### Content Guidelines

- Use clear, concise language
- Provide complete, runnable code examples
- Include both simple and complex examples
- Cite sources and external resources
- Follow the existing pattern structure

## 📝 License

This project is licensed under the MIT License - see below:

```
MIT License

Copyright (c) 2025 Haskell Design Patterns Guide Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## 🔗 Resources

### Learning Haskell
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Programming from First Principles](https://haskellbook.com/)

### Documentation
- [Haskell.org](https://www.haskell.org/)
- [Hackage](https://hackage.haskell.org/) - Package repository
- [Hoogle](https://hoogle.haskell.org/) - Type-based search engine

### Community
- [Haskell Discourse](https://discourse.haskell.org/)
- [r/haskell](https://www.reddit.com/r/haskell/)
- [Haskell IRC](https://wiki.haskell.org/IRC_channel)

## ❓ FAQ

**Q: Do I need to know Haskell to use this guide?**
A: Basic Haskell knowledge is recommended. Start with the Basic Patterns section.

**Q: Can I use this for my own learning or teaching?**
A: Absolutely! That's what it's for. Feel free to fork, modify, and share.

**Q: How do I report errors or suggest improvements?**
A: Open an issue on GitHub or submit a pull request.

**Q: Why isn't pattern X included?**
A: This guide covers common and important patterns. If you think something is missing, please contribute!

## 🙏 Acknowledgments

This guide is inspired by and draws from many excellent resources in the Haskell community, including:
- The Haskell Wiki
- Typeclassopedia
- Various academic papers and blog posts
- The broader functional programming community

## 📧 Contact

For questions or feedback, please open an issue on GitHub.

---

**Happy Haskell Learning!** 🎉
